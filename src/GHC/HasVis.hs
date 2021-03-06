{-# LANGUAGE CPP #-}

module GHC.HasVis (vis, view, clear, close) where

-- Imports for networking
import qualified Network.Socket as SK
import qualified Network.WebSockets as WSK
import qualified Data.ByteString.Lazy.Char8 as BStr
-- Imports for HeapView
import qualified GHC.HeapView as HV
import qualified Data.IntMap as IM
-- Imports for JSON conversion
import qualified Data.Aeson as Aeson
import qualified Data.HashMap.Strict as HM
import Data.Scientific (scientific, floatingOrInteger)
import qualified Data.Text as T
import qualified Data.Vector as Vec
-- Other
import qualified System.IO as IO
import System.Process (createProcess, CreateProcess(std_out, std_err), proc, StdStream(CreatePipe))
import Control.Exception
import Control.Concurrent
import Control.Monad
import System.IO.Unsafe
import Data.Maybe (fromJust, isJust)
import Unsafe.Coerce (unsafeCoerce)
import Data.List (elemIndex, sortBy, (\\))

-- Whether the visualisation is currently running
visRunning :: MVar Bool
visRunning = unsafePerformIO (newMVar False)

-- Messages for the main thread to act upon
data Message = ViewMessage HV.Box String  -- visualise the boxed value
             | ClearMessage               -- clear the visualisation
             | CloseMessage               -- close the visualisation

-- Messages from GHCi and the listener to the main thread
messageChan :: Chan Message
messageChan = unsafePerformIO (newChan :: IO (Chan Message))

-- Start the main thread
vis :: IO ()
vis = do
    alreadyRunning <- swapMVar visRunning True
    if alreadyRunning then
        IO.putStrLn "Visualisation is already running."
    else
        (void $ forkIO mainThread)

-- Visualise an expression using the given label
view :: a -> String -> IO ()
view a label = sendOnChan $ ViewMessage (HV.asBox a) label

-- Clear the visualisation
clear :: IO ()
clear = sendOnChan ClearMessage

-- Close the browser window
close :: IO ()
close = sendOnChan CloseMessage

-- Send a message to the main thread
sendOnChan :: Message -> IO ()
sendOnChan m = do
    isRunning <- readMVar visRunning
    if isRunning then
        writeChan messageChan m
    else
        IO.putStrLn "HasVis not running. Initiate with \":vis\"."

port :: Int
port = 56250

-- This thread acts as the intermediary between GHCi and the browser.
-- It is responsible for taking commands from both and processing the data
-- to be visualised.
mainThread :: IO ()
mainThread = SK.withSocketsDo $ do
    -- Open listen socket
    sock <- WSK.makeListenSocket "127.0.0.1" port
    -- Start the browser
    launchBrowser "ui.html"
    IO.putStrLn "If the browser did not open automatically, open \"ui.html\" to view the application."
    -- Wait for the browser to ask to connect
    (pending, _) <- SK.accept sock
    -- Change back to the websockets interface
    pending' <- WSK.makePendingConnection pending WSK.defaultConnectionOptions
    -- Acknowledge request
    browser <- WSK.acceptRequest pending'
    -- Set up a thread to listen for incoming messages
    forkIO $ listenThread browser
    -- Begin reading messages (catch exceptions so we can exit cleanly)
    catch (mainLoop browser) $ \e -> do
        return (e :: SomeException)
        return ()
    -- Quit
    swapMVar visRunning False
    IO.putStrLn "Visualisation has closed. Call :vis again to re-initiate."
    SK.close sock

-- Act on messages from GHCi and the browser
mainLoop :: WSK.Connection -> IO ()
mainLoop browser = do
    message <- readChan messageChan
    case message of
        ViewMessage box label -> do
            -- Build the heap graph to depth of 1000; we're likely to get
            -- performance problems long before hitting this limit.
            graph <- HV.buildHeapGraph 1000 () box
            -- Construct a simplified JSON version and send it to the browser
            let jsGraph = buildJSGraph graph label
            WSK.sendDataMessage browser (WSK.Text (Aeson.encode jsGraph))
            mainLoop browser
        ClearMessage -> do
            let obj = Aeson.Object
                    $ HM.insert (T.pack "action") (Aeson.toJSON "clear")
                    $ HM.empty
            WSK.sendDataMessage browser (WSK.Text (Aeson.encode obj))
            mainLoop browser
        CloseMessage -> do -- Quit
            let obj = Aeson.Object
                    $ HM.insert (T.pack "action") (Aeson.toJSON "close")
                    $ HM.empty
            WSK.sendDataMessage browser (WSK.Text (Aeson.encode obj))

-- Recieve messages from the browser and add them to the channel
listenThread :: WSK.Connection -> IO ()
listenThread browser = do
    result <- try $ do
        msg <- WSK.receiveDataMessage browser
        case msg of
            WSK.Text bstr -> do
                let str = BStr.unpack bstr
                -- TODO: This is a temporary test of communication
                case str of
                    "close" -> sendOnChan CloseMessage
                    _ -> IO.hPutStrLn IO.stderr "Unknown command received."
            _ -> IO.hPutStrLn IO.stderr "Unexpected message format received."
    case result of
        Left (WSK.ParseException e) -> do
            IO.putStrLn "Received malformed message from the UI."
            sendOnChan CloseMessage
        Left WSK.ConnectionClosed -> do
            IO.putStrLn "The connection was unexpectedly closed."
            sendOnChan CloseMessage
        -- Connection terminated normally
        Left (WSK.CloseRequest _ _) -> do
            IO.putStrLn "Connection closed."
            sendOnChan CloseMessage
        -- Wait for next message
        Right _ -> listenThread browser

-- Launch web app in default web browser
launchBrowser :: String -> IO ()
launchBrowser url = forkIO (do
        (_, _, _, _) <- createProcess (proc
#if WINDOWS
            "start"
#elif MAC
            "open"
#else
            "xdg-open"
#endif
            [url]) {std_out = CreatePipe, std_err = CreatePipe} -- Throw output away
        return ()
    ) >> return ()

type HGIndex = HV.HeapGraphIndex
type Closure = HV.GenClosure (Maybe HGIndex)
type Node = Aeson.Object
type Edge = Aeson.Object

-- Turn the messy HeapView graph into a minimal, clean JSON representation that
-- omits unnecessary information.
buildJSGraph :: HV.HeapGraph () -> String -> Aeson.Value
buildJSGraph graph label = let
        startNodeIndex = -1 :: Int
        startNode = HM.insert (T.pack "name") (Aeson.toJSON $ "Expression: " ++ label)
                  $ HM.insert (T.pack "id") (Aeson.toJSON startNodeIndex)
                  $ HM.insert (T.pack "ptrCount") (Aeson.toJSON (1::Int))
                  $ HM.empty
        -- Build (most of) the graph.
        (allNodes, someEdges, findEdges, visitedAcc) =
            innerBuild graph (Just HV.heapGraphRoot) startNodeIndex 0 []
        -- Give the final list of nodes to the thunk which will search for the
        -- remaining edges to be shown.
        allEdges = someEdges ++ fst (findEdges allNodes visitedAcc)
        -- For each node, record the number of edges coming from it
        allNodesWithCount = map (\n -> let
                iD = getIntAttr "id" n
            in
                HM.insert (T.pack "ptrCount") (Aeson.toJSON $ length $ filterEdgesWith "source" iD allEdges) n
            ) allNodes
        -- Sort the cons nodes by biggest first so the merging happens in the right order
        biggestFirst a b = getIntAttr "id" b `compare` getIntAttr "id" a
        consNodes = filter (\n -> getName n == T.pack ":") $ sortBy biggestFirst allNodesWithCount
        -- This huge nasty function takes the graph and returns a new graph with the
        -- children of the given cons node merged together if they have no other parents.
        mergeChildren :: [Node] -> [Edge] -> Node -> ([Node], [Edge])
        mergeChildren nodes edges thisNode = let
                myID = getIntAttr "id" thisNode
                myPtrCount = getIntAttr "ptrCount" thisNode
                myEdges = filterEdgesWith "source" myID edges
                childNodes = filter (\n -> getIntAttr "id" n `elem` (map (getIntAttr "target") myEdges)) nodes
                mergeables = filter (
                    \n -> let iD = getIntAttr "id" n in
                            iD /= myID
                         && length (filterEdgesWith "target" iD edges) == 1
                         && (length (filterEdgesWith "source" iD edges) == 0 || (isJust $ HM.lookup (T.pack "isCons") n))) childNodes
                edgesToMergeables = concat $ map (\n -> filterEdgesWith "target" (getIntAttr "id" n) edges) mergeables
                edgesFromMergeables = map (\n -> filterEdgesWith "source" (getIntAttr "id" n) edges) mergeables
                myNewPtrCount = myPtrCount - length mergeables + length (concat edgesFromMergeables)
                -- Iterate over the existing edges of this node and the edges to be merged
                -- and renumber them into a new contiguous sequence.
                myNewEdges = renumberEdges 0 0
                renumberEdges iOrig iNew = if iOrig == myPtrCount then [] else
                    if currEdge `elem` edgesToMergeables then let
                            mergeI = fromJust $ elemIndex currEdge edgesToMergeables
                            mergeableEdges = edgesFromMergeables !! mergeI
                            mergeableCount = length mergeableEdges
                        in
                            (renumberM iNew mergeableEdges) ++ renumberEdges (iOrig + 1) (iNew + mergeableCount)
                    else
                        (HM.insert (T.pack "ptrIndex") (Aeson.toJSON iNew) currEdge) : renumberEdges (iOrig + 1) (iNew + 1)
                    where
                        currEdge = head $ filterEdgesWith "ptrIndex" iOrig myEdges
                        -- Renumber the edges of the mergeable node and change their owner
                        renumberM startI edges = map (\(i,e) ->
                            HM.insert (T.pack "source") (Aeson.toJSON myID)
                          $ HM.insert (T.pack "ptrIndex") (Aeson.toJSON i)
                            e) (zip [startI..] edges)

                -- Extract the text from the children
                (leftText, rightText) = case length mergeables of
                    0 -> (T.pack "_",T.empty)
                    1 -> if getIntAttr "ptrIndex" m1Edge == 0 then
                            (m1Name, T.empty)
                        else if T.head m1Name == '[' then
                            (T.pack "_", T.tail m1Name)
                        else
                            (T.pack "_", m1Name)
                        where m1 = head mergeables
                              m1Name = getName m1
                              m1Edge = head edgesToMergeables
                    2 -> if getIntAttr "ptrIndex" m1Edge == 0 then
                            if T.head m2Name == '[' then
                                (m1Name, T.tail m2Name)
                            else
                                (m1Name, m2Name)
                        else
                            if T.head m1Name == '[' then
                                (m2Name, T.tail m1Name)
                            else
                                (m2Name, m1Name)
                        where m1 = mergeables !! 0
                              m2 = mergeables !! 1
                              m1Name = getName m1
                              m2Name = getName m2
                              m1Edge = edgesToMergeables !! 0
                -- Merge the text
                name = '[' `T.cons` leftText `T.append` (',' `T.cons` rightText)
                newNode = HM.insert (T.pack "name") (Aeson.toJSON name)
                        $ HM.insert (T.pack "ptrCount") (Aeson.toJSON myNewPtrCount)
                          thisNode
            in (newNode : (nodes \\ (thisNode:mergeables)), myNewEdges ++ ((edges \\ myEdges) \\ concat edgesFromMergeables))
        -- Do the merging
        (finalNodes, finalEdges) = foldl (\(n,e) c -> mergeChildren n e c) (allNodesWithCount, allEdges) consNodes
    in
        Aeson.toJSON (startNode : finalNodes, finalEdges)

-- Recursively build the JSON graph for the subgraph starting at the given node, avoiding already-visited nodes
innerBuild :: HV.HeapGraph ()
    -> Maybe HGIndex -- The starting node
    -> HGIndex -- The parent that we took to get here
    -> Int -- The index of this node in the parent's pointers
    -> [HGIndex] -- Accumulator of visited nodes
    -> ([Node], [Edge], ([Node] -> [HGIndex] -> ([Edge], [HGIndex])), [HGIndex]) -- Return the subgraph, a closure to generate the rest of the edges, and the accumulator
innerBuild graph myPtr parentID myIndex visitedAcc = case myPtr of
  Just iD -> let
      closure = HV.hgeClosure $ fromJust $ HV.lookupHeapGraph iD graph
      makeObjectWithName name =
          HM.insert (T.pack "name") (Aeson.toJSON name)
        $ HM.insert (T.pack "id") (Aeson.toJSON iD)
        $ HM.empty
      makeNodeAndRecurse name ptrs = (n, e, f, v)-- Recurse again normally
        where (n, e, f, v, _) = foldl chainBuilds ([makeObjectWithName name],
                                [makeEdgeI parentID iD myIndex],
                                nullFind,
                                iD : visitedAcc,
                                0) ptrs
      makeNodeAndFind name ptrs = -- Switch to "find edges" mode
        ([makeObjectWithName name],
         [makeEdgeI parentID iD myIndex],
         findEdges,
         iD : visitedAcc)
        where
          findEdges :: [Node] -> [HGIndex] -> ([Edge], [HGIndex])
          findEdges n v = let
              (edges, visitedAcc) = (foldl combineFind nullFind
                $ map (\p -> findEdgesToExisting graph p iD) ptrs) n v
            in -- Add indices to the found edges
              (map (uncurry addPtrIndex) (zip [0..] edges), visitedAcc)
    in
      if iD `elem` visitedAcc then
        ([], [makeEdgeI parentID iD myIndex], nullFind, visitedAcc)
      else case closure of
        HV.ConsClosure _ ptrs dArgs pkg modl name -> let
            returnDeadEnd node =
              ([node], [makeEdgeI parentID iD myIndex], nullFind, iD : visitedAcc)
          in case (pkg, modl, name) of
            ("ghc-prim", "GHC.Types", "I#") -> returnDeadEnd
              $ HM.insert (T.pack "name")
                (Aeson.toJSON $ show $ (unsafeCoerce $ dArgs !! 0 :: Int))
              $ HM.insert (T.pack "id") (Aeson.toJSON iD)
              $ HM.empty
            ("ghc-prim", "GHC.Types", "C#") -> returnDeadEnd
              $ HM.insert (T.pack "name")
                (Aeson.toJSON $ show $ (unsafeCoerce $ dArgs !! 0 :: Char))
              $ HM.insert (T.pack "id") (Aeson.toJSON iD)
              $ HM.empty
            -- TODO: Float and double ignored since they need to be handled differently depending on the "word" size
            otherwise -> let
                (nodes, edges, findEdges, v, _) =
                    foldl chainBuilds ([], [], nullFind, iD : visitedAcc, 0) ptrs
                n = makeObjectWithName name
                thisNode = case (pkg, modl, name) of
                  ("ghc-prim", "GHC.Types", ":") ->
                    HM.insert (T.pack "isCons") (Aeson.Bool True) n
                  otherwise -> n
                thisEdge = makeEdgeI parentID iD myIndex
              in (thisNode : nodes, thisEdge : edges, findEdges, v)
        HV.ThunkClosure _ ptrs _ -> makeNodeAndFind "Thunk" ptrs
        HV.APClosure _ _ _ fun ptrs -> makeNodeAndFind "Thunk" (fun : ptrs)
        HV.PAPClosure _ _ _ fun ptrs -> makeNodeAndFind "Partial AP" (fun : ptrs)
        HV.APStackClosure _ fun ptrs -> makeNodeAndFind "Thunk" (fun : ptrs)
        HV.BCOClosure _ instrs lits ptrs _ _ _ -> makeNodeAndFind "Bytecode" [instrs, lits, ptrs]
        HV.ArrWordsClosure _ _ _ -> makeNodeAndFind "Array" []
        HV.FunClosure _ ptrs _ -> makeNodeAndFind "Function" ptrs
        HV.MutArrClosure _ _ _ ptrs -> makeNodeAndRecurse "Mut array" ptrs
        HV.MutVarClosure _ ptr -> makeNodeAndRecurse "MVar" [ptr]
        HV.SelectorClosure _ ptr -> let
                (nodes, edges, findEdges, v) =
                    innerBuild graph ptr iD 0 (iD : visitedAcc)
            in
                (makeObjectWithName "Selector" : nodes, makeEdgeI parentID iD myIndex : edges, findEdges, v)
        HV.IndClosure _ ptr -> -- Ignore and keep going
            innerBuild graph ptr parentID myIndex (iD : visitedAcc)
        HV.BlackholeClosure _ ptr ->  -- Ignore and keep going
            innerBuild graph ptr parentID myIndex (iD : visitedAcc)
        otherwise -> ([], [], nullFind, iD : visitedAcc)
    where
    chainBuilds (nodes, edges, findEdges, v, i) ptr =
        let (n, e, f, v1) = innerBuild graph ptr iD i v
        in (n ++ nodes, e ++ edges, combineFind findEdges f, v1, i+1)
    combineFind f1 f2 = \n v -> let
            (e1, v1) = f1 n v
            (e2, v2) = f2 n v1
        in
            (e2 ++ e1, v2)
  Nothing -> ([], [], nullFind, visitedAcc)
  where
    nullFind = (\n v -> ([], v))

-- Find the edges in this subgraph that point back to nodes that have already been added to the graph.
-- This should be called with a COMPLETE list of nodes for the graph.
findEdgesToExisting :: HV.HeapGraph () -> Maybe HGIndex -> HGIndex -> [Node] -> [HGIndex] -> ([Edge], [HGIndex])
findEdgesToExisting graph myPtr originID nodes visitedAcc = case myPtr of
    Just iD -> let
            closure = HV.hgeClosure $ fromJust $ HV.lookupHeapGraph iD graph
        in
            if any (hasID iD) nodes then -- If any nodes have this ID
                ([makeEdge originID iD], visitedAcc)
            else if iD `elem` visitedAcc then -- If we've otherwise been here
                ([], visitedAcc)
            else case closure of
                HV.ConsClosure _ ptrs _ _ _ _ ->    processChildren ptrs
                HV.ThunkClosure _ ptrs _ ->         processChildren ptrs
                HV.APClosure _ _ _ fun ptrs ->        processChildren (fun : ptrs)
                HV.PAPClosure _ _ _ fun ptrs ->       processChildren (fun : ptrs)
                HV.APStackClosure _ fun ptrs ->       processChildren (fun : ptrs)
                HV.BCOClosure _ instrs lits ptrs _ _ _ ->   processChildren [instrs, lits, ptrs]
                HV.ArrWordsClosure _ _ _ ->         ([], iD : visitedAcc)
                HV.FunClosure _ ptrs _ ->           processChildren ptrs
                HV.MutArrClosure _ _ _ ptrs ->      processChildren ptrs
                HV.MutVarClosure _ ptr ->           processChildren [ptr]
                HV.SelectorClosure _ ptr ->         processChildren [ptr]
                HV.IndClosure _ ptr ->              processChildren [ptr]
                HV.BlackholeClosure _ ptr ->        processChildren [ptr]
                otherwise ->                        ([], iD : visitedAcc)
        where
            hasID iD node = getIntAttr "id" node == iD
            chainFinds (edges, v) ptr =
                let (e, v1) = findEdgesToExisting graph ptr originID nodes v
                in (e ++ edges, v1)
            processChildren ptrs = foldl chainFinds ([], iD : visitedAcc) ptrs
    Nothing -> ([], visitedAcc)

getName :: Node -> T.Text
getName node = case node HM.! (T.pack "name") of
  Aeson.String a -> a

-- Retrieve the attribute of type Int with the given name
getIntAttr :: String -> Node -> Int
getIntAttr attr node = case node HM.! (T.pack attr) of
  Aeson.Number a -> case floatingOrInteger a of
    Right i -> i

makeEdge :: HGIndex -> HGIndex -> Aeson.Object
makeEdge a b = HM.insert (T.pack "source") (Aeson.toJSON a)
             $ HM.insert (T.pack "target") (Aeson.toJSON b)
             $ HM.empty

addPtrIndex :: Int -> Aeson.Object -> Aeson.Object
addPtrIndex i e = HM.insert (T.pack "ptrIndex") (Aeson.toJSON i) e

-- Make an edge between 'a' and 'b', and assign the pointer index 'i'
makeEdgeI :: HGIndex -> HGIndex -> Int -> Aeson.Object
makeEdgeI a b i = addPtrIndex i $ makeEdge a b

-- Filter edges from the list that have the given Int attribute
filterEdgesWith attr val edges = filter (\e -> getIntAttr attr e == val) edges
