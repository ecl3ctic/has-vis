{-# LANGUAGE CPP #-}

module GHC.HasVis (vis, view, update, clear) where

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
import Control.Exception
import Control.Concurrent
import Control.Monad
import System.IO.Unsafe
import Data.Maybe (fromJust, isJust)
import Unsafe.Coerce (unsafeCoerce)
import Data.List (sortBy, (\\))

-- TODO: Test HasVis on Windows
#if WINDOWS
import Foreign.C.String (CString, withCString)
#else
import qualified System.Process as Proc
#endif

(+++) = T.append

-- Whether the visualisation is currently running
visRunning :: MVar Bool
visRunning = unsafePerformIO (newMVar False)

-- Messages for the main thread to act upon
data Message = ViewMessage HV.Box String   -- visualise the boxed value
             | UpdateMessage            -- update the visualisation
             | ClearMessage             -- clear the visualisation
             | CloseMessage             -- close the visualisation

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

-- Update the visualisation
-- This is needed if values are changed from GHCi's side
update :: IO ()
update = sendOnChan UpdateMessage

-- Clear the visualisation
clear :: IO ()
clear = sendOnChan ClearMessage

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
-- It is responsible for taking commands from both and manipulating the data
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
    -- Begin reading messages
    mainLoop browser
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
            -- Build the heap graph
            graph <- HV.buildHeapGraph 1000 () box
            -- Construct a simplified JSON version and send it to the browser
            let jsGraph = buildJSGraph graph label
            WSK.sendDataMessage browser (WSK.Text (Aeson.encode jsGraph))
            mainLoop browser
        UpdateMessage -> mainLoop browser -- TODO
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

#if WINDOWS
foreign import ccall "launchBrowser"
    launchBrowser' :: CString -> IO ()
launchBrowser url = withCString url launchBrowser'
#else
launchBrowser url = forkIO (Proc.rawSystem
#if MAC
    "open"
#else
    "xdg-open"
#endif
    [url] >> return ()) >> return ()
#endif

type Index = HV.HeapGraphIndex
type Closure = HV.GenClosure (Maybe Index)
type Node = Aeson.Object
type Edge = Aeson.Object

-- Turn the messy HeapView graph into a minimal, clean JSON representation that
-- omits unnecessary information.
buildJSGraph :: HV.HeapGraph () -> String -> Aeson.Value
buildJSGraph graph label = let
        startNodeIndex = -1 :: Int
        startNode = HM.insert (T.pack "name") (Aeson.toJSON $ "Expression: " ++ label)
                  $ HM.insert (T.pack "id") (Aeson.toJSON startNodeIndex)
                  $ HM.empty
        -- Build (most of) the graph.
        (allNodes, someEdges, findEdges, visitedAcc) =
            innerBuild graph (Just HV.heapGraphRoot) startNodeIndex 0 []
        -- Give the final list of nodes to the thunk which will search for the
        -- remaining edges to be shown.
        allEdges = fst (findEdges allNodes visitedAcc) ++ someEdges
        -- Sort the cons nodes by biggest first so the merging happens in the right order
        biggestFirst a b = getIntAttr "id" b `compare` getIntAttr "id" a
        consNodes = filter (\n -> getName n == T.pack ":") $ sortBy biggestFirst allNodes
        -- This huge nasty function takes the graph and returns a new graph with the
        -- children of the given cons node merged together if they have no other parents.
        mergeChildren :: [Node] -> [Edge] -> Node -> ([Node], [Edge])
        mergeChildren nodes edges thisNode = let
                selectEdgesWith attr val = filter (\e -> getIntAttr attr e == val) edges
                myID = getIntAttr "id" thisNode
                myEdges = selectEdgesWith "source" myID
                childNodes = filter (\n -> getIntAttr "id" n `elem` (map (getIntAttr "target") myEdges)) nodes
                mergeables = filter (
                    \n -> let iD = getIntAttr "id" n in
                            iD /= myID
                         && length (selectEdgesWith "target" iD) == 1
                         && (length (selectEdgesWith "source" iD) == 0 || (isJust $ HM.lookup (T.pack "is-cons") n))) childNodes
                edgesToMergeables = concat $ map (\n -> selectEdgesWith "target" $ getIntAttr "id" n) mergeables
                edgesFromMergeables = concat $ map (\n -> selectEdgesWith "source" (getIntAttr "id" n)) mergeables
                -- Change the owner of the edges
                adjustedEdges = map (\e -> HM.insert (T.pack "source") (Aeson.toJSON myID) e) edgesFromMergeables
                -- Extract the text from the children
                (leftText, rightText) = case length mergeables of
                    0 -> (T.pack "_",T.pack "_")
                    1 -> if getIntAttr "ptr-index" m1Edge == 0 then
                            (m1Name, T.pack "_")
                        else if T.head m1Name == '[' then
                            (T.pack "_", T.dropEnd 1 $ T.tail m1Name)
                        else
                            (T.pack "_", m1Name)
                        where m1 = head mergeables
                              m1Name = getName m1
                              m1Edge = head edgesToMergeables
                    2 -> if getIntAttr "ptr-index" m1Edge == 0 then
                            if T.head m2Name == '[' then
                                (m1Name, T.dropEnd 1 $ T.tail m2Name)
                            else
                                (m1Name, m2Name)
                        else
                            if T.head m1Name == '[' then
                                (m2Name, T.dropEnd 1 $ T.tail m1Name)
                            else
                                (m2Name, m1Name)
                        where m1 = mergeables !! 0
                              m2 = mergeables !! 1
                              m1Name = getName m1
                              m2Name = getName m2
                              m1Edge = edgesToMergeables !! 0
                -- Merge the text
                name = '[' `T.cons` leftText +++ (',' `T.cons` rightText +++ (T.pack "]"))
                newNode = HM.insert (T.pack "name") (Aeson.toJSON name) thisNode
            in (newNode : (nodes \\ (thisNode:mergeables)), adjustedEdges ++ ((edges \\ edgesToMergeables) \\ edgesFromMergeables))
        -- Do the merging
        (finalNodes, finalEdges) = foldl (\(n,e) c -> mergeChildren n e c) (allNodes, allEdges) consNodes
    in
        Aeson.toJSON (startNode : finalNodes, finalEdges)

innerBuild :: HV.HeapGraph ()
    -> Maybe Index
    -> Index
    -> Int -- The index of this node in the parent's pointers
    -> [Index] -- Accumulator of visited nodes
    -> ([Node], [Edge], ([Node] -> [Index] -> ([Edge], [Index])), [Index]) -- Return the subgraph, a closure to generate the rest of the edges, and the accumulator
innerBuild graph myPtr parentID myIndex visitedAcc = case myPtr of
  Just iD -> let
      closure = HV.hgeClosure $ fromJust $ HV.lookupHeapGraph iD graph
      makeObjectWithName name =
          HM.insert (T.pack "name") (Aeson.toJSON name)
        $ HM.insert (T.pack "id") (Aeson.toJSON iD)
        $ HM.empty
      makeNodeAndRecurse name ptrs = (n, e, f, v)-- Recurse again normally
        where (n, e, f, v, _) = foldl chainBuilds ([makeObjectWithName name],
                                [makeEdge parentID iD myIndex],
                                nullFind,
                                iD : visitedAcc,
                                0) ptrs
      makeNodeAndFind name ptrs = -- Switch to "find edges" mode
        ([makeObjectWithName name],
         [makeEdge parentID iD myIndex],
         findEdges,
         iD : visitedAcc)
        where
          findEdges = foldl combineFind nullFind
            $ map (\p -> findEdgesToExisting graph p iD) ptrs
    in
      if iD `elem` visitedAcc then
        ([], [makeEdge parentID iD myIndex], nullFind, visitedAcc)
      else case closure of
        HV.ConsClosure _ ptrs dArgs pkg modl name -> let
            returnDeadEnd node =
              ([node], [makeEdge parentID iD myIndex], nullFind, iD : visitedAcc)
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
                    HM.insert (T.pack "is-cons") (Aeson.Bool True) n
                  otherwise -> n
                thisEdge = makeEdge parentID iD myIndex
              in (thisNode : nodes, thisEdge : edges, findEdges, v)
        HV.ThunkClosure _ ptrs _ -> makeNodeAndFind "Thunk" ptrs
        HV.APClosure _ _ _ _ ptrs -> makeNodeAndFind "Thunk" ptrs
        HV.PAPClosure _ _ _ _ ptrs -> makeNodeAndFind "Partial AP" ptrs
        HV.APStackClosure _ _ ptrs -> makeNodeAndFind "Thunk" ptrs
        HV.BCOClosure _ _ _ ptr _ _ _ -> makeNodeAndFind "Bytecode" [ptr]
        HV.ArrWordsClosure _ _ _ -> makeNodeAndFind "Array" []
        HV.FunClosure _ ptrs _ -> makeNodeAndFind "Function" ptrs
        HV.MutArrClosure _ _ _ ptrs -> makeNodeAndRecurse "Mut array" ptrs
        HV.MutVarClosure _ ptr -> makeNodeAndRecurse "MVar" [ptr]
        HV.SelectorClosure _ ptr -> let
                (nodes, edges, findEdges, v) =
                    innerBuild graph ptr iD 0 (iD : visitedAcc)
            in
                (makeObjectWithName "Selector" : nodes, makeEdge parentID iD myIndex : edges, findEdges, v)
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

-- This should be called on a COMPLETE list of nodes to be shown in the graph
findEdgesToExisting :: HV.HeapGraph () -> Maybe Index -> Index -> [Node] -> [Index] -> ([Edge], [Index])
findEdgesToExisting graph myPtr originID nodes visitedAcc = case myPtr of
    Just iD -> let
            closure = HV.hgeClosure $ fromJust $ HV.lookupHeapGraph iD graph
        in
            if any (hasID iD) nodes then -- If any nodes have this ID
                ([makeEdge originID iD (-1)], visitedAcc)
            else if iD `elem` visitedAcc then -- If we've otherwise been here
                ([], visitedAcc)
            else case closure of
                HV.ConsClosure _ ptrs _ _ _ _ ->    processChildren ptrs
                HV.ThunkClosure _ ptrs _ ->         processChildren ptrs
                HV.APClosure _ _ _ _ ptrs ->        processChildren ptrs
                HV.PAPClosure _ _ _ _ ptrs ->       processChildren ptrs
                HV.APStackClosure _ _ ptrs ->       processChildren ptrs
                HV.BCOClosure _ _ _ ptr _ _ _ ->   processChildren [ptr]
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

getIntAttr :: String -> Node -> Int
getIntAttr attr node = case node HM.! (T.pack attr) of
  Aeson.Number a -> case floatingOrInteger a of
    Right i -> i

makeEdge :: Index -> Index -> Int -> Aeson.Object
makeEdge a b i = HM.insert (T.pack "source") (Aeson.toJSON a)
             $ HM.insert (T.pack "target") (Aeson.toJSON b)
             $ HM.insert (T.pack "ptr-index") (Aeson.toJSON i)
             $ HM.empty

{- DEBUG printing (pkg, modl, name) of data constructors
buildJSGraph :: HV.HeapGraph () -> Aeson.Value
buildJSGraph (HV.HeapGraph im) = unsafePerformIO $ do
    IM.foldl (\a b -> printCons (HV.hgeClosure b) >> a) (return ()) im
    return Aeson.Null


printCons :: Closure -> IO ()
printCons c = case c of
    HV.ConsClosure info pArgs dArgs pkg modl name -> IO.putStrLn $ "Package [" ++ pkg ++ "] Module [" ++ modl ++ "] Name [" ++ name ++ "]"
    _ -> return ()
-}

----------------------------
-- ToJSON implementations --
----------------------------
{-
    Convert heap objects to JSON format. See
    http://hackage.haskell.org/package/ghc-heap-view-0.5.3/docs/GHC-HeapView.html
    and
    https://ghc.haskell.org/trac/ghc/wiki/Commentary/Rts/Storage/HeapObjects
    for an understanding of the closure types.

    Pointer fields are prefixed with "ptr" to make it easier to parse links in the graph.


instance Aeson.ToJSON b => Aeson.ToJSON (HV.GenClosure b) where
    toJSON closure = case closure of
        HV.ConsClosure info pArgs dArgs pkg modl name -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Data constructor")
            $ HM.insert (T.pack "ptrArgs") (Aeson.toJSON pArgs)
            $ HM.insert (T.pack "dataArgs") Aeson.Null --TODO: Parse using cons info?
            $ HM.insert (T.pack "package") (Aeson.toJSON pkg)
            $ HM.insert (T.pack "module") (Aeson.toJSON modl)
            $ HM.insert (T.pack "name") (Aeson.toJSON name)
            $ HM.empty
        HV.ThunkClosure info pArgs dArgs -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Thunk")
            $ HM.insert (T.pack "ptrArgs") (Aeson.toJSON pArgs)
            $ HM.insert (T.pack "dataArgs") Aeson.Null --TODO: Can we determine types?
            $ HM.empty
        HV.SelectorClosure info selectee -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Selector thunk")
            $ HM.insert (T.pack "ptrSelectee") (Aeson.toJSON selectee)
            $ HM.empty
        HV.IndClosure info indirectee -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Indirection")
            $ HM.insert (T.pack "ptrIndirectee") (Aeson.toJSON indirectee)
            $ HM.empty
        HV.BlackholeClosure info indirectee -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Black hole")
            $ HM.insert (T.pack "ptrIndirectee") (Aeson.toJSON indirectee)
            $ HM.empty
        HV.APClosure info _ nArgs fun args -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "AP Thunk")
            $ HM.insert (T.pack "ptrFun") (Aeson.toJSON fun)
            $ HM.insert (T.pack "args") Aeson.Null --TODO: Need bitmap to separate pointers from data
            $ HM.empty
        HV.PAPClosure info arity nArgs fun args -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "PA Function")
            $ HM.insert (T.pack "arity") (Aeson.toJSON arity)
            $ HM.insert (T.pack "ptrFun") (Aeson.toJSON fun)
            $ HM.insert (T.pack "args") Aeson.Null --TODO: Need bitmap to separate pointers from data
            $ HM.empty
        HV.APStackClosure info closure stackData -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Stack application")
            $ HM.insert (T.pack "ptrFun") (Aeson.toJSON closure)
            $ HM.empty
        HV.BCOClosure info _ _ ptr _ _ _ -> Aeson.Object --TODO: Can we get useful info from bytecode?
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Byte code object")
            $ HM.insert (T.pack "ptr") (Aeson.toJSON ptr)
            $ HM.empty
        HV.ArrWordsClosure info _ _ -> Aeson.Object --TODO: Decoding arrays might be hard
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Array")
            $ HM.empty
        HV.MutArrClosure info _ _ elems -> Aeson.Object --TODO: Decoding arrays might be hard
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Mutable array")
            $ HM.insert (T.pack "ptrElems") (Aeson.toJSON elems)
            $ HM.empty
        HV.MutVarClosure info var -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "MVar")
            $ HM.insert (T.pack "ptrVar") (Aeson.toJSON var)
            $ HM.empty
        HV.FunClosure info pArgs dArgs -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Function")
            $ HM.insert (T.pack "ptrArgs") (Aeson.toJSON pArgs)
            $ HM.insert (T.pack "dataArgs") Aeson.Null --TODO: Can we determine types?
            $ HM.empty
        _ -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Other closure")
            $ HM.empty
-}
