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
import Data.Scientific (scientific)
import qualified Data.Text as T
import qualified Data.Vector as Vec
-- Other
import qualified System.IO as IO
import Control.Exception
import Control.Concurrent
import Control.Monad
import System.IO.Unsafe

-- TODO: Test HasVis on Windows
#if WINDOWS
import Foreign.C.String (CString, withCString)
#else
import qualified System.Process as Proc
#endif

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
            HV.HeapGraph hgMap <- HV.buildHeapGraph maxBound () box
            -- ASSUMPTION: The IntMap's keys correspond to array indices (in order)
            let heapObjects = IM.elems $ IM.map HV.hgeClosure hgMap
            -- Send JSON-ified heap objects to the browser app
            WSK.sendDataMessage browser (WSK.Text (Aeson.encode heapObjects))
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
-}

instance Aeson.ToJSON b => Aeson.ToJSON (HV.GenClosure b) where
    toJSON closure = case closure of
        HV.ConsClosure info pArgs dArgs pkg modl name -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.String (T.pack "Data constructor"))
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
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Generic application")
            $ HM.insert (T.pack "ptrFun") (Aeson.toJSON fun)
            $ HM.insert (T.pack "args") Aeson.Null --TODO: Need bitmap to separate pointers from data
            $ HM.empty
        HV.PAPClosure info arity nArgs fun args -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Partial application")
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
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Function closure")
            $ HM.insert (T.pack "ptrArgs") (Aeson.toJSON pArgs)
            $ HM.insert (T.pack "dataArgs") Aeson.Null --TODO: Can we determine types?
            $ HM.empty
        _ -> Aeson.Object
            $ HM.insert (T.pack "closureType") (Aeson.toJSON "Other closure")
            $ HM.empty
