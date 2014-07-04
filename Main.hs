module Main where

import Network
import qualified Data.ByteString.Char8 as S
import Data.Serialize -- Cereal
import System.IO
import System.Environment
import Control.Monad
import Control.Applicative
import Control.Exception
import Data.Word

-- Protocol description from the server's Main.hs:
-- client a connects
-- client b connects
-- server sends Response (ready)
-- client a sends Request
-- client b sends Request
-- server sends Response (ready)

data Request = Betray | Cooperate deriving Show
data Response = Betrayed | Cooperated deriving Show

instance Serialize Request where
    put Betray = put (1 :: Word8)
    put Cooperate = put (2 :: Word8)
    get = do
        byte <- get :: Get Word8
        case byte of
            1 -> return Betray
            2 -> return Cooperate
            _ -> fail "Invalid Request"

instance Serialize Response where
    put Betrayed = put (4 :: Word8)
    put Cooperated = put (5 :: Word8)
    get = do
        byte <- get :: Get Word8
        case byte of
            4 -> return Betrayed
            5 -> return Cooperated
            _ -> fail "Invalid Response"

port = PortNumber 1234

main :: IO ()
main = do
    [hostName, strategy] <- getArgs -- TODO: Parse arguments properly
    bracket (connectTo hostName port)
            hClose
            (runGame (dispatch strategy) Nothing)
    `catch` disconnected
  where
    dispatch strategy = -- TODO: There must be a better way to do this
      case strategy of
        "titForTat"       -> titForTat
        "alwaysBetray"    -> alwaysBetray
        "alwaysCooperate" -> alwaysCooperate
        _                 -> fail "Invalid strategy"
    disconnected :: IOException -> IO ()
    disconnected ex = putStrLn "Done." -- Protocol: just terminate connection. :(

runGame :: (Maybe Response -> Request) -> Maybe Response -> Handle -> IO ()
runGame strategy last h = do
    request  <- S.hPutStrLn h (encode (strategy last))
    response <- getResponse
    runGame strategy (Just response) h
  where
    tryDecode bs =
      case (decode bs) of
        Left e         -> (hClose h) >> error e
        Right response -> return response
    getResponse = do
      bs <- S.hGetLine h
      tryDecode bs

-- Strategies
-- TODO: These type signatures are all the same. I feel like I'm missing an abstraction

titForTat :: (Maybe Response) -> Request
titForTat lastAction =
  case lastAction of
    Just Betrayed   -> Betray -- Retaliating
    Just Cooperated -> Cooperate -- Forgiving, Non-Envious
    Nothing         -> Cooperate -- Be Nice

alwaysBetray :: (Maybe Response) -> Request
alwaysBetray _ = Betray

alwaysCooperate :: (Maybe Response) -> Request
alwaysCooperate _ = Cooperate

