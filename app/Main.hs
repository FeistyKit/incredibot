module Main where

import System.IO
import System.Environment
import Data.Functor
import Text.Regex.PCRE (Regex, matchTest, getAllTextMatches, (=~), AllTextMatches (AllTextMatches))

-- Stolen from https://github.com/aquarial/discord-haskell
import Control.Monad (when, void)
import Data.Text (Text, pack, unpack, stripPrefix)
import qualified Data.Text.IO as TIO
import Data.Maybe
import qualified Data.List as DL

import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R
import Discord.Internal.Rest.Channel (MessageDetailedOpts(messageDetailedContent))
--


main :: IO ()
main = do
    tok <- getToken
    void $ runDiscord $ def
            { discordToken = pack tok
            , discordOnEvent = handleEvents
            , discordOnLog = putStrLn . unpack}

getToken :: IO String
getToken = getArgs >>= readFile . head

handleEvents :: Event -> DiscordHandler ()
handleEvents ev = case ev of
    MessageCreate m -> do
        handleMessage m False
    _ -> return ()

subredditLink :: String -> String
subredditLink raw = "https://www.reddit.com/" ++ stripped "/" (stripped " " (stripped "\n" raw))
    where stripped a s = fromMaybe s $ DL.stripPrefix a s



callbacks :: [(String, String -> String)]
callbacks = [("(?im)(^|[^\\S\\r\\n])\\/?(r\\/\\w+)", subredditLink)]

handleMessage :: Message -> Bool -> DiscordHandler ()
handleMessage m False = if fromBot m then return ()
                                     else handleMessage m True
handleMessage m True = mapM_ (\(reg, fn) -> mapM_ (msgFromString m . fn) (allTheMatches (unpack cont) reg)) matched
    where
         matched = filter (\(reg, _) -> unpack cont =~ reg :: Bool) callbacks
         cont = messageText m

msgFromString :: Message -> String -> DiscordHandler ()
msgFromString m s = void $ restCall $ R.CreateMessage (messageChannel m) $ pack s

allTheMatches :: String -> String -> [String]
allTheMatches cont reg = getAllTextMatches $ cont =~ reg :: [String]


-- Stolen from https://github.com/aquarial/discord-haskell
fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor
--
