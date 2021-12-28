module Main where

import System.IO
import System.Environment
import Data.Functor
import Text.Regex.TDFA

-- Stolen from https://github.com/aquarial/discord-haskell
import Control.Monad (when, void)
import Data.Text (Text, pack)
import qualified Data.Text.IO as TIO

import UnliftIO.Concurrent

import Discord
import Discord.Types
import qualified Discord.Requests as R
--


main :: IO ()
main = do
    tok <- getToken
    runDiscord $ def
            { discordToken = pack tok
            , discordOnEvent = handleEvents }
    return ()

getToken :: IO String
getToken = getArgs >>= readFile . head

handleEvents :: Event -> DiscordHandler ()
handleEvents ev = case ev of
    MessageCreate m -> handleMessage m False
    _ -> return ()

callbacks :: [(Regex, String -> String)]
callbacks = []

handleMessage :: Message -> Bool -> DiscordHandler ()
handleMessage m False = if fromBot m then return ()
                                     else handleMessage m True
handleMessage m True = if nonePassed then return ()
                                     else mapM_ (\(reg, fn) -> mapM_ R.CreateMessage (messageChannel m) . fn $ cont =~ reg :: [String]) matched
    where
         matched = filter (\(reg, _) -> cont =~ reg :: Bool) callbacks
         cont = messageText m
         nonePassed = empty matched


-- Stolen from https://github.com/aquarial/discord-haskell
fromBot :: Message -> Bool
fromBot = userIsBot . messageAuthor
--
