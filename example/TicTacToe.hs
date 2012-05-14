{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

import qualified Control.Applicative as App
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class (liftIO)
import Data.Aeson
import Data.Aeson.Types
import Data.Default
import Data.List (transpose)
import Data.Monoid (mconcat)
import qualified Data.Text.Lazy as T

import Network.Wai.Middleware.Static
import Web.Scotty
import Web.Tractor

main = scotty 3000 $ do
    middleware static

    get "/" $ file "tictactoe.html"

    j_tractor <- liftIO jTractorStatic
    get "/jquery-tractor.js" $ file $ j_tractor

    -- connect /example to the following web_app
    connect (def { verbose = 3 }) web_app

web_app :: Document -> IO ()
web_app doc = do
    register doc "click" "return { id : $(widget).attr('id') };"

    turn <- newMVar X
    board <- newMVar (replicate 9 Empty)

    forkIO $ forever $ do
        res <- waitFor doc "click"
        let Success (Id i) :: Result Id = fromJSON res
        print i

        marker <- takeMVar turn
        cb <- takeMVar board
        let cb' = setPos i marker cb

        case cb !! i of
            Empty -> do putMVar board cb'
                        if marker == X then putMVar turn O else putMVar turn X
                        if winning cb'
                        then do send doc $ mconcat ["$('.square').attr('src', '", imgFor Empty, "').addClass('click')"]
                                swapMVar board (replicate 9 Empty)
                                swapMVar turn X
                                return ()
                        else send doc $ mconcat ["$('#", T.pack (show i), "').attr('src', '", imgFor marker, "').removeClass('click')"]
            _ -> return ()


    return ()

data Space = Empty | X | O deriving (Eq, Show)

setPos :: Int -> Space -> [Space] -> [Space]
setPos p m bd = b ++ (m:a)
    where (b,a') = splitAt p bd
          a = drop 1 a'

imgFor :: Space -> T.Text
imgFor Empty = "blank.png"
imgFor X     = "x.png"
imgFor O     = "o.png"

winning :: [Space] -> Bool
winning bd = or $ map winner $ rows bd ++ cols bd ++ diags bd

winner :: [Space] -> Bool
winner [X,X,X] = True
winner [O,O,O] = True
winner _       = False

rows, cols, diags :: [Space] -> [[Space]]
rows [] = []
rows bd = take 3 bd : rows (drop 3 bd)

cols = transpose . rows

diags bd = [go bd,go (concatMap reverse (rows bd))]
    where go [] = []
          go (s:ss) = s : go (drop 3 ss)

data Id = Id Int deriving Show

instance FromJSON Id where
   parseJSON (Object v) = Id App.<$> read App.<$> (v .: "id")
   parseJSON _          = mzero
