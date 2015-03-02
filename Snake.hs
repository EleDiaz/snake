-- Snake game in haskell by Eleazar Díaz Delgado
{-# LANGUAGE TypeOperators, RecordWildCards #-}
module Main where

import Control.Monad
import Control.Monad.State
import Control.Concurrent
import Control.Concurrent.STM
import qualified Data.Vector as V
import Data.Default (def)

import System.Random (randomRIO)

import Graphics.Vty

data Block = Apple | Poison | Wall deriving (Show, Eq, Enum)
type PairI = (Int, Int)

data Board  = Board
              { lastMove :: PairI
              , endGame  :: Bool
              , speed    :: Int
              , score    :: Int
              , snake   :: [PairI]
              , objects :: [(Block, PairI)]
              , size    :: PairI
              } deriving (Show, Eq)

defaultBoard :: Board
defaultBoard = Board { lastMove = (1,0)
                     , endGame = False
                     , speed = 200000
                     , score = 0
                     , snake = [(0,0),(0,1),(0, 2)]
                     , objects = []
                     , size = (20,10)
                     }

add :: PairI -> PairI -> PairI
(x, y) `add` (t, s) = (x+t, y+s)

headsafe :: [a] -> Maybe a
headsafe []    = Nothing
headsafe (x:_) = Just x

moveSnake :: PairI -> Board -> Board
moveSnake p bd@(Board {..})
  | null snake = defaultBoard { size = size
                              , snake = map (`add` (fst size `div` 2, snd size `div` 2)) [(0,0),(0,1),(0, 2)]}
  | otherwise  =
      let (x:xs)   = snake
          x'@(a,b) = x `add` p
      in  if (x' `elem` xs) || (a == fst size) || (b == snd size) || (a < 0) || (b < 0) then
            bd { snake = [] } -- Dead
          else
            let object = map fst $ filter (\(_,pos) -> pos == x') objects
                objects' = filter (\(_,pos) -> pos /= x') objects
            in case headsafe object of
                Nothing     -> bd { snake = x':x:init xs, objects = objects'}
                Just Apple  -> bd { snake = x':x:xs
                                  , objects = objects'
                                  , speed = speed-10000
                                  , score = score + 10}
                Just Poison -> bd { snake = x':x:reverse (drop 2 (reverse xs))
                                  , objects = objects'
                                  , score = score -10}
                Just Wall   -> bd { snake = []} -- Dead

type Game a = StateT Board IO a

apply :: PairI -> Game ()
apply pos =
  modify (\st -> if lastMove st `add` pos == (0,0) then
                   moveSnake (lastMove st) st
                 else
                   moveSnake pos st {lastMove = pos })


-------------------------------------------------------------------------------
-- Impure ---------------------------------------------------------------------

main :: IO ()
main = do
  vty <- mkVty def
  key <- atomically $ newTVar (EvKey KBS [])

  forkIO $ fix $ \x -> do
    a <- nextEvent vty
    atomically $ writeTVar key a
    x

  let loop :: Game ()
      loop = do
        st <- get
        unless (endGame st) $ do
          liftIO $ threadDelay (speed st)
          k <- liftIO $ readTVarIO key
          handleNextEvent k
          a <- liftIO $
               liftM2 (,) (liftM toEnum $ randomRIO (fromEnum Apple, fromEnum Wall))
                          (liftM2 (,) (randomRIO (0,(fst $ size st) - 1)) (randomRIO (0,(snd $ size st) - 1)))
          modify (\gm -> gm { objects =  a : objects gm })
          updateDisplay vty
          loop

  (x,y) <- displayBounds $ outputIface vty
  execStateT loop (defaultBoard { size = (x,y-1), snake = map (`add` (x `div` 2, y `div` 2)) (snake defaultBoard)})
  shutdown vty

handleNextEvent :: Event -> Game ()
handleNextEvent e = do
  case e of
   EvKey k _ -> case k of
     KUp -> apply (0,-1)
     KDown -> apply (0,1)
     KLeft -> apply (-1,0)
     KRight -> apply (1,0)
     KEsc -> modify (\st -> st { endGame = True })
     _ -> get >>= \st -> apply (lastMove st)
   _         -> return ()


(//) :: V.Vector (V.Vector a) -> (a,(Int, Int)) -> V.Vector (V.Vector a)
(//) vec (a,(x,y)) = vec V.// [(y,(vec V.! y) V.// [(x,a)])]

updateDisplay :: Vty -> Game ()
updateDisplay vty = do
  st <- get
  let (w,h) = size st
      obj   = map
              (\(t,p) -> case t of
                          Apple -> ('a',p)
                          Poison -> ('p',p)
                          Wall -> ('w',p))
              (objects st)
      snk   = map (\pos -> ('#',pos)) (snake st)
      mat   = foldl (//) (V.replicate h (V.replicate w ' ')) (obj++snk)
      scor  = string defAttr ("Score: " ++ (show $ score st))
      game  = map (string defAttr) (V.toList $ V.map V.toList mat)

  liftIO $ update vty $ picForImage $ vertCat $ scor:game
