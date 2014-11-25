module Main (main) where

import Hip8.System
import Hip8.Instruction (Instruction(Instruction),
                         InstructionInfo,
                         parseInstruction)
import qualified Hip8.Bitmap as Bitmap
import Data.Bits
import Control.Applicative
import Text.Printf
import qualified Data.ByteString as BS
import System.Environment
import System.IO
import Data.Vector.Unboxed (Vector)
import qualified Data.Vector.Unboxed as Vector
import Data.Word
import System.Random
import qualified Graphics.Gloss.Interface.IO.Game as G
import Control.Monad
import Data.List
import Options.Applicative

data World = World {
  state :: SystemState,
  accumTime :: Double
  }

cpuFrequency :: Int
cpuFrequency = 800

cycleTime :: Double
cycleTime = 1 / realToFrac cpuFrequency

displayScale :: Int
displayScale = 20

gfxDisplay :: G.Display
gfxDisplay = G.InWindow "Hip8"
             (displayScale * w, displayScale * h) (100, 100)
  where w = fromIntegral w0
        h = fromIntegral h0
        (w0, h0) = displaySize

concatBytes :: Word8 -> Word8 -> Word16
concatBytes ih il = fromIntegral $ (shiftL ih16 8) .|. il16
  where ih16 = fromIntegral ih :: Word16
        il16 = fromIntegral il :: Word16

performCycle :: System InstructionInfo
performCycle = do
  pc <- getPC
  ih <- fromIntegral <$> getMem pc
  il <- fromIntegral <$> (getMem $ pc + 1)
  let i = concatBytes ih il
  case parseInstruction i of
   Nothing -> systemException $ printf "Invalid instruction 0x%04X" i
   Just (Instruction instructionInfo sys) -> sys >> return instructionInfo

squareAt :: Int -> Int -> G.Picture
squareAt x y = G.Translate (scale * offsetx) (scale * offsety) square
  where square = G.Polygon [(0, 0), (scale, 0), (scale, scale), (0, scale)]
        scale = realToFrac displayScale
        (w0, h0) = displaySize
        offsetx = realToFrac x - realToFrac w0 * 0.5
        offsety = realToFrac y - realToFrac h0 * 0.5

displayWorld :: World -> IO G.Picture
displayWorld world = return $ G.scale 1 (-1) $ G.color G.white pixels
  where disp = display $ state world
        (w, h) = Bitmap.dimensions disp
        pixels = G.Pictures [squareAt x y |
                             x <- [0..w - 1],
                             y <- [0..h - 1],
                             Bitmap.pixelAt disp (x, y)]

handleInput :: G.Event -> World -> IO World
handleInput (G.EventKey (G.Char char) keyState _ _) world = do
  putStrLn $ show keyState ++ " " ++ show char
  return $ world { state = state',
                   accumTime = accumTime world }
  where key = fromIntegral <$> elemIndex char "x123qweasdzc4rfv"
        boolState = if (keyState == G.Down) then True else False
        mstate = case key of
          Nothing -> Right $ state world
          Just k -> execSystem (state world) $ setKeyState k boolState
        state' = case mstate of
          Left (SystemException msg) -> error msg
          Right s -> s
handleInput _ world = return world

execSteps :: World -> ([InstructionInfo], World)
execSteps world
  | accumTime world < cycleTime = ([], world)
  | otherwise = (i:is, world')
  where
    (i, s) = case result of
      Left (SystemException msg) -> error msg
      Right r -> r
    result = runSystem (state world) $ do
      instructionInfo <- performCycle
      sleep cycleTime
      return instructionInfo
    (is, world') = execSteps $ world {
      accumTime = accumTime world - cycleTime,
      state = s }

stepWorld :: Float -> World -> IO World
stepWorld t world = do
  let world' = world { accumTime = accumTime world + realToFrac t }
      (infos, nextWorld) = execSteps world'
  forM_ infos print
  return nextWorld

execute :: SystemState -> IO ()
execute s0 = G.playIO
             gfxDisplay
             G.black
             cpuFrequency
             world
             displayWorld
             handleInput
             stepWorld
  where world = World { state = s0, accumTime = 0 }


makeSystem :: Vector Word8 -> IO (Either SystemException SystemState)
makeSystem rom = do
  stdGen <- getStdGen
  return $ execSystem initialSystemState $ do
    setRandoms $ randoms stdGen
    writeMem userMemoryStart rom
    setPC userMemoryStart

printException :: SystemException -> IO ()
printException (SystemException msg) = putStrLn $ "SystemException: " ++ msg

disassemble :: Word16 -> Vector Word8 -> [(Word16, Maybe InstructionInfo)]
disassemble addr rom
  | Vector.null rom = []
  | otherwise = row : disassemble (addr + 2) (Vector.drop 2 rom)
  where row = (addr, mInfo)
        mInfo = instructionInfo <$> parseInstruction word
        instructionInfo (Instruction iinfo _) = iinfo
        word = concatBytes ((Vector.!) rom 0) ((Vector.!) rom 1)

doDisassemble :: Vector Word8 -> IO ()
doDisassemble rom = do
  forM_ (disassemble 0x200 rom) $ \(addr, mInfo) -> do
    printf "0x%04X: " addr
    putStrLn $ case mInfo of
     Nothing -> "???"
     Just instructionInfo -> show instructionInfo

doExecute :: Vector Word8 -> IO ()
doExecute rom = do s0 <- makeSystem rom
                   case s0 of
                    Left e -> printException e
                    Right s -> execute s

loadRom :: FilePath -> IO (Vector Word8)
loadRom romFile = do
  romData <- withFile romFile ReadMode BS.hGetContents
  return $ Vector.generate (BS.length romData) $ BS.index romData

romAction :: (Vector Word8 -> IO ()) -> Parser (IO ())
romAction act =
  (>>=)
  <$> (loadRom <$> (argument str $ metavar "FILE"))
  <*> pure act

actionDisassemble :: Parser (IO ())
actionDisassemble = (flag' () (long "disassemble" <>
                               short 'd' <>
                               help "Disassemble ROM file")) *>
                    romAction doDisassemble

actionRun :: Parser (IO ())
actionRun = romAction doExecute

options :: Parser (IO ())
options = actionRun <|> actionDisassemble

main :: IO ()
main = do
  let opts = info (helper <*> options) fullDesc
  act <- execParser opts
  act
