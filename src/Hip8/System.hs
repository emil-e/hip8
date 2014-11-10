{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module     : Hip8.System
Maintainer : Emil Eriksson <shadewind@gmail.com>

The System monad
-}

module Hip8.System (
  Environment(..),
  SystemState,
  register,
  stack,
  programCounter,
  display,
  memorySize,
  numRegisters,
  userMemoryStart,
  initialSystemState,
  initialDisplay,
  displaySize,
  SystemException(..),
  System,
  runSystem,
  evalSystem,
  execSystem,
  getEnvironment,
  getSystemState,
  systemException,

  setMem,
  getMem,
  writeMem,
  readMem,
  setReg,
  getReg,
  setRegI,
  getRegI,
  setPC,
  getPC,
  stepPC,
  push,
  pop,
  blit,
  clearDisplay,
  setRandoms,
  nextRandom,
  getKey
  ) where

import Hip8.Bitmap (Bitmap)
import qualified Hip8.Bitmap as Bitmap
import Control.Monad.Trans.State
import Control.Applicative
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as Vector
import qualified Data.Vector.Unboxed.Mutable as MVector
import Data.Word
import Control.Monad
import Text.Printf

-- |Describes the input to the emulated system consisting of the current time
-- and the currently pressed key.
data Environment = Environment Float (Maybe Word8)
                 deriving (Eq, Show)

-- |Describes a value set at a particular time for the sound timer or delay timer.
data TimerSetting = NotSet | Set Float Word8
                  deriving (Eq, Show)

-- |Newtype around list to ensure not displaying all elements.
newtype Infinite a = Infinite [a]

instance (Show a) => Show (Infinite a) where
  show (Infinite lst) = case (splitAt 10 lst) of
                         (x, []) -> show x
                         (x, _) -> show x ++ "..."

-- |Describes the state of the system including the CPU and the display.
data SystemState = SystemState {
  -- |The main memory vector
  _mainMemory :: Vector Word8,
  -- |The standard Vx registers
  _registers :: Vector Word8,
  -- |The I register
  _registerI :: Word16,
  -- |The stack
  _stack :: [Word16],
  -- |The program counter
  _programCounter :: Word16,
  -- |The delay timer setting
  _delayTimerSetting :: TimerSetting,
  -- |The sound timer setting
  _soundTimerSetting :: TimerSetting,
  -- |The display buffer.
  _display :: Bitmap,
  -- |A list of random numbers which acts as the random source of the system.
  _randoms :: Infinite Word8
  } deriving (Show)

instance Eq SystemState where
  s1 == s2 = (_mainMemory s1 == _mainMemory s2) &&
             (_registers s1 == _registers s2) &&
             (_registerI s1 == _registerI s2) &&
             (_stack s1 == _stack s2) &&
             (_programCounter s1 == _programCounter s2) &&
             (_delayTimerSetting s1 == _delayTimerSetting s2) &&
             (_soundTimerSetting s1 == _soundTimerSetting s2) &&
             (_display s1 == _display s2)

-- |Returns the value of the register with the given index for the given 'SystemState'.
register :: SystemState -> Word8 -> Word8
register st index
  | index >= numRegisters = error $ "Invalid register index " ++ show index
  | otherwise = _registers st ! fromIntegral index

-- |Returns the stack of the given 'SystemState'.
stack :: SystemState -> [Word16]
stack = _stack

-- |Returns the program count of the given 'SystemState'.
programCounter :: SystemState -> Word16
programCounter = _programCounter

-- |Returns the display buffer of the given 'SystemState'.
display :: SystemState -> Bitmap
display = _display

-- |The memory size of the Chip-8 system
memorySize :: Word16
memorySize = 4096

-- |The number of standard registers in the Chip-8 system
numRegisters :: Word8
numRegisters = 16

-- |The start of the memory available for programs
userMemoryStart :: Word16
userMemoryStart = 0x200

-- |The initial system state with no program loaded
initialSystemState :: SystemState
initialSystemState = SystemState {
  _mainMemory = Vector.replicate (fromIntegral memorySize) 0,
  _registers = Vector.replicate (fromIntegral numRegisters) 0,
  _registerI = 0,
  _stack = [],
  _programCounter = userMemoryStart,
  _delayTimerSetting = NotSet,
  _soundTimerSetting = NotSet,
  _display = initialDisplay,
  _randoms = Infinite $ repeat 0
  }

-- |The Chip-8 standard display dimensions.
displaySize :: (Int, Int)
displaySize = (64, 32)

-- |A new black display.
initialDisplay :: Bitmap
initialDisplay = Bitmap.black displaySize

-- |Indicates an error while simulating the hardware, i.e. invalid memory access etc.
data SystemException = SystemException String
                     deriving (Show, Eq)

-- |The system monad for performing operations on a 'SystemState'.
newtype System a = System (StateT (Environment, SystemState) (Either SystemException) a)
                 deriving (Functor, Applicative, Monad)

-- |Given an 'Environment' and an initial 'SystemState', runs the given 'System' monad returning
-- either a tuple of the value and the transformed state or a 'SystemException' describing an
-- error.
runSystem :: Environment
          -> SystemState
          -> System a
          -> Either SystemException (a, SystemState)
runSystem env s (System sys) =
  case ret of
    Right (x, (_, s')) -> Right (x, s')
    Left err -> Left err
  where ret = runStateT sys (env, s)

-- |Like 'runSystem' but only returns the value.
evalSystem :: Environment
           -> SystemState
           -> System a
           -> Either SystemException a
evalSystem env s sys = fst <$> runSystem env s sys

-- |Like 'runSystem' but only returns the transformed state.
execSystem :: Environment
           -> SystemState
           -> System a
           -> Either SystemException SystemState
execSystem env s sys = snd <$> runSystem env s sys

-- |Returns the current 'Environment'.
getEnvironment :: System Environment
getEnvironment = System $ fst <$> get

-- |Returns the current 'SystemState'.
getSystemState :: System SystemState
getSystemState = System $ snd <$> get

-- |Sets the current 'SystemState'.
putSystemState :: SystemState -> System ()
putSystemState s = do
  (env, _) <- System get
  System $ put (env, s)

-- |Applies the given function to the current state.
modifySystemState :: (SystemState -> SystemState) -> System ()
modifySystemState f = do
  st <- getSystemState
  putSystemState $ f st

-- |Throws a 'SystemException'.
systemException :: String -> System a
systemException msg = System $ StateT $ const $ Left (SystemException msg)

-- |Sets a value a the given memory address.
setMem :: Word16 -> Word8 -> System ()
setMem addr value = do
  when (addr < userMemoryStart) $
    systemException (printf "Read-only memory at 0x%X" addr)
  unless (addr < memorySize) $
    systemException (printf "Invalid memory write at 0x%X" addr)

  modifySystemState $ \st ->
    st { _mainMemory = let index = fromIntegral addr
                       in Vector.modify
                          (\v -> MVector.write v index value)
                          (_mainMemory st)}

-- |Returns the value at the given memory address.
getMem :: Word16 -> System Word8
getMem addr = do
  unless (addr < memorySize) $
    systemException (printf "Invalid memory write at 0x%X" addr)
  st <- getSystemState
  return $ _mainMemory st Vector.! fromIntegral addr

-- |Writes the contents of the given vector into memory.
writeMem :: Word16 -> Vector Word8 -> System ()
writeMem addr bytes = do
  let index = fromIntegral addr
      len = Vector.length bytes
      endAddr = addr + fromIntegral len

  when (addr < userMemoryStart) $
    systemException (printf "Write begins on read-only address 0x%X" addr)
  unless (endAddr <= memorySize) $
    systemException (printf "Write ends on invalid memory address: 0x%X" endAddr)

  modifySystemState $ \st ->
    st { _mainMemory = Vector.modify
                        (\v -> Vector.copy (MVector.slice index len v) bytes)
                        (_mainMemory st) }

-- |Reads the given number of bytes starting at the given address.
readMem :: Word16 -- ^The start address
        -> Word16 -- ^The number of bytes to read
        -> System (Vector Word8)
readMem addr len = do
  let endAddr = addr + len
  unless (endAddr <= memorySize) $
    systemException (printf "Read ends on invalid memory address 0x%X" endAddr)

  st <- getSystemState
  return $ Vector.slice (fromIntegral addr) (fromIntegral len) (_mainMemory st)

-- |Sets the value of the registry with the given index.
setReg :: Word8 -- ^The registry index
       -> Word8 -- ^The value
       -> System ()
setReg index value = do
  unless (index < numRegisters) $
    systemException (printf "Write to invalid register 0x%X" index)
  modifySystemState $ \st ->
    st { _registers = Vector.modify
                       (\v -> MVector.write v (fromIntegral index) value)
                       (_registers st) }

-- |Returns the value of the registry with the given index.
getReg :: Word8 -> System Word8
getReg index = do
  unless (index < numRegisters) $
    systemException (printf "Read from invalid register 0x%X" index)
  st <- getSystemState
  return $ _registers st ! fromIntegral index

-- |Sets the value of the I register.
setRegI :: Word16 -> System ()
setRegI value = modifySystemState $ \st -> st { _registerI = value }

-- |Returns the value of the I register.
getRegI :: System Word16
getRegI = _registerI <$> getSystemState

-- |Sets the program counter. Must be even.
setPC :: Word16 -> System ()
setPC value = do
  unless (even value) $
    systemException (printf "Address 0x%X is not even and thus not a valid PC" value)
  unless (value < memorySize) $
    systemException (printf "Invalid address 0x%X" value)
  modifySystemState $ \st -> st { _programCounter = value }

-- |Returns the program counter.
getPC :: System Word16
getPC = _programCounter <$> getSystemState

-- |Steps the program counter.
stepPC :: System ()
stepPC = do
  pc <- getPC
  setPC (pc + 2)

-- |Pushes the given address onto the stack.
push :: Word16 -> System ()
push value = modifySystemState $ \st -> st { _stack = value : _stack st }

-- |Pops the topmost value from the stack.
pop :: System Word16
pop = do
  st <- getSystemState
  case _stack st of
   [] -> systemException "Cannot pop empty stack"
   (top:rest) -> do
     putSystemState $ st { _stack = rest }
     return top

-- |Blits the given 'Bitmap' to the display at the given coordinates.
blit :: Bitmap -> (Int, Int) -> System ()
blit bm pos = do
  modifySystemState $ \st -> st { _display = Bitmap.blit (_display st) bm pos }

-- |Clears the display.
clearDisplay :: System ()
clearDisplay = modifySystemState $ \st -> st { _display = initialDisplay }

-- |Sets the source of random numbers.
setRandoms :: [Word8] -> System ()
setRandoms randoms = modifySystemState $ \st ->
  st { _randoms = Infinite randoms }

-- |Returns the next random number.
nextRandom :: System Word8
nextRandom = do
  st <- getSystemState
  let (Infinite randoms) = _randoms st
  case randoms of
   [] -> systemException "Out of randoms"
   (next:rest) -> do
     putSystemState $ st { _randoms = Infinite rest }
     return next

-- |Returns the currently pressed key, if any.
getKey :: System (Maybe Word8)
getKey = getEnvironment >>= \(Environment _ key) -> return key
