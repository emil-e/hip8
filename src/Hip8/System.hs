{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-|
Module     : Hip8.System
Maintainer : Emil Eriksson <shadewind@gmail.com>

The System monad
-}

module Hip8.System (
  Environment(..),
  SystemState,
  memorySize,
  numRegisters,
  userMemoryStart,
  initialSystemState,
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
  push,
  pop
  ) where

import Control.Monad.Trans.State
import Control.Applicative
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
  
-- |Describes the state of the system including the CPU and the display.
data SystemState = SystemState {
  -- |The main memory vector
  mainMemory :: Vector.Vector Word8,
  -- |The standard Vx registers
  registers :: Vector.Vector Word8,
  -- |The I register
  registerI :: Word16,
  -- |The stack
  stack :: [Word16],
  -- |The program counter
  programCounter :: Word16,
  -- |The delay timer setting
  delayTimerSetting :: TimerSetting,
  -- |The sound timer setting
  soundTimerSetting :: TimerSetting
  } deriving (Eq, Show)

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
  mainMemory = Vector.replicate (fromIntegral memorySize) 0,
  registers = Vector.replicate (fromIntegral numRegisters) 0,
  registerI = 0,
  stack = [],
  programCounter = userMemoryStart,
  delayTimerSetting = NotSet,
  soundTimerSetting = NotSet
  }

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
    st { mainMemory = let index = fromIntegral addr
                      in Vector.modify
                           (\v -> MVector.write v index value)
                           (mainMemory st)}

-- |Returns the value at the given memory address.
getMem :: Word16 -> System Word8
getMem addr = do
  unless (addr < memorySize) $
    systemException (printf "Invalid memory write at 0x%X" addr)
  st <- getSystemState
  return $ mainMemory st Vector.! fromIntegral addr

-- |Writes the contents of the given vector into memory.
writeMem :: Word16 -> Vector.Vector Word8 -> System ()
writeMem addr bytes = do
  let index = fromIntegral addr
      len = Vector.length bytes
      endAddr = addr + fromIntegral len

  when (addr < userMemoryStart) $
    systemException (printf "Write begins on read-only address 0x%X" addr)
  unless (endAddr <= memorySize) $
    systemException (printf "Write ends on invalid memory address: 0x%X" endAddr)
    
  modifySystemState $ \st ->
    st { mainMemory = Vector.modify
                        (\v -> Vector.copy (MVector.slice index len v) bytes)
                        (mainMemory st) }

-- |Reads the given number of bytes starting at the given address.
readMem :: Word16 -- ^The start address
        -> Word16 -- ^The number of bytes to read
        -> System (Vector.Vector Word8)
readMem addr len = do
  let endAddr = addr + len
  unless (endAddr <= memorySize) $
    systemException (printf "Read ends on invalid memory address 0x%X" endAddr)
    
  st <- getSystemState
  return $ Vector.slice (fromIntegral addr) (fromIntegral len) (mainMemory st)

-- |Sets the value of the registry with the given index.
setReg :: Word8   -- ^The registry index
       -> Word8 -- ^The value
       -> System ()
setReg index value = do
  unless (index < numRegisters) $
    systemException (printf "Write to invalid register 0x%X" index)
  modifySystemState $ \st ->
    st { registers = Vector.modify
                       (\v -> MVector.write v (fromIntegral index) value)
                       (registers st) }

-- |Returns the value of the registry with the given index.
getReg :: Word8 -> System Word8
getReg index = do
  unless (index < numRegisters) $
    systemException (printf "Read from invalid register 0x%X" index)
  st <- getSystemState
  return $ registers st Vector.! fromIntegral index

-- |Sets the value of the I register.
setRegI :: Word16 -> System ()
setRegI value = modifySystemState $ \st -> st { registerI = value }

-- |Returns the value of the I register.
getRegI :: System Word16
getRegI = registerI <$> getSystemState

-- |Sets the program counter. Must be even.
setPC :: Word16 -> System ()
setPC value = do
  unless (even value) $
    systemException (printf "Address 0x%X is not even and thus not a valid PC" value)
  unless (value < memorySize) $
    systemException (printf "Invalid address 0x%X" value)
  modifySystemState $ \st ->  st { programCounter = value }

-- |Returns the program counter.
getPC :: System Word16
getPC = programCounter <$> getSystemState

-- |Pushes the given address onto the stack.
push :: Word16 -> System ()
push value = modifySystemState $ \st -> st { stack = value : stack st }

-- |Pops the topmost value from the stack.
pop :: System Word16
pop = do
  st <- getSystemState
  let stk = stack st
  when (null stk) $ systemException "Cannot pop empty stack"
  let (ret:newStack) = stk
  putSystemState $ st { stack = newStack }
  return ret

