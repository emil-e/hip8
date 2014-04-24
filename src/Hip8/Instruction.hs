{-|
Module     : Hip8.Instruction
Maintainer : Emil Eriksson <shadewind@gmail.com>

Instructions and instruction parsing
-}

module Hip8.Instruction (
  Environment,
  SystemState,
  SystemTransformer,
  InstructionInfo(..),
  Instruction(..),
  showInstruction,
  binToString,
  parseInstruction,
  sys,
  cls,
  ret
  ) where

import Hip8.BitParser
import Control.Applicative
import Data.Word
import Data.List
import Data.Maybe

-- |The input environment for the emulation
type Environment = ()

-- |The state of the system
type SystemState = ()

-- |Indicates an exception in the CPU with an associated description.
newtype CpuException = CpuException String

-- |Function which transforms the state of the system given an environment.
type SystemTransformer = Environment -> SystemState -> Either CpuException SystemState

-- |Describes the name and arguments of an instruction
data InstructionInfo = InstructionInfo String [Word16] deriving (Show, Eq, Ord)

-- |An instruction with info and a 'SystemTransformer' which performs the operation
-- described by the info.
data Instruction = Instruction InstructionInfo SystemTransformer

-- |Calls 'showInfo' on the info of the 'Instruction'
showInstruction :: Instruction -> String
showInstruction (Instruction info _) = showInfo info

-- |Returns a string describing the instruction of the form "<NAME> <ARG>, <ARGS>, ..."
showInfo :: InstructionInfo -> String
showInfo (InstructionInfo name []) = name
showInfo (InstructionInfo name args) = name ++ " " ++ intercalate ", " (map show args)

-- |Parses the given binary instruction to a string representation.
binToString :: Word16 -> String
binToString word = fromMaybe "" (showInstruction <$> parseInstruction word)

parseInstruction :: Word16 -> Maybe Instruction
parseInstruction word =
  runParser 16 word 
     $  assertEqBits 16 0x00E0 *> pure cls 
    <|> assertEqBits 16 0x00EE *> pure ret 
    <|> assertEqBits 4 0x0 *> (sys <$> getBits 12)

sys :: Word16 -> Instruction
sys loc = Instruction (InstructionInfo "SYS" [loc]) exec
  where exec _ _ = Left $ CpuException "SYS instruction is not allowed"

cls :: Instruction
cls = Instruction (InstructionInfo "CLS" []) exec
  where exec = undefined

ret :: Instruction
ret = Instruction (InstructionInfo "RET" []) exec
  where exec = undefined
