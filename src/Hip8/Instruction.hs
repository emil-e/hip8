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
     $  const16 0x00E0 *> pure cls 
    <|> const16 0x00EE *> pure ret 
    <|> const4 0x0 *> (sys <$> addr)
    <|> const4 0x1 *> (jp <$> addr)
    <|> const4 0x2 *> (call <$> addr)
    <|> const4 0x3 *> (se <$> reg <*> byte)
    <|> const4 0x4 *> (sne <$> reg <*> byte)
    <|> const4 0x5 *> (sev <$> reg <*> reg) <* const4 0x0
    <|> const4 0x6 *> (ld <$> reg <*> byte)
    <|> const4 0x7 *> (add <$> reg <*> byte)
  where const4 = assertEqBits 4
        const8 = assertEqBits 8
        const16 = assertEqBits 16
        addr = getBits 12
        reg = getBits 4
        byte = getBits 8

sys :: Word16 -> Instruction
sys loc = Instruction (InstructionInfo "SYS" [loc]) exec
  where exec _ _ = Left $ CpuException "SYS instruction is not allowed"

cls :: Instruction
cls = Instruction (InstructionInfo "CLS" []) exec
  where exec = undefined

ret :: Instruction
ret = Instruction (InstructionInfo "RET" []) exec
  where exec = undefined

jp :: Word16 -> Instruction
jp loc = Instruction (InstructionInfo "JP" [loc]) exec
  where exec = undefined
  
call :: Word16 -> Instruction
call loc = Instruction (InstructionInfo "CALL" [loc]) exec
  where exec = undefined
  
se :: Word16 -> Word16 -> Instruction
se reg byte = Instruction (InstructionInfo "SE" [reg, byte]) exec
  where exec = undefined
  
sne :: Word16 -> Word16 -> Instruction
sne reg byte = Instruction (InstructionInfo "SNE" [reg, byte]) exec
  where exec = undefined

sev :: Word16 -> Word16 -> Instruction
sev reg1 reg2 = Instruction (InstructionInfo "SEV" [reg1, reg2]) exec
  where exec = undefined
  
ld :: Word16 -> Word16 -> Instruction
ld reg byte = Instruction (InstructionInfo "LD" [reg, byte]) exec
  where exec = undefined

add :: Word16 -> Word16 -> Instruction
add reg byte = Instruction (InstructionInfo "ADD" [reg, byte]) exec
  where exec = undefined
  
ldv :: Word16 -> Word16 -> Instruction
ldv dest src = Instruction (InstructionInfo "LDV" [dest, src]) exec
  where exec = undefined
  
or :: Word16 -> Word16 -> Instruction
or reg1 reg2 = Instruction (InstructionInfo "OR" [reg1, reg2]) exec
  where exec = undefined

and :: Word16 -> Word16 -> Instruction
and reg1 reg2 = Instruction (InstructionInfo "AND" [reg1, reg2]) exec
  where exec = undefined

xor :: Word16 -> Word16 -> Instruction
xor reg1 reg2 = Instruction (InstructionInfo "XOR" [reg1, reg2]) exec
  where exec = undefined