{-|
Module     : Hip8.Instruction
Maintainer : Emil Eriksson <shadewind@gmail.com>

Instructions and instruction parsing
-}

module Hip8.Instruction (
  InstructionInfo(..),
  Instruction(..),
  parseInstruction,
  execInstruction,
  sys,
  cls,
  ret,
  jpAddr,
  callAddr,
  seRegByte,
  sneRegByte,
  seRegReg,
  ldRegByte,
  addRegByte,
  ldRegReg,
  orRegReg,
  andRegReg,
  xorRegReg,
  addRegReg,
  subRegReg,
  shr,
  subnRegReg,
  shl,
  sneRegReg,
  ldIAddr,
  jpV0Addr,
  rnd,
  drw,
  skp,
  sknp,
  ldRegDT,
  ldRegKey,
  ldDTReg,
  ldSTReg,
  addIReg,
  ldFReg,
  ldBReg,
  ldMemRegs,
  ldRegsMem
  ) where

import Hip8.BitParser
import Hip8.System
import Control.Applicative
import Data.Word
import Control.Monad
import Data.Bits

-- |A pseudo-argument to an instruction.
data PseudoArg = Addr Word16
               | Byte Word8
               | Nibble Word8
               | Reg Word8
               | RegI
               | AddrI
               | DelayTimer
               | SoundTimer
               | SpriteLoc
               | BCD
               | Key
               deriving (Show, Eq)

-- |Information about an instruction.
data InstructionInfo = InstructionInfo String [PseudoArg]
                     deriving (Show, Eq)

-- |An instruction with a describing string and a 'System' action which performs
-- the operation described by the info.
data Instruction = Instruction InstructionInfo (System ())

instance Show Instruction where
  show (Instruction info _) = show info

instance Eq Instruction where
  (Instruction a _) == (Instruction b _) = a == b

parseInstruction :: Word16 -> Maybe Instruction
parseInstruction word =
  runParser 16 word
     $  const16 0x00E0 *> pure cls
    <|> const16 0x00EE *> pure ret
    <|> const4 0x0 *> (sys <$> addr)
    <|> const4 0x1 *> (jpAddr <$> addr)
    <|> const4 0x2 *> (callAddr <$> addr)
    <|> const4 0x3 *> (seRegByte <$> reg <*> byte)
    <|> const4 0x4 *> (sneRegByte <$> reg <*> byte)
    <|> const4 0x5 *> (seRegReg <$> reg <*> reg) <* const4 0x0
    <|> const4 0x6 *> (ldRegByte <$> reg <*> byte)
    <|> const4 0x7 *> (addRegByte <$> reg <*> byte)
    <|> const4 0x8 *> (ldRegReg <$> reg <*> reg) <* const4 0x0
    <|> const4 0x8 *> (orRegReg <$> reg <*> reg) <* const4 0x1
    <|> const4 0x8 *> (andRegReg <$> reg <*> reg) <* const4 0x2
    <|> const4 0x8 *> (xorRegReg <$> reg <*> reg) <* const4 0x3
    <|> const4 0x8 *> (addRegReg <$> reg <*> reg) <* const4 0x4
    <|> const4 0x8 *> (subRegReg <$> reg <*> reg) <* const4 0x5
    <|> const4 0x8 *> (shr <$> reg <*> reg) <* const4 0x6
    <|> const4 0x8 *> (subnRegReg <$> reg <*> reg) <* const4 0x7
    <|> const4 0x8 *> (shl <$> reg <*> reg) <* const4 0xE
    <|> const4 0x9 *> (sneRegReg <$> reg <*> reg) <* const4 0x0
    <|> const4 0xA *> (ldIAddr <$> addr)
    <|> const4 0xB *> (jpV0Addr <$> addr)
    <|> const4 0xC *> (rnd <$> reg <*> byte)
    <|> const4 0xD *> (drw <$> reg <*> reg <*> nibble)
    <|> const4 0xE *> (skp <$> reg) <* const8 0x9E
    <|> const4 0xE *> (sknp <$> reg) <* const8 0xA1
    <|> const4 0xF *> (ldRegDT <$> reg) <* const8 0x07
    <|> const4 0xF *> (ldRegKey <$> reg) <* const8 0x0A
    <|> const4 0xF *> (ldDTReg <$> reg) <* const8 0x15
    <|> const4 0xF *> (ldSTReg <$> reg) <* const8 0x18
    <|> const4 0xF *> (addIReg <$> reg) <* const8 0x1E
    <|> const4 0xF *> (ldFReg <$> reg) <* const8 0x29
    <|> const4 0xF *> (ldBReg <$> reg) <* const8 0x33
    <|> const4 0xF *> (ldMemRegs <$> reg) <* const8 0x55
    <|> const4 0xF *> (ldRegsMem <$> reg) <* const8 0x65
  where const4 = assertEqBits 4
        const8 = assertEqBits 8
        const16 = assertEqBits 16
        addr = getBits 12
        nibble = fromIntegral <$> getBits 4
        reg = nibble
        byte = fromIntegral <$> getBits 8

-- |Executes the given instruction
execInstruction :: Instruction -> System ()
execInstruction (Instruction _ system) = system

-- |Applies the given binary operator to the given registers and stores the result
-- in the first register. Steps the PC by one.
liftRegReg :: (Word8 -> Word8 -> Word8) -> Word8 -> Word8 -> System ()
liftRegReg f regx regy = do result <- f <$> getReg regx <*> getReg regy
                            setReg regx result
                            stepPC

sys :: Word16 -> Instruction
sys addr = Instruction (InstructionInfo "SYS" [Addr addr]) exec
  where exec = systemException "SYS instruction is not allowed"

cls :: Instruction
cls = Instruction (InstructionInfo "CLS" []) exec
  where exec = clearDisplay

ret :: Instruction
ret = Instruction (InstructionInfo "RET" []) exec
  where exec = pop >>= setPC >> stepPC

jpAddr :: Word16 -> Instruction
jpAddr addr = Instruction (InstructionInfo "JP" [Addr addr]) exec
  where exec = setPC addr

callAddr :: Word16 -> Instruction
callAddr addr = Instruction (InstructionInfo  "CALL" [Addr addr]) exec
  where exec = getPC >>= push >> setPC addr

seRegByte :: Word8 -> Word8 -> Instruction
seRegByte reg byte = Instruction (InstructionInfo "SE" [Reg reg, Byte byte]) exec
  where exec = do x <- getReg reg
                  when (x == byte) stepPC
                  stepPC

sneRegByte :: Word8 -> Word8 -> Instruction
sneRegByte reg byte = Instruction (InstructionInfo "SNE" [Reg reg, Byte byte]) exec
  where exec = do x <- getReg reg
                  when (x /= byte) stepPC
                  stepPC

seRegReg :: Word8 -> Word8 -> Instruction
seRegReg regx regy = Instruction (InstructionInfo "SE" [Reg regx, Reg regy]) exec
  where exec = do x <- getReg regx
                  y <- getReg regy
                  when (x == y) stepPC
                  stepPC

ldRegByte :: Word8 -> Word8 -> Instruction
ldRegByte reg byte = Instruction (InstructionInfo "LDI" [Reg reg, Byte byte]) exec
  where exec = setReg reg byte >> stepPC

addRegByte :: Word8 -> Word8 -> Instruction
addRegByte reg byte = Instruction (InstructionInfo "ADDI" [Reg reg, Byte byte]) exec
  where exec = do x <- getReg reg
                  setReg reg $ x + byte
                  stepPC

ldRegReg :: Word8 -> Word8 -> Instruction
ldRegReg regx regy = Instruction (InstructionInfo "LD" [Reg regx, Reg regy]) exec
  where exec = do y <- getReg regy
                  setReg regx y
                  stepPC

orRegReg :: Word8 -> Word8 -> Instruction
orRegReg regx regy = Instruction (InstructionInfo "OR" [Reg regx, Reg regy]) exec
  where exec = liftRegReg (.|.) regx regy

andRegReg :: Word8 -> Word8 -> Instruction
andRegReg regx regy = Instruction (InstructionInfo "AND" [Reg regx, Reg regy]) exec
  where exec = liftRegReg (.&.) regx regy

xorRegReg :: Word8 -> Word8 -> Instruction
xorRegReg regx regy = Instruction (InstructionInfo "XOR" [Reg regx, Reg regy]) exec
  where exec = liftRegReg xor regx regy

addRegReg :: Word8 -> Word8 -> Instruction
addRegReg regx regy = Instruction (InstructionInfo "ADD" [Reg regx, Reg regy]) exec
  where exec = do x <- getReg regx
                  y <- getReg regy
                  let z = fromIntegral x + fromIntegral y :: Word16
                  setReg regx $ fromIntegral z
                  setReg 0xF $ if z > 255 then 1 else 0
                  stepPC

subRegReg :: Word8 -> Word8 -> Instruction
subRegReg regx regy = Instruction (InstructionInfo "SUB" [Reg regx, Reg regy]) exec
  where exec = do x <- getReg regx
                  y <- getReg regy
                  setReg regx $ x - y
                  setReg 0xF $ if x < y then 0 else 1
                  stepPC

shr :: Word8 -> Word8 -> Instruction
shr regx regy = Instruction (InstructionInfo "SHR" [Reg regx, Reg regy]) exec
  where exec = do x <- getReg regx
                  setReg regx $ shiftR x 1
                  setReg 0xF $ if testBit x 0 then 1 else 0
                  stepPC

subnRegReg :: Word8 -> Word8 -> Instruction
subnRegReg regx regy = Instruction (InstructionInfo "SUBN" [Reg regx, Reg regy]) exec
  where exec = do x <- getReg regx
                  y <- getReg regy
                  setReg regx $ y - x
                  setReg 0xF $ if y < x then 0 else 1
                  stepPC

shl :: Word8 -> Word8 -> Instruction
shl regx regy = Instruction (InstructionInfo "SHL" [Reg regx, Reg regy]) exec
  where exec = do x <- getReg regx
                  setReg regx $ shiftL x 1
                  setReg 0xF $ if testBit x 7 then 1 else 0
                  stepPC

sneRegReg :: Word8 -> Word8 -> Instruction
sneRegReg regx regy = Instruction (InstructionInfo "SNE" [Reg regx, Reg regy]) exec
  where exec = do x <- getReg regx
                  y <- getReg regy
                  when (x /= y) stepPC
                  stepPC

ldIAddr :: Word16 -> Instruction
ldIAddr addr = Instruction (InstructionInfo "LD" [RegI, Addr addr]) exec
  where exec = setRegI addr >> stepPC

jpV0Addr :: Word16 -> Instruction
jpV0Addr addr = Instruction (InstructionInfo "JP" [Reg 0, Addr addr]) exec
  where exec = do x <- getReg 0
                  setPC $ addr + fromIntegral x

rnd :: Word8 -> Word8 -> Instruction
rnd reg mask = Instruction (InstructionInfo "RND" [Reg reg, Byte mask]) exec
  where exec = undefined

drw :: Word8 -> Word8 -> Word8 -> Instruction
drw xreg yreg n  = Instruction (InstructionInfo "DRW" [Reg xreg, Reg yreg, Nibble n]) exec
  where exec = undefined

skp :: Word8 -> Instruction
skp reg = Instruction (InstructionInfo "SKP" [Reg reg]) exec
  where exec = undefined

sknp :: Word8 -> Instruction
sknp reg = Instruction (InstructionInfo "SKNP" [Reg reg]) exec
  where exec = undefined

ldRegDT :: Word8 -> Instruction
ldRegDT reg = Instruction (InstructionInfo "LD" [Reg reg, DelayTimer]) exec
  where exec = undefined

ldRegKey :: Word8 -> Instruction
ldRegKey reg = Instruction (InstructionInfo "LD" [Reg reg, Key]) exec
  where exec = undefined

ldDTReg :: Word8 -> Instruction
ldDTReg reg = Instruction (InstructionInfo "LD" [DelayTimer, Reg reg]) exec
  where exec = undefined

ldSTReg :: Word8 -> Instruction
ldSTReg reg = Instruction (InstructionInfo "LD" [SoundTimer, Reg reg]) exec
  where exec = undefined

addIReg :: Word8 -> Instruction
addIReg reg = Instruction (InstructionInfo "ADD" [RegI, Reg reg]) exec
  where exec = undefined

ldFReg :: Word8 -> Instruction
ldFReg reg = Instruction (InstructionInfo "LD" [SpriteLoc, Reg reg]) exec
  where exec = undefined

ldBReg :: Word8 -> Instruction
ldBReg reg = Instruction (InstructionInfo "LD" [BCD, Reg reg]) exec
  where exec = undefined

ldMemRegs :: Word8 -> Instruction
ldMemRegs x = Instruction (InstructionInfo "LD" [AddrI, Reg x]) exec
  where exec = undefined

ldRegsMem :: Word8 -> Instruction
ldRegsMem x = Instruction (InstructionInfo "LD" [Reg x, AddrI]) exec
  where exec = undefined
