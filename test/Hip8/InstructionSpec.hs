module Hip8.InstructionSpec (Hip8.InstructionSpec.spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck (Property,
                        forAll,
                        arbitrary,
                        suchThat,
                        (==>),
                        choose)
import Hip8.System
import Hip8.Instruction
import qualified Hip8.Bitmap as Bitmap
import Hip8.SystemSpec
import Data.Maybe
import Control.Monad
import Control.Applicative
import Data.Word
import Data.Bits
import qualified Data.Vector.Unboxed as Vector
import Text.Printf

-- |Checks if the given action steps the PC by the given number of steps (i.e. increases the PC
-- by N * 2) when run with the given 'Environment' and 'SystemState'.
stepsPCBy :: Word16 -> System a -> Environment -> SystemState -> Property
stepsPCBy n action env state =
  forAll (pcWithStepMargin n) $ \pc ->
    evalSystem env state $ do
      setPC pc
      action
      pc' <- getPC
      return $ (pc' - pc) `quot` 2 == n

-- |Convenience function for testing that a register Vz is set to a function of the values of
-- two other registers Vx and Vy when executing an instruction with Vx and Vy as arguments.
checkRegReg :: Reg -> Reg -> Reg
            -> (Word8 -> Word8 -> Instruction)
            -> (Word8 -> Word8 -> Word8)
            -> System Bool
checkRegReg (Reg regx) (Reg regy) (Reg regz) ins f =
  do x <- getReg regx
     y <- getReg regy
     execInstruction $ ins regx regy
     z <- getReg regz
     return $ z == f x y

spec :: Spec
spec = do
  describe "parseInstruction" $ do
    it "parses SYS addr" $
      parseInstruction 0x0ABC `shouldBe` Just (sys 0xABC)
    it "parses CLS" $
      parseInstruction 0x00E0 `shouldBe` Just cls
    it "parses RET" $
      parseInstruction 0x00EE `shouldBe` Just ret
    it "parses JP addr" $
      parseInstruction 0x1ABC `shouldBe` Just (jpAddr 0xABC)
    it "parses CALL addr" $
      parseInstruction 0x2ABC `shouldBe` Just (callAddr 0xABC)
    it "parses SE Vx, byte" $
      parseInstruction 0x3ABC `shouldBe` Just (seRegByte 0xA 0xBC)
    it "parses SNE Vx, byte" $
      parseInstruction 0x4ABC `shouldBe` Just (sneRegByte 0xA 0xBC)
    it "parses SE Vx, Vy" $
      parseInstruction 0x5AB0 `shouldBe` Just (seRegReg 0xA 0xB)
    it "parses LD Vx, byte" $
      parseInstruction 0x6ABC `shouldBe` Just (ldRegByte 0xA 0xBC)
    it "parses ADD Vx, byte" $
      parseInstruction 0x7ABC `shouldBe` Just (addRegByte 0xA 0xBC)
    it "parses LD Vx, Vy" $
      parseInstruction 0x8AB0 `shouldBe` Just (ldRegReg 0xA 0xB)
    it "parses OR Vx, Vy" $
      parseInstruction 0x8AB1 `shouldBe` Just (orRegReg 0xA 0xB)
    it "parses AND Vx, Vy" $
      parseInstruction 0x8AB2 `shouldBe` Just (andRegReg 0xA 0xB)
    it "parses XOR Vx, Vy" $
      parseInstruction 0x8AB3 `shouldBe` Just (xorRegReg 0xA 0xB)
    it "parses ADD Vx, Vy" $
      parseInstruction 0x8AB4 `shouldBe` Just (addRegReg 0xA 0xB)
    it "parses SUB Vx, Vy" $
      parseInstruction 0x8AB5 `shouldBe` Just (subRegReg 0xA 0xB)
    it "parses SHR Vx, Vy" $
      parseInstruction 0x8AB6 `shouldBe` Just (shr 0xA 0xB)
    it "parses SUBN Vx, Vy" $
      parseInstruction 0x8AB7 `shouldBe` Just (subnRegReg 0xA 0xB)
    it "parses SHL Vx, Vy" $
      parseInstruction 0x8ABE `shouldBe` Just (shl 0xA 0xB)
    it "parses SNE Vx, Vy" $
      parseInstruction 0x9AB0 `shouldBe` Just (sneRegReg 0xA 0xB)
    it "parses LD I, addr" $
      parseInstruction 0xAABC `shouldBe` Just (ldIAddr 0xABC)
    it "parses JP V0, addr" $
      parseInstruction 0xBABC `shouldBe` Just (jpV0Addr 0xABC)
    it "parses RND Vx, byte" $
      parseInstruction 0xCABC `shouldBe` Just (rnd 0xA 0xBC)
    it "parses DRW Vx, Vy, nibble" $
      parseInstruction 0xDABC `shouldBe` Just (drw 0xA 0xB 0xC)
    it "parses SKP Vx" $
      parseInstruction 0xEA9E `shouldBe` Just (skp 0xA)
    it "parses SKNP Vx" $
      parseInstruction 0xEAA1 `shouldBe` Just (sknp 0xA)
    it "parses LD Vx, DT" $
      parseInstruction 0xFA07 `shouldBe` Just (ldRegDT 0xA)
    it "parses LD Vx, K" $
      parseInstruction 0xFA0A `shouldBe` Just (ldRegKey 0xA)
    it "parses LD DT, Vx" $
      parseInstruction 0xFA15 `shouldBe` Just (ldDTReg 0xA)
    it "parses LD ST, Vx" $
      parseInstruction 0xFA18 `shouldBe` Just (ldSTReg 0xA)
    it "parses ADD I, Vx" $
      parseInstruction 0xFA1E `shouldBe` Just (addIReg 0xA)
    it "parses LD F, Vx" $
      parseInstruction 0xFA29 `shouldBe` Just (ldFReg 0xA)
    it "parses LD B, Vx" $
      parseInstruction 0xFA33 `shouldBe` Just (ldBReg 0xA)
    it "parses LD [I], Vx" $
      parseInstruction 0xFA55 `shouldBe` Just (ldMemRegs 0xA)
    it "parses LD Vx, [I]" $
      parseInstruction 0xFA65 `shouldBe` Just (ldRegsMem 0xA)

    prop "never parses different words to same instruction" $
      \w1 w2 -> (w1 /= w2) ==>
        let i1 = parseInstruction w1
            i2 = parseInstruction w2
        in isNothing i1 || i1 /= i2

  describe "sys" $
    prop "always fails" $
      \addr -> isError $ execInstruction $ sys addr

  describe "cls" $
    prop "clears the screen" $
      \env state -> (display <$> execSystem env state (execInstruction cls))
                     ==
                    Right initialDisplay

  describe "ret" $ do
    prop "sets the PC to the top of the stack plus one step" $
      \(PC top) -> do push top
                      execInstruction ret
                      x <- getPC
                      return $ x == (top + 2)

    prop "pops the stack" $
      forAll (arbitrary `suchThat` (not . null . stack)) $ \state env ->
        (stack <$> execSystem env state (execInstruction ret)) == Right (tail $ stack state)

    prop "fails on empty stack" $
      \env -> isError (execInstruction ret) env initialSystemState

    prop "fails if return address is at end of memory" $
      isError $ push (memorySize - 2) >> execInstruction ret

  describe "jpAddr" $
    prop "sets the PC to the argument" $
      \(PC addr) -> do execInstruction (jpAddr addr)
                       x <- getPC
                       return $ x == addr

  describe "callAddr" $ do
    prop "jumps to the argument address" $
      \(PC addr) -> do execInstruction (callAddr addr)
                       x <- getPC
                       return $ x == addr

    prop "pushes the current PC onto the stack" $
      \(PC pre) (PC addr) -> do setPC pre
                                execInstruction (callAddr addr)
                                x <- pop
                                return $ x == pre

  describe "seRegByte" $ do
    prop "steps PC by one if register not equal to immediate" $
      \env state (Reg reg) byte ->
        byte /= register state reg ==>
          stepsPCBy 1 (execInstruction $ seRegByte reg byte) env state

    prop "steps PC by two if register equal to immediate" $
      \(Reg reg) byte ->
        stepsPCBy 2 (setReg reg byte >> execInstruction (seRegByte reg byte))

  describe "sneRegByte" $ do
    prop "steps PC by two if register not equal to immediate" $
      \env state (Reg reg) byte ->
        byte /= register state reg ==>
          stepsPCBy 2 (execInstruction $ sneRegByte reg byte) env state

    prop "steps PC by one if register equal to immediate" $
      \(Reg reg) byte ->
        stepsPCBy 1 $ setReg reg byte >> execInstruction (sneRegByte reg byte)

  describe "seRegReg" $ do
    prop "steps PC by one if register not equal to immediate" $
      \env state (Reg regx) (Reg regy) ->
        register state regx /= register state regy ==>
          stepsPCBy 1 (execInstruction $ seRegReg regx regy) env state

    prop "steps PC by two if register equal to immediate" $
      \(Reg regx) (Reg regy) byte ->
        stepsPCBy 2 $ do setReg regx byte
                         setReg regy byte
                         execInstruction (seRegReg regx regy)

  describe "ldRegByte" $ do
    prop "sets the given register to the immediate value" $
      \(Reg reg) byte ->
        do execInstruction (ldRegByte reg byte)
           x <- getReg reg
           return $ x == byte

    prop "steps PC by one" $
      \(Reg reg) byte -> stepsPCBy 1 $ execInstruction (ldRegByte reg byte)

  describe "addRegByte" $ do
    prop "adds the immediate value to the given register" $
      \(Reg reg) byte ->
        do x <- getReg reg
           execInstruction (addRegByte reg byte)
           x' <- getReg reg
           return $ x' == (x + byte)

    prop "steps PC by one" $
      \(Reg reg) byte -> stepsPCBy 1 $ execInstruction (addRegByte reg byte)

  describe "ldRegReg" $ do
    prop "load Y into X" $
      \(Reg regx) (Reg regy) ->
        do y <- getReg regy
           execInstruction (ldRegReg regx regy)
           x <- getReg regx
           return $ x == y

    prop "steps PC by one" $
      \(Reg regx) (Reg regy) -> stepsPCBy 1 $ execInstruction (ldRegReg regx regy)

  describe "orRegReg" $ do
    prop "stores Vx OR Vy into Vx" $
      \regx regy -> checkRegReg regx regy regx orRegReg (Data.Bits..|.)

    prop "steps PC by one" $
      \(Reg regx) (Reg regy) -> stepsPCBy 1 $ execInstruction (orRegReg regx regy)

  describe "andRegReg" $ do
    prop "stores Vx OR Vy into Vx" $
      \regx regy -> checkRegReg regx regy regx andRegReg (Data.Bits..&.)

    prop "steps PC by one" $
      \(Reg regx) (Reg regy) -> stepsPCBy 1 $ execInstruction (andRegReg regx regy)

  describe "xorRegReg" $ do
    prop "stores Vx OR Vy into Vx" $
      \regx regy -> checkRegReg regx regy regx xorRegReg Data.Bits.xor

    prop "steps PC by one" $
      \(Reg regx) (Reg regy) -> stepsPCBy 1 $ execInstruction (xorRegReg regx regy)

  describe "addRegReg" $ do
    prop "stores the lower 8 bits of Vx + Vy into Vx if Vx != VF" $
      \regx regy ->
        regx /= 0xF ==>
          checkRegReg regx regy regx addRegReg $ \x y ->
            let z = (fromIntegral x + fromIntegral y) Data.Bits..&. 0xFF :: Word16
            in fromIntegral z

    prop "sets VF to Vx + Vy > 255" $
      \regx regy -> checkRegReg regx regy 0xF addRegReg $ \x y ->
        let z = (fromIntegral x + fromIntegral y) :: Word16
        in if z > 255 then 1 else 0

    prop "steps PC by one" $
      \(Reg regx) (Reg regy) -> stepsPCBy 1 $ execInstruction (addRegReg regx regy)

  describe "subRegReg" $ do
    prop "stores the lower 8 bits of Vx - Vy into Vx if Vx != VF" $
      \regx regy ->
        regx /= 0xF ==>
          checkRegReg regx regy regx subRegReg $ \x y ->
            let z = (fromIntegral x - fromIntegral y) Data.Bits..&. 0xFF :: Int
            in fromIntegral z

    prop "sets VF to 'not borrow'" $
      \regx regy -> checkRegReg regx regy 0xF subRegReg $ \x y ->
        if x < y then 0 else 1

    prop "steps PC by one" $
      \(Reg regx) (Reg regy) -> stepsPCBy 1 $ execInstruction (subRegReg regx regy)

  describe "shr" $ do
    prop "shifts Vx to the right by one bit if Vx != VF" $
      \regx regy ->
        regx /= 0xF ==>
          checkRegReg regx regy regx shr $ \x _ -> shiftR x 1

    prop "doesn't change Vy unless Vy is Vx or VF" $
      \regx regy ->
        (regy /= 0xF) && (regy /= regx) ==>
          checkRegReg regx regy regy shr $ \_ y -> y

    prop "sets VF to 1 if LSB is set" $
      \regx regy -> checkRegReg regx regy 0xF shr $ \x _ ->
        if testBit x 0 then 1 else 0

    prop "steps PC by one" $
      \(Reg regx) (Reg regy) -> stepsPCBy 1 $ execInstruction (shr regx regy)

  describe "subnRegReg" $ do
    prop "stores the lower 8 bits of Vy - Vx into Vx if Vx != VF" $
      \regx regy ->
        regx /= 0xF ==>
          checkRegReg regx regy regx subnRegReg $ \x y ->
            let z = (fromIntegral y - fromIntegral x) Data.Bits..&. 0xFF :: Int
            in fromIntegral z

    prop "sets VF to 'not borrow'" $
      \regx regy -> checkRegReg regx regy 0xF subnRegReg $ \x y ->
        if y < x then 0 else 1

    prop "steps PC by one" $
      \(Reg regx) (Reg regy) -> stepsPCBy 1 $ execInstruction (shr regx regy)

  describe "shl" $ do
    prop "shifts Vx to the left by one bit if Vx != VF" $
      \regx regy ->
        regx /= 0xF ==>
          checkRegReg regx regy regx shl $ \x _ -> shiftL x 1

    prop "doesn't change Vy unless Vy is Vx or VF" $
      \regx regy ->
        (regy /= 0xF) && (regy /= regx) ==>
          checkRegReg regx regy regy shl $ \_ y -> y

    prop "sets VF to 1 if MSB is set" $
      \regx regy -> checkRegReg regx regy 0xF shl $ \x _ ->
        if testBit x 7 then 1 else 0

    prop "steps PC by one" $
      \(Reg regx) (Reg regy) -> stepsPCBy 1 $ execInstruction (shl regx regy)

  describe "sneRegReg" $ do
    prop "steps PC by two if register not equal to immediate" $
      \env state (Reg regx) (Reg regy) ->
        register state regx /= register state regy ==>
          stepsPCBy 2 (execInstruction $ sneRegReg regx regy) env state

    prop "steps PC by one if register equal to immediate" $
      \(Reg regx) (Reg regy) byte ->
        stepsPCBy 1 $ do setReg regx byte
                         setReg regy byte
                         execInstruction (sneRegReg regx regy)

  describe "ldIAddr" $ do
    prop "sets I to the immediate value" $
      \(Address addr) -> do execInstruction $ ldIAddr addr
                            x <- getRegI
                            return $ x == addr

    prop "steps PC by one" $
      \(Address addr) -> stepsPCBy 1 $ execInstruction (ldIAddr addr)

  describe "jpV0Addr" $
    prop "sets PC to V0 plus the immediate value" $
      let pre (Address a, x) = dest < memorySize && even dest
            where dest = a + fromIntegral x
      in forAll (arbitrary `suchThat` pre) $ \(Address addr, x) -> do
        setReg 0 x
        execInstruction $ jpV0Addr addr
        pc <- getPC
        return $ pc == fromIntegral (addr + fromIntegral x)

  describe "rnd" $ do
    prop "pops off a random value, one at a time" $
      \randoms -> do
        setRandoms randoms
        actual <- replicateM (length randoms) $ do
          setPC 0
          execInstruction $ rnd 0 0xFF
          getReg 0
        return $ actual == randoms

    prop "writes the value to the given register" $
      \value (Reg reg) -> do
        setRandoms [value]
        execInstruction $ rnd reg 0xFF
        (value ==) <$> getReg reg

    prop "ANDs the byte with the random" $
      \value mask (Reg reg) -> do
        setRandoms [value]
        execInstruction $ rnd reg mask
        x <- getReg reg
        return $ (complement mask .&. x) == 0

    prop "steps PC by one" $
      \(Reg reg) mask -> stepsPCBy 1 $ execInstruction (rnd reg mask)

  describe "drw" $ do
    prop "blits the contents of an in-memory sprite" $
      forAll (choose (0, snd displaySize)) $ \n ->
      forAll (choose (0, memorySize - fromIntegral n)) $
        \addr (Reg regx) (Reg regy) -> do
          disp <- display <$> getSystemState
          x <- fromIntegral <$> getReg regx
          y <- fromIntegral <$> getReg regy

          setRegI addr
          execInstruction $ drw regx regy (fromIntegral n)

          disp' <- display <$> getSystemState
          spr <- Bitmap.make (8, n) <$> readMem addr (fromIntegral n)
          return $ disp' == Bitmap.blit disp spr (x, y)

    prop "if any pixel was erased, sets VF to 1" $
      forAll (choose (0, snd displaySize)) $ \n ->
      forAll (choose (0, memorySize - fromIntegral n)) $
        \addr (Reg x) (Reg y) -> do
          disp <- display <$> getSystemState

          setRegI addr
          execInstruction $ drw x y (fromIntegral n)

          disp' <- display <$> getSystemState
          let erased = (not . Bitmap.isBlack) $
                       Bitmap.lift (\a b -> (a `xor` b) .&. a) disp disp'
          regF <- getReg 0xF
          return $ (regF == 1) == erased

    prop "steps PC by one" $
      forAll (choose (0, snd displaySize)) $ \n ->
      forAll (choose (0, memorySize - fromIntegral n)) $
        \addr (Reg regx) (Reg regy) -> stepsPCBy 1 $ do
          setRegI addr
          execInstruction $ drw regx regy (fromIntegral n)

  describe "skp" $ do
    prop "steps PC by two if key is equal to value in register" $
      \key (Reg reg) -> stepsPCBy 2
                          (setReg reg key >> execInstruction (skp reg))
                          (Environment $ Just key)

    prop "steps PC by one if key is not equal to value in register" $
      \key (Reg reg) -> forAll (arbitrary `suchThat` (/= key)) $ \value ->
        stepsPCBy 1
          (setReg reg value >> execInstruction (skp reg))
          (Environment $ Just key)

  describe "sknp" $ do
    prop "steps PC by one if key is equal to value in register" $
      \key (Reg reg) -> stepsPCBy 1
                          (setReg reg key >> execInstruction (sknp reg))
                          (Environment $ Just key)

    prop "steps PC by two if key is not equal to value in register" $
      \key (Reg reg) ->
      forAll (arbitrary `suchThat` (/= key)) $ \value ->
        stepsPCBy 2
          (setReg reg value >> execInstruction (sknp reg))
          (Environment $ Just key)

  describe "ldRegDT" $ do
    prop "load the value of the delay timer into the given register" $
      \(Reg reg) -> do dt <- getDelayTimer
                       execInstruction $ ldRegDT reg
                       regDt <- getReg reg
                       return $ dt == regDt

    prop "steps PC by one" $
      \(Reg reg) -> do stepsPCBy 1 $ execInstruction $ ldRegDT reg

  describe "ldRegKey" $ do
    prop "loads the key into the given register" $
      \(Reg reg) key state -> evalSystem (Environment $ Just key) state $ do
        execInstruction $ ldRegKey reg
        regKey <- getReg reg
        return $ regKey == key

    prop "does not step PC if there is no key" $ do
      \(Reg reg) -> stepsPCBy 0
                      (execInstruction $ ldRegKey reg)
                      (Environment Nothing)

    prop "step PC by one if there is a key" $ do
      \(Reg reg) key -> stepsPCBy 1
                          (execInstruction $ ldRegKey reg)
                          (Environment (Just key))

  describe "ldDTReg" $ do
    prop "loads value of the given register into DT" $
      \(Reg reg) -> do x <- getReg reg
                       execInstruction $ ldDTReg reg
                       dtx <- getDelayTimer
                       return $ x == dtx

    prop "steps PC by one" $ do
      \(Reg reg) -> stepsPCBy 1 $ execInstruction $ ldDTReg reg

  describe "ldSTReg" $ do
    prop "loads value of the given register into ST" $
      \(Reg reg) -> do x <- getReg reg
                       execInstruction $ ldSTReg reg
                       dtx <- getSoundTimer
                       return $ x == dtx

    prop "steps PC by one" $ do
      \(Reg reg) -> stepsPCBy 1 $ execInstruction $ ldSTReg reg

  describe "addIReg" $ do
    prop "adds I and the given register to together and stores in I" $
      \(Reg reg) -> do i <- getRegI
                       x <- getReg reg
                       execInstruction $ addIReg reg
                       i' <- getRegI
                       return $ i' == (fromIntegral x + i)

    prop "steps PC by one" $
      \(Reg reg) -> stepsPCBy 1 $ execInstruction $ addIReg reg

  describe "ldFReg" $ do
    prop "loads the address to the sprite for Vx into I" $
      forAll (choose (0, 0xF)) $ \n (Reg reg) ->
        do setReg reg n
           execInstruction $ ldFReg reg
           addr <- getRegI
           mem <- readMem addr 5
           let offset = fromIntegral n * fromIntegral charSpriteSize
               len = fromIntegral charSpriteSize
               spr = Vector.slice offset len charSprites
           return $ mem == spr

    prop "returned address is in read only area" $
      forAll (choose (0, 0xF)) $ \value (Reg reg) ->
        do setReg reg value
           execInstruction $ ldFReg reg
           addr <- getRegI
           return $ (addr + fromIntegral charSpriteSize) < userMemoryStart

    prop "fails if register contains value > 0xF" $
      forAll (arbitrary `suchThat` (>0xF)) $ \value (Reg reg) ->
        isError $ do setReg reg value
                     execInstruction $ ldFReg reg

    prop "steps PC by one" $
      forAll (choose (0, 0xF)) $ \value (Reg reg) ->
        stepsPCBy 1 $ do setReg reg value
                         execInstruction $ ldFReg reg

  describe "ldBReg" $ do
    prop "stores the value of the given register as a BCD on address I" $
      forAll (choose (userMemoryStart, memorySize - 3)) $
      \addr (Reg reg) value -> do
        setReg reg value
        setRegI addr
        execInstruction $ ldBReg reg
        mem <- readMem addr 3
        return $
          (Vector.foldl' (\a b -> a ++ show b) [] mem)
          ==
          (printf "%03d" value)

    prop "steps PC by one" $
      forAll (choose (userMemoryStart, memorySize - 3)) $ \addr (Reg reg) ->
        stepsPCBy 1 $ do setRegI addr
                         execInstruction $ ldBReg reg

  describe "ldMemRegs" $ do
    prop "stores registers into memory address I" $
      forAll (choose (userMemoryStart, memorySize - 16)) $
      \addr (Reg lastReg) -> do regs <- Vector.generateM
                                        (fromIntegral lastReg + 1)
                                        (getReg . fromIntegral)
                                setRegI addr
                                execInstruction $ ldMemRegs lastReg
                                mem <- readMem addr (fromIntegral lastReg + 1)
                                return $ mem == regs

    prop "steps PC by one" $
      forAll (choose (userMemoryStart, memorySize - 16)) $
      \addr (Reg lastReg) -> stepsPCBy 1 $ do
        setRegI addr
        execInstruction $ ldMemRegs lastReg

  describe "ldRegsMem" $ do
    prop "loads registers from memory address I" $
      forAll (choose (userMemoryStart, memorySize - 16)) $
      \addr (Reg lastReg) -> do mem <- readMem addr (fromIntegral lastReg + 1)
                                setRegI addr
                                execInstruction $ ldRegsMem lastReg
                                regs <- Vector.generateM
                                          (fromIntegral lastReg + 1)
                                          (getReg . fromIntegral)
                                return $ mem == regs

    prop "steps PC by one" $
      forAll (choose (userMemoryStart, memorySize - 16)) $
      \addr (Reg lastReg) -> stepsPCBy 1 $ do
        setRegI addr
        execInstruction $ ldRegsMem lastReg
