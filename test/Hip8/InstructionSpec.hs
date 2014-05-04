module Hip8.InstructionSpec (Hip8.InstructionSpec.spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Hip8.System
import Hip8.Instruction
import Hip8.SystemSpec
import Data.Maybe
import Control.Applicative

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
      \env state -> (displayBuffer <$> execSystem env state (execInstruction cls)) == Right initialDisplay

  describe "ret" $
    prop "sets the PC to the top of the stack + 2" $
      forAll (arbitrary `suchThat` (not . null . stack)) $ \state env ->
        let stackHead = head $ stack state
            pc = evalSystem env state (execInstruction ret >> getPC)
        in (stackHead <= memorySize - 4) ==> pc == Right (stackHead + 2)

  describe "jpAddr" $
    prop "sets the PC to the argument" $
      forAll validPC $ \addr env state ->
        evalSystem env state (execInstruction (jpAddr addr) >> getPC) == Right addr
