module Day7Spec where

import Test.Hspec
import Day7

test_day7 :: IO ()
test_day7 = hspec $ do
  describe "runIntCodeWithInputs" $ do
    it "1,0,0,0,99" $ do
      runIntCodeWithInputs "1,0,0,0,99" [] `shouldBe` ([2,0,0,0,99], ([], []))

    it "2,3,0,3,99" $ do
      runIntCodeWithInputs "2,3,0,3,99" [] `shouldBe` ([2,3,0,6,99], ([], []))

    it "2,4,4,5,99,0" $ do
      runIntCodeWithInputs "2,4,4,5,99,0" [] `shouldBe` ([2,4,4,5,99,9801], ([], []))

    it "1,1,1,4,99,5,6,0,99" $ do
      runIntCodeWithInputs "1,1,1,4,99,5,6,0,99" [] `shouldBe` ([30,1,1,4,2,5,6,0,99], ([], []))

    it "Test input: 3,3,99,0" $ do
      runIntCodeWithInputs "3,3,99,0" [5] `shouldBe` ([3,3,99,5], ([], []))

    it "Test output: 1,0,0,0,4,0,99" $ do
      runIntCodeWithInputs "1,0,0,0,4,0,99" [] `shouldBe` ([2,0,0,0,4,0,99], ([], [2]))

    it "Test direct params - website example" $ do
      runIntCodeWithInputs "1002,4,3,4,33" [] `shouldBe` ([1002,4,3,4,99], ([], []))

    it "Test direct params - website example 2" $ do
      runIntCodeWithInputs "1101,100,-1,4,0" [] `shouldBe` ([1101,100,-1,4,99], ([], []))

    it "Test JumpIfTrue - direct params" $ do
      runIntCodeWithInputs "1105,1,1,0,6,99,0" [] `shouldBe` ([1105,1,1,0,6,99,1106], ([], []))

    it "Test JumpIfTrue - indirect params" $ do
      runIntCodeWithInputs "5,2,6,0,6,99,1" [] `shouldBe` ([5,2,6,0,6,99,5], ([], []))

    it "Test JumpIfTrue - no jump" $ do
      runIntCodeWithInputs "1105,0,1,99" [] `shouldBe` ([1105,0,1,99], ([], []))

    it "Test JumpIfFalse - direct params" $ do
      runIntCodeWithInputs "1106,0,2,1,6,7,99,0" [] `shouldBe` ([1106,0,2,1,6,7,99,0], ([], []))

    it "Test JumpIfFalse - indirect params" $ do
      runIntCodeWithInputs "6,6,4,99,1,6,0,9,99,0" [] `shouldBe` ([6,6,4,99,1,6,0,9,99,6], ([], []))

    it "Test JumpIfFalse - no jump" $ do
      runIntCodeWithInputs "1006,0,1,99" [] `shouldBe` ([1006,0,1,99], ([], []))

    it "Test LessThan - a < b" $ do
      runIntCodeWithInputs "1107,0,1,5,99,8" [] `shouldBe` ([1107,0,1,5,99,1], ([], []))

    it "Test LessThan - a = b" $ do
      runIntCodeWithInputs "1107,1,1,5,99,8" [] `shouldBe` ([1107,1,1,5,99,0], ([], []))

    it "Test LessThan - a > b" $ do
      runIntCodeWithInputs "1107,2,1,5,99,8" [] `shouldBe` ([1107,2,1,5,99,0], ([], []))

    it "Test LessThan - a < b, indirect" $ do
      runIntCodeWithInputs "7,0,4,5,99,8" [] `shouldBe` ([7,0,4,5,99,1], ([], []))

    it "Test Equals - a < b" $ do
      runIntCodeWithInputs "1108,0,1,5,99,8" [] `shouldBe` ([1108,0,1,5,99,0], ([], []))

    it "Test Equals - a = b" $ do
      runIntCodeWithInputs "1108,1,1,5,99,8" [] `shouldBe` ([1108,1,1,5,99,1], ([], []))

    it "Test Equals - a > b" $ do
      runIntCodeWithInputs "1108,2,1,5,99,8" [] `shouldBe` ([1108,2,1,5,99,0], ([], []))

    it "Test Equals - a = b, indirect" $ do
      runIntCodeWithInputs "8,0,5,5,99,8" [] `shouldBe` ([8,0,5,5,99,1], ([], []))

    it "Test multiple inputs" $ do
      runIntCodeWithInputs "3,2,0,4,0" [3,99] `shouldBe` ([3,2,3,4,99], ([], []))

    it "Test multiple inputs with direct" $ do
      runIntCodeWithInputs "103,2,3,4,0" [93,99] `shouldBe` ([103,93,3,4,99], ([], []))

    it "Test multiple outputs" $ do
      runIntCodeWithInputs "4,0,104,912,99" [] `shouldBe` ([4,0,104,912,99], ([], [4,912]))

  describe "chainAmps" $ do
    it "Website example 1" $ do
      -- Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0):
      chainAmps "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" [4,3,2,1,0] 0 `shouldBe` 43210

    it "Website example 2" $ do
      -- Max thruster signal 54321 (from phase setting sequence 0,1,2,3,4):
      chainAmps "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0" [0,1,2,3,4] 0 `shouldBe` 54321

    it "Website example 3" $ do
      -- Max thruster signal 65210 (from phase setting sequence 1,0,4,3,2):
      chainAmps "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" [1,0,4,3,2] 0 `shouldBe` 65210

  describe "bestAmplification" $ do
    it "Website example 1" $ do
      -- Max thruster signal 43210 (from phase setting sequence 4,3,2,1,0):
      bestAmplification "3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" `shouldBe` 43210

    it "Website example 2" $ do
      -- Max thruster signal 54321 (from phase setting sequence 0,1,2,3,4):
      bestAmplification "3,23,3,24,1002,24,10,24,1002,23,-1,23,101,5,23,23,1,24,23,23,4,23,99,0,0" `shouldBe` 54321

    it "Website example 3" $ do
      -- Max thruster signal 65210 (from phase setting sequence 1,0,4,3,2):
      bestAmplification "3,31,3,32,1002,32,10,32,1001,31,-2,31,1007,31,0,33,1002,33,7,33,1,33,31,31,1,32,31,31,4,31,99,0,0,0" `shouldBe` 65210
