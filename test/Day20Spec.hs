module Day20Spec (spec) where

import qualified Data.Map as Map
import Day20 (
  Grid,
  Tile (..),
  addAllSerpents,
  fillGrid,
  flipTile,
  naturalTileEdges,
  renderFinalGrid,
  rotateTile,
 )
import Test.Hspec (Spec, describe, it, shouldBe)

{-# ANN module "HLint: ignore Redundant do" #-}

spec :: Spec
spec = do
  let t3 = Tile{identifier = 0, raw_data = ["123", "456", "789"]}

  describe "naturalTileEdges" $ do
    it "1x1" $ do
      let t = Tile{identifier = 0, raw_data = ["X"]}
      naturalTileEdges t `shouldBe` ["X", "X", "X", "X"]
    it "2x2" $ do
      let t = Tile{identifier = 0, raw_data = ["12", "34"]}
      naturalTileEdges t `shouldBe` ["12", "24", "43", "31"]

  describe "rotateTile" $ do
    it "3x3 r1" $ do
      rotateTile t3 `shouldBe` Tile{identifier = 0, raw_data = ["741", "852", "963"]}
    it "3x3 r2" $ do
      (rotateTile . rotateTile) t3 `shouldBe` Tile{identifier = 0, raw_data = ["987", "654", "321"]}
    it "3x3 r3" $ do
      (rotateTile . rotateTile . rotateTile) t3 `shouldBe` Tile{identifier = 0, raw_data = ["369", "258", "147"]}
    it "3x3 r4" $ do
      (rotateTile . rotateTile . rotateTile . rotateTile) t3 `shouldBe` t3

  describe "flipTile" $ do
    it "3x3 f1" $ do
      flipTile t3 `shouldBe` Tile{identifier = 0, raw_data = ["789", "456", "123"]}
    it "3x3 f2" $ do
      (flipTile . flipTile) t3 `shouldBe` t3

  describe "fillGrid" $ do
    -- 2x2 tiles
    let init_corner = Tile{identifier = 0, raw_data = ["12", "63"]}
    let init_grid = Map.fromList [((0, 0), init_corner)]
    let solution =
          Map.fromList
            [ ((0, 0), Tile{identifier = 0, raw_data = ["12", "63"]})
            , ((0, 1), Tile{identifier = 1, raw_data = ["00", "12"]})
            , ((1, 0), Tile{identifier = 2, raw_data = ["24", "37"]})
            , ((1, 1), Tile{identifier = 0, raw_data = ["05", "24"]})
            ]
    it "no rotations" $ do
      let rem_tiles = zipWith (\ix rd -> Tile{identifier = ix, raw_data = rd}) [0 ..] [["05", "24"], ["00", "12"], ["24", "37"]]
      fillGrid 2 init_grid rem_tiles `shouldBe` solution
    it "one rotation" $ do
      let rem_tiles = zipWith (\ix rd -> Tile{identifier = ix, raw_data = rd}) [0 ..] [["05", "24"], ["10", "20"], ["24", "37"]]
      fillGrid 2 init_grid rem_tiles `shouldBe` solution
    it "all rotated" $ do
      let rem_tiles = zipWith (\ix rd -> Tile{identifier = ix, raw_data = rd}) [0 ..] [["20", "45"], ["10", "20"], ["73", "42"]]
      fillGrid 2 init_grid rem_tiles `shouldBe` solution
    it "one flip" $ do
      let rem_tiles = zipWith (\ix rd -> Tile{identifier = ix, raw_data = rd}) [0 ..] [["05", "24"], ["00", "12"], ["42", "73"]]
      fillGrid 2 init_grid rem_tiles `shouldBe` solution
    it "all flipped" $ do
      let rem_tiles = zipWith (\ix rd -> Tile{identifier = ix, raw_data = rd}) [0 ..] [["24", "05"], ["00", "21"], ["73", "42"]]
      fillGrid 2 init_grid rem_tiles `shouldBe` solution

  describe "addAllSerpents" $ do
    it "3x3 grid" $ do
      let final_render = Map.fromList [((0, 0), '.'), ((0, 1), '#'), ((0, 2), '#'), ((1, 0), '#'), ((1, 1), '.'), ((1, 2), '.'), ((2, 0), '#'), ((2, 1), '.'), ((2, 2), '#')]
      let serpents = [[(0, 0), (1, -1), (2, -1)]]
      addAllSerpents final_render serpents [] `shouldBe` [(0, 1), (1, 0), (2, 0)]

  describe "renderFinalGrid" $ do
    it "Basic single 2x2 tile" $ do
      let t = Tile{identifier = 0, raw_data = ["12", "34"]}
      renderFinalGrid (Map.fromList [((0, 0), t)]) `shouldBe` Map.fromList [((0, 0), '3'), ((0, 1), '1'), ((1, 0), '4'), ((1, 1), '2')]
    it "Four 2x2 tiles" $ do
      -- Arranged as:
      -- t1 t2
      -- t3 t4
      let t1 = Tile{identifier = 0, raw_data = ["12", "34"]}
      let t2 = Tile{identifier = 0, raw_data = ["23", "45"]}
      let t3 = Tile{identifier = 0, raw_data = ["34", "56"]}
      let t4 = Tile{identifier = 0, raw_data = ["45", "67"]}
      renderFinalGrid (Map.fromList [((0, 1), t1), ((1, 1), t2), ((0, 0), t3), ((1, 0), t4)]) `shouldBe` Map.fromList [((0, 0), '5'), ((0, 1), '3'), ((0, 9), '3'), ((0, 10), '1'), ((1, 0), '6'), ((1, 1), '4'), ((1, 9), '4'), ((1, 10), '2'), ((9, 0), '6'), ((9, 1), '4'), ((9, 9), '4'), ((9, 10), '2'), ((10, 0), '7'), ((10, 1), '5'), ((10, 9), '5'), ((10, 10), '3')]
    it "One 10x10 with 3 2x2" $ do
      let t1 = Tile{identifier = 0, raw_data = ["12", "34"]}
      let t2 = Tile{identifier = 0, raw_data = ["23", "45"]}
      let t3 = Tile{identifier = 0, raw_data = ["ABCDEFGHIJ", "0123456789", "ABCDEFGHIJ", "0123456789", "ABCDEFGHIJ", "0123456789", "ABCDEFGHIJ", "0123456789", "ABCDEFGHIJ", "0123456789"]}
      let t4 = Tile{identifier = 0, raw_data = ["45", "67"]}
      renderFinalGrid (Map.fromList [((0, 1), t1), ((1, 1), t2), ((0, 0), t3), ((1, 0), t4)]) `shouldBe` Map.fromList [((0, 0), '5'), ((0, 1), '3'), ((0, 9), '3'), ((0, 10), '1'), ((1, 0), '6'), ((1, 1), '4'), ((1, 9), '4'), ((1, 10), '2'), ((9, 0), '6'), ((9, 1), '4'), ((9, 9), '4'), ((9, 10), '2'), ((10, 0), '7'), ((10, 1), '5'), ((10, 9), '5'), ((10, 10), '3')]