{-# LANGUAGE ScopedTypeVariables #-}

module Day21 (day21) where

import Data.Function (on)
import Data.List (intercalate, sortBy)
import Data.List.Split
import Data.Set (Set)
import qualified Data.Set as Set
import Debug.Trace

data Rule = Rule {ingredients :: Set String, allergens :: Set String} deriving (Show)

day21 :: IO ()
day21 = do
  putStrLn "day21 start"
  contents <- readFile "data/day21"
  -- Part 1
  let rules = parse contents
  print . show . take 3 $ rules
  print . show $ possibleCauses "fish" rules
  print . show $ possibleCauses "wheat" rules
  print . show $ allAllergens rules
  print . show $ allPossibleCauses rules
  print . show . Set.size $ allIngredients rules
  print . show $ part1 rules
  -- Part 2
  print . show . Set.map (`possibleCauses'` rules) . allAllergens $ rules
  print . canonicalList $ rules
  putStrLn "day21 end"

parse :: String -> [Rule]
parse = map parseRule . lines

parseRule :: String -> Rule
parseRule s = Rule{ingredients = ings, allergens = alls}
 where
  ings = Set.fromList $ words ingredients_part
  alls = Set.fromList $ splitOn ", " . init $ allergens_part
  (ingredients_part : allergens_part : _) = splitOn " (contains " s

allAllergens :: [Rule] -> Set String
allAllergens = Set.unions . map allergens

allIngredients :: [Rule] -> Set String
allIngredients = Set.unions . map ingredients

possibleCauses :: String -> [Rule] -> Set String
possibleCauses allergen =
  intersections . map ingredients . filter (Set.member allergen . allergens)

allPossibleCauses :: [Rule] -> Set String
allPossibleCauses rules = Set.unions . Set.map (`possibleCauses` rules) . allAllergens $ rules

-- Count number of ingredient instances that aren't allergic
part1 :: [Rule] -> Int
part1 rules = length $ inertIngredientsList rules

inertIngredientsList :: [Rule] -> [String]
inertIngredientsList rules = concatMap (Set.toList . (\x -> x Set.\\ allPossibleCauses rules) . ingredients) rules

inertIngredientsSet :: [Rule] -> Set String
inertIngredientsSet = Set.fromList . inertIngredientsList

-- Work out the ingredient for each allergen
possibleCauses' :: String -> [Rule] -> Set String
possibleCauses' allergen rules = possibleCauses allergen rules Set.\\ inertIngredientsSet rules

canonicalList :: [Rule] -> String
canonicalList = intercalate "," . map snd . sortBy (compare `on` fst) . dangerousIngredients

dangerousIngredients :: [Rule] -> [(String, String)]
dangerousIngredients rules = processOfElimination initial_list []
 where
  initial_list = zip allergen_list (map (`possibleCauses'` rules) allergen_list)
  allergen_list = Set.toList $ allAllergens rules

processOfElimination :: [(String, Set String)] -> [String] -> [(String, String)]
processOfElimination [] _ = []
processOfElimination rem_rules taken = next_eliminated : processOfElimination rem_rules' taken'
 where
  next_eliminated = (allergen, ingredient)
  allergen = fst next_rule
  [ingredient] = Set.toList . snd $ next_rule
  (next_rule : rem_rules') = sortBy (compare `on` Set.size . snd) . map (`removeTaken` taken) $ rem_rules
  taken' = ingredient : taken

removeTaken :: (String, Set String) -> [String] -> (String, Set String)
removeTaken (allergen, possible_ingredients) taken = (allergen, possible_ingredients Set.\\ Set.fromList taken)

-- Get the equivalent to Set.unions but for intersections
intersections :: (Ord a) => [Set a] -> Set a
intersections [s] = s
intersections (s : ss) = foldl Set.intersection s ss