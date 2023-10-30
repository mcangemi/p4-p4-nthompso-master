module Mushroom where
import Data.Ratio ((%))
import Data.Tuple (swap)
import Data.List
import Data.List.Split
import Data.Maybe
import Debug.Trace


--
-- --                                      Pre-defined functions.
-- 
-- These functions are used to convert between counts, rationals, and string percentages. The counts
-- are similar to those used in the summaries for Project 1. You may not need these functions at all,
-- depending on your implementation choices.

outOf :: Int -> Int -> Rational
outOf a b =  (fromIntegral a) % (fromIntegral b)

ratioOfCount :: (Int, Int) -> Rational
ratioOfCount (a,b) = (fromIntegral a) % (fromIntegral $ a+b)

percentOfRatio :: Rational -> String
percentOfRatio r = (show $ truncate $ 100 * r) ++ "%"

percentOfCount :: (Int, Int) -> String
percentOfCount c = percentOfRatio $ ratioOfCount c 


-- All undefined values and functions should be completed. Your code will compile 
-- even if some functions are left undefined.

--
-- --                                       Milestone Part One
--

-- Mushroom edibility is either Nom or NoNom. We should only eat mushrooms we are pretty certain are
-- Noms. Please do not actually eat any mushrooms based on advice from this project.
data Edible = Nom | NoNom deriving (Show, Eq, Ord)

--Define an algebraic data type for all the different possible attributes.
--You may decompose this by having sub-types (like Operators and Tokens for the class project),
--but every possible attribute should be representable as an Attribute.
--You may not use Chars, Strings, or Integers in this type.  You should derive Eq and Show (at least to
--start). You may also derive Ord, but do not rely on the ordering.
data Attribute = StalkColor | CapColor | SporeColor | CapShape | StalkShape | GillSize | Odor deriving (Show, Eq)
newtype StalkColor = Hue Color deriving (Eq)
newtype CapColor = Shade Color deriving (Eq)
newtype SporeColor = Tint Color deriving (Eq)
data CapShape = Bell | Conical | Knobbed deriving (Eq)
data StalkShape = Bulbous | Club | Missing deriving (Eq)
data GillSize = Broad | Narrow deriving (Eq)
data Odor = Almond | Anise | Foul | None | Musty deriving (Eq)
data Color = Brown | Green | Purple | White | Yellow deriving (Eq)


-- Make a list of all possible attributes. There should be 28 different attributes.
allAttributes :: [Attribute]
allAttributes = [StalkColor, CapColor, SporeColor, CapShape, StalkShape, GillSize, Odor]
--A mushroom is a list of attributes.
type Mushroom = [Attribute]

--An observation is a mushroom that is known to be nomable, or not nomable.  Thus, it is a tuple of
--a mushroom and an edibility.
type Observation = (Mushroom, Edible)

-- readObservation takes a single line of the input file, in the format described on the
-- project page, and return the corresponding observation.  You may find the splitOn function
-- helpful. splitOn takes a string delimiter, a string to be split, and splits the string based on
-- the delimiter.  For instance (words str) is the same as (splitOn " " str)
-- I suggest you make helper functions for reading different columns.
-- The sequence function may be helpful.
readObservation :: String -> Maybe Observation
readObservation = undefined
--readObservation = splitOn Edible? " ,"? theFile

-- readObservationFile takes the entire contents of an entire file and return the list of
-- observations. Note the first line has header information and is not a valid observation. 
-- The lines function may be helpful. 
-- The sequence function may be helpful.
readObservationFile :: String ->  Maybe [Observation]
readObservationFile = undefined 

--NOTE: Look up lines, splitOn, and sequence.
-- --                                       Milestone Part One
--

--numCorrect computes how much information about edibility can be gained by checking a specific
--attribute. It takes a single attribute and a list of observations, and answers the question: 
--"If all we had to go on was this attribute, how many mushrooms would we label correctly?"
--1) Split the observations into the those with the attribute, and those without the attribute. 
--2) One of these sets will have a higher percentage of edible mushrooms. Call that set A, and the
--   other set B. Note that A may be the set with the attribute, or the set without the attribute.
--3) If mushrooms in set A are assumed to be edible, and those in the other set Bare assumed to be
--   inedible, return the number of correct guesses.
--4) Important: if either set is empty, no information is gained by looking at the attribute. Return 0.
--
--You may find the built-in partition function useful.
--
-- This was done in the in-class activity! Refer to that for assistance.
numCorrect :: Attribute -> [Observation] -> Int
numCorrect att lobs = 
    let nomAt = length[inat | (inat, edi) <- lobs, att `elem` inat, edi == Nom]
        noNomAt = length[inat | (inat, edi) <- lobs, att `elem` inat, edi == NoNom]
    in if nomAt == 0 || noNomAt == 0 then 0 else max nomAt noNomAt

-- A decision tree is a binary tree that stores the likelihood of a mushroom being edible based on
-- its attributes.  Decision nodes are labeled with an attribute and have two children, with the
-- left child applying to mushrooms with that attribute, and the right child applying to mushrooms
-- without that attribute.  End nodes are leaves, and  should store enough information to compute
-- the percent chance of a mushroom being edible.  Do not store lists of observations or mushrooms.
-- Doubles are likely not precise enough, but Rationals or tuples (or even triples) of Integers will
-- be sufficient.

-- Define an algebraic data type for decision trees.
data DTree = UnfinishedTree deriving Show

-- Given a list of attributes and a list of observations, build a decision tree.
--  * If all the observations have the same edibility, you can safely make an end node: there is no
--    need to further analyze a mushroom.  
--  * If all the observations have the same attributes, you must make an end node : there is no way
--    to futher analyze a mushroom.
--  * Otherwise, go through the list of attributes and find the one that gives you the most
--    information about edibility, as measured by the number of correct guesses that can be obtained if
--    this was the only attribute used.  Then create a decision node using that attribute as a pivot.
--  * For efficiency, you can delete the pivot from the list of attributes, but it doesn't really
--    matter.
--  * You should create helper functions for this problem. 
buildTree :: [Attribute] -> [Observation] -> DTree
buildTree = undefined
--buildTree attributes observations = 
--    if sameEdibility observations || sameAttributes observations
--        then Leaf (ratioOfCount (edibleRatio observations))
--        else 
--            let best = bestAttribute attributes observations
--                (withAttr, withoutAttr) = partition (\(m, e) -> best `elem` m) observations
--                -- newAttributes = filter (\a -> a == best) attributes
--            in Node best (buildTree attributes withAttr) (buildTree attributes withoutAttr) 
--missing helper functions


--
----                                     Core Project
--

-- rateMushroom takes a mushroom, a decision tree, and a safety limit, and returns a string
-- describing if we can eat the mushroom.  Follow the decision tree for this mushroom, and check if
-- the corresponding end node estimates the chance of edibility to be higher than the safety limit.
-- If it is greater than the safety limit, return the string "Eat the mushroom" 
-- If it is less than or equal to the safety limit, return the string "Do not eat the mushroom"
-- For full credit, append the estimate to the string similar to the below:
--   "Eat the mushroom: estimate a 95% chance of being edible."
--   "Do not eat the mushroom: estimate a 40% chance of being poisonous."
-- The ``precentOfRatio`` and ``percentOfCount`` functions may be helful.

--how do you work with rationals again?
rateMushroom :: Mushroom -> DTree -> Rational -> String
rateMushroom = undefined

-- buildGuide takes a decision tree, a safety limit, and return an itemized guide. 
-- Each line is numbered separately and should have one of two forms.
--  "n: Eat the mushroom." / "n: Do not eat the mushroom."
--  "n: If the mushroom has (attribute) go to step x, otherwise go to step y."
-- For this implementation, every node in the tree will have a separate line.
-- You will need helper functions.
buildGuide :: DTree -> Rational -> [String]
buildGuide = undefined
--buildGuide (Node attr left right) limit = helper attr left ++ helper attr right

--helper :: Attribute -> [(Attribute, Maybe Attribute)]
    --helper a (leaf x) = [(a, Nothing)]
    --helper a (Node attr left right) = (a,attr)
    
--this is two different people trying to help me make my own shit but i have no idea what im doing and they cant fix me
--from what I can tell you have a DTree which is a Node containg an attribute and a left and right child?
-- the helper function goes through trying to go throught the tree to detemine what line itshould say.
-- What we really need is some recursive calls if it has children and slightly different lines we'll link them later)






--probably going to ignore this?
--                                     Full Credit
--

-- For the first full credit, improve on the derived Show instance for attributes.  Make a custom
-- instance of Show that returns a proper English clause describing the attribute. For instance, "a
-- club stalk", "narrow gills", or "an almond odor." 


-- For the second full credit portion, you will eliminate redundancies in the guide. This will be
-- done using common subexpression elimination. We will keep an index mapping strings to integer
-- locations. Since indexes are useful for other types, we will write generic functions.
type Index a = [(a, Int)]

-- makeEntry adds an element to the index. It returns the location of the element in 
-- the index and, if necessary, an updated index.
-- If the element is already in the index, you should not add it again. 
-- If it does not occur in the index, find the next largest location and associate that element with
-- that location.
-- Index locations should start at 1.
makeEntry :: Eq a => a -> Index a -> (Int, Index a)
makeEntry = undefined


-- For instance: makeEntry 'x' [('a',1),('x',2),('b',3)] = (2, [('a',1),('x',2),('b',3)])
-- For instance: makeEntry 'y' [('a',1),('x',2),('b',3)] = (4, [('a',1),('x',2),('b',3),('y',4)])
-- The order of the entries in the index does not matter, and will quite likely be reversed.

-- Once makeEntry is working, make a version of buildGuide (buildGuideCSE) that passes around an index of
-- strings.  When you want to add a string to the index, use makeEntry to avoid creating duplicates.
-- As a natural consequence of this, you will return an "upside-down" guide: the entry point will
-- have the largest location.

buildGuideCSE :: DTree -> Rational -> [String]
buildGuideCSE = undefined

-- For extra credit, change indexes from association lists to association binary search trees.
