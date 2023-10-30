import Mushroom
import Data.Maybe

observationStr = "edible,white,brown,white,bell,missing,broad,musty"

observation = fromJust $ readObservation observationStr

observationsStr = 
  unlines [ "edible,stalk-color,cap-color,spore-color,cap-shape,stalk-shape,gill-size,odor"
          , "poison,white,brown,brown,knobbed,missing,narrow,musty"
          , "edible,white,yellow,brown,knobbed,club,narrow,musty"
          , "edible,white,white,brown,bell,club,narrow,musty"
          , "poison,white,white,brown,knobbed,missing,narrow,musty"
          ]

observations = fromJust $ readObservationFile observationsStr


attr = "anise" -- replace "anise" with your representation of the anise smell 
mushA = [ attr ] 
mushB = [] -- has no attributes!
obs = replicate 8 (mushA, Nom) ++ replicate 2 (mushA, NoNom) ++ replicate 60 (mushB, Nom) ++ replicate 30 (mushB, NoNom)
--uncomment the line below once you have replacted 'attr'
--result = numCorrect attr obs
--result should be 38!

sampleTree = buildTree allAttributes observations
--should just split on the club feet! Clearly you should do more testing, try modifying
--observationsStr to see what happens in other cases.
