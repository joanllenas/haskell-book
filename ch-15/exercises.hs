module Exercises where


type Adjective = String
type Adverb = String
type Noun = String
type Exclamation = String

madlibbinBetter' :: Exclamation
                 -> Adverb
                 -> Noun
                 -> Adjective
                 -> String
madlibbinBetter' e adv noun adj 
  = mconcat 
  [ e , "! he said "
  , adv, " as he jumped into his car "
  , noun, " and drove off with his "
  , adj, " wife." 
  ]


