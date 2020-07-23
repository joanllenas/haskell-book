module Programmers where

data OperatingSystem 
  = GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill 
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgLang 
  = Haskell
  | Agda
  | Idris
  | PureScript 
  deriving (Eq, Show)

data Programmer 
  = Programmer { os :: OperatingSystem
               , lang :: ProgLang 
               } deriving (Eq, Show)


allOperatingSystems :: [OperatingSystem] 
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill , Mac
  , Windows
  ]
allLanguages :: [ProgLang] 
allLanguages =
  [Haskell, Agda, Idris, PureScript]


allProgrammers :: [Programmer]
allProgrammers 
  = [ Programmer os lang
  | os <- allOperatingSystems
  , lang <- allLanguages
  ]

{-
[
Programmer: {os : 'GnuPlusLinux', lang : 'Haskell'},
Programmer: {os : 'GnuPlusLinux', lang : 'Agda'},
Programmer: {os : 'GnuPlusLinux', lang : 'Idris'},
Programmer: {os : 'GnuPlusLinux', lang : 'PureScript'},
Programmer: {os : 'OpenBSDPlusNevermindJustBSDStill', lang : 'Haskell'},
Programmer: {os : 'OpenBSDPlusNevermindJustBSDStill', lang : 'Agda'},
Programmer: {os : 'OpenBSDPlusNevermindJustBSDStill', lang : 'Idris'},
Programmer: {os : 'OpenBSDPlusNevermindJustBSDStill', lang : 'PureScript'},
Programmer: {os : 'Mac', lang : 'Haskell'},
Programmer: {os : 'Mac', lang : 'Agda'},
Programmer: {os : 'Mac', lang : 'Idris'},
Programmer: {os : 'Mac', lang : 'PureScript'},
Programmer: {os : 'Windows', lang : 'Haskell'},
Programmer: {os : 'Windows', lang : 'Agda'},
Programmer: {os : 'Windows', lang : 'Idris'},
Programmer: {os : 'Windows', lang : 'PureScript'}
]
-}
