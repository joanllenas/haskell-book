module Vehicles where

data Price 
  = Price Integer 
  deriving (Eq, Show)

data Manufacturer 
  = Mini
  | Mazda
  | Tata
  deriving (Eq, Show)


data Airline 
  = PapuAir
  | CatapultsR'Us
  | TakeYourChancesUnited
  deriving (Eq, Show)

data Vehicle 
  = Car Manufacturer Price
  | Plane Airline
  deriving (Eq, Show)

myCar = Car Mini (Price 14000) 
urCar = Car Mazda (Price 20000) 
clownCar = Car Tata (Price 7000)
doge = Plane PapuAir


isCar :: Vehicle -> Bool 
isCar v = case v of
  Car _ _ -> True
  _ -> False

isPlane :: Vehicle -> Bool 
isPlane p = case p of
  Plane _ -> True
  _ -> False

areCars :: [Vehicle] -> [Bool] 
areCars = map isCar


getManu :: Vehicle -> Manufacturer 
getManu v = case v of
  Car m _ -> m
  _ -> error "only Cars have manufacturers"