module SpaceAge (Planet(..), ageOn) where

data Planet =
  | Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune

ageOn :: Planet -> Float -> Float
ageOn planet seconds = case planet of
  Mercury -> earthYear / 0.2408467
  Venus -> earthYear / 0.61519726
  Mars -> earthYear / 1.8808158
  Jupiter -> earthYear /11.862615
  Saturn -> earthYear / 29.447498
  Uranus -> earthYear / 84.016846
  Neptune -> earthYear / 164.79132
  _ -> earthYear
  where earthYear = seconds / 31557600
