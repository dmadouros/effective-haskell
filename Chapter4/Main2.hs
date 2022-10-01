module Aliases where

  type Meters = Double
  type Seconds = Double
  type MetersPerSecond = Double

  -- velocity :: Meters -> Seconds -> MetersPerSecond
  velocity :: Seconds -> Meters -> MetersPerSecond
  velocity meters seconds = meters / seconds

  gravity :: Double
  gravity =
    let
      meters = 9.8 :: Double
      seconds = 1.0 :: Double
    in velocity seconds meters