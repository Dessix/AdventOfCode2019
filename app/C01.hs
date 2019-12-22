module C01 where

import Control.Monad (liftM)
import Data.Function
import Data.Functor
import Data.Ratio
import Utils

-- Part 1

moduleFuelAmount :: (RealFrac a, Integral b) => a -> b
moduleFuelAmount mass = max (floor (mass / 3) - 2) 0

calculateFuelForCraft :: (RealFrac a, Integral b) => [a] -> b
calculateFuelForCraft craftModules = sum (map moduleFuelAmount craftModules)

readCraftFromLines :: IO [Int]
readCraftFromLines = (fmap (read :: String -> Int)) <$> Utils.getLinesUntilBlank

calculateCraftFuelRequirementsFromConsole :: IO Int
calculateCraftFuelRequirementsFromConsole =
    readCraftFromLines
    <&>
    map fromIntegral
    <&>
    calculateFuelForCraft


-- Part 2

compensatoryFuelAmount :: (RealFrac a, Integral b) => a -> b
compensatoryFuelAmount 0 = 0
compensatoryFuelAmount mass =
    let result = moduleFuelAmount mass in
    result + (compensatoryFuelAmount (fromIntegral result))

calculateFuelForCraftCompensatory :: (RealFrac a, Integral b) => [a] -> b
calculateFuelForCraftCompensatory craftModules = sum (map compensatoryFuelAmount craftModules)

calculateCraftCompensatoryFuelRequirementsFromConsole :: IO Int
calculateCraftCompensatoryFuelRequirementsFromConsole =
    readCraftFromLines
    <&>
    map fromIntegral
    <&>
    calculateFuelForCraftCompensatory
