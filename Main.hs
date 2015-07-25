{-# LANGUAGE ConstraintKinds,
             DataKinds,
             FlexibleContexts,
             GADTs,
             OverloadedStrings,
             PatternSynonyms,
             QuasiQuotes,
             ScopedTypeVariables,
             TemplateHaskell,
             TypeOperators,
             ViewPatterns #-}

import           Boxplot

import Frames

tableTypes "CDC" "data/cdc.csv"

loadCDC :: IO (Frame CDC)
loadCDC = inCoreAoS $ readTable "data/cdc.csv"

main :: IO ()
main = do
     c <- loadCDC
     boxPlot "test1.png" weight gender c
     boxPlot "test2.png" weight genhlth c
