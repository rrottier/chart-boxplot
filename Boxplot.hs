{-# LANGUAGE ConstraintKinds,
             DataKinds,
             FlexibleContexts,
             GADTs,
             OverloadedStrings,
             PatternSynonyms,
             ScopedTypeVariables,
             TypeOperators #-}

module Boxplot where

import qualified Data.Foldable       as F
import           Frames
import           Lens.Family hiding ((.~))
import Graphics.Rendering.Chart.Easy hiding (view, (^.), LensLike', over)
import Graphics.Rendering.Chart.Backend.Cairo
import Statistics.Quantile (weightedAvg)
import qualified Data.Vector as V
import qualified Data.Map as M

type BoxValues = (Double, Double, Double, Double, Double)

lineStyle :: Double -> Colour Double -> LineStyle
lineStyle n colour = line_width .~ n
                   $ line_color .~ opaque colour
                   $ def

candle label color vals = liftEC $ do
  plot_candle_line_style  .= lineStyle 1 color
  plot_candle_fill .= True
  plot_candle_rise_fill_style .= solidFillStyle (opaque color)
  plot_candle_fall_fill_style .= solidFillStyle (opaque color)
  plot_candle_tick_length .= 2
  plot_candle_width .= 20
  plot_candle_centre .= 10
  plot_candle_values .= [ Candle ind mn first med third mx | (ind,(mn,first,med,third,mx)) <- vals]
  plot_candle_title .= label

boxValues :: (Integral a) => FoldLike a (Record rs) (Record rs) a a -> [Record rs] -> BoxValues
boxValues t fs = (V.minimum xs, q 1 xs, q 2 xs, q 3 xs, V.maximum xs)
                 where
                 xs = V.fromList (map (fromIntegral . view t) fs)
                 q p = weightedAvg p 4

calcBoxplot :: (Ord s, Integral a) => FoldLike a (Record rs) (Record rs) a a ->
                                      M.Map s [Record rs] ->
                                      [(Double, BoxValues)]
calcBoxplot t fs = zip [0.5..] $ map (\k -> boxValues t (fs M.! k)) $ M.keys fs



-- | Create a boxplot in a file by using the supplied variables for grouping and plotting
--boxPlot <filename> <variable to plot> <variable to use for grouping> <frame to plot>
boxPlot :: (Foldable f, Integral a, Ord s) => FilePath ->
                                              FoldLike a (Record rs) (Record rs) a a ->
                                              FoldLike s (Record rs) (Record rs) s s ->
                                              f (Record rs) ->
                                              IO ()
boxPlot fn xs grp fs = toFile def fn $ plot (candle "" blue (calcBoxplot xs (groupBy grp fs)))

groupBy :: (Ord s, Foldable f) => FoldLike s (Record rs) (Record rs) s s -> f (Record rs) -> M.Map s [Record rs]
groupBy t fs = F.foldr (\x mp -> M.insertWith (++) (view t x) [x] mp) M.empty $ F.toList fs

