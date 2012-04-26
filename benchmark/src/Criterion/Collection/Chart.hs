module Criterion.Collection.Chart
  ( errBarChart
  , defaultColors
  ) where


import Criterion.Measurement
import Data.Accessor
import Data.Colour
import Data.Colour.Names
import Graphics.Rendering.Chart                    hiding (Vector)

import Criterion.Collection.Sample


defaultColors :: [AlphaColour Double]
defaultColors = cycle $ map opaque [
                 blue,
                 red,
                 brown,
                 black,
                 darkgoldenrod,
                 coral,
                 cyan,
                 darkcyan,
                 darkkhaki,
                 darkmagenta,
                 darkslategrey
                ]


plotErrBars :: String
            -> CairoLineStyle
            -> [SampleData]
            -> Plot Double Double
plotErrBars name lineStyle samples = toPlot plot
  where
    value sd = symErrPoint size m 0 s
      where
        size  = fromIntegral $ sdInputSize sd
        (m,s) = computeMeanAndStddev sd

    plot = plot_errbars_values ^= map value samples
         $ plot_errbars_line_style ^= lineStyle
         $ plot_errbars_title ^= name
         $ defaultPlotErrBars


plotPoints :: String
           -> CairoPointStyle
           -> [SampleData]
           -> Plot Double Double
plotPoints name pointStyle samples = toPlot plot
  where
    value sd = (fromIntegral size, m)
      where
        size  = sdInputSize sd
        (m,_) = computeMeanAndStddev sd

    plot = plot_points_values ^= map value samples
         $ plot_points_style ^= pointStyle
         $ plot_points_title ^= name
         $ defaultPlotPoints


errBarChart :: Bool
            -> Double
            -> String
            -> [(AlphaColour Double, String, [SampleData])]
            -> Renderable ()
errBarChart logPlot lineWidth plotTitle plotData = toRenderable layout
  where
    mkPlot (colour, plotName, samples) = joinPlot eb pts
      where
        lStyle = line_width ^= lineWidth
               $ line_color ^= colour
               $ defaultPlotErrBars ^. plot_errbars_line_style

        pStyle = filledCircles (1.5 * lineWidth) colour

        eb  = plotErrBars plotName lStyle samples
        pts = plotPoints plotName pStyle samples

    remapLabels = axis_labels ^: f
      where
        f labels = map (map g) labels
        g (x,_) = (x, secs x)

    axisfn = if logPlot
               then autoScaledLogAxis defaultLogAxis
               else autoScaledAxis defaultLinearAxis

    layout = layout1_title ^= plotTitle
           $ layout1_background ^= solidFillStyle (opaque white)
           $ layout1_left_axis ^: laxis_generate ^= axisfn
           $ layout1_left_axis ^: laxis_override ^= remapLabels
           $ layout1_left_axis ^: laxis_title ^= "Time (seconds)"
           $ layout1_bottom_axis ^: laxis_generate ^= axisfn
           $ layout1_bottom_axis ^: laxis_title ^= "# of items in collection"
           $ layout1_plots ^= (map (Left . mkPlot) plotData)
           $ defaultLayout1
