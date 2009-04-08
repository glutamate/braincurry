{-# LANGUAGE FlexibleInstances, UndecidableInstances, OverlappingInstances #-}

module Charts where

--import Graphics.Rendering.Chart.Simple
import Graphics.Rendering.Chart hiding (Plot, Legend, toPlot, ToPlot) 
import qualified Graphics.Rendering.Chart as C 
import Graphics.Rendering.Chart.Gtk
import Control.Concurrent
import Waves4
import ArrayWave
import Data.Array.Vector
--import ApparatusPersistence
import SaveWaves hiding (Col)
import Data.Accessor
import Data.List

--interactive


data AxisNames = L | R | T | B deriving (Eq, Show)

instance Eq Color where
    c1 == c2 = False

data PlotStyle = Lines | FilledCircles | LineWidth Double | Legend String | Col Color deriving (Eq)

data GraphStyle = Title String | Axes [AxisNames] | Height Int 
                | Width Int deriving (Eq, Show) -- | HWeight Int | WWeight Int

--data FigStyle = Foo -- Height Int | Width Int deriving (Eq, Show)

data Plot = Plot [(Double,Double)] [PlotStyle]
data Graph = Plots [Plot] [GraphStyle] 
--data Figure = FigGraph Graph | FigBesides [Figure] | FigAbove [Figure] 

--plotWindow [tms w,pts w]
           
tms w= map (p2t w) [0..(npnts w -1)]
pts w = pntsAsList w

t n = do w' <- loadWave $ "/home/tomn/waves/"++show n++"_ecVoltage.twv"
         let w = downSample 1000 w' --restrict w' 4 6)
         print $ npnts w
         let mrks = [(x,0) | x <- [1..6]]::[(Double,Double)]
         plotGraph $ (w % blue <+> mrks % red % "some dots" )%%(Width 640)%%"foo"
         return ()

--main = t 8017

noGrid = \mf pts-> case mf pts of
                     Just ad -> Just $ axisGridNone ad
                     noth -> noth

--plotLayout ::  ToGraph g => g -> IO ()
graphRenderable :: (ToGraph a) => a -> Renderable ()
graphRenderable togr = toRenderable layout 
    where (Plots plots styles) = toGraph togr
          layout = layout1_title ^= [s | Title s <- styles] `headOr` "Graph"
                   $ updateXAxesData (noGrid) 
                   $ updateYAxesData (noGrid) 
                   $ setAxes [ anms | Axes anms <- styles ] 
                   $ layout1_plots ^= (map plotToLayout plots) 
                   $ defaultLayout1
          setAxes [] lay =  layout1_top_axis ^= noAxis $ layout1_right_axis ^= noAxis $ lay
          setAxes (axnms:_) lay = remAxes ([L,R,T,B]\\axnms) lay
          remAxes [] lay = lay
          remAxes (L:as) lay = layout1_left_axis ^= noAxis $ remAxes as lay
          remAxes (R:as) lay = layout1_right_axis ^= noAxis $ remAxes as lay
          remAxes (T:as) lay = layout1_top_axis ^= noAxis $ remAxes as lay
          remAxes (B:as) lay = layout1_bottom_axis ^= noAxis $ remAxes as lay
          plotToLayout (Plot pts pstys) = if Lines `elem` pstys
                                             then (leg, Left (C.toPlot lnPlot))
                                             else (leg, Left (C.toPlot ptPlot))
              where leg = [ l | Legend l <- pstys] `headOr` "a graph"
                    col = [ l | Col l <- pstys] `headOr` black
                    lnPlot = plot_lines_values ^= [pts] $ 
                         plot_lines_style  .> line_color ^= col
                         $ defaultPlotLines
                    ptPlot = plot_points_style ^= filledCircles 2 col
                             $ plot_points_values ^= pts
                             $ defaultPlotPoints


plotGraph :: ToGraph g => g -> IO ()
-- plotGraph' (GBesides ps) = renderableToWindow (toRenderable $ layout) width height
plotGraph togr = renderableToWindow (r) width height
    where r = graphRenderable g
          height = [ h | Height h <- styles] `headOr` 480
          width = [ w | Width w <- styles] `headOr` 640
          g@(Plots plots styles) = toGraph togr
                
plotGraphPng :: (ToGraph a) => FilePath -> a -> IO ()
plotGraphPng fnm togr = renderableToPNGFile (r) width height fnm
    where r = graphRenderable g
          height = [ h | Height h <- styles] `headOr` 480
          width = [ w | Width w <- styles] `headOr` 640
          g@(Plots plots styles) = toGraph togr
                         

headOr [] def = def
headOr (x:_) _ = x

Plot x ys +^^ y = Plot x (y:ys)

class ToPlotStyle a where 
    toPlotStyle :: a -> PlotStyle

class ToGraphStyle a where 
    toGraphStyle :: a -> GraphStyle

instance ToGraphStyle GraphStyle where
    toGraphStyle = id

instance ToGraphStyle [AxisNames] where
    toGraphStyle = Axes

instance ToGraphStyle [Char] where
    toGraphStyle = Title

instance ToPlotStyle Color where
    toPlotStyle = Col

instance ToPlotStyle [Char] where
    toPlotStyle = Legend

--instance ToPlotStyle [AxisNames] where
--    toPlotStyle = Axes

instance ToPlotStyle PlotStyle where
    toPlotStyle = id

instance ToPlotStyle Double where
    toPlotStyle = LineWidth

class ToPlot a where
    toPlot :: a -> Plot

instance ToPlot Plot where
    toPlot = id

instance ToPlot (UVecWave Double) where
    toPlot w = Plot (zip (tms w) (pts w)) [Lines]

instance ToPlot [(Double,Double)] where
    toPlot p = Plot p [FilledCircles]
instance ToPlot [Double] where
    toPlot p = Plot (zip [1..] p) [FilledCircles]


class ToGraph a where
    toGraph :: a -> Graph

instance ToGraph Graph where
    toGraph = id

instance ToPlot a => ToGraph a where
    toGraph p = Plots [toPlot p] []

instance ToPlot a => ToGraph [a] where
    toGraph ps = Plots (map toPlot ps) []

(<+>) :: (ToGraph a, ToGraph b) => a-> b->Graph 
a <+> b = amal (toGraph a) (toGraph b)
    where amal (Plots pls1 prb1) (Plots pls2 prb2) = Plots (pls1++pls2) (prb1++prb2)

infixl 6 %
infixl 5 %%
infixl 5 <+>
-- infixl 4 <|>
-- infixl 3 <->

(%) :: (ToPlot a, ToPlotStyle b) => a -> b -> Plot
p % s = addst (toPlot p) (toPlotStyle s)
        where addst (Plot pts s) s' = Plot pts (s':s)

(%%) :: (ToGraph a, ToGraphStyle b) => a -> b -> Graph
p %% s = addst (toGraph p) (toGraphStyle s)
        where addst (Plots plts stys) s' = (Plots plts (s':stys))

{-
(<|>) :: (ToGraph a, ToGraph b) => a-> b->Graph 
a <|> b = amal (toGraph a) (toGraph b)
    where amal p1@(Plots pls1 prb1) p2@(Plots pls2 prb2) = GBesides [p1,p2]
          amal p1@(Plots pls1 prb1) (GBesides p2) = GBesides (p1:p2)
          amal (GBesides p2) p1@(Plots pls2 prb2) = GBesides (p2++[p1])
-}