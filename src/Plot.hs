{- Plot
Gregory W. Schwartz

Collects the functions pertaining to plotting the differential between groups for
each entity.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Plot
    ( plotDiff
    , plotSingleDiff
    ) where

-- Standard

-- Cabal
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as B

import Language.R.QQ (r)
import qualified Foreign.R.Internal as R
import qualified Language.R.Instance as R
import qualified Language.R.Literal as R

-- Local
import Types

-- | Plot the input values as a scatterplot with designated cutoffs for the fold
-- change (>=) and p-value (<) respectively.
plotDiff :: Double -> PValue -> [Double] -> [PValue] -> R.R s (R.SomeSEXP s)
plotDiff valCut pCut vals ps = do
    let pCutR = unPValue pCut
        psR   = fmap unPValue ps

    [r| suppressMessages(library(ggplot2))
        suppressMessages(library(cowplot))

        df = data.frame( x = vals_hs
                       , y = psR_hs
                       , significant = as.character(((vals_hs >= valCut_hs) & (psR_hs < pCutR_hs)))
                       )

        p = ggplot(df, aes(x = x, y = y)) +
                geom_point(aes(color = significant)) +
                scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red"))

        return(p)
    |]

-- | Plot the difference between groups for all features. Here, Name refers
-- to the feature while Status refers to the differential group.
plotSingleDiff :: [Entity] -> R.R s (R.SomeSEXP s)
plotSingleDiff vals = do
    let jsonR = B.unpack $ A.encode vals

    [r| suppressMessages(library(ggplot2))
        suppressMessages(library(cowplot))
        suppressMessages(library(jsonlite))
        suppressMessages(library(RColorBrewer))

        df = fromJSON(jsonR_hs)

        p = ggplot(df, aes(x = name, y = value, fill = status)) +
                geom_violin(alpha = 0.5, draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
                scale_fill_brewer(palette = "Set1") +
                ylab("Abundance") +
                xlab("Feature") +
                theme_classic() +
                theme( axis.text = element_text(color = "black")
                    , panel.grid.major.y = element_blank()
                    , axis.ticks.x = element_blank()
                    , axis.ticks.y = element_line(color = "black")
                    , axis.text.x = element_text(angle=315, hjust=0)
                    )

        return(p)
    |]
