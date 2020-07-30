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
import qualified Data.Text as T

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
-- to the feature while Status refers to the differential group. Choose whether
-- to normalize by the maximum value for a name. Returns a list where the first
-- entry is the plot and the second entry is a data frame of the values.
plotSingleDiff :: Bool -> Bool -> Bool -> [Entity] -> R.R s (R.SomeSEXP s)
plotSingleDiff normalizeBool violin noOutlier vals = do
    let normalize = if normalizeBool then 1 else 0 :: Double
        violinFlag = if violin then 1 else 0 :: Double
        noOutlierFlag = if noOutlier then 1 else 0 :: Double
        names = fmap (T.unpack . unName . _name) vals
        statuses = fmap (T.unpack . unStatus . _status) vals
        values = fmap _value vals

    [r| suppressMessages(library(ggplot2))
        suppressMessages(library(plyr))
        suppressMessages(library(cowplot))
        suppressMessages(library(jsonlite))
        suppressMessages(library(RColorBrewer))

        df = data.frame(name = names_hs, value = values_hs, status = statuses_hs)

        if(normalize_hs) {
          df = ddply(df, "name", transform, value = value / max(value))
        }

        colors = brewer.pal(9, "Set1")
        pal = colorRampPalette(colors)
        statuses = sort(unique(df$status))
        myColors = pal(length(statuses))
        names(myColors) = statuses

        p = ggplot(df, aes(x = name, y = value, fill = status)) +
                scale_fill_manual(values=myColors) +
                xlab("Feature") +
                theme_classic() +
                theme( axis.text = element_text(color = "black")
                    , panel.grid.major.y = element_blank()
                    , axis.ticks.x = element_blank()
                    , axis.ticks.y = element_line(color = "black")
                    , axis.text.x = element_text(angle=315, hjust=0)
                    )

        if(violinFlag_hs) {
          p = p + geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "width")
        } else {
          if(noOutlierFlag_hs) {
            p = p + geom_boxplot(outlier.shape = NA)
          } else {
            p = p + geom_boxplot()
          }
        }

        if(normalize_hs) {
          p = p + ylab("Normalized abundance")
        } else {
          p = p + ylab("Abundance")
        }

        return(list(p, df))
    |]
