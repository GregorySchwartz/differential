{- Plot
Gregory W. Schwartz

Collects the functions pertaining to plotting the differential between groups for
each entity.
-}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Plot
    ( plotDiff
    ) where

-- Standard

-- Cabal

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

    [r| library(ggplot2)
        library(cowplot)

        df = data.frame( x = vals_hs
                       , y = psR_hs
                       , significant = as.character(((vals_hs >= valCut_hs) & (psR_hs < pCutR_hs)))
                       )

        p = ggplot(df, aes(x = x, y = y)) +
                geom_point(aes(color = significant)) +
                scale_color_manual(values = c("FALSE" = "gray", "TRUE" = "red"))

        return(p)
    |]
