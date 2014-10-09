
{-# LANGUAGE OverloadedStrings #-}

-- TODO: factoring this interface
module Data.Stdf.WaferMap ( XyBin(..)
                          , stdfToXyBin
                          , xybsToGrid
                          , gridToString 
                          , stdfToWaferMapString 
                          , map2d
                          ) where

import Data.Stdf
import qualified Data.ByteString.Lazy as BL
import Data.Aeson
import Data.Maybe (fromJust)
import Data.List (sortBy, groupBy)
import Data.Function (on)
import Data.Ix (range)
import Data.List.Split (chunksOf)

data XyBin = XyBin { x :: Int
                   , y :: Int
                   , bin :: Int }
           | Missing
    deriving (Show, Eq)

stdfToXyBin :: Stdf -> [XyBin]
stdfToXyBin (prr@(Prr { xCoord = Just xc
                      , yCoord = Just yc
                      , hardBin = hb }):prrs) =
    XyBin { x = fromIntegral xc
          , y = fromIntegral yc
          , bin = fromIntegral hb } : stdfToXyBin prrs
stdfToXyBin (_:prrs) = stdfToXyBin prrs
stdfToXyBin [] = []

-- Take a list of XyBin sorted by x,y and a list of all x,y in the wafer map
-- Make a list of XyBin with Missing inserted where there wasn't an XyBin for
-- a given x,y
addmissing :: [XyBin] -> [(Int, Int)] -> [XyBin]
addmissing [] []      = []
addmissing [] xys     = replicate (length xys) Missing
addmissing (d:ds) (xy:xys)
    | x d == fst xy && y d == snd xy = d : addmissing ds xys
    | otherwise                      = Missing : addmissing (d:ds) xys


binToStr :: Maybe Int -> String
binToStr Nothing = "."
binToStr (Just bn) = show bn

-- sort by y,x
-- groupby y,x
-- take lowest bin # from groups
-- use min max x y to insert Missing dies
xybsToGrid :: [XyBin] -> [[XyBin]]
xybsToGrid xybs = mkgrid cols gridlist
      where ordXy a b = case compare (y a) (y b) of
                            EQ -> compare (x a) (x b)
                            ordy -> ordy
            eqXy a b = x a == x b && y a == y b
            ordBin a b = compare (bin a) (bin b)
            waferxy =  [(atx, aty) | aty <- range (miny, maxy)
                                   , atx <- range (minx, maxx)]
            sortedxy = sortBy ordXy xybs
            groupedxy = groupBy eqXy sortedxy
            sortedbin = map (sortBy ordBin) groupedxy
            dropedextra = map head sortedbin
            xs = map x dropedextra
            ys = map y dropedextra
            minx = minimum xs
            maxx = maximum xs
            miny = minimum ys
            maxy = maximum ys
            gridlist = addmissing dropedextra waferxy
            cols = maxx - minx + 1

mkgrid :: Int -> [a] -> [[a]]
mkgrid = chunksOf

-- take list of rows
-- convert to rows with columns of show bin
-- pad to maximum show bin width
-- print
gridToString :: [[XyBin]] -> String
gridToString xybs = unlines $ map unwords padded
    where
        pad w s = s ++ replicate (w - length s) ' '

        binOrNothing Missing = Nothing
        binOrNothing XyBin { bin = bin } = Just bin

        bins = map2d binOrNothing xybs -- start here
        binstrs :: [[String]]
        binstrs = map2d binToStr bins
        maxwidth :: Int
        maxwidth = maximum $ (concatMap . map) length binstrs
        padded = map2d (pad maxwidth) binstrs
        cols = length $ head xybs
        rows = length xybs

map2d :: (a -> b) -> [[a]] -> [[b]]
map2d = map . map

stdfToWaferMapString :: Stdf -> String
stdfToWaferMapString bodies = let xybs = stdfToXyBin bodies
                                  grid = xybsToGrid xybs
                              in gridToString grid
