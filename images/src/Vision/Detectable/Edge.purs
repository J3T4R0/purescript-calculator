{-# LANGUAGE BangPatterns
           , FlexibleContexts
           , MultiWayIf #-}

module Vision.Detector.Edge (canny) where

import Control.Monad (when)
import Control.Monad.ST (ST)
import Data.Int
import Data.Vector.Storable (enumFromN, forM_)
import Foreign.Storable (Storable)

import Vision.Image (
      Image, ImagePixel, Manifest, MutableManifest, Grey, DerivativeType (..)
    , (!), shape, linearIndex, fromFunction
    , create, new', linearRead, linearWrite
    , sobel
    )
import Vision.Primitive (Z (..), (:.) (..), inShape, ix2)

data EdgeDirection = NorthSouth         -- ^ |
                   | WestEast           -- ^ ―
                   | NorthEastSouthWest -- ^ /
                   | NorthWestSouthEast -- ^ \

-- | Detects edges using the Canny's algorithm. Edges are given the value
-- 'maxBound' while non-edges are given the value 'minBound'.
--
-- This implementation doesn't perform any noise erasing (as blurring) before
-- edge detection. Noisy images might need to be pre-processed using a Gaussian
-- blur.
--
-- The bidirectional derivative (gradient magnitude) is computed from @x@ and
-- @y@ derivatives using @sqrt(dx² + dy²)@.
--
-- See <http://en.wikipedia.org/wiki/Canny_edge_detector> for details.
--
-- This function is specialized for 'Grey' images but is declared @INLINABLE@
-- to be further specialized for new image types.
canny :: ( Image src, Integral (ImagePixel src), Bounded res, Eq res
         , Storable res)
      => Int
      -- ^ Radius of the Sobel's filter.
      -> Int32
      -- ^ Low threshold. Pixels for which the bidirectional derivative is
      -- greater than this value and which are connected to another pixel which
      -- is part of an edge will be part of this edge.
      -> Int32
      -- ^ High threshold. Pixels for which the bidirectional derivative is
      -- greater than this value will be part of an edge.
      -> src
      -> Manifest res
canny !derivSize !lowThres !highThres !img =
    create $ do
        edges <- newManifest
        forM_ (enumFromN 0 h) $ \y -> do
            let !lineOffset = y * w
            forM_ (enumFromN 0 w) $ \x -> do
                visitPoint edges x y (lineOffset + x) highThres'
        return edges
  where
    !size@(Z :. h :. w) = shape img

    -- Squares both thresholds as they will be compared to 'dxy' which contains
    -- squared gradient magnitudes.
    (!lowThres', !highThres') = (square lowThres, square highThres)

    dx, dy :: Manifest Int16
    !dx = sobel derivSize DerivativeX img
    !dy = sobel derivSize DerivativeY img

    -- Gradient magnitude, squared.
    dxy :: Manifest Int32
    !dxy = fromFunction size $ \pt ->
                  square (fromIntegral $ dx ! pt)
                + square (fromIntegral $ dy ! pt)

    newManifest :: (Storable p, Bounded p) => ST s (MutableManifest p s)
    newManifest = new' size minBound

    -- Visits a point and compares its gradient magnitude to the given
    -- threshold, visits neighbor if the point is perceived an an edge.
    visitPoint !edges !x !y !linearIX !thres = do
        val <- linearRead edges linearIX

        when (val == minBound) $ do
            let !ptDxy    = dxy `linearIndex` linearIX
                ptDx      = dx  `linearIndex` linearIX
                ptDy      = dy  `linearIndex` linearIX
                direction = edgeDirection ptDx ptDy

            -- When the current pixel has a greater magnitude than the threshold
            -- and is a local maximum, considers it as a new starting point of
            -- an edge. Tries to draw the remaining of the edge using the low
            -- threshold and by following the edge direction.

            when (ptDxy > thres && isMaximum x y ptDxy direction) $ do
                linearWrite edges linearIX maxBound
                visitNeighbour edges x y direction

    visitNeighbour !edges !x !y !direction = do
        let (!x1, !y1, !x2, !y2) =
                case direction of
                    NorthSouth         -> (x,     y - 1, x,     y + 1)
                    WestEast           -> (x - 1, y,     x + 1, y    )
                    NorthEastSouthWest -> (x - 1, y - 1, x + 1, y + 1)
                    NorthWestSouthEast -> (x + 1, y - 1, x - 1, y + 1)

        when (inShape size (ix2 y1 x1)) $
            visitPoint edges x1 y1 (y1 * w + x1) lowThres'

        when (inShape size (ix2 y2 x2)) $
            visitPoint edges x2 y2 (y2 * w + x2) lowThres'

    isMaximum !x !y !ptDxy !direction =
        let (!x1, !y1, !x2, !y2) =
                case direction of
                    NorthSouth         -> (x - 1, y,     x + 1, y    )
                    WestEast           -> (x,     y - 1, x,     y + 1)
                    NorthEastSouthWest -> (x + 1, y - 1, x - 1, y + 1)
                    NorthWestSouthEast -> (x - 1, y - 1, x + 1, y + 1)
        in tryCompare ptDxy (>) (x1, y1) && tryCompare ptDxy (>=) (x2, y2)

    tryCompare !ptDxy op !(x, y)
        | inShape size (ix2 y x) = ptDxy `op` fromIntegral (dxy ! ix2 y x)
        | otherwise              = True

    -- Returns the direction of the edge, not to be confused with the direction
    -- of the gradient which is the perpendicular of this value.
    edgeDirection ptDx ptDy =
        let !angle = atan2 (double ptDy) (double ptDx)
        in if angle >= 0 then if | angle >  pi8x7 -> NorthSouth
                                 | angle >  pi8x5 -> NorthEastSouthWest
                                 | angle >  pi8x3 -> WestEast
                                 | angle >    pi8 -> NorthWestSouthEast
                                 | otherwise      -> NorthSouth
                         else if | angle < -pi8x7 -> NorthSouth
                                 | angle < -pi8x5 -> NorthWestSouthEast
                                 | angle < -pi8x3 -> WestEast
                                 | angle <   -pi8 -> NorthEastSouthWest
                                 | otherwise      -> NorthSouth

    !pi8   = pi / 8
    !pi8x3 = pi8 * 3
    !pi8x5 = pi8 * 5
    !pi8x7 = pi8 * 7
{-# INLINABLE  canny #-}
{-# SPECIALIZE canny :: Int -> Int32 -> Int32 -> Grey -> Grey #-}

square :: Num a => a -> a
square a = a * a

double :: Integral a => a -> Double
double = fromIntegral