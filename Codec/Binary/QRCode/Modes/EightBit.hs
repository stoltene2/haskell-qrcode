{-# OPTIONS_HADDOCK hide #-}
module Codec.Binary.QRCode.Modes.EightBit where

import Codec.Binary.QRCode.Utils
import Codec.Binary.QRCode.Spec

import Data.Char


encode :: Version -> Input -> Maybe BitStream
encode ver input = ((modeIndicatorBits ++ characterCountBits) ++) `fmap` (Just input)
    where
        modeIndicatorBits = "0100"
        characterCountBits = showBinPadded cciLength $ length input
        cciLength = qrLengthOfCCI ver EightBit
