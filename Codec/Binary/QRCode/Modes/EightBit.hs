{-# OPTIONS_HADDOCK hide #-}
module Codec.Binary.QRCode.Modes.EightBit where

import Codec.Binary.QRCode.Utils
import Codec.Binary.QRCode.Spec

import Codec.Binary.QRCode.GaloisField (readBin)

import Data.Char
import Numeric


-- Input might need to be in "1010101001" representation, not characters...

encode :: Version -> Input -> Maybe BitStream
encode ver input = ((modeIndicatorBits ++ characterCountBits) ++) `fmap` (Just bitInput)
    where
        modeIndicatorBits = "0100"
        characterCountBits = showBinPadded cciLength $ length input
        cciLength = qrLengthOfCCI ver EightBit
        bitInput = concatMap showBinaryByte input 


-- This needs to be padded to 8-bit! or it won't work
showBinaryByte :: Char -> String
showBinaryByte c = paddedBits ++ bits
               where 
                   bits = showIntAtBase 2 intToDigit (fromEnum c) ""
                   paddedBits = replicate (8 - length bits) '0'


