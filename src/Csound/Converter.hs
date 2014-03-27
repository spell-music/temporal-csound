-- | Converters for the instruments.
module Csound.Converter(
    -- * Pitched instruments
    fromF, fromFs, fromAF, fromAsFs, fromAFs, fromAsF,

    -- * Drums
    fromDrum
) where

import Csound

fromF :: SigSpace a => (D -> a) -> CsdNote Unit -> a
fromF f (amp, cps, _) = mul (sig amp) $ f cps

fromFs :: SigSpace a => (Sig -> a) -> CsdNote Unit -> a
fromFs f (amp, cps, _) = mul (sig amp) $ f (sig cps)

fromAF :: (D -> D -> a) -> CsdNote Unit -> a
fromAF f (amp, cps, _) =  f amp cps

fromAsFs :: (Sig -> Sig -> a) -> CsdNote Unit -> a
fromAsFs f (amp, cps, _) = f (sig amp) (sig cps)

fromAFs :: (D -> Sig -> a) -> CsdNote Unit -> a
fromAFs f (amp, cps, _) = f amp (sig cps)

fromAsF :: (Sig -> D -> a) -> CsdNote Unit -> a
fromAsF f (amp, cps, _) = f (sig amp) cps

-- Drums

fromDrum :: SigSpace a => a -> Dr -> a
fromDrum a (amp, _) = mul (sig amp) a
