{-# Language FlexibleInstances #-}
module Csound (
    {-
    -- * Converters
    CsdNote, csdNote, CsdDrum, csdDrum, 
    
    -- * Scores
    
    -- | Tools to make compositions out of timbres.
    sco, notes, drums,
    -}
    module Temporal.Music,
    
    -- * Colors
    -- | Tools to construct timbres.
    module Csound.Base
) where

import Temporal.Music 
import Temporal.Media(Track)
import Csound.Base

instance CsdSco (Track Double) where
    toCsdEventList x = CsdEventList (dur x) (fmap toEvt $ render x)
        where toEvt a = (eventStart a, eventDur a, eventContent a)
    singleCsdEvent start dt a = delay start $ stretch dt $ temp a

{-
-- | Plays some notes with Csound instrument. 
sco :: Arg a => (a -> Out) -> Score a -> SigOut
sco instr s = score instr (fmap unpackEvent $ alignByZero $ render s)
    where unpackEvent e = (eventStart e, eventDur e, eventContent e)

-- | Playes notes.
notes :: Arg a => (CsdNote a -> Out) -> Score (Note a) -> SigOut
notes instr as = sco instr (fmap csdNote as)

-- | Plays drum-notes.
drums :: Arg a => (CsdDrum a -> Out) -> Score (Drum a) -> SigOut
drums instr as = sco instr (fmap csdDrum as)
  
-- | Csound note: (amplitude, cyclesPerSecond, otherParams) 
type CsdNote a = (D, D, a)    

csdNote :: Note a -> CsdNote a
csdNote a = (double $ amp $ noteVolume a, double $ hz  $ notePitch a, noteParam a)

-- | Csound drum-note: (amplitude, otherParams)
type CsdDrum a = (D, a)

csdDrum :: Drum a -> CsdDrum a
csdDrum a = (double $ amp $ drumVolume a, drumParam a)
-}
