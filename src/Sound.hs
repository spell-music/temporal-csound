module Csound (
    -- * Converters
    CsdNote, csdNote, CsdDrum, csdDrum, 
    
    -- * Scores
    sco,      
    module Temporal.Music,
    
    -- * Colors
    module Csound.Base
) where

import Temporal.Music
import Csound.Base

-- | Plays some notes with Csound instrument. 
sco :: Arg a => (a -> Out) -> Score a -> SigOut
sco instr s = score instr (fmap unpackEvent $ alignByZero $ render s)
    where unpackEvent e = (eventStart e, eventDur e, eventContent e)
    
-- | Csound note: (amplitude, cyclesPerSecond, otherParams) 
type CsdNote a = (D, D, Maybe a)    
    
csdNote :: Note a -> CsdNote a
csdNote a = (double $ amp $ noteVolume a, double $ hz  $ notePitch a, noteParam a)

-- | Csound drum-note: (amplitude, otherParams)
type CsdDrum a = (D, Maybe a)

csdDrum :: Drum a -> CsdDrum a
csdDrum a = (double $ amp $ drumVolume a, drumParam a)

