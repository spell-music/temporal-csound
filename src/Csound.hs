{-# Language FlexibleInstances #-}
-- | Defines instance of 'Csound.Base.CsdSco' for 'Temporal.Music.Score.Score'. 
--
-- We can trigger Csound orchestra with 'Temporal.Music.Score.Score'.
--
-- How to put the values in the container 'Temporal.Music.Score.Score'? There are many functions to construct the 'Temporal.Music.Score.Score'.
--
-- They live in the module "Temporal.Music.Score". If you are not familiar with it, you can start with six basic functions. 
--
-- * 'Temporal.Music.Score.rest' -- makes a pause that lasts for some time (in seconds).
--
-- * 'Temporal.Music.Score.temp' -- makes a score of one note that lasts for one second.    
--
-- * 'Temporal.Music.Score.line' -- plays a list of notes in sequence (one after the other).
-- 
-- * 'Temporal.Music.Score.chord' -- plays a list of notes in parallel (at the same time).
--
-- * 'Temporal.Music.Score.delay' -- delays all notes for some time.
--
-- * 'Temporal.Music.Score.stretch' -- change the tempo for all notes by the given ratio.
--
-- Let's play something:
--
-- > res = stretch 0.5 $ line [ temp a, stretch 2 $ temp b, rest 1, chord [temp a, temp b] ]
--
-- There are two handy infix operators for delay and stretch: @(+|)@ and @(*|)@. So we can write the previous score:
--
-- > res = 0.5 *| line [ temp a, 2 *| temp b, 1 +| chord [temp a, temp b] ]
module Csound (
    {-
    -- * Converters
    CsdNote, csdNote, CsdDrum, csdDrum,       

    -- * Scores
    
    -- | Tools to make compositions out of timbres.
    sco, notes, drums,
    -}
    
    module Temporal.Music,
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
