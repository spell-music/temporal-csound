{-# Language FlexibleInstances, FlexibleContexts #-}
-- | Defines instance of 'Csound.Base.CsdSco' for 'Temporal.Music.Score.Score' and
--  reexports all functions from packages csound-expression and temporal-music-notation-western.
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
-- * 'Temporal.Music.Score.mel' -- plays a list of notes in sequence (one after the other, short for @melody@).
-- 
-- * 'Temporal.Music.Score.har' -- plays a list of notes in parallel (at the same time, short for @harmony@).
--
-- * 'Temporal.Music.Score.del' -- delays all notes for some time (short for @delay@).
--
-- * 'Temporal.Music.Score.str' -- change the tempo for all notes by the given ratio (short for @stretch@).
--
-- Let's play something:
--
-- > res = str 0.5 $ mel [ temp a, str 2 $ temp b, rest 1, har [temp a, temp b] ]
--
-- There are two handy infix operators for delay and stretch: @(+|)@ and @(*|)@. So we can write the previous score:
--
-- > res = 0.5 *| mel [ temp a, 2 *| temp b, 1 +| har [temp a, temp b] ]
--
-- There are shortcuts for notes in western notation (a is 440 Hz).
--
-- > a, b, c, d, e, f, g
--
-- Notes reside in the same octave. To get the notes in higher or lower octaves
-- we can apply the functions:
--
-- * 'Temporal.Music.Score.high', 'Temporal.Music.Score.low' -- take note an octaver higher or lower
--
-- * 'Temporal.Music.Score.higher' n, 'Temporal.Music.Score.lower' n -- take note for @n@ octaves higher or lower
--
-- There are shortcuts for stretching the notes and rests:
--
-- > bn, wn, qn, en, sn -- brevis, whole, quarter, eight, sixteenth notes
--
-- and for rests
--
-- > bnr, wnr, qnr, enr, snr
--
-- These functions transform the melodies with given factors.
-- We can construct melodies:
--
-- > melody = mel [qn $ mel [c, e, g], bn $ har [c, e, g, high c], wnr]
--
-- Then we can apply a csound instrument to the melody to get the signal.
--
-- > res = notes someInstr melody
--
-- Now let's mix it to the signal and send the output to speakers:
--
-- > dac $ mix res 
--
-- WARNING: The function 'dac' spawns a csound process in the background which
-- can run forever. If your haskell build tool doesn't kills the child processes with
-- haskell-runing process (As far as I know Sublime Editor doesn't, but vim does) 
-- it's better to run the program from ghci and to stop it press @Ctrl+C@:
--
-- > % ghci MyMusic
-- > MyMusic> main 
-- >
-- >    ... The programm runs ... press Ctrl+C to stop it
-- >
-- 
-- @runhaskell@ doesn't stop the child process. So it's better to use the
-- @dac@ function with terminal.
-- 
-- If signal is to loud or to quiet we can scale it:
--
-- > dac $ mul factor $ mix res 
-- 
-- We can make it brighter with reverb ('Csound.Air.smallRoom', 'Csound.Air.smallHall', 'Csound.Air.largeHall', 'Csound.Air.reverTime')
--
-- > dac $ mul 0.2 $ smallHall $ mix res 

module Csound (
    
    -- * Converters
    CsdNote(..), csdNote, CsdDrum, csdDrum, N, Dr,

    -- * Scores
    --
    -- | Funxtions that apply instruments to scores. 
    --
    -- Notes on signatures:
    --
    -- * The class 'Csound.Base.Outs' includes the tuples of signals 
    --   that have side effects or have no side effects.
    --
    -- * @SigOuts@ -- means an underlying tuple of signals.
    --   For instance, it can be @Sig@  or @SE Sig@, the @SigOuts@
    --   converts it to the @Sig@. The @SigOuts@ removes the 
    --   prefix @SE@ if it is present.
    --
    -- * To get the final signal out of the type @Score (Mix (SigOuts b))@
    --   we should apply the function 'Csound.Base.mix' to it:
    --
    -- > mix :: (CsdSco f, Sigs a) => f (Mix a) -> a
    -- 
    --  Or we can continue to build the track of signals with 
    --  functions loke 'Temporal.Music.Score.mel', 'Temporal.Music.Score.har', 'Temporal.Music.Score.str'.
    notes, drums,    

    -- * Midis   
    --
    -- | Plays instruments with midi devices.
    --
    -- > import Csound 
    -- > import Csound.Patch(vibraphone2)
    -- >
    -- > -- | Plays with virtual midi device (if you have a midi device
    -- > -- you can substitute @vdac@ for @dac@).
    -- > main = vdac $ mul 0.1 $ largeHall $ onMidi vibraphone2
    -- 
    onMidi, onMidin, onPgmidi, 
    onMidiWith, onMidinWith, onPgmidiWith,    
    
    module Temporal.Music.Western.P12,
    module Csound.Base
) where

import Temporal.Media(Track)
import Csound.Base
import Temporal.Music.Western.P12 hiding (delay, line, tone)

instance CsdSco (Track Double) where
    toCsdEventList x = CsdEventList (dur x) (fmap toEvt $ render x)
        where toEvt a = (eventStart a, eventDur a, eventContent a)
    singleCsdEvent (start, dt, a) = del start $ str dt $ temp a

type N  = CsdNote Unit
type Dr = CsdDrum Unit

-- | Contains amplitude, frequency and auxiliary parameters.
--
-- > (amplitude, frequencyInHz, timbralParameters)
type CsdNote a = (D, D, a)

-- | Contains amplitude and auxiliary parameters.
--
-- > (amplitude, timbralParameters)
type CsdDrum a = (D, a)

-- | Converts the @Note@ to low level @CsdNote@.
csdNote :: Default a => Note a -> CsdNote a
csdNote a =  	
	( double $ volumeAsDouble $ noteVolume a
    , double $ absPitch $ notePitch a
    , maybe def id $ noteParam a)

-- | Converts the @Note@ to low level @CsdNote@.
csdDrum :: Default a => Drum a -> CsdDrum a
csdDrum a =  	
	( double $ volumeAsDouble $ drumVolume a
    , maybe def id $ drumParam a)

-- | Plays the notes with csound instrument.
notes :: (Arg a, Default a, Outs b) => (CsdNote a -> b) -> Score (Note a) -> Score (Mix (SigOuts b))
notes g f = sco (toOuts . g) (fmap csdNote f)

-- | Plays the drum notes with csound instrument.
drums :: (Arg a, Default a, Outs b) => (CsdDrum a -> b) -> Score (Drum a) -> Score (Mix (SigOuts b))
drums g f = sco (toOuts . g) (fmap csdDrum f)

toMidiInstr :: (Default a, Outs b) => (CsdNote a -> b) -> (Msg -> SE (SigOuts b))
toMidiInstr f = \msg -> toOuts $ f (ampmidi msg 1, cpsmidi msg, def) 

toMidiInstrWith :: (Outs b) => a -> (CsdNote a -> b) -> (Msg -> SE (SigOuts b))
toMidiInstrWith defVal f = \msg -> toOuts $ f (ampmidi msg 1, cpsmidi msg, defVal) 

-- | Triggers an instrument on all midi-channels.
onMidi :: (Default a, Outs b, Num (SigOuts b)) => (CsdNote a -> b) -> SE (SigOuts b)
onMidi f = midi $ toMidiInstr f

-- | Triggers an instrument on the given midi-channel.
onMidin :: (Default a, Outs b, Num (SigOuts b)) => Channel -> (CsdNote a -> b) -> SE (SigOuts b)
onMidin chn f = midin chn $ toMidiInstr f

-- | Triggers an instrument on channel and programm bank.
onPgmidi :: (Default a, Outs b, Num (SigOuts b)) => Maybe Int -> Channel -> (CsdNote a -> b) -> SE (SigOuts b)
onPgmidi pgm chn f = pgmidi pgm chn $ toMidiInstr f

-- | Just like @onMidi@ but takes a value for default auxiliary parameters.
onMidiWith :: (Outs b, Num (SigOuts b)) => a -> (CsdNote a -> b) -> SE (SigOuts b)
onMidiWith defVal f = midi $ toMidiInstrWith defVal f

-- | Just like @onMidin@ but takes a value for default auxiliary parameters.
onMidinWith :: (Outs b, Num (SigOuts b)) => a -> Channel -> (CsdNote a -> b) -> SE (SigOuts b)
onMidinWith defVal chn f = midin chn $ toMidiInstrWith defVal f

-- | Just like @onPgmidi@ but takes a value for default auxiliary parameters.
onPgmidiWith :: (Outs b, Num (SigOuts b)) => a -> Maybe Int -> Channel -> (CsdNote a -> b) -> SE (SigOuts b)
onPgmidiWith defVal pgm chn f = pgmidi pgm chn $ toMidiInstrWith defVal f

