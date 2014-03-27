-- | Ready to use instruments.
--
-- An instrument takes in a note (@N@ which is shortcut for @CsdNote Unit@ 
-- or @Dr@ which is shortcut for @CsdDrum Unit@) and procuces a signal.
--  We can use instruments with functions 'Csound.notes' and 'Csound.drums'.
--
-- > import Csound 
-- > import Csound.Patch(stringPad)
-- >
-- > -- | Plays C-major chord.
-- > main = dac $ mul 0.2 $ smallHall $ mix $ notes stringPad $	
-- > 	 str 0.5 $ mel [c, e, g, str 4 $ har [c, e, g, high c], rest 4]
--
-- Let's explain the functions:
-- 
-- > -- dac -- sends signal to speakers
-- > -- 
-- > -- mul -- scales the signal
-- > -- 
-- > -- smallHall -- adds a reverb
-- > --
-- > -- mix -- mixes several tracks to a single signal
-- > --
-- > -- notes -- applies an instrument to the notes
-- > --
-- > -- stringPad -- predefined instrument
-- > -- 
-- > -- str -- stretch the notes in time domain
-- > --
-- > -- mel, har -- sequential and parallel composition of the notes
-- > --
-- > -- c, e, g -- notes in western notation (a, b, c, d, ...), a is 440 Hz 
-- > --
-- > -- high -- an octave higher
-- > --
-- > -- rest -- pause for a given amount of time
module Csound.Patch(
    -- * Pads
    stringPad, phasingSynth, pulseWidthPad,
    melodica, tibetan, 

    -- * Mystic
    sparkles, xanaduHarp,

    -- * Lead
    delaySaw, pulseWidth, toneWheel,

    -- * Tech
    okComp, OkCompParam(..), fmMod,

    -- * Plucked
    delayedStringLong, delayedStringShort, 
    plucked, xanadu1, xanadu2,
    guitar, harpsichord, harpsichordHarp, plainString, plainStringHarp,

    -- * Strike
    noisyMarimba, dahina, banyan, xylophone,
    spinelSphere, aluminumBar, 
    vibraphone1, vibraphone2, wineGlass, xing,

    -- ** Bells 
    amBell, fmTubularBell, tubularBell, albertClockBellBelfast,

    -- * Drums
    dumb, dumbBass, pluckSnare, knockSweep, metalBoink,
    snare, openHihat, closedHihat, bassDrum, crash, handClap,
    bambooDr, guiroDr, tambourineDr, cabasaDr, crunchDr,
    sleighbellsDr, sekereDr, sandpaperDr    

) where

import Csound(N, Dr, CsdNote, CsdDrum)
import Csound.Base

import qualified Csound.Catalog.Wave as C
import qualified Csound.Catalog.Effect as C
import qualified Csound.Catalog.Reson as C
import qualified Csound.Catalog.Drum as C

import Csound.Converter

fade :: SigSpace a => D -> D -> (b -> a) -> (b -> a)
fade ris dec f = mul (fades ris dec) . f

--------------------------------------------------------------------
-- Padds

stringPad :: N -> Sig
stringPad = fade 0.5 1.5 $ fromAsFs C.stringPad

phasingSynth :: N -> Sig
phasingSynth = fade 0.3 1.5 $ fromAsFs C.phasingSynth

melodica :: N -> SE Sig
melodica = fade 0.5 2.5 $ fromFs $ C.melody 5

-- | Parameter is time of fade out in seconds.
tibetan :: D -> N -> Sig
tibetan dec = fade 1 dec $ fromF $ C.tibetan 9 0.02

pulseWidthPad :: N -> Sig
pulseWidthPad = fade 1 1.5 $ fromAsFs C.pulseWidth

--------------------------------------------------------------------
-- Mystic

sparkles :: N -> SE Sig
sparkles = fade 0.01 2.5 $ fromFs $ C.blue 3 8 0.5 15

xanaduHarp :: N -> SE Sig
xanaduHarp = fade 0.01 5 $ fromF C.xanadu1

--------------------------------------------------------------------
-- Lead

pulseWidth :: N -> Sig
pulseWidth = fade 0.1 0.1 $ fromAsFs C.pulseWidth

delaySaw :: N -> Sig
delaySaw = fade 0.1 0.2 $ fromFs $ C.delaySaw

toneWheel :: N -> Sig
toneWheel = fade 0.01 0.1 $ fromF C.toneWheel

--------------------------------------------------------------------
-- Tech

okComp :: CsdDrum OkCompParam -> SE Sig
okComp = fade 0.01 0.1 $ \(amp, OkCompParam rate) -> mul (sig amp) $ C.okComputer (sig rate)

newtype OkCompParam = OkCompParam { unOkCompParam :: D }

instance Tuple OkCompParam where
	tupleMethods = makeTupleMethods OkCompParam unOkCompParam

instance Arg OkCompParam

instance Default OkCompParam where
	def = OkCompParam 10

fmMod :: N -> Sig
fmMod = fade 0.01 0.1 $ fromFs $ C.fmMod 5

--------------------------------------------------------------------
-- Plucked

pick :: SigSpace b => D -> (a -> b) -> (a -> b)
pick dec = fade 0.01 dec

delayedStringLong :: N -> Sig
delayedStringLong = pick 2.5 $ fromF C.delayedString

delayedStringShort :: N -> Sig
delayedStringShort = pick 0.1 $ fromF C.delayedString

plucked :: N -> Sig
plucked = pick 0.3 $ fromFs C.rhodes

xanadu1 :: N -> SE Sig
xanadu1 = pick 1.5 $ fromF C.xanadu1

xanadu2 :: N -> SE Sig
xanadu2 = pick 0.8 $ fromF C.xanadu2

guitar :: N -> Sig
guitar = fade 0.05 2 $ fromF C.guitar

harpsichord :: N -> Sig
harpsichord = pick 0.2 $ fromF C.harpsichord

harpsichordHarp :: N -> Sig
harpsichordHarp = pick 5 $ fromF C.harpsichord

plainString :: N -> Sig
plainString = pick 0.5 $ fromF C.plainString

plainStringHarp :: N -> Sig
plainStringHarp = pick 5 $ fromF C.plainString

--------------------------------------------------------------------
-- Striked

strk :: SigSpace b => (a -> b) -> (a -> b)
strk = pick 2.5

noisyMarimba :: N -> SE Sig
noisyMarimba = strk $ fromFs C.blackMarimba

dahina :: N -> Sig
dahina = strk $ fromFs C.dahina

banyan :: N -> Sig
banyan = strk $ fromFs C.banyan

xylophone :: N -> Sig
xylophone = strk $ fromFs C.xylophone

spinelSphere :: N -> Sig
spinelSphere = strk $ fromFs C.spinelSphere

aluminumBar :: N -> Sig
aluminumBar = strk $ fromFs C.uniformAluminumBar

vibraphone1 :: N -> Sig
vibraphone1 = strk $ fromFs C.vibraphone1

vibraphone2 :: N -> Sig
vibraphone2 = strk $ fromFs C.vibraphone2

wineGlass :: N -> Sig
wineGlass = strk $ fromFs C.wineGlass

xing :: N -> Sig 
xing = strk $ fromFs $ C.xing 3

-- Bells

bl :: SigSpace b => (a -> b) -> (a -> b)
bl = pick 4

amBell :: N -> Sig
amBell = bl $ fromAFs $ C.amBell

fmTubularBell :: N -> Sig
fmTubularBell = bl $ fromFs $ C.fmTubularBell

tubularBell :: N -> Sig
tubularBell = bl $ fromFs C.tubularBell

albertClockBellBelfast :: N -> Sig
albertClockBellBelfast = bl $ fromFs C.albertClockBellBelfast

--------------------------------------------------------------------
-- drums

dumb :: Dr -> SE Sig
dumb = fromDrum C.dumb

dumbBass :: Dr -> SE Sig
dumbBass = fromDrum C.dumbBass

pluckSnare :: Dr -> Sig
pluckSnare = fromDrum C.pluckSnare

knockSweep :: Dr -> SE Sig
knockSweep = fromDrum C.sortaKnockSweep

metalBoink :: Dr -> Sig
metalBoink = fromDrum C.metalBoink

snare :: Dr -> SE Sig
snare = fromDrum C.snare

openHihat :: Dr -> SE Sig
openHihat = fromDrum C.openHihat

closedHihat :: Dr -> SE Sig
closedHihat = fromDrum C.closedHihat

bassDrum :: D -> Dr -> Sig
bassDrum cps = fromDrum $ C.bassDrum cps

-- | Recommended values for frequency parameter @cpspch(13.03) - cpspch(13.10)@ 
crash :: D -> Dr -> SE Sig
crash cps = fromDrum $ C.crash cps

handClap :: D -> Dr -> SE Sig
handClap cps = fromDrum $ C.handClap cps

-- models

fromModel :: (Sig -> D -> Sig) -> Dr -> Sig
fromModel f (amp, _) = f (sig amp) 0.01

fromModelD :: (D -> D -> Sig) -> Dr -> Sig
fromModelD f (amp, _) = f amp 0.01

bambooDr :: Dr -> Sig
bambooDr = fromModel bamboo 

guiroDr :: Dr -> Sig
guiroDr = fromModel guiro 

tambourineDr :: Dr -> Sig
tambourineDr = fromModel tambourine

cabasaDr :: Dr -> Sig
cabasaDr = fromModelD cabasa

crunchDr :: Dr -> Sig
crunchDr = fromModelD crunch

sleighbellsDr :: Dr -> Sig
sleighbellsDr = fromModel sleighbells

sekereDr :: Dr -> Sig
sekereDr = fromModelD sekere

sandpaperDr :: Dr -> Sig
sandpaperDr = fromModelD sandpaper

