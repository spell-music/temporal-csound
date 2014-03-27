-- | Ready to use instruments.
--
-- > import Csound 
-- > import Csound.Patch(stringPad)
-- >
-- > -- | Adds fade in for 0.1 seconds and fade out for 0.5 seconds:
-- > instr = mul (fades 0.1 0.5) . stringPad
-- >
-- > -- | Plays C-major chord.
-- > main = dac $ mul 0.2 $ smallHall $ mix $ notes instr $	
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
-- > -- instr -- is defined above (stringPad + fades)
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
    stringPad, phasingSynth, delaySaw, 
    melodica, tibetan, toneWheel,

    -- * Mystic
    sparkles,

    -- * Lead
    pulseWidth,

    -- * Tech
    okComp, OkCompParam(..), fmMod,

    -- * Plucked
    delayedString, plucked, xanadu1, xanadu2,
    guitar, harpsichord, plainString,

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

fade :: SigSpace a => D -> D -> (N -> a) -> (N -> a)
fade ris dec f = mul (fades ris dec) . f

--------------------------------------------------------------------
-- Padds

stringPad :: N -> Sig
stringPad = fade 0.5 1.5 $ fromAsFs C.stringPad

phasingSynth :: N -> Sig
phasingSynth = fade 0.3 1.5 $ fromAsFs C.phasingSynth

delaySaw :: N -> Sig
delaySaw = fromFs $ C.delaySaw

melodica :: N -> SE Sig
melodica = fromFs $ C.melody 5

tibetan :: N -> Sig
tibetan = fromF $ C.tibetan 9 0.02

toneWheel :: N -> Sig
toneWheel = fromF C.toneWheel

--------------------------------------------------------------------
-- Mystic

sparkles :: N -> SE Sig
sparkles = fromFs $ C.blue 3 8 0.5 15

--------------------------------------------------------------------
-- Lead

pulseWidth :: N -> Sig
pulseWidth = fromAsFs C.pulseWidth

--------------------------------------------------------------------
-- Tech

okComp :: CsdDrum OkCompParam -> SE Sig
okComp (amp, OkCompParam rate) = mul (sig amp) $ C.okComputer (sig rate)

newtype OkCompParam = OkCompParam { unOkCompParam :: D }

instance Tuple OkCompParam where
	tupleMethods = makeTupleMethods OkCompParam unOkCompParam

instance Arg OkCompParam

instance Default OkCompParam where
	def = OkCompParam 10

fmMod :: N -> Sig
fmMod = fromFs $ C.fmMod 5

--------------------------------------------------------------------
-- Plucked

delayedString :: N -> Sig
delayedString = fromF C.delayedString

plucked :: N -> Sig
plucked = fromFs C.rhodes

xanadu1 :: N -> SE Sig
xanadu1 = fromF C.xanadu1

xanadu2 :: N -> SE Sig
xanadu2 = fromF C.xanadu2

guitar :: N -> Sig
guitar = fromF C.guitar

harpsichord :: N -> Sig
harpsichord = fromF C.harpsichord

plainString :: N -> Sig
plainString = fromF C.plainString

--------------------------------------------------------------------
-- Striked

noisyMarimba :: N -> SE Sig
noisyMarimba = fromFs C.blackMarimba

dahina :: N -> Sig
dahina = fromFs C.dahina

banyan :: N -> Sig
banyan = fromFs C.banyan

xylophone :: N -> Sig
xylophone = fromFs C.xylophone

spinelSphere :: N -> Sig
spinelSphere = fromFs C.spinelSphere

aluminumBar :: N -> Sig
aluminumBar = fromFs C.uniformAluminumBar

vibraphone1 :: N -> Sig
vibraphone1 = fromFs C.vibraphone1

vibraphone2 :: N -> Sig
vibraphone2 = fromFs C.vibraphone2

wineGlass :: N -> Sig
wineGlass = fromFs C.wineGlass

xing :: N -> Sig 
xing = fromFs $ C.xing 3

-- Bells

amBell :: N -> Sig
amBell = fromAFs $ C.amBell

fmTubularBell :: N -> Sig
fmTubularBell = fromFs $ C.fmTubularBell

tubularBell :: N -> Sig
tubularBell = fromFs C.tubularBell

albertClockBellBelfast :: N -> Sig
albertClockBellBelfast = fromFs C.albertClockBellBelfast

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

