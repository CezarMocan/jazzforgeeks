Final Project for CPSC 431 Spring 2013, Yale University 
Kenta Koga, Cezar Mocan

After loading final.lhs into ghci, run finalProject. 

Important comments: 
Controls (not case sensitive):
Same note as before:      G
Next note in scale:       H
Second next in scale:     J
Third next in scale:      K
Fourth next in scale:     L
Previous note in scale:   F
Second previous in scale: D
Third previous in scale:  S
Fourth previous in scale: A
Volume up:                . or >
Volume down:              , or <
Reset pitch value to key value: R

Always before using the keyboard controls select the text box (aka write in the textbox), that is where the program takes its input from. 
We made the textbox always store the last typed character and delete it exactly after storing, such that its buffer won't overflow. (rec str <- textbox " " -< " ").
According to the character found in str, we create events. (one event for one character - these are the pressEvPlusXB / pressEvMinusXB / pressEvResetB).
In the same way we create events for the buttons corresponding to the keyboard actions (these are the ones without a B in the end, pressEvPlusX / pressEvMinusX ...)
We have the currPitch variable, managed with the "accum" mediator and the functions newPitchBPlus and newPitchBMinus (for a given scale, key, current pitch 
and distance to the new pitch (between -4 and and +4) they return the new pitch that respects the scale and the key)



> {-# LANGUAGE Arrows #-}
> {-# LANGUAGE ForeignFunctionInterface #-}

> module Final where
> import Euterpea
> import Control.Arrow
> import Codec.Midi
> import Data.Maybe (mapMaybe)
> import Control.SF.AuxFunctions ((=>>), (->>), (.|.), snapshot, snapshot_)
> import Data.Char
> import Foreign.C.Types
> import Euterpea.IO.MUI
> import Euterpea.IO.MUI.UISF
> import qualified Codec.Midi as Midi

> stringToPitch :: String -> AbsPitch
> stringToPitch [] = -1
> stringToPitch s = (if (null (reads s :: [(Pitch, String)])) then -1 else absPitch(read s::Pitch))


For default scales (modes) that users can choose to play from. Depending on the scale/mode of the song to improvise, user should set the right key with right scale before improvising. 

> normalScale = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12]
> bluesScale = [3, 5, 6, 7, 10, 12]
> dorianScale = [2, 3, 5, 7, 9, 10, 12]
> phrygianScale = [1, 3, 5, 7, 8, 10, 12]


> findElementPos t list (-1) = -1
> findElementPos t list index = (if ((list !! index) == t) then index else findElementPos t list (index - 1))

> getElement key pitch = let ret = (pitch + 144 - key) `mod` 12 
>                           in if (ret == 0) then 12 else ret


> newPitch 48 n = 48
> newPitch x n = if (n + x >= 0) then 
>                   if (n + x < 128) then n + x 
>                   else 127
>               else 1

> newPitchBMinus list key x n = let position = findElementPos (getElement key n) list ((length list) - 1)
>                                   value = (((list !! position) - (list !! ((position + x + (length list)) `mod` (length list))) + 12) `mod` 12) 
>                               in if (n - value > 0 && n - value < 128) then n - value else n

> newPitchBPlus list key 48 n = key
> newPitchBPlus list key x n = let position = findElementPos (getElement key n) list ((length list) - 1)
>                                  value = ((((list !! ((position + x + (length list)) `mod` (length list))) + 12 - (list !! position))) `mod` 12)
>                              in if (n + value > 0 && n + value < 128) then n + value else n


> newVolume x n = if (n + x >= 0) then 
>                   if (n + x <= 100) then n + x 
>                   else 100
>               else 0


> ui0_8 :: UISF () ()
> ui0_8 = proc _ -> do
>   devidOut <- selectOutput -< ()
>   (str) <- (|(setSize(200, 80) . title "Type in here") (do
>           rec str <- textbox " " -< " "
>           returnA -< (str))|)

>   (pressEvMinus1, pressEvMinus2, pressEvMinus3, pressEvMinus4, pressEvPlay, pressEvPlus1, pressEvPlus2, pressEvPlus3, pressEvPlus4, pressEvReset) <- (|(setSize(800, 60) . title "Control" . leftRight) (do
>               pressEvMinus4 <- edge <<< button "-4 (A)" -< ()
>               pressEvMinus3 <- edge <<< button "-3 (S)" -< ()
>               pressEvMinus2 <- edge <<< button "-2 (D)" -< ()
>               pressEvMinus1 <- edge <<< button "-1 (F)" -< ()
>               pressEvPlay <- edge <<< button "0 (G)"   -< ()
>               pressEvPlus1 <- edge <<< button "+1 (H)" -< ()
>               pressEvPlus2 <- edge <<< button "+2 (J)" -< ()
>               pressEvPlus3 <- edge <<< button "+3 (K)" -< ()
>               pressEvPlus4 <- edge <<< button "+4 (L)" -< ()
>               pressEvReset <- edge <<< button "Reset (R)" -< ()
>               returnA -< (pressEvMinus1, pressEvMinus2, pressEvMinus3, pressEvMinus4, pressEvPlay, pressEvPlus1, pressEvPlus2, pressEvPlus3, pressEvPlus4, pressEvReset))|)

>   pressEvPlayB <- edge -< (if ((last str) == 'g' || (last str) == 'G') then True else False)
>   pressEvPlus1B <- edge -< (if ((last str) == 'h' || (last str) == 'H') then True else False)
>   pressEvPlus2B <- edge -< (if ((last str) == 'j' || (last str) == 'J') then True else False)
>   pressEvPlus3B <- edge -< (if ((last str) == 'k' || (last str) == 'K') then True else False)
>   pressEvPlus4B <- edge -< (if ((last str) == 'l' || (last str) == 'L') then True else False)
>   pressEvMinus1B <- edge -< (if ((last str) == 'f' || (last str) == 'F') then True else False)
>   pressEvMinus2B <- edge -< (if ((last str) == 'd' || (last str) == 'D') then True else False)
>   pressEvMinus3B <- edge -< (if ((last str) == 's' || (last str) == 'S') then True else False)
>   pressEvMinus4B <- edge -< (if ((last str) == 'a' || (last str) == 'A') then True else False)
>   pressEvResetB <- edge -< (if ((last str) == 'r' || (last str) == 'R') then True else False)
>   pressEvVolumeUp <- edge -< (if ((last str) == '.' || (last str) == '>') then True else False)
>   pressEvVolumeDown <- edge -< (if ((last str) == ',' || (last str) == '<') then True else False)

Create Radio buttons for setting the scale 

>   scale <- setSize (600, 60) . title "Scale select" . leftRight $ radio ["Normal", "Blues", "Dorian", "Phrygian"] 0 -< ()
>   changedScale <- unique -< scale
>   let currentScale = if (scale == 0) then normalScale 
>                           else if (scale == 1) then bluesScale
>                           else if (scale == 2) then dorianScale
>							else if (scale == 3) then phrysianScale


Create Radio buttons for setting the key  

>   key <- setSize (600, 60) . title "Key select" . leftRight $ radio ["C", "D", "E", "F", "G", "A", "B"] 0 -< ()
>   changedKey <- unique -< key
>   let currentKey = if (key == 0) then 48
>                       else if (key == 1) then 50
>                       else if (key == 2) then 52
>                       else if (key == 3) then 53
>                       else if (key == 4) then 55
>                       else if (key == 5) then 57
>                       else if (key == 6) then 59
>                       else 48

>   currPitch <- accum 48 -< (((pressEvPlus1 .|. pressEvPlus1B) ->> (newPitchBPlus currentScale currentKey 1))        .|. 
>                             ((pressEvPlus2 .|. pressEvPlus2B) ->> (newPitchBPlus currentScale currentKey 2))        .|.
>                             ((pressEvPlus3 .|. pressEvPlus3B) ->> (newPitchBPlus currentScale currentKey 3))      .|.
>                             ((pressEvPlus4 .|. pressEvPlus4B) ->> (newPitchBPlus currentScale currentKey 4))       .|. 
>                             ((pressEvMinus1 .|. pressEvMinus1B) ->> (newPitchBMinus currentScale currentKey (-1)))    .|.
>                             ((pressEvMinus2 .|. pressEvMinus2B) ->> (newPitchBMinus currentScale currentKey (-2)))    .|.
>                             ((pressEvMinus3 .|. pressEvMinus3B) ->> (newPitchBMinus currentScale currentKey (-3)))  .|.
>                             ((pressEvMinus4 .|. pressEvMinus4B) ->> (newPitchBMinus currentScale currentKey (-4))) .|.
>                             ((pressEvReset .|. pressEvResetB .|. (changedScale ->> ()) .|. (changedKey ->> ()) ) ->> (newPitchBPlus currentScale currentKey 48)))   

>   volume <- accum 50 -< ((pressEvVolumeUp ->> newVolume 5) .|. (pressEvVolumeDown ->> newVolume (-5)))

>   title "Current Pitch" display -< pitch currPitch
>   dur <- setSize (600,60) . title "Note length" . leftRight $ 
>           radio ["Whole","Half","Quarter","Eighth","Sixteenth"] 2 -< ()
>   instr <- setSize (800,60) . title "Instrument" . leftRight $ 
>               radio ["Acous Piano","Elec Piano","Violin","Saxophone","Flute"] 0 -< ()

>   let note = (pressEvPlay .|. pressEvPlus1 .|.        
>               pressEvMinus1 .|. pressEvMinus2 .|.
>               pressEvMinus3 .|. pressEvMinus4 .|.
>               pressEvPlus1 .|. pressEvPlus2 .|.
>               pressEvPlus3 .|. pressEvPlus4 .|.
>               pressEvPlayB .|. pressEvPlus1B .|.
>               pressEvMinus1B .|. pressEvMinus2B .|.
>               pressEvMinus3B .|. pressEvMinus4B .|.
>               pressEvPlus1B .|. pressEvPlus2B .|.
>               pressEvPlus3B .|. pressEvPlus4B) ->> [ANote instr currPitch volume (1 / fromIntegral(2^dur))]

>   nowE <- now -< ()
>   let progChan = nowE ->> (map Std $
>                   zipWith Midi.ProgramChange [0,1,2,3,4] [0,4,40,66,73])
>       midiMsgs = progChan .|. note
>   midiOut -< (devidOut, midiMsgs)                    
   
> finalProject = runUIEx (800, 500) "Simple Pitch" ui0_8
