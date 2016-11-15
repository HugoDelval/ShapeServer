module Animate where

import System.Posix (usleep)

import Signal
import Shapes
import Render
import Ansi

-- Animate a drawing valued signal.
-- Window represents the view onto the plans
animate :: Window -> Time -> Time -> Signal Drawing -> IO ()
animate win t0 t1 ss = mapM_ display frames
  where
    ts     = samples t0 t1 (round $ (t1 - t0) * 10)  -- we generate 10 fps
    frames = map (sample ss) ts
    display sh = do
      cls
      render win sh
      usleep 70000  -- sleeping removes flickering

