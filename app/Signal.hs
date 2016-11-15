-- A very simple library for manipulating continuous signals.
--

module Signal (Time, Signal,      
               timeS,     
               mapT,  
               sample )  
       where
import Control.Monad (forM_)

-- Shallow embedding

type Time = Double
newtype Signal a = Signal {at :: Time -> a}

-- Equivalent to the following two definitions
-- newtype Signal a = Signal (Time -> a)
-- at (Signal s) = s

-- Time varying signal
timeS  :: Signal Time
timeS = Signal id

instance Functor Signal where
  fmap f xs = pure f <*> xs

instance Applicative Signal where
  pure x = Signal $ const x
  fs <*> xs = Signal $ \t -> (fs `at` t)  (xs `at` t)

-- Transforming the time.
mapT   :: (Time -> Time)  -> Signal a -> Signal a
mapT f xs = Signal $ \t -> xs `at` (f t)
-- Equivalent to :
--  = Signal (at xs . f)
-- which I'm sure you'll agree is much neater!


-- Sampling a signal at a given time point.
-- This is the /semantic function/ of our library.
sample :: Signal a -> Time -> a  
sample = at

