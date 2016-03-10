module Kleisli where

import Prelude hiding (id, (>>), (*>), Maybe(..), sequence)

class Category cat where
    id   :: cat a a
    (>>) :: cat a b -> cat b c -> cat a c

class Kleisli m where
    idK  :: a ->m a
    (*>) :: (a -> m b) -> (b -> m c) -> (a -> m c)

(+>) :: Kleisli m => (a -> m b) -> (b -> c) -> (a -> m c)
f +> g = f *> (g >> idK)

instance Category (->) where
    id     = \x -> x
    f >> g = \x -> g (f x)

data Maybe a = Nothing | Just a
    deriving (Show, Eq, Ord)

instance Kleisli Maybe where
    idK = Just
    f *> g = \a -> case f a of
                    Nothing -> Nothing
                    Just b  -> g b

instance Kleisli [] where
    idK = \a -> [a]
    f *> g = f >> map g >> concat

data State s a = State (s -> (a, s))

instance Kleisli (State s) where
    idK    = \a -> State (\s -> (a, s))
    f *> g = \a ->
            let
                c = \s ->
                    let
                        State f' = f a
                        (b, s')  = f' s
                        State g' = g b
                    in
                        g' s'
            in
                State c

runState :: State s a -> s -> (a, s)
runState (State f) = f

infixr 0 +$, *$

(*$) :: Kleisli m => (a -> m b) -> m a -> m b
(+$) :: Kleisli m => (a -> b)   -> m a -> m b

f *$ a = (const a *> f) ()
f +$ a = (const a +> f) ()

($$) :: Kleisli m => m (a -> b) -> m a -> m b
mf $$ ma = ( +$ ma) *$ mf

lift1 :: Kleisli m => (a -> b) -> m a -> m b
lift1 = (+$)

lift2 :: Kleisli m => (a -> b -> c) -> m a -> m b -> m c
lift2 f a b = lift1 f a $$ b

lift3 :: Kleisli m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 f a b c = lift2 f a b $$ c

sequence :: Kleisli m => [m a] -> m [a]
sequence = foldr (lift2 (:)) (idK [])

mapK :: Kleisli m => (a -> m b) -> [a] -> m [b]
mapK f = sequence . map f
