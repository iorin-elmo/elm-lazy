module Lazy exposing
  ( Lazy(..)
  , lazy
  , force
  , evaluate
  , map
  , map2
  , map3
  , map4
  , map5
  , apply
  , andThen
  )

type Lazy a
  = Lazy (() -> a)
  | Evaluated a

lazy : (() -> a) -> Lazy a
lazy thunk =
  Lazy thunk

force : Lazy a -> a
force piece =
  case piece of
    Evaluated a ->
      a

    Lazy thunk ->
      thunk ()

evaluate : Lazy a -> Lazy a
evaluate piece =
  case piece of
    Evaluated a ->
      Evaluated a

    Lazy thunk ->
      thunk ()
        |> Evaluated

map : (a -> b) -> Lazy a -> Lazy b
map f a =
  lazy (\() -> f (force a))

map2 : (a -> b -> result) -> Lazy a -> Lazy b -> Lazy result
map2 f a b =
  lazy (\() -> f (force a) (force b))

map3 : (a -> b -> c -> result) -> Lazy a -> Lazy b -> Lazy c -> Lazy result
map3 f a b c =
  lazy (\() -> f (force a) (force b) (force c))

map4 f a b c d =
  lazy (\() -> f (force a) (force b) (force c) (force d))

map5 f a b c d e =
  lazy (\() -> f (force a) (force b) (force c) (force d) (force e))

apply : Lazy (a -> b) -> Lazy a -> Lazy b
apply f x =
  lazy (\() -> (force f) (force x) )

andThen : (a -> Lazy b) -> Lazy a -> Lazy b
andThen callback a =
  lazy (\() -> force (callback (force a) ) )
