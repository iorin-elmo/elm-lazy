module LazyList exposing
  ( LazyList
  , empty
  , singleton
  , cons
  , map
  , map2
  , iterate
  , take
  , foldl
  )

import Lazy exposing (Lazy, lazy, force)
import Array exposing (Array)

type LazyListView a
  = Empty
  | Cons a (LazyList a)

type alias LazyList a = Lazy (LazyListView a)


empty : LazyList a
empty = lazy <| \() -> Empty

singleton : a -> LazyList a
singleton a =
  cons a empty

cons : a -> LazyList a -> LazyList a
cons a li = lazy <| \() ->
  Cons a li

map : (a -> b) -> LazyList a -> LazyList b
map f li = lazy <| \() ->
  case li of
    Empty -> Empty
    Cons hd tl ->
      Cons (f hd) (map f tl)

map2 : (a -> b -> c) -> LazyList a -> LazyList b -> LazyList c
map2 f li1 li2 = lazy <| \() ->
  case li1 of
    Empty -> Empty
    Cons hd1 tl1 ->
      case li2 of
        Empty -> Empty
        Cons hd2 tl2 ->
          cons (f hd1 hd2) (map2 f tl1 tl2)

iterate : (a -> a) -> a -> LazyList a
iterate f a = lazy <| \() ->
  Cons a (iterate f (f a))

take : Int -> LazyList a -> LazyList a
take n list = lazy <| \() ->
  if n <= 0
  then Empty
  else
    case force list of
      Empty -> Empty
      Cons hd tl ->
        Cons hd (take (n-1) tl)

foldl : (a -> b -> b) -> b -> LazyList a -> b
foldl f b li =
  case force li of
    Empty -> b
    Cons hd tl ->
      foldl f (f hd b) tl
