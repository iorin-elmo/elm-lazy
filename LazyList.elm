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
  case force li of
    Empty -> Empty
    Cons hd tl ->
      Cons (f hd) (map f tl)

map2 : (a -> b -> c) -> LazyList a -> LazyList b -> LazyList c
map2 f li1 li2 = lazy <| \() ->
  case force li1 of
    Empty -> Empty
    Cons hd1 tl1 ->
      case force li2 of
        Empty -> Empty
        Cons hd2 tl2 ->
          Cons (f hd1 hd2) (map2 f tl1 tl2)

andMap : LazyList (a -> b) -> LazyList a -> LazyList b
andMap =
  map2 (<|)

map3 : (a -> b -> c -> d) -> LazyList a -> LazyList b -> LazyList c -> LazyList d
map3 f l1 l2 l3 =
  map f l1
    |> (\l -> andMap l l2)
    |> (\l -> andMap l l3)

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

drop : Int -> LazyList a -> LazyList a
drop n list = lazy <| \() ->
  if n <= 0
  then force list
  else
    case force list of
      Empty -> Empty
      Cons hd tl ->
        force (drop (n-1) tl)

foldl : (a -> b -> b) -> b -> LazyList a -> b
foldl f b li =
  case force li of
    Empty -> b
    Cons hd tl ->
      foldl f (f hd b) tl

toList : LazyList a -> List a
toList list =
  case force list of
    Empty -> []
    Cons hd tl ->
      hd :: toList tl
