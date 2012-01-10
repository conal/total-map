The usual finite map type (`Map` from `Data.Map`) is not an applicative functor, as it doesn't have a pure.
Consequently, it's also not a monad.
On the other hand, we can decompose `Map` into two parts: a *total* map, and `Maybe`, i.e.,

    type Map k v = TMap k (Maybe v)

The type `TMap` of total maps does have `Applicative` and `Monad` instances, and hence this hypothetically rebuilt `Map` would as well.

The idea for `TMap` is introduced in the paper [*Denotational design with type class morphisms*](http://conal.net/papers/type-class-morphisms/).
The meaning `Map k v` is given by its semantic function

    (!) :: Map k v -> (k -> v)

The type class morphism (TCM) principle then exactly dictates the meanings of several class instances for `TMap`, including `Functor`, `Applicative`, `Monad`, and `Monoid`.
For instance, `(!)` must be a monoid (homo)morphism, i.e.,

    (!) mempty == mempty
    (!) (s `mappend` t) == (!) s `mappend` (!) t

The current implementation of `TMap` is via `Data.Map`.
