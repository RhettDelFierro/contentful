module Global.GlobalHelpers where

import Control.Applicative
import Control.Applicative.Lift
import Data.Either
import Data.Functor.Constant
import Data.Monoid

type FieldName = String

convertMaybeToEither :: Maybe a -> FieldName -> Either [FieldName] FieldName
convertMaybeToEither Nothing name = Left [name]
convertMaybeToEither _ name = Right name

hoistErrors :: Either e a -> Errors e a
hoistErrors e = case e of
    Left es ->
        Other (Constant es)
    Right a ->
        Pure a

sequenceEither :: (Monoid e, Traversable f) => f (Either e a) -> Either e (f a)
sequenceEither = runErrors . traverse hoistErrors        
