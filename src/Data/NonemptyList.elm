module Data.NonemptyList exposing (NonemptyList, fromList, head, toList)


type NonemptyList a
    = NonemptyList a (List a)


fromList : List a -> Maybe (NonemptyList a)
fromList list =
    case list of
        x :: xs ->
            Just (NonemptyList x xs)

        [] ->
            Nothing


toList : NonemptyList a -> List a
toList (NonemptyList x xs) =
    x :: xs


head : NonemptyList a -> a
head (NonemptyList a _) =
    a
