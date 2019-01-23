{--
A partial implementation of rtfeldman/selectlist. Richard's package has not been 
upgraded to Elm 0.19, so I re-implemented just the functionality I needed for ths app
--}


module Data.SelectList exposing (SelectList, fromList, select, selected, toList)


type SelectList a
    = SelectList (List a) a (List a)


fromList : List a -> Maybe (SelectList a)
fromList list =
    case list of
        x :: xs ->
            Just (SelectList [] x xs)

        [] ->
            Nothing


toList : SelectList a -> List a
toList (SelectList prev current next) =
    prev ++ [ current ] ++ next


selected : SelectList a -> a
selected (SelectList _ current _) =
    current


select : (a -> Bool) -> SelectList a -> SelectList a
select isSelectable ((SelectList beforeSel sel afterSel) as original) =
    case selectHelp isSelectable beforeSel sel afterSel of
        Nothing ->
            original

        Just ( newBefore, newSel, newAfter ) ->
            SelectList newBefore newSel newAfter


selectHelp : (a -> Bool) -> List a -> a -> List a -> Maybe ( List a, a, List a )
selectHelp isSelectable beforeList selectedElem afterList =
    case ( beforeList, afterList ) of
        ( [], [] ) ->
            Nothing

        ( [], first :: rest ) ->
            if isSelectable selectedElem then
                Just ( beforeList, selectedElem, afterList )

            else if isSelectable first then
                Just ( beforeList ++ [ selectedElem ], first, rest )

            else
                case selectHelp isSelectable [] first rest of
                    Nothing ->
                        Nothing

                    Just ( newBefore, newSelected, newAfter ) ->
                        Just ( selectedElem :: newBefore, newSelected, newAfter )

        ( first :: rest, _ ) ->
            if isSelectable first then
                Just ( [], first, rest ++ selectedElem :: afterList )

            else
                case selectHelp isSelectable rest selectedElem afterList of
                    Nothing ->
                        Nothing

                    Just ( newBefore, newSelected, newAfter ) ->
                        Just ( first :: newBefore, newSelected, newAfter )
