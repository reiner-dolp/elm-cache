module Cache exposing (Cache, CacheError(..), currentSize, empty, errorToString, get, insert, member, safeEmpty)

import PositiveInt exposing (PositiveInt)


type Cache tag value
    = Cache (CacheStructure tag value)


type CacheError
    = ItemTooBig { cacheMax : PositiveInt, itemSize : PositiveInt }


errorToString : CacheError -> String
errorToString err =
    case err of
        ItemTooBig { cacheMax, itemSize } ->
            "The given item with size " ++ PositiveInt.toString itemSize ++ " is to large for a cache of size " ++ PositiveInt.toString cacheMax


type alias CacheItem tag value =
    { size : PositiveInt, tag : tag, value : value }


type alias CacheStructure tag value =
    { -- The items are sorted according to the LRU pattern
      -- Most recently used item is the first in the list
      -- Least recently used item is the last in the list
      items : List (CacheItem tag value)
    , sizeMax : PositiveInt
    , sizeCurrent : PositiveInt
    }


safeEmpty : PositiveInt -> Cache tag value
safeEmpty sizeMax =
    Cache
        { items = []
        , sizeMax = sizeMax
        , sizeCurrent = PositiveInt.zero
        }


empty : Int -> Result PositiveInt.Error (Cache tag value)
empty sizeMax =
    PositiveInt.from sizeMax |> Result.map safeEmpty


ejectUntilSize : Cache tag value -> PositiveInt -> Cache tag value
ejectUntilSize (Cache cache) target_size =
    -- TODO recreates the whole list :/
    let
        (Cache emptyCache) =
            safeEmpty cache.sizeMax

        takeWhile acc items =
            case items of
                [] ->
                    { acc | items = List.reverse acc.items }

                item :: remaining ->
                    let
                        size_afterwards =
                            PositiveInt.add acc.sizeCurrent item.size
                    in
                    if PositiveInt.unwrap size_afterwards <= PositiveInt.unwrap target_size then
                        takeWhile { acc | items = item :: acc.items, sizeCurrent = size_afterwards } remaining

                    else
                        { acc | items = List.reverse acc.items }
    in
    Cache (takeWhile emptyCache cache.items)


insert : Cache tag value -> CacheItem tag value -> Result CacheError (Cache tag value)
insert (Cache cache) item =
    case PositiveInt.subs cache.sizeMax item.size of
        Ok targetSize ->
            let
                (Cache withSpace) =
                    ejectUntilSize (Cache cache) targetSize
            in
            Ok <|
                Cache
                    { withSpace
                        | items = item :: withSpace.items
                        , sizeCurrent = PositiveInt.add cache.sizeCurrent item.size
                    }

        Err _ ->
            Err <| ItemTooBig { cacheMax = cache.sizeMax, itemSize = item.size }


get : Cache tag value -> tag -> Maybe value
get (Cache cache) tag =
    -- Given that the data has to have a temporal access pattern for LRU to
    -- work, search the list from most recent to oldest should have good performance...
    let
        getHelp list =
            case list of
                [] ->
                    Nothing

                x :: xs ->
                    if x.tag == tag then
                        Just x.value

                    else
                        getHelp xs
    in
    getHelp cache.items


currentSize : Cache tag value -> Int
currentSize (Cache cache) =
    cache.sizeCurrent |> PositiveInt.unwrap


member : Cache tag value -> tag -> Bool
member (Cache cache) tag =
    -- Given that the data has to have a temporal access pattern for LRU to
    -- work, search the list from most recent to oldest should have good performance...
    let
        memberHelp list =
            case list of
                [] ->
                    False

                x :: xs ->
                    if x.tag == tag then
                        True

                    else
                        memberHelp xs
    in
    memberHelp cache.items
