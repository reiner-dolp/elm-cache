module Http.Cache exposing (request, response, CacheSize(..))

import Cache exposing (Cache, CacheError(..))
import PositiveInt exposing (PositiveInt)
import Http exposing (Request)
import Json.Decode as Decode exposing (Decoder)

type CacheSize = Byte Int
        | KiloByte Int
        | MegaByte Int

type alias CacheRequest = String

type CachedRequest a
        = Cached a
        | NoCache (Request CacheRequest)

empty : CacheSize -> Cache 
empty sizeMax =
        toByte sizeMax |> PositiveInt.cast |> Cache.safeEmpty

toByte : CacheSize -> Int
toByte size =
        case size of
                Byte int -> int
                KiloByte kb -> kb * 1000
                MegaByte mb -> mb * 1000000
    
request : Cache String (Result Decode.Error a) -> String -> CachedRequest (Result Decode.Error a)
request cache url =
        case Cache.get cache url of
                Just decoded -> Cached decoded
                Nothing -> NoCache <| Http.getString url

response : Cache String (Result Decode.Error a) -> String -> CacheRequest -> Decoder a -> (Cache String (Result Decode.Error a), Result Decode.Error a)
response cache url responseBody decoder =
    let
        decoded = Decode.decodeString decoder responseBody

        maybeNewCache = Cache.insert cache {
            size = PositiveInt.cast <| String.length responseBody,
            tag = url,
            value = decoded
            }
    in
    case maybeNewCache of
        -- to big, do not cache this object
        Err (ItemTooBig _) -> (cache, decoded)
        Ok newCache -> (newCache, decoded)

