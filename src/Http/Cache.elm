module Http.Cache exposing (CacheRequestResult, CacheSize(..), CachedRequest(..), NetworkCache, empty, jsonResponse, request, textResponse)

import Cache exposing (Cache, CacheError(..))
import Http exposing (Request, Response)
import Json.Decode as Decode exposing (Decoder)
import PositiveInt exposing (PositiveInt)


type alias NetworkCache a =
    Cache String (Result Http.Error a)


type CacheSize
    = Byte Int
    | KiloByte Int
    | MegaByte Int


type alias CacheRequest =
    Response String


type alias CacheRequestResult =
    Result Http.Error CacheRequest


type CachedRequest a
    = Cached (Result Http.Error a)
    | NoCache (Request CacheRequest)


empty : CacheSize -> NetworkCache a
empty sizeMax =
    toByte sizeMax |> PositiveInt.cast |> Cache.safeEmpty


toByte : CacheSize -> Int
toByte size =
    case size of
        Byte int ->
            int

        KiloByte kb ->
            kb * 1000

        MegaByte mb ->
            mb * 1000000


request : NetworkCache a -> String -> CachedRequest a
request cache url =
    case Cache.get cache url of
        Just decoded ->
            Cached decoded

        Nothing ->
            NoCache (getReq url)



-- TODO: this is a stupid hack to give errors a cache size...


estimateHttpErrorSize : Http.Error -> Int
estimateHttpErrorSize err =
    10


decodeErrorToHttpError : Response String -> Decode.Error -> Http.Error
decodeErrorToHttpError fullResponse decodeErrExplanation =
    Http.BadPayload (Decode.errorToString decodeErrExplanation) fullResponse


getReq : String -> Request (Response String)
getReq url =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectStringResponse (\fullResponse -> Ok fullResponse)
        , timeout = Nothing
        , withCredentials = False
        }


response : NetworkCache a -> String -> CacheRequestResult -> (String -> Result Decode.Error a) -> ( NetworkCache a, Result Http.Error a )
response cache url httpResponse handler =
    let
        ( estimatedSize, decoded ) =
            case httpResponse of
                Err err ->
                    ( estimateHttpErrorSize err, Err err )

                Ok fullResponse ->
                    ( String.length fullResponse.body
                    , handler fullResponse.body
                        |> Result.mapError (decodeErrorToHttpError fullResponse)
                    )

        maybeNewCache =
            Cache.insert cache
                { size = PositiveInt.cast <| estimatedSize
                , tag = url
                , value = decoded
                }
    in
    case maybeNewCache of
        -- to big, do not cache this object
        Err (ItemTooBig _) ->
            ( cache, decoded )

        Ok newCache ->
            ( newCache, decoded )


jsonResponse : NetworkCache a -> String -> CacheRequestResult -> Decoder a -> ( NetworkCache a, Result Http.Error a )
jsonResponse cache url httpResponse decoder =
    response cache url httpResponse (Decode.decodeString decoder)


textResponse : NetworkCache String -> String -> CacheRequestResult -> ( NetworkCache String, Result Http.Error String )
textResponse cache url httpResponse =
    response cache url httpResponse (\body -> Ok body)
