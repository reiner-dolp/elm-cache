A LRU cache, primarily designed to cache parsed data obtained over HTTP to
speed up navigation inside an Elm SPA. Maximum cache size can be specified in
byte. Data is cached *after* parsing in RAM.

Only use this if the lifetime of data is hard to judge and temporal in nature,
e.g. the next request is triggered by user interactions and a more recent
request is more likely to occur again. This is for example the case if a user
navigates through pages of your Elm application and uses the back button. 

To speed up initial page loads, you can also do predictive/speculative page
loading by executing requests for possible next pages and inserting them into
the cache.

> **NOTE**: alpha release. Not in the Elm package manager yet.

```elm
type Status
    = Loading
    | Loaded a
    | Failure

statusFromResult : Result e a -> Status a
statusFromResult result =
    case result of
        Ok val ->
            Loaded val

        Err reason ->
            Failed

-- SPA Main: Add a cache object to the model

type alias Model = {
    data: Page,
    cache: NetworkCache
    }

initialCache : NetworkCache
initialCache =
    Http.Cache.empty (MegaByte 2)

-- in the page of your SPA: recieve and send back the cache object

type Msg
    = LoadedUrl Url CacheRequestResult

url : String
url = "http://example.org/thing.json"


init : NetworkCache -> ( Model, Cmd Msg )
init cache =
    let
        cacheRequest =
            NetworkCache.request cache url
    in
    case cacheRequest of
        Cached value ->
            (statusFromResult value, Cmd.none)

        NoCache request ->
            (Loading, Http.send (LoadedUrl url) request)

update : Msg -> Model -> (Model, NetworkCache, Cmd Msg)
update (LoadedUrl url response) cache =
    let
        ( newCache, result ) =
            NetworkCache.textResponse cache url response
    in
    (statusFromResult result, newCache, Cmd.none )

view : Model -> Html msg
view model =
    case model of
        Loading -> text "loading"
        Failure -> text "failure"
        Loaded data -> text <| "loaded: " ++ Debug.toString data

```
