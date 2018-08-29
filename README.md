A LRU cache, primarily designed to cache parsed data obtained over HTTP to
speed up navigation inside an Elm SPA. Maximum cache size can be specified in
byte. Data is cached *after* parsing in RAM.

Only use this if the lifetime of data is hard to judge and temporal in nature,
e.g. the next request is triggered by user interactions and a more recent
request is more likely to occurr again. This is for example the case if a user
navigates through pages of your Elm application and uses the back button. 

To speed up initial page loads, you can also do predictive page loading by
executing requests for possible next pages and inserting them into the cache.

--
**NOTE**: alpha release. Not in the Elm package manager yet.
--

```elm
-- PSEUDOCODE FOR USAGE

decodeData = ...

cache =
    Http.Cache.empty (MegaByte 2)

update msg model =
    case msg of
        LoadPageDataFrom url ->
            case Http.cache.request model.cache url of
                Cached response -> ({ model | data = response })
                NoCache request -> (model, Http.send <| RecievePageDataFor url request)

        RecievePageDataFor url response ->
            let 
               (newCache, data) = Http.cache.response model.cache url response decodeData
            in
            ({model | data = response, cache = newCache}, Cmd.none)
```
