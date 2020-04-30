module Track exposing (..)


type alias TrackId =
    Int


type alias Track =
    { id : TrackId
    , artist : String
    , artwork_url : String
    , title : String
    , streamingInfo : StreamingInfo
    , sourceUrl : String
    , progress : Float
    , currentTime : Float
    , error : Bool
    }


type StreamingInfo
    = Soundcloud StreamUrl
    | Youtube YoutubeId


type alias StreamUrl =
    String


type alias YoutubeId =
    String


markAsErrored : Track -> Track
markAsErrored track =
    { track | error = True }


recordProgress : Float -> Float -> Track -> Track
recordProgress progress currentTime track =
    { track | progress = progress, currentTime = currentTime }
