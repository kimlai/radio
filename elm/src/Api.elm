module Api exposing (..)

import Http
import Json.Decode exposing (field)
import Json.Decode.Extra exposing ((|:))
import Track exposing (StreamingInfo(..), Track, TrackId)


fetchPlaylist : String -> Json.Decode.Decoder Track -> Http.Request ( List Track, Maybe String )
fetchPlaylist url trackDecoder =
    Http.request
        { method = "GET"
        , headers = []
        , url = url
        , body = Http.emptyBody
        , expect = Http.expectJson (decodePlaylist trackDecoder)
        , timeout = Nothing
        , withCredentials = False
        }


fetchFeedPlaylist : String -> Json.Decode.Decoder Track -> Http.Request ( List Track, Maybe String )
fetchFeedPlaylist url trackDecoder =
    Http.get url (decodePlaylist trackDecoder)


decodePlaylist : Json.Decode.Decoder Track -> Json.Decode.Decoder ( List Track, Maybe String )
decodePlaylist trackDecoder =
    Json.Decode.map2 (,)
        (field "tracks" (Json.Decode.list trackDecoder))
        (field "next_href" (Json.Decode.nullable Json.Decode.string))


decodeTrack : Json.Decode.Decoder Track
decodeTrack =
    Json.Decode.succeed Track
        |: field "id" Json.Decode.string
        |: field "artist" Json.Decode.string
        |: field "cover" (Json.Decode.Extra.withDefault "/images/placeholder.jpg" Json.Decode.string)
        |: field "title" Json.Decode.string
        |: decodeStreamingInfo
        |: field "source" Json.Decode.string
        |: field "created_at" Json.Decode.Extra.date
        |: field "liked" (Json.Decode.Extra.withDefault False Json.Decode.bool)
        |: Json.Decode.succeed 0
        |: Json.Decode.succeed 0
        |: Json.Decode.succeed False


decodeStreamingInfo : Json.Decode.Decoder StreamingInfo
decodeStreamingInfo =
    Json.Decode.oneOf [ decodeSoundcloudStreamingInfo, decodeYoutubeStreamingInfo ]


decodeSoundcloudStreamingInfo : Json.Decode.Decoder StreamingInfo
decodeSoundcloudStreamingInfo =
    Json.Decode.at [ "soundcloud", "stream_url" ] Json.Decode.string
        |> Json.Decode.andThen (\url -> Json.Decode.succeed (Soundcloud url))


decodeYoutubeStreamingInfo : Json.Decode.Decoder StreamingInfo
decodeYoutubeStreamingInfo =
    Json.Decode.at [ "youtube", "id" ] Json.Decode.string
        |> Json.Decode.andThen (\id -> Json.Decode.succeed (Youtube id))
