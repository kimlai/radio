module Api exposing (..)

import Http
import Json.Decode exposing (field)
import Json.Decode.Pipeline exposing (custom, hardcoded, optional, required)
import Track exposing (StreamingInfo(..), Track, TrackId)


decodePlaylist : Json.Decode.Decoder ( List Track, Maybe String )
decodePlaylist =
    Json.Decode.map2 (\a b -> ( a, b ))
        (field "tracks" (Json.Decode.list decodeTrack))
        (field "next_href" (Json.Decode.nullable Json.Decode.string))


decodeTrack : Json.Decode.Decoder Track
decodeTrack =
    Json.Decode.succeed Track
        |> required "id" Json.Decode.int
        |> required "artist" Json.Decode.string
        |> optional "cover" Json.Decode.string "/images/placeholder.jpg"
        |> required "title" Json.Decode.string
        |> custom decodeStreamingInfo
        |> required "source" Json.Decode.string
        |> hardcoded 0
        |> hardcoded 0
        |> hardcoded False


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
