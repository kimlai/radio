module Radio.Model exposing (..)

import Model exposing (NavigationItem)
import Player exposing (Player)
import Time exposing (Time)
import Track exposing (Track, TrackId)
import Tracklist exposing (Tracklist)


type alias Model =
    { tracks : Tracklist
    , radio : Playlist
    , showRadioPlaylist : Bool
    , latestTracks : Playlist
    , played : List TrackId
    , playing : Bool
    , currentPage : Page
    , lastKeyPressed : Maybe Char
    , currentTime : Maybe Time
    , player : Player PlaylistId TrackId
    , navigation : List (NavigationItem Page PlaylistId)
    }


type alias Playlist =
    { id : PlaylistId
    , status : PlaylistStatus
    , nextLink : Maybe String
    }


type PlaylistStatus
    = NotRequested
    | Fetching
    | Fetched


emptyPlaylist : PlaylistId -> String -> Playlist
emptyPlaylist id fetchUrl =
    { id = id
    , status = NotRequested
    , nextLink = Just fetchUrl
    }


type Page
    = RadioPage
    | UpNextPage
    | PlayedPage
    | LatestTracksPage
    | PageNotFound


type PlaylistId
    = Radio
    | LatestTracks


currentTrack : Model -> Maybe Track
currentTrack model =
    Player.currentTrack model.player
        |> Maybe.andThen (flip Tracklist.get model.tracks)
