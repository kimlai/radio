module Radio.Model exposing (..)

import Browser.Navigation as Nav
import Model exposing (NavigationItem)
import Player exposing (Player)
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
    , player : Player PlaylistId TrackId
    , navigation : List (NavigationItem Page PlaylistId)
    , key : Nav.Key
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
    Player.getCurrentTrack model.player
        |> Maybe.andThen (\a -> Tracklist.get a model.tracks)
