module Radio.Update exposing (..)

import Api
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Char
import Http exposing (Error(..))
import Json.Decode
import Player
import PlayerEngine
import Radio.Model as Model exposing (Model, Page(..), PlaylistId(..), PlaylistStatus(..))
import Radio.Ports as Ports
import Radio.Router
import Task exposing (Task)
import Track exposing (StreamingInfo(..), Track, TrackId)
import Tracklist
import Update
import Url exposing (Url)


type Msg
    = TogglePlayback
    | Next
    | TrackProgress ( TrackId, Float, Float )
    | Play
    | Pause
    | TrackError TrackId
    | FastForward
    | Rewind
    | LinkClicked UrlRequest
    | UrlChanged Url
    | PlayFromPlaylist PlaylistId Int
    | PlayOutsidePlaylist TrackId
    | FetchMore PlaylistId Bool
    | FetchedMore PlaylistId Bool (Result Http.Error ( List Track, Maybe String ))
    | ResumeRadio
    | SeekTo Float
    | ToggleRadioPlaylist


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        FetchedMore playlistId autoplay (Ok ( tracks, nextLink )) ->
            let
                nextTrackIndex =
                    model.player
                        |> Player.playlistContent playlistId
                        |> List.length

                updatePlaylist playlist =
                    { playlist
                        | nextLink = nextLink
                        , status = Fetched
                    }

                updatedModel =
                    { model
                        | tracks = Tracklist.add tracks model.tracks
                        , player = Player.appendTracksToPlaylist playlistId (List.map .id tracks) model.player
                    }

                newModel =
                    case playlistId of
                        Radio ->
                            ( { updatedModel | radio = updatePlaylist model.radio }
                            , Cmd.none
                            )

                        LatestTracks ->
                            ( { updatedModel | latestTracks = updatePlaylist model.latestTracks }
                            , Cmd.none
                            )
            in
            newModel
                |> Update.when (always autoplay) (update (PlayFromPlaylist playlistId nextTrackIndex))

        FetchedMore playlistId autoplay (Err error) ->
            ( model, Cmd.none )

        PlayFromPlaylist playlistId position ->
            let
                player =
                    Player.select playlistId position model.player

                msg =
                    if Player.getCurrentTrack player == Player.getCurrentTrack model.player then
                        TogglePlayback

                    else
                        Play
            in
            update
                msg
                { model | player = player }

        PlayOutsidePlaylist trackId ->
            let
                player =
                    Player.selectOutsidePlaylist trackId model.player

                msg =
                    if Player.getCurrentTrack player == Player.getCurrentTrack model.player then
                        TogglePlayback

                    else
                        Play
            in
            update
                msg
                { model | player = player }

        FetchMore playlistId autoplay ->
            let
                markAsFetching p =
                    { p | status = Fetching }

                ( playlist, updateModel ) =
                    case playlistId of
                        Radio ->
                            ( model.radio, \m fn -> { m | radio = fn model.radio } )

                        LatestTracks ->
                            ( model.latestTracks, \m fn -> { m | latestTracks = fn model.latestTracks } )
            in
            case playlist.nextLink of
                Nothing ->
                    ( model, Cmd.none )

                Just url ->
                    ( updateModel model markAsFetching
                    , Http.get
                        { url = url
                        , expect = Http.expectJson (FetchedMore playlistId autoplay) Api.decodePlaylist
                        }
                    )

        Play ->
            case Model.currentTrack model of
                Nothing ->
                    ( model
                    , PlayerEngine.pause Nothing
                    )

                Just track ->
                    let
                        resetTrack t =
                            if t.progress > 99.9 then
                                { t | currentTime = 0, progress = 0 }

                            else
                                t

                        updatePlayedTracks tracks =
                            if List.head tracks /= Just track.id && track.progress == 0 then
                                track.id :: tracks

                            else
                                tracks

                        updatedModel =
                            { model
                                | tracks = Tracklist.update track.id resetTrack model.tracks
                                , played = updatePlayedTracks model.played
                            }
                    in
                    ( { updatedModel | playing = True }
                    , PlayerEngine.play (resetTrack track)
                    )

        Pause ->
            ( { model | playing = False }
            , PlayerEngine.pause (Player.getCurrentTrack model.player)
            )

        ResumeRadio ->
            update Play { model | player = Player.selectPlaylist Radio model.player }

        TrackError trackId ->
            let
                newModel =
                    { model | tracks = Tracklist.update trackId Track.markAsErrored model.tracks }

                ( newModelWithNext, command ) =
                    update Next newModel
            in
            ( newModelWithNext
            , command
            )

        TogglePlayback ->
            if model.playing then
                update Pause model

            else
                update Play model

        Next ->
            let
                currentPlaylist =
                    Player.getCurrentPlaylist model.player

                nextTrack =
                    (Player.getCurrentTrack << Player.next) model.player
            in
            case nextTrack of
                Just track ->
                    update Play { model | player = Player.next model.player }

                Nothing ->
                    case currentPlaylist of
                        Nothing ->
                            ( model, Cmd.none )

                        Just playlistId ->
                            update (FetchMore playlistId True) model

        TrackProgress ( trackId, progress, currentTime ) ->
            ( { model
                | tracks = Tracklist.update trackId (Track.recordProgress progress currentTime) model.tracks
              }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | currentPage = Radio.Router.urlToPage url }
            , Cmd.none
            )

        SeekTo positionInPercentage ->
            ( model
            , PlayerEngine.seekToPercentage (Model.currentTrack model) positionInPercentage
            )

        FastForward ->
            ( model
            , PlayerEngine.changeCurrentTime (Model.currentTrack model) 10
            )

        Rewind ->
            ( model
            , PlayerEngine.changeCurrentTime (Model.currentTrack model) -10
            )

        ToggleRadioPlaylist ->
            ( { model | showRadioPlaylist = not model.showRadioPlaylist }
            , Cmd.none
            )
