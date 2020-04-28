module View exposing (..)

import Html exposing (Html, a, div, img, li, nav, node, text, ul)
import Html.Attributes exposing (..)
import Html.Events exposing (on, onClick, onWithOptions)
import Html.Extra exposing (link)
import Icons
import Json.Decode exposing (field)
import Json.Decode.Extra exposing ((|:))
import Model exposing (NavigationItem)
import Track exposing (Track, TrackId)


viewGlobalPlayer :
    (String -> msg)
    -> msg
    -> msg
    -> (Float -> msg)
    -> Maybe Track
    -> Bool
    -> Html msg
viewGlobalPlayer followLink tooglePlayback next seekTo track playing =
    case track of
        Nothing ->
            text ""

        Just track ->
            let
                icon =
                    if track.error then
                        Icons.error

                    else if playing then
                        Icons.pause

                    else
                        Icons.play
            in
            div
                [ class "global-player" ]
                [ viewProgressBar seekTo track
                , node "cluster-l"
                    []
                    [ div
                        [ class "controls" ]
                        [ viewShowRadioPlaylistToggle followLink
                        , div
                            [ classList
                                [ ( "playback-button", True )
                                , ( "playing", playing && not track.error )
                                , ( "error", track.error )
                                ]
                            , onClick tooglePlayback
                            ]
                            [ icon ]
                        , div
                            [ class "next-button"
                            , onClick next
                            ]
                            [ Icons.next ]
                        , div [ class "track" ]
                            [ div [ class "cover" ]
                                [ img
                                    [ src track.artwork_url, alt "" ]
                                    []
                                ]
                            , div
                                [ class "track-info" ]
                                [ node "stack-l"
                                    [ attribute "space" "var(--s-2)" ]
                                    [ div [ class "title" ] [ text track.title ]
                                    , div [ class "artist" ] [ text ("by " ++ track.artist) ]
                                    ]
                                ]
                            ]
                        ]
                    ]
                ]


viewShowRadioPlaylistToggle : (String -> msg) -> Html msg
viewShowRadioPlaylistToggle followLink =
    link
        followLink
        "/queue/next"
        [ class "show-radio-playlist" ]
        [ Icons.playlist ]


viewLikeButton : (TrackId -> msg) -> (TrackId -> msg) -> Track -> Html msg
viewLikeButton addLike removeLike track =
    if track.liked then
        div
            [ class "unlike"
            , alt "Unlike"
            , onClick (removeLike track.id)
            ]
            [ Icons.heart ]

    else
        div
            [ class "like"
            , alt "Like"
            , onClick (addLike track.id)
            ]
            [ Icons.heart ]


viewProgressBar : (Float -> msg) -> Track -> Html msg
viewProgressBar seekTo track =
    div
        [ class "progress-bar"
        , on "click" (decodeClickXPosition |> Json.Decode.map seekTo)
        ]
        [ div
            [ class "outer" ]
            [ div
                [ class "inner"
                , style [ ( "width", toString track.progress ++ "%" ) ]
                ]
                []
            ]
        , div
            [ class "drag"
            , on "click" (decodeClickXPosition |> Json.Decode.map seekTo)
            ]
            [ text "" ]
        ]


decodeClickXPosition : Json.Decode.Decoder Float
decodeClickXPosition =
    let
        totalOffset (Element { offsetLeft, offsetParent }) =
            case offsetParent of
                Nothing ->
                    offsetLeft

                Just element ->
                    offsetLeft + totalOffset element
    in
    Json.Decode.map2 (/)
        (Json.Decode.map2 (-)
            (Json.Decode.at [ "pageX" ] Json.Decode.float)
            (Json.Decode.at [ "target" ] decodeElement |> Json.Decode.map totalOffset)
        )
        (Json.Decode.at [ "target", "offsetWidth" ] Json.Decode.float)
        |> Json.Decode.map ((*) 100)


type Element
    = Element { offsetLeft : Float, offsetParent : Maybe Element }


instanciateElement : Float -> Maybe Element -> Element
instanciateElement offsetLeft offsetParent =
    Element
        { offsetLeft = offsetLeft
        , offsetParent = offsetParent
        }


decodeElement : Json.Decode.Decoder Element
decodeElement =
    Json.Decode.succeed instanciateElement
        |: field "offsetLeft" Json.Decode.float
        |: field "offsetParent" (Json.Decode.nullable (Json.Decode.lazy (\_ -> decodeElement)))


viewNavigation : (String -> msg) -> List (NavigationItem page playlist) -> page -> Maybe playlist -> Html msg
viewNavigation followLink navigationItems currentPage currentPlaylist =
    nav
        []
        [ node "cluster-l"
            [ attribute "space" "0px" ]
            [ ul
                [ class "navigation" ]
                (List.map
                    (viewNavigationItem followLink currentPage currentPlaylist)
                    navigationItems
                )
            ]
        ]


viewNavigationItem : (String -> msg) -> page -> Maybe playlist -> NavigationItem page playlist -> Html msg
viewNavigationItem followLink currentPage currentPlaylist navigationItem =
    li
        [ onWithOptions
            "click"
            { stopPropagation = False
            , preventDefault = True
            }
            (Json.Decode.succeed (followLink navigationItem.href))
        ]
        [ a
            (classList
                [ ( "active", navigationItem.page == currentPage )
                , ( "playing", navigationItem.playlist /= Nothing && navigationItem.playlist == currentPlaylist )
                ]
                :: [ href navigationItem.href ]
            )
            [ text navigationItem.displayName ]
        ]
