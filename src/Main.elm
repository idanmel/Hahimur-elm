module Main exposing (..)

{-| -}

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)


blue =
    Element.rgb 0 0 0.4


red =
    Element.rgb 0.8 0 0



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- UPDATE


type Msg
    = UpdateScore Int HomeOrAway String


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateScore matchId homeOrAway score ->
            let
                updateMatch : Match -> Match
                updateMatch m =
                    if m.id == matchId then
                        if homeOrAway == Home then
                            { m | homeScore = String.toInt score }

                        else
                            { m | awayScore = String.toInt score }

                    else
                        m

                newMatches =
                    List.map updateMatch model.matches
            in
            { model
                | matches = newMatches
                , group =
                    newMatches
                        |> List.foldl updateGroup initGroup
                        |> List.sortBy .score
                        |> List.reverse
            }


getScore : Int -> Int -> Int -> Int -> Int
getScore pts gd gf w =
    (pts * 1000) + (gd * 100) + (gf * 10) + w


updateTeam2 : Team -> Int -> Int -> Team
updateTeam2 team gf ga =
    let
        newPld =
            team.pld + 1

        newGf =
            team.gf + gf

        newGa =
            team.ga + ga

        newGd =
            newGf - newGa
    in
    if gf > ga then
        let
            newW =
                team.w + 1

            newPts =
                3 * newW + team.d

            newScore =
                getScore newPts newGd newGf newW
        in
        { team | pld = newPld, w = newW, gf = newGf, ga = newGa, gd = newGd, pts = newPts, score = newScore }

    else if gf == ga then
        let
            newD =
                team.d + 1

            newPts =
                3 * team.w + newD

            newScore =
                getScore newPts newGd newGf team.w
        in
        { team | pld = newPld, d = newD, gf = newGf, ga = newGa, gd = newGd, pts = newPts, score = newScore }

    else
        let
            newL =
                team.l + 1

            newscore =
                getScore team.pts newGd newGf team.w
        in
        { team | pld = newPld, l = newL, gf = newGf, ga = newGa, gd = newGd, score = newscore }


updateTeam : Match -> Team -> Team
updateTeam match team =
    case ( match.homeScore, match.awayScore ) of
        ( Just homeScore, Just awayScore ) ->
            if team.name == match.homeTeam then
                updateTeam2 team homeScore awayScore

            else if team.name == match.awayTeam then
                updateTeam2 team awayScore homeScore

            else
                team

        _ ->
            team


updateGroup : Match -> List Team -> List Team
updateGroup match group =
    List.map (updateTeam match) group



-- MODEL


type alias Match =
    { id : Int
    , homeTeam : String
    , homeScore : Maybe Int
    , awayTeam : String
    , awayScore : Maybe Int
    }


type alias Team =
    { name : String
    , pld : Int
    , w : Int
    , d : Int
    , l : Int
    , gf : Int
    , ga : Int
    , gd : Int
    , pts : Int
    , score : Int
    , pos : Int
    }


type alias Model =
    { matches : List Match
    , group : List Team
    }


type HomeOrAway
    = Home
    | Away


bel =
    Team "Belgium" 0 0 0 0 0 0 0 0 1 1


rus =
    Team "Russia" 0 0 0 0 0 0 0 0 2 2


fin =
    Team "Finland" 0 0 0 0 0 0 0 0 3 3


den =
    Team "Denmark" 0 0 0 0 0 0 0 0 4 4


initGroup =
    [ bel, rus, fin, den ]
        |> List.sortBy .score
        |> List.reverse


init =
    { matches =
        [ Match 1 "Denmark" Nothing "Finland" Nothing
        , Match 2 "Belgium" Nothing "Russia" Nothing
        , Match 3 "Finland" Nothing "Russia" Nothing
        , Match 4 "Denmark" Nothing "Belgium" Nothing
        , Match 5 "Russia" Nothing "Denmark" Nothing
        , Match 6 "Finland" Nothing "Belgium" Nothing
        ]
    , group = initGroup
    }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color (rgba 1 1 1 1)
        , Font.color (rgba 0 0 0 1)
        , inFront <| header
        ]
    <|
        column [ width fill ]
            [ header
            , viewGroup model.group
            , viewMatches model.matches
            ]


header : Element Msg
header =
    row
        [ width fill
        , Background.color red
        , padding 18
        ]
        [ el [] (text "Hahimur!") ]


viewMatch : Match -> Element Msg
viewMatch match =
    row
        [ Background.color blue
        , padding 16
        , spacing 150
        , width fill
        , centerX
        , Font.color (rgba 1 1 1 1)
        ]
        [ el [] (text match.homeTeam)
        , el [] (viewTeamGoalsInput match.id Home match.homeScore)
        , el [] (text "-")
        , el [] (viewTeamGoalsInput match.id Away match.awayScore)
        , el [] (text match.awayTeam)
        ]


viewMatches : List Match -> Element Msg
viewMatches matches =
    List.map viewMatch matches
        |> column [ width fill ]


viewGroup : List Team -> Element Msg
viewGroup group =
    Element.table [ padding 16, spacing 8 ]
        { data = group
        , columns =
            [ { header = Element.text "Team"
              , width = fillPortion 2
              , view =
                    \team ->
                        Element.text team.name
              }
            , { header = Element.text "Pld"
              , width = fillPortion 1
              , view =
                    \team ->
                        Element.text (String.fromInt team.pld)
              }
            , { header = Element.text "W"
              , width = fillPortion 1
              , view =
                    \team ->
                        Element.text (String.fromInt team.w)
              }
            , { header = Element.text "D"
              , width = fillPortion 1
              , view =
                    \team ->
                        Element.text (String.fromInt team.d)
              }
            , { header = Element.text "L"
              , width = fillPortion 1
              , view =
                    \team ->
                        Element.text (String.fromInt team.l)
              }
            , { header = Element.text "GF"
              , width = fillPortion 1
              , view =
                    \team ->
                        Element.text (String.fromInt team.gf)
              }
            , { header = Element.text "GA"
              , width = fillPortion 1
              , view =
                    \team ->
                        Element.text (String.fromInt team.ga)
              }
            , { header = Element.text "GD"
              , width = fillPortion 1
              , view =
                    \team ->
                        Element.text (String.fromInt team.gd)
              }
            , { header = Element.text "Pts"
              , width = fillPortion 1
              , view =
                    \team ->
                        Element.text (String.fromInt team.pts)
              }
            ]
        }


viewTeamGoalsInput : Int -> HomeOrAway -> Maybe Int -> Element Msg
viewTeamGoalsInput matchId homeOrAway score =
    Element.Input.text [ width (px 100), Font.color (rgba 0 0 0 1) ]
        { onChange = UpdateScore matchId homeOrAway
        , text =
            case score of
                Nothing ->
                    ""

                Just value ->
                    String.fromInt value
        , placeholder = Nothing
        , label = Element.Input.labelHidden ""
        }
