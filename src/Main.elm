module Main exposing (..)

{-| -}

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Element.Region as Region
import Euro2020 exposing (GroupRow, Match, Team, groups, matches)
import Html exposing (Html)


blue =
    Element.rgb 0 0 0.4


red =
    Element.rgb 0.8 0 0


grey =
    Element.rgb255 211 211 211


edges =
    { top = 16
    , right = 0
    , bottom = 0
    , left = 24
    }



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- UPDATE


type Msg
    = UpdateScore Int HomeOrAway String
    | ClickedGroupButton


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
                , groups =
                    newMatches
                        |> List.foldl updateGroup groups
                        |> List.sortBy .score
                        |> List.reverse
            }

        ClickedGroupButton ->
            { model | selectedGroup = "groupB" }


getScore : Int -> Int -> Int -> Int -> Int
getScore pts gd gf w =
    (pts * 1000) + (gd * 100) + (gf * 10) + w


updateGroupRow2 : GroupRow -> Int -> Int -> GroupRow
updateGroupRow2 groupRow gf ga =
    let
        newPld =
            groupRow.pld + 1

        newGf =
            groupRow.gf + gf

        newGa =
            groupRow.ga + ga

        newGd =
            newGf - newGa
    in
    if gf > ga then
        let
            newW =
                groupRow.w + 1

            newPts =
                3 * newW + groupRow.d

            newScore =
                getScore newPts newGd newGf newW
        in
        { groupRow | pld = newPld, w = newW, gf = newGf, ga = newGa, gd = newGd, pts = newPts, score = newScore }

    else if gf == ga then
        let
            newD =
                groupRow.d + 1

            newPts =
                3 * groupRow.w + newD

            newScore =
                getScore newPts newGd newGf groupRow.w
        in
        { groupRow | pld = newPld, d = newD, gf = newGf, ga = newGa, gd = newGd, pts = newPts, score = newScore }

    else
        let
            newL =
                groupRow.l + 1

            newscore =
                getScore groupRow.pts newGd newGf groupRow.w
        in
        { groupRow | pld = newPld, l = newL, gf = newGf, ga = newGa, gd = newGd, score = newscore }


updateGroupRow : Match -> GroupRow -> GroupRow
updateGroupRow match groupRow =
    case ( match.homeScore, match.awayScore ) of
        ( Just homeScore, Just awayScore ) ->
            if groupRow.team.name == match.homeTeam.name then
                updateGroupRow2 groupRow homeScore awayScore

            else if groupRow.team.name == match.awayTeam.name then
                updateGroupRow2 groupRow awayScore homeScore

            else
                groupRow

        _ ->
            groupRow


updateGroup : Match -> List GroupRow -> List GroupRow
updateGroup match group =
    List.map (updateGroupRow match) group



-- MODEL


type alias Model =
    { matches : List Match
    , groups : List GroupRow
    , selectedGroup : String
    }


type HomeOrAway
    = Home
    | Away


filterByGroup : String -> List GroupRow -> List GroupRow
filterByGroup groupName groupRows =
    List.filter (\gr -> gr.group == groupName) groupRows


getGroup : List GroupRow -> String -> List GroupRow
getGroup groups groupName =
    groups
        |> filterByGroup groupName
        |> List.sortBy .score
        |> List.reverse


init =
    { matches = matches
    , groups = groups
    , selectedGroup = "groupA"
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
        column
            [ width fill ]
            [ header
            , column [ width fill, centerX ]
                [ viewGroupTitle "Group A"
                , viewGroup model.groups "groupA"
                , viewMatches model.matches "groupA"
                , viewGroupTitle "Group B"
                , viewGroup model.groups "groupB"
                , viewMatches model.matches "groupB"
                , viewGroupTitle "Group C"
                , viewGroup model.groups "groupC"
                , viewMatches model.matches "groupC"
                , viewGroupTitle "Group D"
                , viewGroup model.groups "groupD"
                , viewMatches model.matches "groupD"
                , viewGroupTitle "Group E"
                , viewGroup model.groups "groupE"
                , viewMatches model.matches "groupE"
                , viewGroupTitle "Group F"
                , viewGroup model.groups "groupF"
                , viewMatches model.matches "groupF"
                ]
            ]


header : Element Msg
header =
    row
        [ width fill
        , Background.color red
        , padding 18
        ]
        [ el [] (text "Hahimur!")

        --, myNav
        ]


myNav : Element Msg
myNav =
    row
        [ Region.navigation
        , alignRight
        ]
        [ myButton ]


viewMatch : Match -> Element Msg
viewMatch match =
    column
        [ Border.width 2
        , width (px 250)
        ]
        [ row
            [ width fill ]
            [ el [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY ] (text match.homeTeam.name)
            , el [ alignRight ] (viewMatchInput match.id Home match.homeScore)
            ]
        , row
            [ width fill ]
            [ el [ alignLeft, paddingEach { edges | left = 10, top = 0 } ] (text match.awayTeam.name)
            , el [ alignRight ] (viewMatchInput match.id Away match.awayScore)
            ]
        ]


viewMatches : List Match -> String -> Element Msg
viewMatches matches group =
    matches
        |> List.filter (\m -> m.group == group)
        |> List.map viewMatch
        |> wrappedRow [ width fill, spacing 20, padding 24 ]


viewGroupTitle : String -> Element Msg
viewGroupTitle groupName =
    row
        [ paddingEach edges
        , Background.color grey
        , width fill
        ]
        [ text groupName ]


viewGroup : List GroupRow -> String -> Element Msg
viewGroup groups groupName =
    let
        group =
            getGroup groups groupName
    in
    Element.indexedTable
        [ paddingEach { edges | bottom = 16 }
        , spacing 8
        , Background.color grey
        ]
        { data = group
        , columns =
            [ { header = Element.text "Pos"
              , width = fillPortion 1
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt (n + 1))
              }
            , { header = Element.text "Team"
              , width = fillPortion 2
              , view =
                    \n groupRow ->
                        Element.text groupRow.team.name
              }
            , { header = Element.text "Pld"
              , width = fillPortion 1
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.pld)
              }
            , { header = Element.text "W"
              , width = fillPortion 1
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.w)
              }
            , { header = Element.text "D"
              , width = fillPortion 1
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.d)
              }
            , { header = Element.text "L"
              , width = fillPortion 1
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.l)
              }
            , { header = Element.text "GF"
              , width = fillPortion 1
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.gf)
              }
            , { header = Element.text "GA"
              , width = fillPortion 1
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.ga)
              }
            , { header = Element.text "GD"
              , width = fillPortion 1
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.gd)
              }
            , { header = Element.text "Pts"
              , width = fillPortion 1
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.pts)
              }
            ]
        }


viewMatchInput : Int -> HomeOrAway -> Maybe Int -> Element Msg
viewMatchInput matchId homeOrAway score =
    Element.Input.text [ width (px 50), Font.color (rgba 0 0 0 1) ]
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


myButton =
    button
        [ Background.color red
        , Element.focused
            [ Background.color blue ]
        ]
        { onPress = Just ClickedGroupButton
        , label = text "groupB"
        }


myButton2 =
    button
        [ Background.color red
        , Element.focused
            [ Background.color blue ]
        ]
        { onPress = Just ClickedGroupButton
        , label = text "groupC"
        }
