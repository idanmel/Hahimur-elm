module Main exposing (..)

{-| -}

import Array
import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Element.Region as Region
import Euro2020 exposing (Group(..), GroupRow, Match, Team, defaultFlag, filterByGroup, getGroupRows, getTeamPlaying, groups, matches, playOffMatches)
import Html exposing (Html)
import Html.Attributes
import Set
import Tuple


blue =
    Element.rgb 0 0 0.4


red =
    Element.rgb 0.8 0 0


grey =
    Element.rgb255 242 242 242


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


update : Msg -> Model -> Model
update msg model =
    case msg of
        UpdateScore matchId homeOrAway score ->
            let
                updateScore : Match -> Match
                updateScore m =
                    if m.id == matchId then
                        if homeOrAway == Home then
                            { m | homeScore = String.toInt score }

                        else
                            { m | awayScore = String.toInt score }

                    else
                        m

                newMatchesScores =
                    List.map updateScore model.matches

                newGroupRows =
                    newMatchesScores
                        |> List.foldl updateGroup groups
                        |> List.sortBy .score
                        |> List.reverse

                thirdPlaces =
                    get3rdTeamTable newGroupRows

                updateTeams : List GroupRow -> Match -> Match
                updateTeams groupRows m =
                    case m.id of
                        37 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Winner Group A" defaultFlag) GroupA 1 groupRows
                                , awayTeam = getTeamPlaying (Team "Runner-up Group C" defaultFlag) GroupC 2 groupRows
                            }

                        38 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Runner-up Group A" defaultFlag) GroupA 2 groupRows
                                , awayTeam = getTeamPlaying (Team "Runner-up Group B" defaultFlag) GroupB 2 groupRows
                            }

                        39 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Winner Group B" defaultFlag) GroupB 1 groupRows
                                , awayTeam = get3rdTeam (Team "3rd Group A/D/E/F" defaultFlag) B1 thirdPlaces
                            }

                        40 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Winner Group C" defaultFlag) GroupC 1 groupRows
                                , awayTeam = get3rdTeam (Team "3rd Group D/E/F" defaultFlag) C1 thirdPlaces
                            }

                        41 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Winner Group F" defaultFlag) GroupF 1 groupRows
                                , awayTeam = get3rdTeam (Team "3rd Group A/B/C" defaultFlag) F1 thirdPlaces
                            }

                        42 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Runner-up Group D" defaultFlag) GroupD 2 groupRows
                                , awayTeam = getTeamPlaying (Team "Runner-up Group E" defaultFlag) GroupE 2 groupRows
                            }

                        43 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Winner Group E" defaultFlag) GroupE 1 groupRows
                                , awayTeam = get3rdTeam (Team "3rd Group A/B/C/D" defaultFlag) E1 thirdPlaces
                            }

                        44 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Winner Group D" defaultFlag) GroupD 1 groupRows
                                , awayTeam = getTeamPlaying (Team "Runner-up Group F" defaultFlag) GroupF 2 groupRows
                            }

                        _ ->
                            m

                newPlayOffMatches =
                    List.map (updateTeams newGroupRows) newMatchesScores
            in
            { model
                | matches = newPlayOffMatches
                , groups = newGroupRows
            }


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


get3rdPlaceTeam : List GroupRow -> GroupRow
get3rdPlaceTeam groupRows =
    let
        groupRowsArray =
            Array.fromList groupRows

        thirdPlace =
            Array.get 2 groupRowsArray
    in
    case thirdPlace of
        Just gr ->
            gr

        Nothing ->
            GroupRow (Team "Turkey" "") 0 0 0 0 0 0 0 0 4 GroupA


get3rdTeamTable : List GroupRow -> List GroupRow
get3rdTeamTable groupRows =
    let
        thirdPlaceA =
            groupRows
                |> getGroupRows GroupA
                |> get3rdPlaceTeam

        thirdPlaceB =
            groupRows
                |> getGroupRows GroupB
                |> get3rdPlaceTeam

        thirdPlaceC =
            groupRows
                |> getGroupRows GroupC
                |> get3rdPlaceTeam

        thirdPlaceD =
            groupRows
                |> getGroupRows GroupD
                |> get3rdPlaceTeam

        thirdPlaceE =
            groupRows
                |> getGroupRows GroupE
                |> get3rdPlaceTeam

        thirdPlaceF =
            groupRows
                |> getGroupRows GroupF
                |> get3rdPlaceTeam
    in
    [ thirdPlaceA, thirdPlaceB, thirdPlaceC, thirdPlaceD, thirdPlaceE, thirdPlaceF ]
        |> List.sortBy .score
        |> List.reverse


get3rdTeam : Team -> TeamPosition -> List GroupRow -> Team
get3rdTeam defaultTeam tp groupRows =
    let
        topFour =
            List.take 4 groupRows
    in
    if List.all (\gr -> gr.pld == 3) groupRows then
        case tp of
            B1 ->
                applyCrazyUefaLogic topFour B1

            C1 ->
                applyCrazyUefaLogic topFour C1

            E1 ->
                applyCrazyUefaLogic topFour E1

            F1 ->
                applyCrazyUefaLogic topFour F1

    else
        defaultTeam



--subset : List comparable -> List comparable -> Bool
--subset group1 group2 =
--    let
--        set1 =
--            Set.fromList group1
--        set2 =
--            Set.fromList group2
--    in
--    if Set.size set1 <= Set.size set2 then
--        Set.intersect set1 set2 == set2
--    else
--        Set.intersect set1 set2 == set1


groupToString : Group -> String
groupToString g =
    case g of
        GroupA ->
            "A"

        GroupB ->
            "B"

        GroupC ->
            "C"

        GroupD ->
            "D"

        GroupE ->
            "E"

        GroupF ->
            "F"

        _ ->
            ""


teamFromMaybeGroupRow : Maybe GroupRow -> Team
teamFromMaybeGroupRow gr =
    case gr of
        Just row ->
            row.team

        Nothing ->
            Team "Wow14" ""


getRoundOf16Team : Group -> List GroupRow -> Team
getRoundOf16Team g top4Table =
    top4Table
        |> filterByGroup g
        |> Array.fromList
        |> Array.get 0
        |> teamFromMaybeGroupRow


applyCrazyUefaLogic : List GroupRow -> TeamPosition -> Team
applyCrazyUefaLogic top4groupRows tp =
    let
        sortedStringGroups =
            top4groupRows
                |> List.map .group
                |> List.map groupToString
                |> List.sort
                |> String.concat
    in
    case tp of
        B1 ->
            if sortedStringGroups == "ABCD" then
                getRoundOf16Team GroupA top4groupRows

            else if sortedStringGroups == "ABCE" then
                getRoundOf16Team GroupA top4groupRows

            else if sortedStringGroups == "ABCF" then
                getRoundOf16Team GroupA top4groupRows

            else if sortedStringGroups == "ABDE" then
                getRoundOf16Team GroupD top4groupRows

            else if sortedStringGroups == "ABDF" then
                getRoundOf16Team GroupD top4groupRows

            else if sortedStringGroups == "ABEF" then
                getRoundOf16Team GroupE top4groupRows

            else if sortedStringGroups == "ACDE" then
                getRoundOf16Team GroupE top4groupRows

            else if sortedStringGroups == "ACDF" then
                getRoundOf16Team GroupF top4groupRows

            else if sortedStringGroups == "ACEF" then
                getRoundOf16Team GroupE top4groupRows

            else if sortedStringGroups == "ADEF" then
                getRoundOf16Team GroupE top4groupRows

            else if sortedStringGroups == "BCDE" then
                getRoundOf16Team GroupE top4groupRows

            else if sortedStringGroups == "BCDF" then
                getRoundOf16Team GroupF top4groupRows

            else if sortedStringGroups == "BCEF" then
                getRoundOf16Team GroupF top4groupRows

            else if sortedStringGroups == "BDEF" then
                getRoundOf16Team GroupF top4groupRows

            else
                getRoundOf16Team GroupF top4groupRows

        C1 ->
            if sortedStringGroups == "ABCD" then
                getRoundOf16Team GroupD top4groupRows

            else if sortedStringGroups == "ABCE" then
                getRoundOf16Team GroupE top4groupRows

            else if sortedStringGroups == "ABCF" then
                getRoundOf16Team GroupF top4groupRows

            else if sortedStringGroups == "ABDE" then
                getRoundOf16Team GroupE top4groupRows

            else if sortedStringGroups == "ABDF" then
                getRoundOf16Team GroupF top4groupRows

            else if sortedStringGroups == "ABEF" then
                getRoundOf16Team GroupF top4groupRows

            else if sortedStringGroups == "ACDE" then
                getRoundOf16Team GroupD top4groupRows

            else if sortedStringGroups == "ACDF" then
                getRoundOf16Team GroupD top4groupRows

            else if sortedStringGroups == "ACEF" then
                getRoundOf16Team GroupF top4groupRows

            else if sortedStringGroups == "ADEF" then
                getRoundOf16Team GroupF top4groupRows

            else if sortedStringGroups == "BCDE" then
                getRoundOf16Team GroupD top4groupRows

            else if sortedStringGroups == "BCDF" then
                getRoundOf16Team GroupD top4groupRows

            else if sortedStringGroups == "BCEF" then
                getRoundOf16Team GroupE top4groupRows

            else if sortedStringGroups == "BDEF" then
                getRoundOf16Team GroupE top4groupRows

            else
                getRoundOf16Team GroupE top4groupRows

        E1 ->
            if sortedStringGroups == "ABCD" then
                getRoundOf16Team GroupB top4groupRows

            else if sortedStringGroups == "ABCE" then
                getRoundOf16Team GroupB top4groupRows

            else if sortedStringGroups == "ABCF" then
                getRoundOf16Team GroupB top4groupRows

            else if sortedStringGroups == "ABDE" then
                getRoundOf16Team GroupA top4groupRows

            else if sortedStringGroups == "ABDF" then
                getRoundOf16Team GroupA top4groupRows

            else if sortedStringGroups == "ABEF" then
                getRoundOf16Team GroupB top4groupRows

            else if sortedStringGroups == "ACDE" then
                getRoundOf16Team GroupC top4groupRows

            else if sortedStringGroups == "ACDF" then
                getRoundOf16Team GroupC top4groupRows

            else if sortedStringGroups == "ACEF" then
                getRoundOf16Team GroupC top4groupRows

            else if sortedStringGroups == "ADEF" then
                getRoundOf16Team GroupD top4groupRows

            else if sortedStringGroups == "BCDE" then
                getRoundOf16Team GroupB top4groupRows

            else if sortedStringGroups == "BCDF" then
                getRoundOf16Team GroupC top4groupRows

            else if sortedStringGroups == "BCEF" then
                getRoundOf16Team GroupC top4groupRows

            else if sortedStringGroups == "BDEF" then
                getRoundOf16Team GroupD top4groupRows

            else
                getRoundOf16Team GroupD top4groupRows

        F1 ->
            if sortedStringGroups == "ABCD" then
                getRoundOf16Team GroupC top4groupRows

            else if sortedStringGroups == "ABCE" then
                getRoundOf16Team GroupC top4groupRows

            else if sortedStringGroups == "ABCF" then
                getRoundOf16Team GroupC top4groupRows

            else if sortedStringGroups == "ABDE" then
                getRoundOf16Team GroupB top4groupRows

            else if sortedStringGroups == "ABDF" then
                getRoundOf16Team GroupB top4groupRows

            else if sortedStringGroups == "ABEF" then
                getRoundOf16Team GroupA top4groupRows

            else if sortedStringGroups == "ACDE" then
                getRoundOf16Team GroupA top4groupRows

            else if sortedStringGroups == "ACDF" then
                getRoundOf16Team GroupA top4groupRows

            else if sortedStringGroups == "ACEF" then
                getRoundOf16Team GroupA top4groupRows

            else if sortedStringGroups == "ADEF" then
                getRoundOf16Team GroupA top4groupRows

            else if sortedStringGroups == "BCDE" then
                getRoundOf16Team GroupC top4groupRows

            else if sortedStringGroups == "BCDF" then
                getRoundOf16Team GroupB top4groupRows

            else if sortedStringGroups == "BCEF" then
                getRoundOf16Team GroupB top4groupRows

            else if sortedStringGroups == "BDEF" then
                getRoundOf16Team GroupB top4groupRows

            else
                getRoundOf16Team GroupC top4groupRows



-- MODEL


type alias Model =
    { matches : List Match
    , groups : List GroupRow
    }


type HomeOrAway
    = Home
    | Away


init =
    { matches = matches
    , groups = groups
    }


type TeamPosition
    = B1
    | C1
    | E1
    | F1



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.color (rgba 0 0 0 1)
        , Background.color grey
        , inFront <| header
        ]
    <|
        row
            [ width (fillPortion 1)
            ]
            [ column [ width (fillPortion 1) ] []
            , column [ width (fillPortion 2), centerX ]
                [ row [ padding 40 ] []
                , viewGroupTitle "Group A"
                , viewGroup model.groups GroupA
                , viewMatches model.matches GroupA
                , viewGroupTitle "Group B"
                , viewGroup model.groups GroupB
                , viewMatches model.matches GroupB
                , viewGroupTitle "Group C"
                , viewGroup model.groups GroupC
                , viewMatches model.matches GroupC
                , viewGroupTitle "Group D"
                , viewGroup model.groups GroupD
                , viewMatches model.matches GroupD
                , viewGroupTitle "Group E"
                , viewGroup model.groups GroupE
                , viewMatches model.matches GroupE
                , viewGroupTitle "Group F"
                , viewGroup model.groups GroupF
                , viewMatches model.matches GroupF
                , viewGroupTitle "Round Of 16"
                , viewPlayoffMatches model.matches RoundOf16
                ]
            , column [ width (fillPortion 1) ] []
            ]


header : Element Msg
header =
    row
        [ width fill
        , Background.color (rgb255 9 62 132)
        , paddingEach { edges | left = 4, bottom = 24, top = 24 }
        , Font.color (rgb 1 1 1)
        ]
        [ el [ width (fillPortion 1), centerY ] (text "")
        , el [ width (fillPortion 8) ] (text "Hahimur!")
        , myNav
        ]


myNav : Element Msg
myNav =
    row
        [ Region.navigation
        , alignRight
        , width (fillPortion 1)
        ]
        [ text "navbar " ]


viewMatch : Match -> Element Msg
viewMatch match =
    column
        [ Border.width 2
        , width (px 350)
        , Background.color (rgba 1 1 1 1)
        ]
        [ row
            [ width fill ]
            [ image [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY, width (fillPortion 1) ] { src = match.homeTeam.flag, description = "" }
            , el [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY, width (fillPortion 6) ] (text match.homeTeam.name)
            , el [ alignRight ] (viewMatchInput match.id Home match.homeScore)
            ]
        , row
            [ width fill ]
            [ image [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY, width (fillPortion 1) ] { src = match.awayTeam.flag, description = "" }
            , el [ alignLeft, paddingEach { edges | left = 10, top = 0 }, width (fillPortion 6) ] (text match.awayTeam.name)
            , el [ alignRight ] (viewMatchInput match.id Away match.awayScore)
            ]
        ]


viewTwoMatches : List Match -> Element Msg
viewTwoMatches matches =
    row [ spacing 20, padding 24 ] (List.map viewMatch matches)


viewMatches : List Match -> Group -> Element Msg
viewMatches matches group =
    let
        groupMatches =
            List.filter (\m -> m.group == group) matches

        firstDay =
            List.take 2 groupMatches

        secondDay =
            List.take 2 (List.drop 2 groupMatches)

        thirdDay =
            List.drop 4 groupMatches
    in
    column [ centerX ]
        [ viewTwoMatches firstDay
        , viewTwoMatches secondDay
        , viewTwoMatches thirdDay
        ]


viewPlayoffMatches : List Match -> Group -> Element Msg
viewPlayoffMatches matches group =
    let
        playOffMatches =
            List.filter (\m -> m.group == group) matches

        firstCouple =
            List.take 2 playOffMatches

        secondCouple =
            playOffMatches
                |> List.drop 2
                |> List.take 2

        thirdCouple =
            playOffMatches
                |> List.drop 4
                |> List.take 2

        fourthCouple =
            List.drop 6 playOffMatches
    in
    column [ centerX ]
        [ viewTwoMatches firstCouple
        , viewTwoMatches secondCouple
        , viewTwoMatches thirdCouple
        , viewTwoMatches fourthCouple
        ]


viewGroupTitle : String -> Element Msg
viewGroupTitle groupName =
    row
        [ paddingEach { edges | bottom = 16 }
        , width fill
        ]
        [ column
            [ width fill ]
            [ text groupName ]
        ]


viewGroup : List GroupRow -> Group -> Element Msg
viewGroup groups groupName =
    let
        group =
            getGroupRows groupName groups
    in
    Element.indexedTable
        [ paddingEach { edges | bottom = 16 }
        , spacing 8
        , width (fill |> maximum 800)
        , centerX
        ]
        { data = group
        , columns =
            [ { header = Element.text "Pos"
              , width = fill
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt (n + 1))
              }
            , { header = Element.text "Team"
              , width = fill
              , view =
                    \n groupRow ->
                        Element.text groupRow.team.name
              }
            , { header = Element.text "Pld"
              , width = fill
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.pld)
              }
            , { header = Element.text "W"
              , width = fill
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.w)
              }
            , { header = Element.text "D"
              , width = fill
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.d)
              }
            , { header = Element.text "L"
              , width = fill
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.l)
              }
            , { header = Element.text "GF"
              , width = fill
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.gf)
              }
            , { header = Element.text "GA"
              , width = fill
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.ga)
              }
            , { header = Element.text "GD"
              , width = fill
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.gd)
              }
            , { header = Element.text "Pts"
              , width = fill
              , view =
                    \n groupRow ->
                        Element.text (String.fromInt groupRow.pts)
              }
            ]
        }


viewMatchInput : Int -> HomeOrAway -> Maybe Int -> Element Msg
viewMatchInput matchId homeOrAway score =
    Element.Input.text
        [ width (px 80)
        , Font.color (rgba 0 0 0 1)
        , Background.color (rgba 1 1 1 0.8)
        , Element.htmlAttribute (Html.Attributes.type_ "number")
        , paddingEach { edges | top = 12, bottom = 12, left = 12 }
        ]
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
