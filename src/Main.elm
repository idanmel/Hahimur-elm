module Main exposing (..)

{-| -}

import Array
import Browser
import Dict exposing (Dict)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input exposing (button)
import Element.Region as Region
import Euro2020 exposing (Group(..), GroupRow, GroupState(..), Match, Team, defaultFlag, filterByGroup, getGroupRows, getGroupState, getScore, getTeamPlaying, groups, matches, playOffMatches, playedAllGames)
import Html exposing (Html)
import Html.Attributes


blue =
    rgb255 9 62 132


red =
    Element.rgb 0.8 0 0


grey =
    Element.rgb255 242 242 242


white =
    rgb 1 1 1


green =
    rgb255 0 204 102


edges =
    { top = 16
    , right = 0
    , bottom = 0
    , left = 0
    }



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- UPDATE


type Msg
    = UpdatedScore Int HomeOrAway String
    | ClickedGroup Group


update : Msg -> Model -> Model
update msg model =
    case msg of
        ClickedGroup group ->
            { model | selectedGroup = group }

        UpdatedScore matchId homeOrAway score ->
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

                newMatches =
                    List.map updateScore model.matches

                newGroupRows =
                    newMatches
                        |> List.foldl updateGroup groups

                groupRowsAfterTieBreaks =
                    List.map (resolveTieBreak newMatches newGroupRows) newGroupRows

                thirdPlaces =
                    get3rdTeamTable groupRowsAfterTieBreaks

                updateTeams : List GroupRow -> Match -> Match
                updateTeams groupRows m =
                    case m.id of
                        37 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Winner Group A" defaultFlag) GroupA 1 groupRowsAfterTieBreaks
                                , awayTeam = getTeamPlaying (Team "Runner-up Group C" defaultFlag) GroupC 2 groupRowsAfterTieBreaks
                            }

                        38 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Runner-up Group A" defaultFlag) GroupA 2 groupRowsAfterTieBreaks
                                , awayTeam = getTeamPlaying (Team "Runner-up Group B" defaultFlag) GroupB 2 groupRowsAfterTieBreaks
                            }

                        39 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Winner Group B" defaultFlag) GroupB 1 groupRowsAfterTieBreaks
                                , awayTeam = get3rdTeam (Team "3rd Group A/D/E/F" defaultFlag) B1 thirdPlaces groupRowsAfterTieBreaks
                            }

                        40 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Winner Group C" defaultFlag) GroupC 1 groupRowsAfterTieBreaks
                                , awayTeam = get3rdTeam (Team "3rd Group D/E/F" defaultFlag) C1 thirdPlaces groupRowsAfterTieBreaks
                            }

                        41 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Winner Group F" defaultFlag) GroupF 1 groupRowsAfterTieBreaks
                                , awayTeam = get3rdTeam (Team "3rd Group A/B/C" defaultFlag) F1 thirdPlaces groupRowsAfterTieBreaks
                            }

                        42 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Runner-up Group D" defaultFlag) GroupD 2 groupRowsAfterTieBreaks
                                , awayTeam = getTeamPlaying (Team "Runner-up Group E" defaultFlag) GroupE 2 groupRowsAfterTieBreaks
                            }

                        43 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Winner Group E" defaultFlag) GroupE 1 groupRowsAfterTieBreaks
                                , awayTeam = get3rdTeam (Team "3rd Group A/B/C/D" defaultFlag) E1 thirdPlaces groupRowsAfterTieBreaks
                            }

                        44 ->
                            { m
                                | homeTeam = getTeamPlaying (Team "Winner Group D" defaultFlag) GroupD 1 groupRowsAfterTieBreaks
                                , awayTeam = getTeamPlaying (Team "Runner-up Group F" defaultFlag) GroupF 2 groupRowsAfterTieBreaks
                            }

                        _ ->
                            m

                newPlayOffMatches =
                    List.map (updateTeams newGroupRows) newMatches
            in
            { model
                | matches = newPlayOffMatches
                , groups = groupRowsAfterTieBreaks
            }


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
        in
        { groupRow | pld = newPld, w = newW, gf = newGf, ga = newGa, gd = newGd, pts = newPts }

    else if gf == ga then
        let
            newD =
                groupRow.d + 1

            newPts =
                3 * groupRow.w + newD
        in
        { groupRow | pld = newPld, d = newD, gf = newGf, ga = newGa, gd = newGd, pts = newPts }

    else
        let
            newL =
                groupRow.l + 1
        in
        { groupRow | pld = newPld, l = newL, gf = newGf, ga = newGa, gd = newGd }


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
            GroupRow (Team "Turkey" "") 0 0 0 0 0 0 0 0 GroupA 0 0 0



--permutations : List a -> Int


sameTeams : List Team -> Match -> Bool
sameTeams teams m =
    if List.member m.awayTeam teams then
        if List.member m.homeTeam teams then
            True

        else
            False

    else
        False


getPointsFromMatch : Team -> Match -> Int
getPointsFromMatch t m =
    case ( m.homeScore, m.awayScore ) of
        ( Just homeScore, Just awayScore ) ->
            if homeScore > awayScore then
                if m.homeTeam == t then
                    3

                else
                    0

            else if awayScore > homeScore then
                if m.awayTeam == t then
                    3

                else
                    0

            else
                1

        _ ->
            0


getGdFromMatch : Team -> Match -> Int
getGdFromMatch t m =
    case ( m.homeScore, m.awayScore ) of
        ( Just homeScore, Just awayScore ) ->
            if m.homeTeam == t then
                homeScore - awayScore

            else if m.awayTeam == t then
                awayScore - homeScore

            else
                0

        _ ->
            0


getGfFromMatch : Team -> Match -> Int
getGfFromMatch t m =
    case ( m.homeScore, m.awayScore ) of
        ( Just homeScore, Just awayScore ) ->
            if m.homeTeam == t then
                homeScore

            else if m.awayTeam == t then
                awayScore

            else
                0

        _ ->
            0


resolveTieBreak : List Match -> List GroupRow -> GroupRow -> GroupRow
resolveTieBreak matches allGroupRow gr =
    let
        groupRows =
            getGroupRows gr.group allGroupRow

        teamsWithSamePoints =
            groupRows
                |> List.filter (\x -> x.pts == gr.pts)
                |> List.map .team

        getGroupMatches =
            List.filter (\x -> x.group == gr.group) matches

        matchesBetweenTeamWithSamePoints =
            List.filter (sameTeams teamsWithSamePoints) getGroupMatches

        tieBreakPoints =
            List.sum (List.map (getPointsFromMatch gr.team) matchesBetweenTeamWithSamePoints)

        tieBreakGd =
            List.sum (List.map (getGdFromMatch gr.team) matchesBetweenTeamWithSamePoints)

        tieBreakGf =
            List.sum (List.map (getGfFromMatch gr.team) matchesBetweenTeamWithSamePoints)
    in
    { gr | tieBreakPoints = tieBreakPoints, tieBreakGd = tieBreakGd, tieBreakGf = tieBreakGf }


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
        |> List.sortBy getScore
        |> List.reverse


get3rdTeam : Team -> TeamPosition -> List GroupRow -> List GroupRow -> Team
get3rdTeam defaultTeam tp groupRows allGroupRows =
    let
        topFour =
            List.take 4 groupRows
    in
    if List.all playedAllGames allGroupRows then
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


groupToString : Group -> String
groupToString g =
    case g of
        GroupA ->
            "Group A"

        GroupB ->
            "Group B"

        GroupC ->
            "Group C"

        GroupD ->
            "Group D"

        GroupE ->
            "Group E"

        GroupF ->
            "Group F"

        RoundOf16 ->
            "Round Of 16"

        QuarterFinals ->
            "Quarter Finals"

        SemiFinals ->
            "Semi Finals"

        Final ->
            "Final"


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
                |> List.map (String.replace "Group " "")
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
    , selectedGroup : Group
    }


type HomeOrAway
    = Home
    | Away


init =
    { matches = matches
    , groups = groups
    , selectedGroup = GroupA
    }


type TeamPosition
    = B1
    | C1
    | E1
    | F1



-- VIEW


groupStageGroups =
    [ GroupA, GroupB, GroupC, GroupD, GroupE, GroupF ]


view : Model -> Html Msg
view model =
    Element.layout
        [ Font.color (rgba 0 0 0 1)
        , Font.size 20
        , Background.color grey
        , inFront <| header
        ]
    <|
        row
            [ width (fillPortion 1)
            ]
            [ column [ width (fillPortion 1) ] []
            , column [ width (fillPortion 2), centerX ]
                [ row [ padding 50 ] []
                , row [ width fill, spaceEvenly ] (List.map (viewGroupButton model.groups) groupStageGroups)
                , viewSpacer 16
                , viewGroupTitle model.selectedGroup
                , viewSpacer 16
                , viewGroup model.groups model.selectedGroup
                , viewSpacer 8
                , viewMatches model.matches model.selectedGroup
                , viewSpacer 16
                , viewGroupTitle RoundOf16
                , viewSpacer 8
                , viewPlayoffMatches model.matches RoundOf16
                ]
            , column [ width (fillPortion 1) ] []
            ]


header : Element Msg
header =
    row
        [ width fill
        , Background.color blue
        , paddingEach { edges | bottom = 24, top = 24 }
        , Font.color white
        ]
        [ el [ width (fillPortion 1), centerY ] (text "")
        , el [ width (fillPortion 2) ] (text "Hahimur!")
        , el [ width (fillPortion 1), centerY ] (text "")
        ]


viewSpacer : Int -> Element Msg
viewSpacer p =
    row [ padding p ] []


myNav : Element Msg
myNav =
    row
        [ Region.navigation
        , alignRight
        , width (fillPortion 1)
        ]
        [ text "navbar " ]


viewGroupButton : List GroupRow -> Group -> Element Msg
viewGroupButton allGroupRows group =
    let
        groupRows =
            getGroupRows group allGroupRows
    in
    Element.Input.button
        [ if getGroupState groupRows == FinishedDifferentScores then
            Background.color green

          else if getGroupState groupRows == FinishedSameScore then
            Background.color red

          else
            Background.color blue
        , Font.color white
        , padding 20
        ]
        { onPress = Just (ClickedGroup group)
        , label = text (groupToString group)
        }


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
            , el [ alignRight ] (viewMatchInput match.id Home match.homeScore match.homeTeam.flag)
            ]
        , row
            [ width fill ]
            [ image [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY, width (fillPortion 1) ] { src = match.awayTeam.flag, description = "" }
            , el [ alignLeft, paddingEach { edges | left = 10, top = 0 }, width (fillPortion 6) ] (text match.awayTeam.name)
            , el [ alignRight ] (viewMatchInput match.id Away match.awayScore match.awayTeam.flag)
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


viewGroupTitle : Group -> Element Msg
viewGroupTitle group =
    row
        [ centerX
        , paddingEach edges
        , Region.heading 3
        , Font.size 28
        ]
        [ text (groupToString group)
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
              , width = fillPortion 2
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


viewMatchInput : Int -> HomeOrAway -> Maybe Int -> String -> Element Msg
viewMatchInput matchId homeOrAway score flag =
    let
        disableSettings =
            if flag == defaultFlag then
                [ Element.htmlAttribute (Html.Attributes.disabled True)
                , Background.color grey
                ]

            else
                [ Element.htmlAttribute (Html.Attributes.type_ "number")
                , Background.color (rgba 1 1 1 0.8)
                ]
    in
    Element.Input.text
        ([ width (px 80)
         , Font.color (rgba 0 0 0 1)
         , paddingEach { edges | top = 12, bottom = 12, left = 12 }
         ]
            ++ disableSettings
        )
        { onChange = UpdatedScore matchId homeOrAway
        , text =
            case score of
                Nothing ->
                    ""

                Just value ->
                    String.fromInt value
        , placeholder = Nothing
        , label = Element.Input.labelHidden ""
        }
