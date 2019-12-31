module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Element.Region as Region
import Euro2020 exposing (Group(..), GroupRow, GroupState(..), HomeOrAway(..), Match, Team, TeamPosition, defaultFlag, getGroupRows, getGroupState, getScore, groupRows, groupToString, isPlayoffMatch, matches, playOffMatches, updateTeams)
import Html exposing (Html)
import Html.Attributes
import List.Extra


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
    = UpdatedGroupScore Int HomeOrAway String
    | UpdatedPlayoffScore Int HomeOrAway String
    | ClickedGroup Group
    | PickedWinner Int HomeOrAway


updateMatchScoreByID : Int -> HomeOrAway -> String -> Match -> Match
updateMatchScoreByID matchId homeOrAway score m =
    if m.id == matchId then
        case homeOrAway of
            Home ->
                updateMatchScore { m | homeScore = String.toInt score }

            Away ->
                updateMatchScore { m | awayScore = String.toInt score }

    else
        m


updateMatchScore : Match -> Match
updateMatchScore m =
    let
        homeScore =
            if m.homeTeam.flag == defaultFlag then
                Nothing

            else
                m.homeScore

        awayScore =
            if m.awayTeam.flag == defaultFlag then
                Nothing

            else
                m.awayScore
    in
    { m | homeScore = homeScore, awayScore = awayScore }



--getWinner : Team -> Int -> List Match -> Team
--getWinner team matchId ms =
--    let
--        one_matches =
--            filterByMatchId matchId ms
--
--        match =
--            List.Extra.getAt 0 one_matches
--    in
--    case match of
--        Just m ->
--            case ( m.homeScore, m.awayScore ) of
--                ( Just homeScore, Just awayScore ) ->
--                    m.homeTeam
--
--                _ ->
--                    m.awayTeam
--
--        Nothing ->
--            team
--
--
--updatePlayoffMatches : List Match -> Match -> Match
--updatePlayoffMatches ms m =
--    case m.id of
--        46 ->
--            { m
--                | homeTeam = getWinner (Team "Winner Match 39" defaultFlag) 39 ms
--                , awayTeam = getWinner (Team "Winner Match 37" defaultFlag) 37 ms
--            }
--
--        _ ->
--            m


updateWinner : Int -> HomeOrAway -> Match -> Match
updateWinner matchId homeOrAway m =
    if m.id == matchId then
        { m | winner = Just homeOrAway }

    else
        m


updateWinners : Match -> Match
updateWinners m =
    case ( m.homeScore, m.awayScore ) of
        ( Just homeScore, Just awayScore ) ->
            if homeScore > awayScore then
                { m | winner = Just Home }

            else if awayScore > homeScore then
                { m | winner = Just Away }

            else
                { m | winner = Nothing }

        ( _, Nothing ) ->
            { m | winner = Nothing }

        ( Nothing, _ ) ->
            { m | winner = Nothing }


update : Msg -> Model -> Model
update msg model =
    case msg of
        PickedWinner matchId homeOrAway ->
            let
                winnerWow =
                    List.map (updateWinner matchId homeOrAway) model.playOffMatches
            in
            { model | playOffMatches = winnerWow }

        ClickedGroup group ->
            { model | selectedGroup = group }

        UpdatedPlayoffScore matchId homeOrAway score ->
            let
                newPlayOffMatches =
                    List.map (updateMatchScoreByID matchId homeOrAway score) model.playOffMatches

                newPlayoffWinners =
                    List.map updateWinners newPlayOffMatches

                --newPlayOffMatches2 =
                --    List.map (updatePlayoffMatches model.playOffMatches) newPlayOffMatchesScores
            in
            { model
                | playOffMatches = newPlayoffWinners
            }

        UpdatedGroupScore matchId homeOrAway score ->
            let
                newMatches =
                    List.map (updateMatchScoreByID matchId homeOrAway score) model.matches

                newGroupRows =
                    newMatches
                        |> List.foldl updateGroup groupRows

                groupRowsAfterTieBreaks =
                    List.map (resolveTieBreak newMatches newGroupRows) newGroupRows

                thirdPlaces =
                    get3rdTeamTable groupRowsAfterTieBreaks

                newPlayOffMatches =
                    List.map (updateTeams groupRowsAfterTieBreaks thirdPlaces) model.playOffMatches

                newPlayOffMatchesScores =
                    List.map updateMatchScore newPlayOffMatches
            in
            { model
                | matches = newMatches
                , playOffMatches = newPlayOffMatchesScores
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
        thirdPlace =
            List.Extra.getAt 2 groupRows
    in
    case thirdPlace of
        Just gr ->
            gr

        Nothing ->
            GroupRow (Team "Turkey" "") 0 0 0 0 0 0 0 0 GroupA 0 0 0


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



-- MODEL


type alias Model =
    { matches : List Match
    , playOffMatches : List Match
    , groups : List GroupRow
    , selectedGroup : Group
    }


init =
    { matches = matches
    , groups = groupRows
    , playOffMatches = playOffMatches
    , selectedGroup = GroupA
    }



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
                , viewPlayoffMatches model.playOffMatches RoundOf16
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
viewGroupButton allGroupRows gr =
    let
        groupRows =
            getGroupRows gr allGroupRows
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
        { onPress = Just (ClickedGroup gr)
        , label = text (groupToString gr)
        }


viewMatch : Match -> Element Msg
viewMatch m =
    column
        [ Border.width 2
        , width (px 350)
        , Background.color (rgba 1 1 1 1)
        ]
        [ row
            [ width fill ]
            [ image [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY, width (fillPortion 1) ] { src = m.homeTeam.flag, description = "" }
            , el [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY, width (fillPortion 6) ] (text m.homeTeam.name)
            , el [ alignRight ] (viewMatchInput m Home)
            ]
        , row
            [ width fill ]
            [ image [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY, width (fillPortion 1) ] { src = m.awayTeam.flag, description = "" }
            , el [ alignLeft, paddingEach { edges | left = 10, top = 0 }, width (fillPortion 6) ] (text m.awayTeam.name)
            , el [ alignRight ] (viewMatchInput m Away)
            ]
        ]


getPlayoffText : Match -> HomeOrAway -> String
getPlayoffText m homeOrAway =
    case homeOrAway of
        Home ->
            if m.winner == Just Home then
                m.homeTeam.name ++ "   ✓"

            else
                m.homeTeam.name

        Away ->
            if m.winner == Just Away then
                m.awayTeam.name ++ "   ✓"

            else
                m.awayTeam.name


playoffWinnerButton : Match -> HomeOrAway -> Element Msg
playoffWinnerButton m homeOrAway =
    let
        labelText =
            getPlayoffText m homeOrAway
    in
    Element.Input.button
        [ width (fillPortion 6)
        , paddingEach { edges | left = 10, top = 0 }
        ]
        { onPress = Just (PickedWinner m.id homeOrAway)
        , label = text labelText
        }


viewPlayoffMatch : Match -> Element Msg
viewPlayoffMatch m =
    let
        draw =
            case ( m.homeScore, m.awayScore ) of
                ( Just a, Just b ) ->
                    if a == b then
                        True

                    else
                        False

                ( Nothing, _ ) ->
                    False

                ( _, Nothing ) ->
                    False

        homeTeamText =
            getPlayoffText m Home

        awayTeamText =
            getPlayoffText m Away
    in
    column
        [ Border.width 2
        , width (px 350)
        , Background.color (rgba 1 1 1 1)
        ]
        [ row
            [ width fill ]
            [ image [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY, width (fillPortion 1) ] { src = m.homeTeam.flag, description = m.homeTeam.name ++ " flag" }
            , if draw then
                playoffWinnerButton m Home

              else
                el [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY, width (fillPortion 6) ] (text homeTeamText)
            , el [ alignRight ] (viewMatchInput m Home)
            ]
        , row
            [ width fill ]
            [ image [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY, width (fillPortion 1) ] { src = m.awayTeam.flag, description = m.awayTeam.name ++ " flag" }
            , if draw then
                playoffWinnerButton m Away

              else
                el [ alignLeft, paddingEach { edges | left = 10, top = 0 }, width (fillPortion 6) ] (text awayTeamText)
            , el [ alignRight ] (viewMatchInput m Away)
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

        grouped =
            List.Extra.groupsOf 2 groupMatches
    in
    column [ centerX ]
        (List.map viewTwoMatches grouped)


viewPlayoffMatches : List Match -> Group -> Element Msg
viewPlayoffMatches matches group =
    let
        playOffMatches =
            List.filter (\m -> m.group == group) matches
    in
    column []
        (List.map viewPlayoffMatch playOffMatches)


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


viewMatchInput : Match -> HomeOrAway -> Element Msg
viewMatchInput match homeOrAway =
    let
        team =
            case homeOrAway of
                Home ->
                    match.homeTeam

                Away ->
                    match.awayTeam

        score =
            case homeOrAway of
                Home ->
                    match.homeScore

                Away ->
                    match.awayScore

        disableSettings =
            if team.flag == defaultFlag then
                [ Element.htmlAttribute (Html.Attributes.disabled True)
                , Background.color grey
                ]

            else
                [ Element.htmlAttribute (Html.Attributes.type_ "number")
                , Element.htmlAttribute (Html.Attributes.min "0")
                , Element.htmlAttribute (Html.Attributes.max "100")
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
        { onChange =
            if isPlayoffMatch match then
                UpdatedPlayoffScore match.id homeOrAway

            else
                UpdatedGroupScore match.id homeOrAway
        , text =
            case score of
                Nothing ->
                    ""

                Just value ->
                    String.fromInt value
        , placeholder = Nothing
        , label = Element.Input.labelHidden ""
        }
