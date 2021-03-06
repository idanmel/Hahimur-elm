module Main exposing (..)

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input
import Element.Region as Region
import Euro2020 exposing (Group, GroupRow, GroupState(..), HomeOrAway(..), Match, Team, TeamPosition, defaultFlag, encodeGroupRows, encodeMatches, filterByMatchId, final, getGroupRows, getGroupState, getScore, getScoreFor3rdPlace, groupA, groupB, groupC, groupD, groupE, groupF, groupRows, isPlayoffMatch, matches, playOffMatches, quarterFinals, roundOf16, semiFinals, thirdPlacesGroup, updateTeams)
import Html exposing (Html)
import Html.Attributes
import Http
import Json.Decode as JD
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import List.Extra
import Random



--import Random.Extra


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
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATE


type Msg
    = UpdatedGroupScore Int HomeOrAway String
    | UpdatedPlayoffScore Int HomeOrAway String
    | ClickedGroup Group
    | PickedWinner Int Bool
    | ClickedRandom
    | GotRandomScores (List Int)
    | GotRandomPlayoffScores (List Int)
    | UpdateToken String
    | UpdateTopScorer String
    | PredictionsSaved (Result Http.Error ())
    | ClickedSubmit
    | ClickedSubmitWithoutFinishingMatches
    | ClickedSubmitNoToken
    | ClickedSubmitNoScorer



--| GotMatches (Result Http.Error (List Match))


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


resetGame : Match -> Match
resetGame m =
    if isPlayoffMatch m then
        { m | homeScore = Nothing, awayScore = Nothing, homeWin = Nothing }

    else
        m


getWinner : Team -> Int -> List Match -> Team
getWinner team matchId ms =
    let
        one_matches =
            filterByMatchId matchId ms

        match =
            List.Extra.getAt 0 one_matches
    in
    case match of
        Just m ->
            case m.homeWin of
                Just True ->
                    m.homeTeam

                Just False ->
                    m.awayTeam

                Nothing ->
                    team

        Nothing ->
            team



--
--


updatePlayoffMatches : List Match -> Match -> Match
updatePlayoffMatches ms m =
    case m.id of
        45 ->
            { m
                | homeTeam = getWinner (Team "Winner Match 41" defaultFlag) 41 ms
                , awayTeam = getWinner (Team "Winner Match 42" defaultFlag) 42 ms
            }

        46 ->
            { m
                | homeTeam = getWinner (Team "Winner Match 39" defaultFlag) 39 ms
                , awayTeam = getWinner (Team "Winner Match 37" defaultFlag) 37 ms
            }

        47 ->
            { m
                | homeTeam = getWinner (Team "Winner Match 40" defaultFlag) 40 ms
                , awayTeam = getWinner (Team "Winner Match 38" defaultFlag) 38 ms
            }

        48 ->
            { m
                | homeTeam = getWinner (Team "Winner Match 43" defaultFlag) 43 ms
                , awayTeam = getWinner (Team "Winner Match 44" defaultFlag) 44 ms
            }

        49 ->
            { m
                | homeTeam = getWinner (Team "Winner Match 46" defaultFlag) 46 ms
                , awayTeam = getWinner (Team "Winner Match 45" defaultFlag) 45 ms
            }

        50 ->
            { m
                | homeTeam = getWinner (Team "Winner Match 48" defaultFlag) 48 ms
                , awayTeam = getWinner (Team "Winner Match 47" defaultFlag) 47 ms
            }

        51 ->
            { m
                | homeTeam = getWinner (Team "Winner Match 49" defaultFlag) 49 ms
                , awayTeam = getWinner (Team "Winner Match 50" defaultFlag) 50 ms
            }

        _ ->
            m


updateWinner : Int -> Bool -> Match -> Match
updateWinner matchId homeWin m =
    if m.id == matchId then
        { m | homeWin = Just homeWin }

    else
        m


updateWinnerByScore : Int -> Match -> Match
updateWinnerByScore matchId m =
    if m.id == matchId then
        case ( m.homeScore, m.awayScore ) of
            ( Just homeScore, Just awayScore ) ->
                if homeScore > awayScore then
                    { m | homeWin = Just True }

                else if awayScore > homeScore then
                    { m | homeWin = Just False }

                else
                    { m | homeWin = Nothing }

            ( _, Nothing ) ->
                { m | homeWin = Nothing }

            ( Nothing, _ ) ->
                { m | homeWin = Nothing }

    else
        m


randomScoresGen : Int -> Random.Generator (List Int)
randomScoresGen amount =
    let
        numberOfMatches =
            amount * 2
    in
    Random.list numberOfMatches (Random.int 0 3)


updateRandomScore : List Int -> Match -> Match
updateRandomScore scores m =
    let
        homeScore =
            List.head scores

        awayScore =
            List.Extra.last scores
    in
    { m | homeScore = homeScore, awayScore = awayScore }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        --GotMatches result ->
        --    case result of
        --        Ok matchesFromTheWire ->
        --            ( { model | matches = matchesFromTheWire, groups = getGroupRows matchesFromTheWire }, Cmd.none )
        --
        --        Err _ ->
        --            ( model, Cmd.none )
        ClickedSubmitNoScorer ->
            ( { model | message = "Submit Failed: Choose your top scorer" }, Cmd.none )

        ClickedSubmitNoToken ->
            ( { model | message = "Submit Failed: You need a token, yo!" }, Cmd.none )

        ClickedSubmitWithoutFinishingMatches ->
            ( { model | message = "Submit Failed: Finish filling all matches" }, Cmd.none )

        ClickedSubmit ->
            ( model, postPredictions model )

        PredictionsSaved (Ok _) ->
            ( { model | message = "You form was saved :-)", messageColor = green }, Cmd.none )

        PredictionsSaved (Err _) ->
            ( { model | message = "Failed!", messageColor = red }, Cmd.none )

        UpdateToken token ->
            ( { model | token = token }, Cmd.none )

        UpdateTopScorer topScorer ->
            ( { model | topScorer = topScorer }, Cmd.none )

        ClickedRandom ->
            ( model, Random.generate GotRandomScores (randomScoresGen 36) )

        GotRandomScores randomScores ->
            let
                randomScoresGrouped =
                    List.Extra.groupsOf 2 randomScores

                newMatches =
                    List.map2 updateRandomScore randomScoresGrouped model.matches

                newModel =
                    updateGroups newMatches model
            in
            ( updateGroups newMatches model, Random.generate GotRandomPlayoffScores (randomScoresGen 15) )

        GotRandomPlayoffScores randomScores ->
            ( model, Cmd.none )

        PickedWinner matchId homeOrAway ->
            let
                updatedWinners =
                    List.map (updateWinner matchId homeOrAway) model.playOffMatches

                newPlayOffMatches =
                    List.map (updatePlayoffMatches updatedWinners) updatedWinners
            in
            ( { model | playOffMatches = newPlayOffMatches }, Cmd.none )

        ClickedGroup group ->
            ( { model | selectedGroup = group }, Cmd.none )

        UpdatedPlayoffScore matchId homeOrAway score ->
            let
                newPlayOffMatches =
                    List.map (updateMatchScoreByID matchId homeOrAway score) model.playOffMatches
            in
            ( updatePlayoff newPlayOffMatches matchId model, Cmd.none )

        UpdatedGroupScore matchId homeOrAway score ->
            let
                newMatches =
                    List.map (updateMatchScoreByID matchId homeOrAway score) model.matches
            in
            ( updateGroups newMatches model, Cmd.none )


updatePlayoff : List Match -> Int -> Model -> Model
updatePlayoff newPlayOffMatches matchId model =
    let
        newPlayoffWinners =
            List.map (updateWinnerByScore matchId) newPlayOffMatches

        newPlayOffMatches2 =
            List.map (updatePlayoffMatches newPlayoffWinners) newPlayoffWinners

        newPlayOffMatches3 =
            List.map updatePlayoffScore newPlayOffMatches2

        newPlayOffMatches4 =
            List.map (updatePlayoffMatches newPlayOffMatches3) newPlayOffMatches3

        newPlayOffMatches5 =
            List.map updatePlayoffScore newPlayOffMatches4

        newPlayOffMatches6 =
            List.map (updatePlayoffMatches newPlayOffMatches5) newPlayOffMatches5

        newPlayOffMatches7 =
            List.map updatePlayoffScore newPlayOffMatches6
    in
    { model | playOffMatches = newPlayOffMatches7 }


updatePlayoffScore : Match -> Match
updatePlayoffScore m =
    if m.homeTeam.flag == defaultFlag || m.awayTeam.flag == defaultFlag then
        { m | homeScore = Nothing, awayScore = Nothing, homeWin = Nothing }

    else
        m


updateGroups : List Match -> Model -> Model
updateGroups matches model =
    let
        newGroupRows =
            matches
                |> List.foldl updateGroup groupRows

        groupRowsAfterTieBreaks =
            List.map (resolveTieBreak matches newGroupRows) newGroupRows

        thirdPlaces =
            get3rdTeamTable groupRowsAfterTieBreaks

        newPlayOffMatches =
            List.map (updateTeams groupRowsAfterTieBreaks thirdPlaces) playOffMatches

        newPlayOffMatchesScores =
            List.map resetGame newPlayOffMatches

        thirdTeamsMessage =
            get3rdTeamsMessage thirdPlaces
    in
    { model
        | matches = matches
        , playOffMatches = newPlayOffMatchesScores
        , groups = groupRowsAfterTieBreaks
        , thirdTeamsMessage = thirdTeamsMessage
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
            GroupRow (Team "Turkey" "") 0 0 0 0 0 0 0 0 groupA 0 0 0


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


get3rdTeamsMessage : List GroupRow -> String
get3rdTeamsMessage groupRows =
    let
        fourth =
            List.Extra.getAt 3 groupRows

        fifth =
            List.Extra.getAt 4 groupRows
    in
    case ( fourth, fifth ) of
        ( Just a, Just b ) ->
            case compare (getScore a) (getScore b) of
                EQ ->
                    "You have a tie in points, goal difference, goals scored, and wins. I'm choosing randomly between them."

                LT ->
                    ""

                GT ->
                    ""

        ( _, _ ) ->
            "Shouldn't happen"


get3rdTeamTable : List GroupRow -> List GroupRow
get3rdTeamTable groupRows =
    let
        thirdPlaceA =
            groupRows
                |> getGroupRows groupA
                |> get3rdPlaceTeam

        thirdPlaceB =
            groupRows
                |> getGroupRows groupB
                |> get3rdPlaceTeam

        thirdPlaceC =
            groupRows
                |> getGroupRows groupC
                |> get3rdPlaceTeam

        thirdPlaceD =
            groupRows
                |> getGroupRows groupD
                |> get3rdPlaceTeam

        thirdPlaceE =
            groupRows
                |> getGroupRows groupE
                |> get3rdPlaceTeam

        thirdPlaceF =
            groupRows
                |> getGroupRows groupF
                |> get3rdPlaceTeam
    in
    [ thirdPlaceA, thirdPlaceB, thirdPlaceC, thirdPlaceD, thirdPlaceE, thirdPlaceF ]
        |> List.sortBy getScoreFor3rdPlace
        |> List.reverse


playoffMatchHasWinner : Match -> Bool
playoffMatchHasWinner m =
    case m.homeWin of
        Just _ ->
            True

        Nothing ->
            False


allPlayoffMatchesHaveAWinner : List Match -> Bool
allPlayoffMatchesHaveAWinner ms =
    List.length (List.filter playoffMatchHasWinner ms) == List.length ms



-- MODEL


type alias Model =
    { matches : List Match
    , playOffMatches : List Match
    , groups : List GroupRow
    , selectedGroup : Group
    , token : String
    , topScorer : String
    , message : String
    , messageColor : Color
    , thirdTeamsMessage : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { matches = matches
      , groups = groupRows
      , playOffMatches = playOffMatches
      , selectedGroup = groupA
      , token = ""
      , topScorer = ""
      , message = ""
      , messageColor = red
      , thirdTeamsMessage = ""
      }
    , Cmd.none
    )



-- VIEW


groupStageGroups =
    [ groupA, groupB, groupC, groupD, groupE, groupF ]


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
                , viewGroupTitle thirdPlacesGroup
                , viewSpacer 8
                , view3rdTeamTable (get3rdTeamTable model.groups)
                , row [ Font.color red ] [ text model.thirdTeamsMessage ]
                , viewSpacer 16
                , row [ width fill, spaceEvenly ]
                    [ viewGroupTitle roundOf16
                    , viewGroupTitle quarterFinals
                    , viewGroupTitle semiFinals
                    , viewGroupTitle final
                    ]
                , viewSpacer 16
                , row [] (List.map (viewPlayoffMatches model.playOffMatches) [ roundOf16, quarterFinals, semiFinals, final ])
                , viewSpacer 16
                , row [] [ viewTopScorerInput model.topScorer ]
                , viewSpacer 16
                , row [] [ viewTokenInput model.token ]
                , viewSpacer 16
                , row [ Font.color model.messageColor ] [ text model.message ]
                , viewSpacer 4
                , row [] [ viewSubmitButton model ]
                , viewSpacer 16

                --, row [] [ viewRandomButton model.groups QuarterFinals ]
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
        , label = text gr.name
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


getPlayoffText : Match -> Bool -> String
getPlayoffText m homeWin =
    case homeWin of
        True ->
            if m.homeWin == Just True then
                m.homeTeam.name ++ "   ✓"

            else
                m.homeTeam.name

        False ->
            if m.homeWin == Just False then
                m.awayTeam.name ++ "   ✓"

            else
                m.awayTeam.name


playoffWinnerButton : Match -> Bool -> Element Msg
playoffWinnerButton m homeWin =
    let
        labelText =
            getPlayoffText m homeWin
    in
    Element.Input.button
        [ width (fillPortion 6)
        , paddingEach { edges | left = 10, top = 0 }
        ]
        { onPress = Just (PickedWinner m.id homeWin)
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
            getPlayoffText m True

        awayTeamText =
            getPlayoffText m False
    in
    column
        [ Border.width 2
        , width (px 250)
        , Background.color (rgba 1 1 1 1)
        , Font.size 12
        ]
        [ row
            [ width fill ]
            [ image [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY, width (fillPortion 1) ] { src = m.homeTeam.flag, description = m.homeTeam.name ++ " flag" }
            , if draw then
                playoffWinnerButton m True

              else
                el [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY, width (fillPortion 6) ] (text homeTeamText)
            , el [ alignRight ] (viewMatchInput m Home)
            ]
        , row
            [ width fill ]
            [ image [ alignLeft, paddingEach { edges | left = 10, top = 0 }, centerY, width (fillPortion 1) ] { src = m.awayTeam.flag, description = m.awayTeam.name ++ " flag" }
            , if draw then
                playoffWinnerButton m False

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
    column [ spacingXY 0 5 ]
        (List.map viewPlayoffMatch playOffMatches)


viewGroupTitle : Group -> Element Msg
viewGroupTitle group =
    row
        [ paddingEach edges
        , Region.heading 3
        , Font.size 28
        ]
        [ text group.name
        ]


view3rdTeamTable : List GroupRow -> Element Msg
view3rdTeamTable groupRows =
    Element.indexedTable
        [ paddingEach { edges | bottom = 16 }
        , spacing 8
        ]
        { data = groupRows
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


viewRandomButton : List GroupRow -> Group -> Element Msg
viewRandomButton allGroupRows gr =
    let
        groupRows =
            getGroupRows gr allGroupRows
    in
    Element.Input.button
        [ Background.color blue
        , Font.color white
        , padding 20
        ]
        { onPress = Just ClickedRandom
        , label = text "Random"
        }


viewTokenInput : String -> Element Msg
viewTokenInput token =
    Element.Input.text
        []
        { onChange = UpdateToken
        , placeholder = Nothing
        , label = Element.Input.labelAbove [] (text "Token")
        , text = token
        }


viewTopScorerInput : String -> Element Msg
viewTopScorerInput topScorer =
    Element.Input.text
        []
        { onChange = UpdateTopScorer
        , placeholder = Nothing
        , label = Element.Input.labelAbove [] (text "Top Scorer")
        , text = topScorer
        }


encodeMatchPredictions : Model -> Encode.Value
encodeMatchPredictions model =
    Encode.object
        [ ( "group_matches", encodeMatches model.matches )
        , ( "3rd_places", encodeGroupRows (get3rdTeamTable model.groups) )
        , ( "knockout_matches", encodeMatches model.playOffMatches )
        , ( "top_scorer", Encode.string model.topScorer )
        ]


postPredictions : Model -> Cmd Msg
postPredictions model =
    Http.post
        --{ url = "http://localhost:8000/predictions?token=" ++ model.token
        { url = "https://hahimur-django.herokuapp.com/predictions?token=" ++ model.token
        , body = Http.jsonBody (encodeMatchPredictions model)
        , expect = Http.expectWhatever PredictionsSaved
        }


viewSubmitButton : Model -> Element Msg
viewSubmitButton model =
    let
        msg =
            if model.token == "" then
                ClickedSubmitNoToken

            else if model.topScorer == "" then
                ClickedSubmitNoScorer

            else if not (allPlayoffMatchesHaveAWinner model.playOffMatches) then
                ClickedSubmitWithoutFinishingMatches

            else
                ClickedSubmit
    in
    Element.Input.button
        [ Background.color blue
        , Font.color white
        , padding 20
        ]
        { onPress = Just msg
        , label = text "Submit"
        }


viewLoadButton : Model -> Element Msg
viewLoadButton model =
    let
        msg =
            if model.token == "" then
                ClickedSubmitNoToken

            else if model.topScorer == "" then
                ClickedSubmitNoScorer

            else if not (allPlayoffMatchesHaveAWinner model.playOffMatches) then
                ClickedSubmitWithoutFinishingMatches

            else
                ClickedSubmit
    in
    Element.Input.button
        [ Background.color blue
        , Font.color white
        , padding 20
        ]
        { onPress = Just msg
        , label = text "Submit"
        }



--matchIdDecoder : JD.Decoder Int
--matchIdDecoder =
--    JD.field "match_id" JD.int


teamNameDecoder : JD.Decoder String
teamNameDecoder =
    JD.field "name" JD.string


teamflagDecoder : JD.Decoder String
teamflagDecoder =
    JD.field "flag" JD.string


teamDecoder : JD.Decoder Team
teamDecoder =
    JD.map2 Team
        teamNameDecoder
        teamflagDecoder


groupNameDecoder : JD.Decoder String
groupNameDecoder =
    JD.field "name" JD.string


groupDecoder : JD.Decoder Group
groupDecoder =
    JD.map Group groupNameDecoder


matchDecoder : JD.Decoder Match
matchDecoder =
    JD.succeed Match
        |> required "match_id" JD.int
        |> required "home_team" teamDecoder
        |> required "home_score" (JD.maybe JD.int)
        |> required "away_team" teamDecoder
        |> required "away_score" (JD.maybe JD.int)
        |> required "group" groupDecoder
        |> required "date" JD.string
        |> required "time" JD.string
        |> required "home_win" (JD.maybe JD.bool)


matchesInfoDecoder : JD.Decoder (List Match)
matchesInfoDecoder =
    JD.field "euro2020" (JD.field "matches_info" (JD.list matchDecoder))



--getMatchesInfo : Cmd Msg
--getMatchesInfo =
--    Http.get
--        { url = "https://hahimur-django.herokuapp.com/matches"
--        , expect = Http.expectJson GotMatches matchesInfoDecoder
--        }
