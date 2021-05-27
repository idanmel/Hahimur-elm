module Euro2020 exposing (Group, GroupRow, GroupState(..), HomeOrAway(..), Match, Team, TeamPosition, defaultFlag, encodeGroupRows, encodeMatches, filterByMatchId, final, getGroupRows, getGroupState, getScore, groupA, groupB, groupC, groupD, groupE, groupF, groupRows, isPlayoffMatch, matches, maybeOrDefaultTeam, playOffMatches, quarterFinals, roundOf16, semiFinals, thirdPlacesGroup, updateTeams)

import Array
import Json.Encode as Encode
import Set


type alias Team =
    { name : String
    , flag : String
    }


type HomeOrAway
    = Home
    | Away


type alias Match =
    { id : Int
    , homeTeam : Team
    , homeScore : Maybe Int
    , awayTeam : Team
    , awayScore : Maybe Int
    , group : Group
    , date : String
    , time : String
    , homeWin : Maybe Bool
    }


encodeMatchScore : Maybe Int -> Encode.Value
encodeMatchScore matchScore =
    case matchScore of
        Just int ->
            Encode.int int

        Nothing ->
            Encode.null


encodeHomeWin : Maybe Bool -> Encode.Value
encodeHomeWin homeWin =
    case homeWin of
        Just bool ->
            Encode.bool bool

        Nothing ->
            Encode.null


encodeMatch : Match -> Encode.Value
encodeMatch match =
    Encode.object
        [ ( "match_number", Encode.int match.id )
        , ( "home_score", encodeMatchScore match.homeScore )
        , ( "home_team_name", Encode.string match.homeTeam.name )
        , ( "away_score", encodeMatchScore match.awayScore )
        , ( "away_team_name", Encode.string match.awayTeam.name )
        , ( "home_win", encodeHomeWin match.homeWin )
        ]


encodeMatches : List Match -> Encode.Value
encodeMatches koMatches =
    Encode.list encodeMatch koMatches


type alias Group =
    { name : String
    }


groupA =
    Group "Group A"


groupB =
    Group "Group B"


groupC =
    Group "Group C"


groupD =
    Group "Group D"


groupE =
    Group "Group E"


groupF =
    Group "Group F"


thirdPlacesGroup =
    Group "Third Places"


roundOf16 =
    Group "Round of 16"


quarterFinals =
    Group "Quarter Finals"


semiFinals =
    Group "Semi Finals"


final =
    Group "Final"


type GroupState
    = NotFinished
    | FinishedSameScore
    | FinishedDifferentScores


type TeamPosition
    = B1
    | C1
    | E1
    | F1


type alias GroupRow =
    { team : Team
    , pld : Int
    , w : Int
    , d : Int
    , l : Int
    , gf : Int
    , ga : Int
    , gd : Int
    , pts : Int
    , group : Group
    , tieBreakPoints : Int
    , tieBreakGd : Int
    , tieBreakGf : Int
    }


encodeGroupRow : GroupRow -> Encode.Value
encodeGroupRow gr =
    Encode.object
        [ ( "team_name", Encode.string gr.team.name )
        , ( "pld", Encode.int gr.pld )
        , ( "w", Encode.int gr.w )
        , ( "d", Encode.int gr.d )
        , ( "l", Encode.int gr.l )
        , ( "gf", Encode.int gr.gf )
        , ( "ga", Encode.int gr.ga )
        , ( "gd", Encode.int gr.gd )
        , ( "pts", Encode.int gr.pts )
        , ( "group", Encode.string gr.group.name )
        , ( "tieBreakPoints", Encode.int gr.tieBreakPoints )
        , ( "tieBreakGd", Encode.int gr.tieBreakGd )
        , ( "tieBreakGf", Encode.int gr.tieBreakGf )
        ]


encodeGroupRows : List GroupRow -> Encode.Value
encodeGroupRows grs =
    Encode.list encodeGroupRow grs


defaultFlag =
    "https://upload.wikimedia.org/wikipedia/en/9/96/UEFA_Euro_2020_Logo.svg"



-- Teams


turkey =
    Team "Turkey" "https://flagcdn.com/w80/tr.png"


italy =
    Team "Italy" "https://flagcdn.com/w80/it.png"


wales =
    Team "Wales" "https://flagcdn.com/w80/gb-wls.png"


switzerland =
    Team "Switzerland" "https://flagcdn.com/w80/ch.png"


belgium =
    Team "Belgium" "https://flagcdn.com/w80/be.png"


russia =
    Team "Russia" "https://flagcdn.com/w80/ru.png"


finland =
    Team "Finland" "https://flagcdn.com/w80/fi.png"


denmark =
    Team "Denmark" "https://flagcdn.com/w80/dk.png"


netherlands =
    Team "Netherlands" "https://flagcdn.com/w80/nl.png"


ukraine =
    Team "Ukraine" "https://flagcdn.com/w80/ua.png"


austria =
    Team "Austria" "https://flagcdn.com/w80/at.png"


northMacedonia =
    Team "North Macedonia" "https://flagcdn.com/w80/mk.png"


england =
    Team "England" "https://flagcdn.com/w80/gb-eng.png"


crotia =
    Team "Croatia" "https://flagcdn.com/w80/hr.png"


scotland =
    Team "Scotland" "https://flagcdn.com/w80/gb-sct.png"


czech =
    Team "Czech Republic" "https://flagcdn.com/w80/cz.png"


spain =
    Team "Spain" "https://flagcdn.com/w80/es.png"


sweden =
    Team "Sweden" "https://flagcdn.com/w80/se.png"


poland =
    Team "Poland" "https://flagcdn.com/w80/pl.png"


slovakia =
    Team "Slovakia" "https://flagcdn.com/w80/sk.png"


hungary =
    Team "Hungary" "https://flagcdn.com/w80/hu.png"


portugal =
    Team "Portugal" "https://flagcdn.com/w80/pt.png"


france =
    Team "France" "https://flagcdn.com/w80/fr.png"


germany =
    Team "Germany" "https://flagcdn.com/w80/de.png"



-- groupRows


turkeyRow =
    GroupRow turkey 0 0 0 0 0 0 0 0 groupA 0 0 0


italyRow =
    GroupRow italy 0 0 0 0 0 0 0 0 groupA 0 0 0


walesRow =
    GroupRow wales 0 0 0 0 0 0 0 0 groupA 0 0 0


switzerlandRow =
    GroupRow switzerland 0 0 0 0 0 0 0 0 groupA 0 0 0


belgiumRow =
    GroupRow belgium 0 0 0 0 0 0 0 0 groupB 0 0 0


russiaRow =
    GroupRow russia 0 0 0 0 0 0 0 0 groupB 0 0 0


finlandRow =
    GroupRow finland 0 0 0 0 0 0 0 0 groupB 0 0 0


denmarkRow =
    GroupRow denmark 0 0 0 0 0 0 0 0 groupB 0 0 0


netherlandsRow =
    GroupRow netherlands 0 0 0 0 0 0 0 0 groupC 0 0 0


ukraineRow =
    GroupRow ukraine 0 0 0 0 0 0 0 0 groupC 0 0 0


austriaRow =
    GroupRow austria 0 0 0 0 0 0 0 0 groupC 0 0 0


northMacedoniaRow =
    GroupRow northMacedonia 0 0 0 0 0 0 0 0 groupC 0 0 0


englandRow =
    GroupRow england 0 0 0 0 0 0 0 0 groupD 0 0 0


croatiaRow =
    GroupRow crotia 0 0 0 0 0 0 0 0 groupD 0 0 0


scotlandRow =
    GroupRow scotland 0 0 0 0 0 0 0 0 groupD 0 0 0


czechRow =
    GroupRow czech 0 0 0 0 0 0 0 0 groupD 0 0 0


spainRow =
    GroupRow spain 0 0 0 0 0 0 0 0 groupE 0 0 0


swedenRow =
    GroupRow sweden 0 0 0 0 0 0 0 0 groupE 0 0 0


polandRow =
    GroupRow poland 0 0 0 0 0 0 0 0 groupE 0 0 0


slovakiaRow =
    GroupRow slovakia 0 0 0 0 0 0 0 0 groupE 0 0 0


hungaryRow =
    GroupRow hungary 0 0 0 0 0 0 0 0 groupF 0 0 0


portugalRow =
    GroupRow portugal 0 0 0 0 0 0 0 0 groupF 0 0 0


franceRow =
    GroupRow france 0 0 0 0 0 0 0 0 groupF 0 0 0


germanyRow =
    GroupRow germany 0 0 0 0 0 0 0 0 groupF 0 0 0


groupATeams =
    [ turkeyRow, italyRow, walesRow, switzerlandRow ]


groupBTeams =
    [ belgiumRow, russiaRow, finlandRow, denmarkRow ]


groupCTeams =
    [ netherlandsRow, ukraineRow, austriaRow, northMacedoniaRow ]


groupDTeams =
    [ englandRow, croatiaRow, scotlandRow, czechRow ]


groupETeams =
    [ spainRow, swedenRow, polandRow, slovakiaRow ]


groupFTeams =
    [ hungaryRow, portugalRow, franceRow, germanyRow ]


groupRows =
    groupATeams ++ groupBTeams ++ groupCTeams ++ groupDTeams ++ groupETeams ++ groupFTeams



-- Matches


matchesGroupA =
    [ Match 1 turkey Nothing italy Nothing groupA "16 June 2020" "18:00" Nothing
    , Match 2 wales Nothing switzerland Nothing groupA "16 June 2020" "18:00" Nothing
    , Match 13 turkey Nothing wales Nothing groupA "16 June 2020" "18:00" Nothing
    , Match 14 italy Nothing switzerland Nothing groupA "16 June 2020" "18:00" Nothing
    , Match 25 switzerland Nothing turkey Nothing groupA "16 June 2020" "18:00" Nothing
    , Match 26 italy Nothing wales Nothing groupA "16 June 2020" "18:00" Nothing
    ]


matchesGroupB =
    [ Match 3 denmark Nothing finland Nothing groupB "16 June 2020" "18:00" Nothing
    , Match 4 belgium Nothing russia Nothing groupB "16 June 2020" "18:00" Nothing
    , Match 15 finland Nothing russia Nothing groupB "16 June 2020" "18:00" Nothing
    , Match 16 denmark Nothing belgium Nothing groupB "16 June 2020" "18:00" Nothing
    , Match 27 russia Nothing denmark Nothing groupB "16 June 2020" "18:00" Nothing
    , Match 28 finland Nothing belgium Nothing groupB "16 June 2020" "18:00" Nothing
    ]


matchesGroupC =
    [ Match 6 austria Nothing northMacedonia Nothing groupC "16 June 2020" "18:00" Nothing
    , Match 5 netherlands Nothing ukraine Nothing groupC "16 June 2020" "18:00" Nothing
    , Match 18 ukraine Nothing northMacedonia Nothing groupC "16 June 2020" "18:00" Nothing
    , Match 17 netherlands Nothing austria Nothing groupC "16 June 2020" "18:00" Nothing
    , Match 29 northMacedonia Nothing netherlands Nothing groupC "16 June 2020" "18:00" Nothing
    , Match 30 ukraine Nothing austria Nothing groupC "16 June 2020" "18:00" Nothing
    ]


matchesGroupD =
    [ Match 7 england Nothing crotia Nothing groupD "16 June 2020" "18:00" Nothing
    , Match 8 scotland Nothing czech Nothing groupD "16 June 2020" "18:00" Nothing
    , Match 19 crotia Nothing czech Nothing groupD "16 June 2020" "18:00" Nothing
    , Match 20 england Nothing scotland Nothing groupD "16 June 2020" "18:00" Nothing
    , Match 31 crotia Nothing scotland Nothing groupD "16 June 2020" "18:00" Nothing
    , Match 32 czech Nothing england Nothing groupD "16 June 2020" "18:00" Nothing
    ]


matchesGroupE =
    [ Match 10 poland Nothing slovakia Nothing groupE "16 June 2020" "18:00" Nothing
    , Match 9 spain Nothing sweden Nothing groupE "16 June 2020" "18:00" Nothing
    , Match 21 sweden Nothing slovakia Nothing groupE "16 June 2020" "18:00" Nothing
    , Match 22 spain Nothing poland Nothing groupE "16 June 2020" "18:00" Nothing
    , Match 33 slovakia Nothing spain Nothing groupE "16 June 2020" "18:00" Nothing
    , Match 34 sweden Nothing poland Nothing groupE "16 June 2020" "18:00" Nothing
    ]


matchesGroupF =
    [ Match 11 hungary Nothing portugal Nothing groupF "16 June 2020" "18:00" Nothing
    , Match 12 france Nothing germany Nothing groupF "16 June 2020" "18:00" Nothing
    , Match 23 hungary Nothing france Nothing groupF "16 June 2020" "18:00" Nothing
    , Match 24 portugal Nothing germany Nothing groupF "16 June 2020" "18:00" Nothing
    , Match 35 portugal Nothing france Nothing groupF "16 June 2020" "18:00" Nothing
    , Match 36 germany Nothing hungary Nothing groupF "16 June 2020" "18:00" Nothing
    ]


playOffMatches =
    [ Match 39 (Team "Winner Group B" defaultFlag) Nothing (Team "3rd Group A/D/E/F" defaultFlag) Nothing roundOf16 "16 June 2020" "18:00" Nothing
    , Match 37 (Team "Winner Group A" defaultFlag) Nothing (Team "Runner-up Group C" defaultFlag) Nothing roundOf16 "16 June 2020" "18:00" Nothing
    , Match 41 (Team "Winner Group F" defaultFlag) Nothing (Team "3rd Group A/B/C" defaultFlag) Nothing roundOf16 "16 June 2020" "18:00" Nothing
    , Match 42 (Team "Runner-up Group D" defaultFlag) Nothing (Team "Runner-up Group E" defaultFlag) Nothing roundOf16 "16 June 2020" "18:00" Nothing
    , Match 43 (Team "Winner Group E" defaultFlag) Nothing (Team "3rd Group A/B/C/D" defaultFlag) Nothing roundOf16 "16 June 2020" "18:00" Nothing
    , Match 44 (Team "Winner Group D" defaultFlag) Nothing (Team "Runner-up Group F" defaultFlag) Nothing roundOf16 "16 June 2020" "18:00" Nothing
    , Match 40 (Team "Winner Group C" defaultFlag) Nothing (Team "3rd Group D/E/F" defaultFlag) Nothing roundOf16 "16 June 2020" "18:00" Nothing
    , Match 38 (Team "Runner-up Group A" defaultFlag) Nothing (Team "Runner-up Group B" defaultFlag) Nothing roundOf16 "16 June 2020" "18:00" Nothing
    , Match 46 (Team "Winner Match 39" defaultFlag) Nothing (Team "Winner Match 37" defaultFlag) Nothing quarterFinals "16 June 2020" "18:00" Nothing
    , Match 45 (Team "Winner Match 41" defaultFlag) Nothing (Team "Winner Match 42" defaultFlag) Nothing quarterFinals "16 June 2020" "18:00" Nothing
    , Match 48 (Team "Winner Match 43" defaultFlag) Nothing (Team "Winner Match 44" defaultFlag) Nothing quarterFinals "16 June 2020" "18:00" Nothing
    , Match 47 (Team "Winner Match 40" defaultFlag) Nothing (Team "Winner Match 38" defaultFlag) Nothing quarterFinals "16 June 2020" "18:00" Nothing
    , Match 49 (Team "Winner Match 46" defaultFlag) Nothing (Team "Winner Match 45" defaultFlag) Nothing semiFinals "16 June 2020" "18:00" Nothing
    , Match 50 (Team "Winner Match 48" defaultFlag) Nothing (Team "Winner Match 47" defaultFlag) Nothing semiFinals "16 June 2020" "18:00" Nothing
    , Match 51 (Team "Winner Match 49" defaultFlag) Nothing (Team "Winner Match 50" defaultFlag) Nothing final "16 June 2020" "18:00" Nothing
    ]


matches =
    matchesGroupA ++ matchesGroupB ++ matchesGroupC ++ matchesGroupD ++ matchesGroupE ++ matchesGroupF


filterByMatchId : Int -> List Match -> List Match
filterByMatchId matchId ms =
    List.filter (\m -> m.id == matchId) ms


filterByGroup : Group -> List GroupRow -> List GroupRow
filterByGroup group grs =
    List.filter (\gr -> gr.group.name == group.name) grs


getScore : GroupRow -> List Int
getScore gr =
    [ gr.pts
    , gr.tieBreakPoints
    , gr.tieBreakGd
    , gr.tieBreakGf
    , gr.gd
    , gr.gf
    , gr.w
    ]


getScoreFor3rdPlace : GroupRow -> List Int
getScoreFor3rdPlace gr =
    [ gr.pts
    , gr.gd
    , gr.gf
    , gr.w
    ]


getGroupRows : Group -> List GroupRow -> List GroupRow
getGroupRows groupName grs =
    grs
        |> filterByGroup groupName
        |> List.sortBy getScoreFor3rdPlace
        |> List.reverse


playedAllGames : GroupRow -> Bool
playedAllGames gr =
    gr.pld == 3


getGroupState : List GroupRow -> GroupState
getGroupState grs =
    let
        allGamesCompleted =
            List.all playedAllGames grs

        scores =
            List.map getScore grs

        allGamesHaveDifferentScores =
            Set.size (Set.fromList scores) == 4
    in
    if not allGamesCompleted then
        NotFinished

    else if not allGamesHaveDifferentScores then
        FinishedSameScore

    else
        FinishedDifferentScores


getTeamPlaying : Team -> Group -> Int -> List GroupRow -> Team
getTeamPlaying placeHolderTeam group pos allGroupRows =
    let
        grs =
            getGroupRows group allGroupRows

        groupState =
            getGroupState grs
    in
    if groupState == FinishedDifferentScores then
        maybeOrDefaultTeam placeHolderTeam (Array.get (pos - 1) (Array.fromList grs))

    else
        placeHolderTeam


maybeOrDefaultTeam : Team -> Maybe GroupRow -> Team
maybeOrDefaultTeam defaultTeam maybeGroupRow =
    case maybeGroupRow of
        Just gr ->
            gr.team

        Nothing ->
            defaultTeam


isPlayoffMatch : Match -> Bool
isPlayoffMatch match =
    List.member match.id [ 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51 ]


updateTeams : List GroupRow -> List GroupRow -> Match -> Match
updateTeams grs thirdPlaces m =
    case m.id of
        37 ->
            { m
                | homeTeam = getTeamPlaying (Team "Winner Group A" defaultFlag) groupA 1 grs
                , awayTeam = getTeamPlaying (Team "Runner-up Group C" defaultFlag) groupC 2 grs
            }

        38 ->
            { m
                | homeTeam = getTeamPlaying (Team "Runner-up Group A" defaultFlag) groupA 2 grs
                , awayTeam = getTeamPlaying (Team "Runner-up Group B" defaultFlag) groupB 2 grs
            }

        39 ->
            { m
                | homeTeam = getTeamPlaying (Team "Winner Group B" defaultFlag) groupB 1 grs
                , awayTeam = get3rdTeam (Team "3rd Group A/D/E/F" defaultFlag) B1 thirdPlaces grs
            }

        40 ->
            { m
                | homeTeam = getTeamPlaying (Team "Winner Group C" defaultFlag) groupC 1 grs
                , awayTeam = get3rdTeam (Team "3rd Group D/E/F" defaultFlag) C1 thirdPlaces grs
            }

        41 ->
            { m
                | homeTeam = getTeamPlaying (Team "Winner Group F" defaultFlag) groupF 1 grs
                , awayTeam = get3rdTeam (Team "3rd Group A/B/C" defaultFlag) F1 thirdPlaces grs
            }

        42 ->
            { m
                | homeTeam = getTeamPlaying (Team "Runner-up Group D" defaultFlag) groupD 2 grs
                , awayTeam = getTeamPlaying (Team "Runner-up Group E" defaultFlag) groupE 2 grs
            }

        43 ->
            { m
                | homeTeam = getTeamPlaying (Team "Winner Group E" defaultFlag) groupE 1 grs
                , awayTeam = get3rdTeam (Team "3rd Group A/B/C/D" defaultFlag) E1 thirdPlaces grs
            }

        44 ->
            { m
                | homeTeam = getTeamPlaying (Team "Winner Group D" defaultFlag) groupD 1 grs
                , awayTeam = getTeamPlaying (Team "Runner-up Group F" defaultFlag) groupF 2 grs
            }

        _ ->
            m


get3rdTeam : Team -> TeamPosition -> List GroupRow -> List GroupRow -> Team
get3rdTeam defaultTeam tp grs allGroupRows =
    let
        topFour =
            List.take 4 grs
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


applyCrazyUefaLogic : List GroupRow -> TeamPosition -> Team
applyCrazyUefaLogic top4groupRows tp =
    let
        sortedStringGroups =
            top4groupRows
                |> List.map .group
                |> List.map .name
                |> List.map (String.replace "Group " "")
                |> List.sort
                |> String.concat
    in
    case tp of
        B1 ->
            if sortedStringGroups == "ABCD" then
                getRoundOf16Team groupA top4groupRows

            else if sortedStringGroups == "ABCE" then
                getRoundOf16Team groupA top4groupRows

            else if sortedStringGroups == "ABCF" then
                getRoundOf16Team groupA top4groupRows

            else if sortedStringGroups == "ABDE" then
                getRoundOf16Team groupD top4groupRows

            else if sortedStringGroups == "ABDF" then
                getRoundOf16Team groupD top4groupRows

            else if sortedStringGroups == "ABEF" then
                getRoundOf16Team groupE top4groupRows

            else if sortedStringGroups == "ACDE" then
                getRoundOf16Team groupE top4groupRows

            else if sortedStringGroups == "ACDF" then
                getRoundOf16Team groupF top4groupRows

            else if sortedStringGroups == "ACEF" then
                getRoundOf16Team groupE top4groupRows

            else if sortedStringGroups == "ADEF" then
                getRoundOf16Team groupE top4groupRows

            else if sortedStringGroups == "BCDE" then
                getRoundOf16Team groupE top4groupRows

            else if sortedStringGroups == "BCDF" then
                getRoundOf16Team groupF top4groupRows

            else if sortedStringGroups == "BCEF" then
                getRoundOf16Team groupF top4groupRows

            else if sortedStringGroups == "BDEF" then
                getRoundOf16Team groupF top4groupRows

            else
                getRoundOf16Team groupF top4groupRows

        C1 ->
            if sortedStringGroups == "ABCD" then
                getRoundOf16Team groupD top4groupRows

            else if sortedStringGroups == "ABCE" then
                getRoundOf16Team groupE top4groupRows

            else if sortedStringGroups == "ABCF" then
                getRoundOf16Team groupF top4groupRows

            else if sortedStringGroups == "ABDE" then
                getRoundOf16Team groupE top4groupRows

            else if sortedStringGroups == "ABDF" then
                getRoundOf16Team groupF top4groupRows

            else if sortedStringGroups == "ABEF" then
                getRoundOf16Team groupF top4groupRows

            else if sortedStringGroups == "ACDE" then
                getRoundOf16Team groupD top4groupRows

            else if sortedStringGroups == "ACDF" then
                getRoundOf16Team groupD top4groupRows

            else if sortedStringGroups == "ACEF" then
                getRoundOf16Team groupF top4groupRows

            else if sortedStringGroups == "ADEF" then
                getRoundOf16Team groupF top4groupRows

            else if sortedStringGroups == "BCDE" then
                getRoundOf16Team groupD top4groupRows

            else if sortedStringGroups == "BCDF" then
                getRoundOf16Team groupD top4groupRows

            else if sortedStringGroups == "BCEF" then
                getRoundOf16Team groupE top4groupRows

            else if sortedStringGroups == "BDEF" then
                getRoundOf16Team groupE top4groupRows

            else
                getRoundOf16Team groupE top4groupRows

        E1 ->
            if sortedStringGroups == "ABCD" then
                getRoundOf16Team groupB top4groupRows

            else if sortedStringGroups == "ABCE" then
                getRoundOf16Team groupB top4groupRows

            else if sortedStringGroups == "ABCF" then
                getRoundOf16Team groupB top4groupRows

            else if sortedStringGroups == "ABDE" then
                getRoundOf16Team groupA top4groupRows

            else if sortedStringGroups == "ABDF" then
                getRoundOf16Team groupA top4groupRows

            else if sortedStringGroups == "ABEF" then
                getRoundOf16Team groupB top4groupRows

            else if sortedStringGroups == "ACDE" then
                getRoundOf16Team groupC top4groupRows

            else if sortedStringGroups == "ACDF" then
                getRoundOf16Team groupC top4groupRows

            else if sortedStringGroups == "ACEF" then
                getRoundOf16Team groupC top4groupRows

            else if sortedStringGroups == "ADEF" then
                getRoundOf16Team groupD top4groupRows

            else if sortedStringGroups == "BCDE" then
                getRoundOf16Team groupB top4groupRows

            else if sortedStringGroups == "BCDF" then
                getRoundOf16Team groupC top4groupRows

            else if sortedStringGroups == "BCEF" then
                getRoundOf16Team groupC top4groupRows

            else if sortedStringGroups == "BDEF" then
                getRoundOf16Team groupD top4groupRows

            else
                getRoundOf16Team groupD top4groupRows

        F1 ->
            if sortedStringGroups == "ABCD" then
                getRoundOf16Team groupC top4groupRows

            else if sortedStringGroups == "ABCE" then
                getRoundOf16Team groupC top4groupRows

            else if sortedStringGroups == "ABCF" then
                getRoundOf16Team groupC top4groupRows

            else if sortedStringGroups == "ABDE" then
                getRoundOf16Team groupB top4groupRows

            else if sortedStringGroups == "ABDF" then
                getRoundOf16Team groupB top4groupRows

            else if sortedStringGroups == "ABEF" then
                getRoundOf16Team groupA top4groupRows

            else if sortedStringGroups == "ACDE" then
                getRoundOf16Team groupA top4groupRows

            else if sortedStringGroups == "ACDF" then
                getRoundOf16Team groupA top4groupRows

            else if sortedStringGroups == "ACEF" then
                getRoundOf16Team groupA top4groupRows

            else if sortedStringGroups == "ADEF" then
                getRoundOf16Team groupA top4groupRows

            else if sortedStringGroups == "BCDE" then
                getRoundOf16Team groupC top4groupRows

            else if sortedStringGroups == "BCDF" then
                getRoundOf16Team groupB top4groupRows

            else if sortedStringGroups == "BCEF" then
                getRoundOf16Team groupB top4groupRows

            else if sortedStringGroups == "BDEF" then
                getRoundOf16Team groupB top4groupRows

            else
                getRoundOf16Team groupC top4groupRows


getRoundOf16Team : Group -> List GroupRow -> Team
getRoundOf16Team g top4Table =
    top4Table
        |> filterByGroup g
        |> Array.fromList
        |> Array.get 0
        |> teamFromMaybeGroupRow


teamFromMaybeGroupRow : Maybe GroupRow -> Team
teamFromMaybeGroupRow gr =
    case gr of
        Just row ->
            row.team

        Nothing ->
            Team "Wow14" ""
