module Euro2020 exposing (Group(..), GroupRow, GroupState(..), HomeOrAway(..), Match, Team, TeamPosition, defaultFlag, encodeGroupMatches, encodeKoMatches, filterByMatchId, getGroupRows, getGroupState, getScore, groupRows, groupToString, isPlayoffMatch, matches, maybeOrDefaultTeam, playOffMatches, updateTeams)

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


encodeGroupMatch : Match -> Encode.Value
encodeGroupMatch match =
    Encode.object
        [ ( "match_number", Encode.int match.id )
        , ( "home_score", encodeMatchScore match.homeScore )
        , ( "away_score", encodeMatchScore match.awayScore )
        ]


encodeGroupMatches : List Match -> Encode.Value
encodeGroupMatches groupMatches =
    Encode.list encodeGroupMatch groupMatches


encodeKoMatch : Match -> Encode.Value
encodeKoMatch match =
    Encode.object
        [ ( "match_number", Encode.int match.id )
        , ( "home_score", encodeMatchScore match.homeScore )
        , ( "away_score", encodeMatchScore match.awayScore )
        , ( "home_win", encodeHomeWin match.homeWin )
        ]


encodeKoMatches : List Match -> Encode.Value
encodeKoMatches koMatches =
    Encode.list encodeKoMatch koMatches


type Group
    = GroupA
    | GroupB
    | GroupC
    | GroupD
    | GroupE
    | GroupF
    | RoundOf16
    | QuarterFinals
    | SemiFinals
    | Final


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
    Team "England" "https://flagcdn.com/w80/gb.png"


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
    GroupRow turkey 0 0 0 0 0 0 0 0 GroupA 0 0 0


italyRow =
    GroupRow italy 0 0 0 0 0 0 0 0 GroupA 0 0 0


walesRow =
    GroupRow wales 0 0 0 0 0 0 0 0 GroupA 0 0 0


switzerlandRow =
    GroupRow switzerland 0 0 0 0 0 0 0 0 GroupA 0 0 0


belgiumRow =
    GroupRow belgium 0 0 0 0 0 0 0 0 GroupB 0 0 0


russiaRow =
    GroupRow russia 0 0 0 0 0 0 0 0 GroupB 0 0 0


finlandRow =
    GroupRow finland 0 0 0 0 0 0 0 0 GroupB 0 0 0


denmarkRow =
    GroupRow denmark 0 0 0 0 0 0 0 0 GroupB 0 0 0


netherlandsRow =
    GroupRow netherlands 0 0 0 0 0 0 0 0 GroupC 0 0 0


ukraineRow =
    GroupRow ukraine 0 0 0 0 0 0 0 0 GroupC 0 0 0


austriaRow =
    GroupRow austria 0 0 0 0 0 0 0 0 GroupC 0 0 0


northMacedoniaRow =
    GroupRow northMacedonia 0 0 0 0 0 0 0 0 GroupC 0 0 0


englandRow =
    GroupRow england 0 0 0 0 0 0 0 0 GroupD 0 0 0


croatiaRow =
    GroupRow crotia 0 0 0 0 0 0 0 0 GroupD 0 0 0


scotlandRow =
    GroupRow scotland 0 0 0 0 0 0 0 0 GroupD 0 0 0


czechRow =
    GroupRow czech 0 0 0 0 0 0 0 0 GroupD 0 0 0


spainRow =
    GroupRow spain 0 0 0 0 0 0 0 0 GroupE 0 0 0


swedenRow =
    GroupRow sweden 0 0 0 0 0 0 0 0 GroupE 0 0 0


polandRow =
    GroupRow poland 0 0 0 0 0 0 0 0 GroupE 0 0 0


slovakiaRow =
    GroupRow slovakia 0 0 0 0 0 0 0 0 GroupE 0 0 0


hungaryRow =
    GroupRow hungary 0 0 0 0 0 0 0 0 GroupF 0 0 0


portugalRow =
    GroupRow portugal 0 0 0 0 0 0 0 0 GroupF 0 0 0


franceRow =
    GroupRow france 0 0 0 0 0 0 0 0 GroupF 0 0 0


germanyRow =
    GroupRow germany 0 0 0 0 0 0 0 0 GroupF 0 0 0


groupA =
    [ turkeyRow, italyRow, walesRow, switzerlandRow ]


groupB =
    [ belgiumRow, russiaRow, finlandRow, denmarkRow ]


groupC =
    [ netherlandsRow, ukraineRow, austriaRow, northMacedoniaRow ]


groupD =
    [ englandRow, croatiaRow, scotlandRow, czechRow ]


groupE =
    [ spainRow, swedenRow, polandRow, slovakiaRow ]


groupF =
    [ hungaryRow, portugalRow, franceRow, germanyRow ]



-- Matches


matchesGroupA =
    [ Match 1 turkey Nothing italy Nothing GroupA "16 June 2020" "18:00" Nothing
    , Match 2 wales Nothing switzerland Nothing GroupA "16 June 2020" "18:00" Nothing
    , Match 13 turkey Nothing wales Nothing GroupA "16 June 2020" "18:00" Nothing
    , Match 14 italy Nothing switzerland Nothing GroupA "16 June 2020" "18:00" Nothing
    , Match 25 switzerland Nothing turkey Nothing GroupA "16 June 2020" "18:00" Nothing
    , Match 26 italy Nothing wales Nothing GroupA "16 June 2020" "18:00" Nothing
    ]


matchesGroupB =
    [ Match 3 denmark Nothing finland Nothing GroupB "16 June 2020" "18:00" Nothing
    , Match 4 belgium Nothing russia Nothing GroupB "16 June 2020" "18:00" Nothing
    , Match 15 finland Nothing russia Nothing GroupB "16 June 2020" "18:00" Nothing
    , Match 16 denmark Nothing belgium Nothing GroupB "16 June 2020" "18:00" Nothing
    , Match 27 russia Nothing denmark Nothing GroupB "16 June 2020" "18:00" Nothing
    , Match 28 finland Nothing belgium Nothing GroupB "16 June 2020" "18:00" Nothing
    ]


matchesGroupC =
    [ Match 6 austria Nothing northMacedonia Nothing GroupC "16 June 2020" "18:00" Nothing
    , Match 5 netherlands Nothing ukraine Nothing GroupC "16 June 2020" "18:00" Nothing
    , Match 18 ukraine Nothing northMacedonia Nothing GroupC "16 June 2020" "18:00" Nothing
    , Match 17 netherlands Nothing austria Nothing GroupC "16 June 2020" "18:00" Nothing
    , Match 29 northMacedonia Nothing netherlands Nothing GroupC "16 June 2020" "18:00" Nothing
    , Match 30 ukraine Nothing austria Nothing GroupC "16 June 2020" "18:00" Nothing
    ]


matchesGroupD =
    [ Match 7 england Nothing crotia Nothing GroupD "16 June 2020" "18:00" Nothing
    , Match 8 scotland Nothing czech Nothing GroupD "16 June 2020" "18:00" Nothing
    , Match 19 crotia Nothing czech Nothing GroupD "16 June 2020" "18:00" Nothing
    , Match 20 england Nothing scotland Nothing GroupD "16 June 2020" "18:00" Nothing
    , Match 31 crotia Nothing scotland Nothing GroupD "16 June 2020" "18:00" Nothing
    , Match 32 czech Nothing england Nothing GroupD "16 June 2020" "18:00" Nothing
    ]


matchesGroupE =
    [ Match 10 poland Nothing slovakia Nothing GroupE "16 June 2020" "18:00" Nothing
    , Match 9 spain Nothing sweden Nothing GroupE "16 June 2020" "18:00" Nothing
    , Match 21 sweden Nothing slovakia Nothing GroupE "16 June 2020" "18:00" Nothing
    , Match 22 spain Nothing poland Nothing GroupE "16 June 2020" "18:00" Nothing
    , Match 33 slovakia Nothing spain Nothing GroupE "16 June 2020" "18:00" Nothing
    , Match 34 sweden Nothing poland Nothing GroupE "16 June 2020" "18:00" Nothing
    ]


matchesGroupF =
    [ Match 11 hungary Nothing portugal Nothing GroupF "16 June 2020" "18:00" Nothing
    , Match 12 france Nothing germany Nothing GroupF "16 June 2020" "18:00" Nothing
    , Match 23 hungary Nothing france Nothing GroupF "16 June 2020" "18:00" Nothing
    , Match 24 portugal Nothing germany Nothing GroupF "16 June 2020" "18:00" Nothing
    , Match 35 portugal Nothing france Nothing GroupF "16 June 2020" "18:00" Nothing
    , Match 36 germany Nothing hungary Nothing GroupF "16 June 2020" "18:00" Nothing
    ]


playOffMatches =
    [ Match 39 (Team "Winner Group B" defaultFlag) Nothing (Team "3rd Group A/D/E/F" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00" Nothing
    , Match 37 (Team "Winner Group A" defaultFlag) Nothing (Team "Runner-up Group C" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00" Nothing
    , Match 41 (Team "Winner Group F" defaultFlag) Nothing (Team "3rd Group A/B/C" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00" Nothing
    , Match 42 (Team "Runner-up Group D" defaultFlag) Nothing (Team "Runner-up Group E" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00" Nothing
    , Match 43 (Team "Winner Group E" defaultFlag) Nothing (Team "3rd Group A/B/C/D" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00" Nothing
    , Match 44 (Team "Winner Group D" defaultFlag) Nothing (Team "Runner-up Group F" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00" Nothing
    , Match 40 (Team "Winner Group C" defaultFlag) Nothing (Team "3rd Group D/E/F" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00" Nothing
    , Match 38 (Team "Runner-up Group A" defaultFlag) Nothing (Team "Runner-up Group B" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00" Nothing
    , Match 46 (Team "Winner Match 39" defaultFlag) Nothing (Team "Winner Match 37" defaultFlag) Nothing QuarterFinals "16 June 2020" "18:00" Nothing
    , Match 45 (Team "Winner Match 41" defaultFlag) Nothing (Team "Winner Match 42" defaultFlag) Nothing QuarterFinals "16 June 2020" "18:00" Nothing
    , Match 48 (Team "Winner Match 43" defaultFlag) Nothing (Team "Winner Match 44" defaultFlag) Nothing QuarterFinals "16 June 2020" "18:00" Nothing
    , Match 47 (Team "Winner Match 40" defaultFlag) Nothing (Team "Winner Match 38" defaultFlag) Nothing QuarterFinals "16 June 2020" "18:00" Nothing
    , Match 49 (Team "Winner Match 46" defaultFlag) Nothing (Team "Winner Match 45" defaultFlag) Nothing SemiFinals "16 June 2020" "18:00" Nothing
    , Match 50 (Team "Winner Match 48" defaultFlag) Nothing (Team "Winner Match 47" defaultFlag) Nothing SemiFinals "16 June 2020" "18:00" Nothing
    , Match 51 (Team "Winner Match 49" defaultFlag) Nothing (Team "Winner Match 50" defaultFlag) Nothing Final "16 June 2020" "18:00" Nothing
    ]


groupRows : List GroupRow
groupRows =
    groupA ++ groupB ++ groupC ++ groupD ++ groupE ++ groupF


matches =
    matchesGroupA ++ matchesGroupB ++ matchesGroupC ++ matchesGroupD ++ matchesGroupE ++ matchesGroupF


filterByMatchId : Int -> List Match -> List Match
filterByMatchId matchId ms =
    List.filter (\m -> m.id == matchId) ms


filterByGroup : Group -> List GroupRow -> List GroupRow
filterByGroup groupName grs =
    List.filter (\gr -> gr.group == groupName) grs


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


getGroupRows : Group -> List GroupRow -> List GroupRow
getGroupRows groupName grs =
    grs
        |> filterByGroup groupName
        |> List.sortBy getScore
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
                | homeTeam = getTeamPlaying (Team "Winner Group A" defaultFlag) GroupA 1 grs
                , awayTeam = getTeamPlaying (Team "Runner-up Group C" defaultFlag) GroupC 2 grs
            }

        38 ->
            { m
                | homeTeam = getTeamPlaying (Team "Runner-up Group A" defaultFlag) GroupA 2 grs
                , awayTeam = getTeamPlaying (Team "Runner-up Group B" defaultFlag) GroupB 2 grs
            }

        39 ->
            { m
                | homeTeam = getTeamPlaying (Team "Winner Group B" defaultFlag) GroupB 1 grs
                , awayTeam = get3rdTeam (Team "3rd Group A/D/E/F" defaultFlag) B1 thirdPlaces grs
            }

        40 ->
            { m
                | homeTeam = getTeamPlaying (Team "Winner Group C" defaultFlag) GroupC 1 grs
                , awayTeam = get3rdTeam (Team "3rd Group D/E/F" defaultFlag) C1 thirdPlaces grs
            }

        41 ->
            { m
                | homeTeam = getTeamPlaying (Team "Winner Group F" defaultFlag) GroupF 1 grs
                , awayTeam = get3rdTeam (Team "3rd Group A/B/C" defaultFlag) F1 thirdPlaces grs
            }

        42 ->
            { m
                | homeTeam = getTeamPlaying (Team "Runner-up Group D" defaultFlag) GroupD 2 grs
                , awayTeam = getTeamPlaying (Team "Runner-up Group E" defaultFlag) GroupE 2 grs
            }

        43 ->
            { m
                | homeTeam = getTeamPlaying (Team "Winner Group E" defaultFlag) GroupE 1 grs
                , awayTeam = get3rdTeam (Team "3rd Group A/B/C/D" defaultFlag) E1 thirdPlaces grs
            }

        44 ->
            { m
                | homeTeam = getTeamPlaying (Team "Winner Group D" defaultFlag) GroupD 1 grs
                , awayTeam = getTeamPlaying (Team "Runner-up Group F" defaultFlag) GroupF 2 grs
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
