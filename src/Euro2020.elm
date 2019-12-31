module Euro2020 exposing (Group(..), GroupRow, GroupState(..), Match, Team, TeamPosition, defaultFlag, filterByMatchId, getGroupRows, getGroupState, getScore, groupRows, groupToString, isPlayoffMatch, matches, maybeOrDefaultTeam, playOffMatches, updateTeams)

import Array
import Set


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


type alias Match =
    { id : Int
    , homeTeam : Team
    , homeScore : Maybe Int
    , awayTeam : Team
    , awayScore : Maybe Int
    , group : Group
    , date : String
    , time : String
    }


type alias Team =
    { name : String
    , flag : String
    }


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
    Team "Turkey" "https://www.countryflags.io/tr/flat/64.png"


italy =
    Team "Italy" "https://www.countryflags.io/it/flat/64.png"


wales =
    Team "Wales" "https://upload.wikimedia.org/wikipedia/commons/a/a9/Flag_of_Wales_%281959%E2%80%93present%29.svg"


switzerland =
    Team "Switzerland" "https://www.countryflags.io/ch/flat/64.png"


belgium =
    Team "Belgium" "https://www.countryflags.io/be/flat/64.png"


russia =
    Team "Russia" "https://www.countryflags.io/ru/flat/64.png"


finland =
    Team "Finland" "https://www.countryflags.io/fi/flat/64.png"


denmark =
    Team "Denmark" "https://www.countryflags.io/dk/flat/64.png"


netherlands =
    Team "Netherlands" "https://www.countryflags.io/nl/flat/64.png"


ukraine =
    Team "Ukraine" "https://www.countryflags.io/ua/flat/64.png"


austria =
    Team "Austria" "https://www.countryflags.io/at/flat/64.png"


romania =
    Team "Romania" "https://www.countryflags.io/ro/flat/64.png"


england =
    Team "England" "https://www.countryflags.io/gb/flat/64.png"


crotia =
    Team "Croatia" "https://www.countryflags.io/hr/flat/64.png"


ireland =
    Team "Ireland" "https://www.countryflags.io/ie/flat/64.png"


czech =
    Team "Czech Republic" "https://www.countryflags.io/cz/flat/64.png"


spain =
    Team "Spain" "https://www.countryflags.io/es/flat/64.png"


sweden =
    Team "Sweden" "https://www.countryflags.io/se/flat/64.png"


poland =
    Team "Poland" "https://www.countryflags.io/pl/flat/64.png"


iceland =
    Team "Iceland" "https://www.countryflags.io/is/flat/64.png"


serbia =
    Team "Serbia" "https://www.countryflags.io/rs/flat/64.png"


portugal =
    Team "Portugal" "https://www.countryflags.io/pt/flat/64.png"


france =
    Team "France" "https://www.countryflags.io/fr/flat/64.png"


germany =
    Team "Germany" "https://www.countryflags.io/de/flat/64.png"



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


romaniaRow =
    GroupRow romania 0 0 0 0 0 0 0 0 GroupC 0 0 0


englandRow =
    GroupRow england 0 0 0 0 0 0 0 0 GroupD 0 0 0


croatiaRow =
    GroupRow crotia 0 0 0 0 0 0 0 0 GroupD 0 0 0


irelandRow =
    GroupRow ireland 0 0 0 0 0 0 0 0 GroupD 0 0 0


czechRow =
    GroupRow czech 0 0 0 0 0 0 0 0 GroupD 0 0 0


spainRow =
    GroupRow spain 0 0 0 0 0 0 0 0 GroupE 0 0 0


swedenRow =
    GroupRow sweden 0 0 0 0 0 0 0 0 GroupE 0 0 0


polandRow =
    GroupRow poland 0 0 0 0 0 0 0 0 GroupE 0 0 0


icelandRow =
    GroupRow iceland 0 0 0 0 0 0 0 0 GroupE 0 0 0


serbiaRow =
    GroupRow serbia 0 0 0 0 0 0 0 0 GroupF 0 0 0


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
    [ netherlandsRow, ukraineRow, austriaRow, romaniaRow ]


groupD =
    [ englandRow, croatiaRow, irelandRow, czechRow ]


groupE =
    [ spainRow, swedenRow, polandRow, icelandRow ]


groupF =
    [ serbiaRow, portugalRow, franceRow, germanyRow ]



-- Matches


matchesGroupA =
    [ Match 1 turkey Nothing italy Nothing GroupA "16 June 2020" "18:00"
    , Match 2 wales Nothing switzerland Nothing GroupA "16 June 2020" "18:00"
    , Match 3 turkey Nothing wales Nothing GroupA "16 June 2020" "18:00"
    , Match 4 italy Nothing switzerland Nothing GroupA "16 June 2020" "18:00"
    , Match 5 switzerland Nothing turkey Nothing GroupA "16 June 2020" "18:00"
    , Match 6 italy Nothing wales Nothing GroupA "16 June 2020" "18:00"
    ]


matchesGroupB =
    [ Match 7 denmark Nothing finland Nothing GroupB "16 June 2020" "18:00"
    , Match 8 belgium Nothing russia Nothing GroupB "16 June 2020" "18:00"
    , Match 9 finland Nothing russia Nothing GroupB "16 June 2020" "18:00"
    , Match 10 denmark Nothing belgium Nothing GroupB "16 June 2020" "18:00"
    , Match 11 russia Nothing denmark Nothing GroupB "16 June 2020" "18:00"
    , Match 12 finland Nothing belgium Nothing GroupB "16 June 2020" "18:00"
    ]


matchesGroupC =
    [ Match 13 austria Nothing romania Nothing GroupC "16 June 2020" "18:00"
    , Match 14 netherlands Nothing ukraine Nothing GroupC "16 June 2020" "18:00"
    , Match 15 ukraine Nothing romania Nothing GroupC "16 June 2020" "18:00"
    , Match 16 netherlands Nothing austria Nothing GroupC "16 June 2020" "18:00"
    , Match 17 romania Nothing netherlands Nothing GroupC "16 June 2020" "18:00"
    , Match 18 ukraine Nothing austria Nothing GroupC "16 June 2020" "18:00"
    ]


matchesGroupD =
    [ Match 19 england Nothing crotia Nothing GroupD "16 June 2020" "18:00"
    , Match 20 ireland Nothing czech Nothing GroupD "16 June 2020" "18:00"
    , Match 21 crotia Nothing czech Nothing GroupD "16 June 2020" "18:00"
    , Match 22 england Nothing ireland Nothing GroupD "16 June 2020" "18:00"
    , Match 23 crotia Nothing ireland Nothing GroupD "16 June 2020" "18:00"
    , Match 24 czech Nothing england Nothing GroupD "16 June 2020" "18:00"
    ]


matchesGroupE =
    [ Match 25 poland Nothing iceland Nothing GroupE "16 June 2020" "18:00"
    , Match 26 spain Nothing sweden Nothing GroupE "16 June 2020" "18:00"
    , Match 27 sweden Nothing iceland Nothing GroupE "16 June 2020" "18:00"
    , Match 28 spain Nothing poland Nothing GroupE "16 June 2020" "18:00"
    , Match 29 iceland Nothing spain Nothing GroupE "16 June 2020" "18:00"
    , Match 30 sweden Nothing poland Nothing GroupE "16 June 2020" "18:00"
    ]


matchesGroupF =
    [ Match 31 serbia Nothing portugal Nothing GroupF "16 June 2020" "18:00"
    , Match 32 france Nothing germany Nothing GroupF "16 June 2020" "18:00"
    , Match 33 serbia Nothing france Nothing GroupF "16 June 2020" "18:00"
    , Match 34 portugal Nothing germany Nothing GroupF "16 June 2020" "18:00"
    , Match 35 portugal Nothing france Nothing GroupF "16 June 2020" "18:00"
    , Match 36 germany Nothing serbia Nothing GroupF "16 June 2020" "18:00"
    ]


playOffMatches =
    [ Match 38 (Team "Runner-up Group A" defaultFlag) Nothing (Team "Runner-up Group B" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 37 (Team "Winner Group A" defaultFlag) Nothing (Team "Runner-up Group C" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 40 (Team "Winner Group C" defaultFlag) Nothing (Team "3rd Group D/E/F" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 39 (Team "Winner Group B" defaultFlag) Nothing (Team "3rd Group A/D/E/F" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 42 (Team "Runner-up Group D" defaultFlag) Nothing (Team "Runner-up Group E" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 41 (Team "Winner Group F" defaultFlag) Nothing (Team "3rd Group A/B/C" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 44 (Team "Winner Group D" defaultFlag) Nothing (Team "Runner-up Group F" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 43 (Team "Winner Group E" defaultFlag) Nothing (Team "3rd Group A/B/C/D" defaultFlag) Nothing RoundOf16 "16 June 2020" "18:00"
    ]


groupRows : List GroupRow
groupRows =
    groupA ++ groupB ++ groupC ++ groupD ++ groupE ++ groupF


matches =
    matchesGroupA ++ matchesGroupB ++ matchesGroupC ++ matchesGroupD ++ matchesGroupE ++ matchesGroupF


filterByMatchId : Int -> List Match -> List Match
filterByMatchId matchId matchess =
    List.filter (\m -> m.id == matchId) matchess


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
    List.member match.id [ 37, 38, 39, 40, 41, 42, 43, 44 ]


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
