module Euro2020 exposing (Group(..), GroupRow, Match, Team, filterByGroup, getGroupRows, getTeamPlaying, groups, matches, playOffMatches)

import Array


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
    , score : Int
    , group : Group
    }



-- Teams


turkey =
    Team "Turkey"


italy =
    Team "Italy"


wales =
    Team "Wales"


switzerland =
    Team "Switzerland"


belgium =
    Team "Belgium"


russia =
    Team "Russia"


finland =
    Team "Finland"


denmark =
    Team "Denmark"


netherlands =
    Team "Netherlands"


ukraine =
    Team "Ukraine"


austria =
    Team "Austria"


romania =
    Team "Romania"


england =
    Team "England"


crotia =
    Team "Croatia"


ireland =
    Team "Ireland"


czech =
    Team "Czech Republic"


spain =
    Team "Spain"


sweden =
    Team "Sweden"


poland =
    Team "Poland"


iceland =
    Team "Iceland"


serbia =
    Team "Serbia"


portugal =
    Team "Portugal"


france =
    Team "France"


germany =
    Team "Germany"



-- groupRows


turkeyRow =
    GroupRow turkey 0 0 0 0 0 0 0 0 4 GroupA


italyRow =
    GroupRow italy 0 0 0 0 0 0 0 0 3 GroupA


walesRow =
    GroupRow wales 0 0 0 0 0 0 0 0 2 GroupA


switzerlandRow =
    GroupRow switzerland 0 0 0 0 0 0 0 0 1 GroupA


belgiumRow =
    GroupRow belgium 0 0 0 0 0 0 0 0 2 GroupB


russiaRow =
    GroupRow russia 0 0 0 0 0 0 0 0 1 GroupB


finlandRow =
    GroupRow finland 0 0 0 0 0 0 0 0 3 GroupB


denmarkRow =
    GroupRow denmark 0 0 0 0 0 0 0 0 4 GroupB


netherlandsRow =
    GroupRow netherlands 0 0 0 0 0 0 0 0 4 GroupC


ukraineRow =
    GroupRow ukraine 0 0 0 0 0 0 0 0 3 GroupC


austriaRow =
    GroupRow austria 0 0 0 0 0 0 0 0 2 GroupC


romaniaRow =
    GroupRow romania 0 0 0 0 0 0 0 0 1 GroupC


englandRow =
    GroupRow england 0 0 0 0 0 0 0 0 4 GroupD


croatiaRow =
    GroupRow crotia 0 0 0 0 0 0 0 0 3 GroupD


irelandRow =
    GroupRow ireland 0 0 0 0 0 0 0 0 2 GroupD


czechRow =
    GroupRow czech 0 0 0 0 0 0 0 0 1 GroupD


spainRow =
    GroupRow spain 0 0 0 0 0 0 0 0 4 GroupE


swedenRow =
    GroupRow sweden 0 0 0 0 0 0 0 0 3 GroupE


polandRow =
    GroupRow poland 0 0 0 0 0 0 0 0 2 GroupE


icelandRow =
    GroupRow iceland 0 0 0 0 0 0 0 0 1 GroupE


serbiaRow =
    GroupRow serbia 0 0 0 0 0 0 0 0 4 GroupF


portugalRow =
    GroupRow portugal 0 0 0 0 0 0 0 0 3 GroupF


franceRow =
    GroupRow france 0 0 0 0 0 0 0 0 2 GroupF


germanyRow =
    GroupRow germany 0 0 0 0 0 0 0 0 1 GroupF


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
    [ Match 38 (Team "Runner-up Group A") Nothing (Team "Runner-up Group B") Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 37 (Team "Winner Group A") Nothing (Team "Runner-up Group C") Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 40 (Team "Winner Group C") Nothing (Team "3rd Group D/E/F") Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 39 (Team "Winner Group B") Nothing (Team "3rd Group A/D/E/F") Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 42 (Team "Runner-up Group D") Nothing (Team "Runner-up Group E") Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 41 (Team "Winner Group F") Nothing (Team "3rd Group A/B/C") Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 44 (Team "Winner Group D") Nothing (Team "Runner-up Group F") Nothing RoundOf16 "16 June 2020" "18:00"
    , Match 43 (Team "Winner Group E") Nothing (Team "3rd Group A/B/C/D") Nothing RoundOf16 "16 June 2020" "18:00"
    ]


groups : List GroupRow
groups =
    groupA ++ groupB ++ groupC ++ groupD ++ groupE ++ groupF


matches =
    matchesGroupA ++ matchesGroupB ++ matchesGroupC ++ matchesGroupD ++ matchesGroupE ++ matchesGroupF ++ playOffMatches


filterByMatchId : Int -> List Match -> List Match
filterByMatchId matchId matchess =
    List.filter (\m -> m.id == matchId) matchess


filterByGroup : Group -> List GroupRow -> List GroupRow
filterByGroup groupName groupRows =
    List.filter (\gr -> gr.group == groupName) groupRows


getGroupRows : Group -> List GroupRow -> List GroupRow
getGroupRows groupName groupRows =
    groupRows
        |> filterByGroup groupName
        |> List.sortBy .score
        |> List.reverse


playedAllGames : GroupRow -> Bool
playedAllGames gr =
    gr.pld == 3


getTeamPlaying : Team -> Group -> Int -> List GroupRow -> Team
getTeamPlaying placeHolderTeam group pos allGroupRows =
    let
        groupRows =
            getGroupRows group allGroupRows
    in
    if List.all playedAllGames groupRows then
        maybeOrDefaultTeam placeHolderTeam (Array.get (pos - 1) (Array.fromList groupRows))

    else
        placeHolderTeam


maybeOrDefaultTeam : Team -> Maybe GroupRow -> Team
maybeOrDefaultTeam defaultTeam maybeGroupRow =
    case maybeGroupRow of
        Just gr ->
            gr.team

        Nothing ->
            defaultTeam
