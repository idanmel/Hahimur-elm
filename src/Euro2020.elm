module Euro2020 exposing (Group(..), GroupRow, Match, Team, getGroup, groups, matches)

import Array


type Group
    = GroupA
    | GroupB
    | GroupC
    | GroupD
    | GroupE
    | GroupF
    | RoundOf16


type alias Match =
    { id : Int
    , homeTeam : Team
    , homeScore : Maybe Int
    , awayTeam : Team
    , awayScore : Maybe Int
    , group : Group
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
    , pos : Int
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
    GroupRow turkey 0 0 0 0 0 0 0 0 4 1 GroupA


italyRow =
    GroupRow italy 0 0 0 0 0 0 0 0 3 2 GroupA


walesRow =
    GroupRow wales 0 0 0 0 0 0 0 0 2 3 GroupA


switzerlandRow =
    GroupRow switzerland 0 0 0 0 0 0 0 0 1 4 GroupA


belgiumRow =
    GroupRow belgium 0 0 0 0 0 0 0 0 2 1 GroupB


russiaRow =
    GroupRow russia 0 0 0 0 0 0 0 0 1 2 GroupB


finlandRow =
    GroupRow finland 0 0 0 0 0 0 0 0 3 3 GroupB


denmarkRow =
    GroupRow denmark 0 0 0 0 0 0 0 0 4 4 GroupB


netherlandsRow =
    GroupRow netherlands 0 0 0 0 0 0 0 0 4 4 GroupC


ukraineRow =
    GroupRow ukraine 0 0 0 0 0 0 0 0 3 4 GroupC


austriaRow =
    GroupRow austria 0 0 0 0 0 0 0 0 2 4 GroupC


romaniaRow =
    GroupRow romania 0 0 0 0 0 0 0 0 1 4 GroupC


englandRow =
    GroupRow england 0 0 0 0 0 0 0 0 4 4 GroupD


croatiaRow =
    GroupRow crotia 0 0 0 0 0 0 0 0 3 4 GroupD


irelandRow =
    GroupRow ireland 0 0 0 0 0 0 0 0 2 4 GroupD


czechRow =
    GroupRow czech 0 0 0 0 0 0 0 0 1 4 GroupD


spainRow =
    GroupRow spain 0 0 0 0 0 0 0 0 4 4 GroupE


swedenRow =
    GroupRow sweden 0 0 0 0 0 0 0 0 3 4 GroupE


polandRow =
    GroupRow poland 0 0 0 0 0 0 0 0 2 4 GroupE


icelandRow =
    GroupRow iceland 0 0 0 0 0 0 0 0 1 4 GroupE


serbiaRow =
    GroupRow serbia 0 0 0 0 0 0 0 0 4 4 GroupF


portugalRow =
    GroupRow portugal 0 0 0 0 0 0 0 0 3 4 GroupF


franceRow =
    GroupRow france 0 0 0 0 0 0 0 0 2 4 GroupF


germanyRow =
    GroupRow germany 0 0 0 0 0 0 0 0 1 4 GroupF


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
    [ Match 1 turkey Nothing italy Nothing GroupA
    , Match 2 wales Nothing switzerland Nothing GroupA
    , Match 3 turkey Nothing wales Nothing GroupA
    , Match 4 italy Nothing switzerland Nothing GroupA
    , Match 5 switzerland Nothing turkey Nothing GroupA
    , Match 6 italy Nothing wales Nothing GroupA
    ]


matchesGroupB =
    [ Match 7 denmark Nothing finland Nothing GroupB
    , Match 8 belgium Nothing russia Nothing GroupB
    , Match 9 finland Nothing russia Nothing GroupB
    , Match 10 denmark Nothing belgium Nothing GroupB
    , Match 11 russia Nothing denmark Nothing GroupB
    , Match 12 finland Nothing belgium Nothing GroupB
    ]


matchesGroupC =
    [ Match 13 austria Nothing romania Nothing GroupC
    , Match 14 netherlands Nothing ukraine Nothing GroupC
    , Match 15 ukraine Nothing romania Nothing GroupC
    , Match 16 netherlands Nothing austria Nothing GroupC
    , Match 17 romania Nothing netherlands Nothing GroupC
    , Match 18 ukraine Nothing austria Nothing GroupC
    ]


matchesGroupD =
    [ Match 19 england Nothing crotia Nothing GroupD
    , Match 20 ireland Nothing czech Nothing GroupD
    , Match 21 crotia Nothing czech Nothing GroupD
    , Match 22 england Nothing ireland Nothing GroupD
    , Match 23 crotia Nothing ireland Nothing GroupD
    , Match 24 czech Nothing england Nothing GroupD
    ]


matchesGroupE =
    [ Match 25 poland Nothing iceland Nothing GroupE
    , Match 26 spain Nothing sweden Nothing GroupE
    , Match 27 sweden Nothing iceland Nothing GroupE
    , Match 28 spain Nothing poland Nothing GroupE
    , Match 29 iceland Nothing spain Nothing GroupE
    , Match 30 sweden Nothing poland Nothing GroupE
    ]


matchesGroupF =
    [ Match 31 serbia Nothing portugal Nothing GroupF
    , Match 32 france Nothing germany Nothing GroupF
    , Match 33 serbia Nothing france Nothing GroupF
    , Match 34 portugal Nothing germany Nothing GroupF
    , Match 35 portugal Nothing france Nothing GroupF
    , Match 36 germany Nothing serbia Nothing GroupF
    ]


playOffMatches =
    [ Match 38 (getTeamPlaying runnerUpGroupA GroupA 2 groups) Nothing (getTeamPlaying runnerUpGroupB GroupB 2 groups) Nothing RoundOf16
    ]


runnerUpGroupA =
    Team "Runner-up Group A"


runnerUpGroupB =
    Team "Runner-up Group B"


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


getGroup : Group -> List GroupRow -> List GroupRow
getGroup groupName groupRows =
    groupRows
        |> filterByGroup groupName
        |> List.sortBy .score
        |> List.reverse


playedAllGames : GroupRow -> Bool
playedAllGames gr =
    gr.pld == 4


getTeamPlaying : Team -> Group -> Int -> List GroupRow -> Team
getTeamPlaying placeHolderTeam groupName pos groupRows =
    let
        group =
            groupRows
                |> getGroup groupName
    in
    if List.all playedAllGames group then
        maybeOrDefaultTeam placeHolderTeam (Array.get 2 (Array.fromList group))

    else
        placeHolderTeam


maybeOrDefaultTeam : Team -> Maybe GroupRow -> Team
maybeOrDefaultTeam defaultTeam maybeGroupRow =
    case maybeGroupRow of
        Just gr ->
            gr.team

        Nothing ->
            defaultTeam
