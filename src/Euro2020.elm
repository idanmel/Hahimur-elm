module Euro2020 exposing (GroupRow, Match, Team, groups, matches)


type alias Match =
    { id : Int
    , homeTeam : Team
    , homeScore : Maybe Int
    , awayTeam : Team
    , awayScore : Maybe Int
    , group : String
    }


type alias Team =
    { name : String }


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
    , group : String
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
    GroupRow turkey 0 0 0 0 0 0 0 0 4 1 "groupA"


italyRow =
    GroupRow italy 0 0 0 0 0 0 0 0 3 2 "groupA"


walesRow =
    GroupRow wales 0 0 0 0 0 0 0 0 2 3 "groupA"


switzerlandRow =
    GroupRow switzerland 0 0 0 0 0 0 0 0 1 4 "groupA"


belgiumRow =
    GroupRow belgium 0 0 0 0 0 0 0 0 2 1 "groupB"


russiaRow =
    GroupRow russia 0 0 0 0 0 0 0 0 1 2 "groupB"


finlandRow =
    GroupRow finland 0 0 0 0 0 0 0 0 3 3 "groupB"


denmarkRow =
    GroupRow denmark 0 0 0 0 0 0 0 0 4 4 "groupB"


netherlandsRow =
    GroupRow netherlands 0 0 0 0 0 0 0 0 4 4 "groupC"


ukraineRow =
    GroupRow ukraine 0 0 0 0 0 0 0 0 3 4 "groupC"


austriaRow =
    GroupRow austria 0 0 0 0 0 0 0 0 2 4 "groupC"


romaniaRow =
    GroupRow romania 0 0 0 0 0 0 0 0 1 4 "groupC"


englandRow =
    GroupRow england 0 0 0 0 0 0 0 0 4 4 "groupD"


croatiaRow =
    GroupRow crotia 0 0 0 0 0 0 0 0 3 4 "groupD"


irelandRow =
    GroupRow ireland 0 0 0 0 0 0 0 0 2 4 "groupD"


czechRow =
    GroupRow czech 0 0 0 0 0 0 0 0 1 4 "groupD"


spainRow =
    GroupRow spain 0 0 0 0 0 0 0 0 4 4 "groupE"


swedenRow =
    GroupRow sweden 0 0 0 0 0 0 0 0 3 4 "groupE"


polandRow =
    GroupRow poland 0 0 0 0 0 0 0 0 2 4 "groupE"


icelandRow =
    GroupRow iceland 0 0 0 0 0 0 0 0 1 4 "groupE"


serbiaRow =
    GroupRow serbia 0 0 0 0 0 0 0 0 4 4 "groupF"


portugalRow =
    GroupRow portugal 0 0 0 0 0 0 0 0 3 4 "groupF"


franceRow =
    GroupRow france 0 0 0 0 0 0 0 0 2 4 "groupF"


germanyRow =
    GroupRow germany 0 0 0 0 0 0 0 0 1 4 "groupF"


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
    [ Match 1 turkey Nothing italy Nothing "groupA"
    , Match 2 wales Nothing switzerland Nothing "groupA"
    , Match 3 turkey Nothing wales Nothing "groupA"
    , Match 4 italy Nothing switzerland Nothing "groupA"
    , Match 5 switzerland Nothing turkey Nothing "groupA"
    , Match 6 italy Nothing wales Nothing "groupA"
    ]


matchesGroupB =
    [ Match 7 denmark Nothing finland Nothing "groupB"
    , Match 8 belgium Nothing russia Nothing "groupB"
    , Match 9 finland Nothing russia Nothing "groupB"
    , Match 10 denmark Nothing belgium Nothing "groupB"
    , Match 11 russia Nothing denmark Nothing "groupB"
    , Match 12 finland Nothing belgium Nothing "groupB"
    ]


matchesGroupC =
    [ Match 13 austria Nothing romania Nothing "groupC"
    , Match 14 netherlands Nothing ukraine Nothing "groupC"
    , Match 15 ukraine Nothing romania Nothing "groupC"
    , Match 16 netherlands Nothing austria Nothing "groupC"
    , Match 17 romania Nothing netherlands Nothing "groupC"
    , Match 18 ukraine Nothing austria Nothing "groupC"
    ]


matchesGroupD =
    [ Match 19 england Nothing crotia Nothing "groupD"
    , Match 20 ireland Nothing czech Nothing "groupD"
    , Match 21 crotia Nothing czech Nothing "groupD"
    , Match 22 england Nothing ireland Nothing "groupD"
    , Match 23 crotia Nothing ireland Nothing "groupD"
    , Match 24 czech Nothing england Nothing "groupD"
    ]


matchesGroupE =
    [ Match 25 poland Nothing iceland Nothing "groupE"
    , Match 26 spain Nothing sweden Nothing "groupE"
    , Match 27 sweden Nothing iceland Nothing "groupE"
    , Match 28 spain Nothing poland Nothing "groupE"
    , Match 29 iceland Nothing spain Nothing "groupE"
    , Match 30 sweden Nothing poland Nothing "groupE"
    ]


matchesGroupF =
    [ Match 31 serbia Nothing portugal Nothing "groupF"
    , Match 32 france Nothing germany Nothing "groupF"
    , Match 33 serbia Nothing france Nothing "groupF"
    , Match 34 portugal Nothing germany Nothing "groupF"
    , Match 35 portugal Nothing france Nothing "groupF"
    , Match 36 germany Nothing serbia Nothing "groupF"
    ]


groups =
    groupA ++ groupB ++ groupC ++ groupD ++ groupE ++ groupF


matches =
    matchesGroupA ++ matchesGroupB ++ matchesGroupC ++ matchesGroupD ++ matchesGroupE ++ matchesGroupF
