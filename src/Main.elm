module Main exposing (..)

{-| -}

import Browser
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)


blue =
    Element.rgb 0 0 0.4


red =
    Element.rgb 0.8 0 0



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
                updateMatch : Match -> Match
                updateMatch m =
                    if m.id == matchId then
                        if homeOrAway == Home then
                            { m | homeScore = String.toInt score }

                        else
                            { m | awayScore = String.toInt score }

                    else
                        m

                newMatches =
                    List.map updateMatch model.matches
            in
            { model
                | matches = newMatches
                , group =
                    newMatches
                        |> List.foldl updateGroup initGroup
                        |> List.sortBy .score
                        |> List.reverse
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



-- MODEL


type alias Match =
    { id : Int
    , homeTeam : Team
    , homeScore : Maybe Int
    , awayTeam : Team
    , awayScore : Maybe Int
    , group : String
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
    , group : String
    }


type alias Team =
    { name : String }


type alias Model =
    { matches : List Match
    , group : List GroupRow
    }


type HomeOrAway
    = Home
    | Away



-- GROUP A


turkey =
    Team "Turkey"


italy =
    Team "Italy"


wales =
    Team "Wales"


switzerland =
    Team "Switzerland"


turkeyRow =
    GroupRow turkey 0 0 0 0 0 0 0 0 4 1 "groupA"


italyRow =
    GroupRow italy 0 0 0 0 0 0 0 0 3 1 "groupA"


walesRow =
    GroupRow wales 0 0 0 0 0 0 0 0 2 1 "groupA"


switzerlandRow =
    GroupRow switzerland 0 0 0 0 0 0 0 0 1 1 "groupA"


groupA =
    [ turkeyRow, italyRow, walesRow, switzerlandRow ]


matchesGroupA =
    [ Match 1 turkey Nothing italy Nothing "groupA"
    , Match 2 wales Nothing switzerland Nothing "groupA"
    , Match 3 turkey Nothing wales Nothing "groupA"
    , Match 4 italy Nothing switzerland Nothing "groupA"
    , Match 5 switzerland Nothing italy Nothing "groupA"
    , Match 6 turkey Nothing wales Nothing "groupA"
    ]



-- GROUP B


belgium =
    Team "Belgium"


russia =
    Team "Russia"


finland =
    Team "Finland"


denmark =
    Team "Denmark"


belgiumRow =
    GroupRow belgium 0 0 0 0 0 0 0 0 1 1 "groupB"


russiaRow =
    GroupRow russia 0 0 0 0 0 0 0 0 2 2 "groupB"


finlandRow =
    GroupRow finland 0 0 0 0 0 0 0 0 3 3 "groupB"


denmarkRow =
    GroupRow denmark 0 0 0 0 0 0 0 0 4 4 "groupB"


groupB =
    [ belgiumRow, russiaRow, finlandRow, denmarkRow ]


matchesGroupB =
    [ Match 7 denmark Nothing finland Nothing "groupB"
    , Match 8 belgium Nothing russia Nothing "groupB"
    , Match 9 finland Nothing russia Nothing "groupB"
    , Match 10 denmark Nothing belgium Nothing "groupB"
    , Match 11 russia Nothing denmark Nothing "groupB"
    , Match 12 finland Nothing belgium Nothing "groupB"
    ]


groups =
    groupA ++ groupB


filterByGroup : String -> List GroupRow -> List GroupRow
filterByGroup groupName groupRows =
    List.filter (\gr -> gr.group == groupName) groupRows


initGroup =
    groups
        |> filterByGroup "groupA"
        |> List.sortBy .score
        |> List.reverse


init =
    { matches = matchesGroupA ++ matchesGroupB
    , group = initGroup
    }



-- VIEW


view : Model -> Html Msg
view model =
    Element.layout
        [ Background.color (rgba 1 1 1 1)
        , Font.color (rgba 0 0 0 1)
        , inFront <| header
        ]
    <|
        column [ width fill ]
            [ header
            , viewGroup model.group
            , viewMatches model.matches "groupA"
            ]


header : Element Msg
header =
    row
        [ width fill
        , Background.color red
        , padding 18
        ]
        [ el [] (text "Hahimur!") ]


viewMatch : Match -> Element Msg
viewMatch match =
    row
        [ Background.color blue
        , padding 16
        , spacing 120
        , width fill
        , Font.color (rgba 1 1 1 1)
        ]
        [ el [] (text match.homeTeam.name)
        , el [] (viewMatchInput match.id Home match.homeScore)
        , el [] (text "-")
        , el [] (viewMatchInput match.id Away match.awayScore)
        , el [] (text match.awayTeam.name)
        ]


viewMatches : List Match -> String -> Element Msg
viewMatches matches group =
    matches
        |> List.filter (\m -> m.group == group)
        |> List.map viewMatch
        |> column [ width fill ]


viewGroup : List GroupRow -> Element Msg
viewGroup group =
    Element.table [ padding 16, spacing 8 ]
        { data = group
        , columns =
            [ { header = Element.text "Team"
              , width = fillPortion 2
              , view =
                    \groupRow ->
                        Element.text groupRow.team.name
              }
            , { header = Element.text "Pld"
              , width = fillPortion 1
              , view =
                    \groupRow ->
                        Element.text (String.fromInt groupRow.pld)
              }
            , { header = Element.text "W"
              , width = fillPortion 1
              , view =
                    \groupRow ->
                        Element.text (String.fromInt groupRow.w)
              }
            , { header = Element.text "D"
              , width = fillPortion 1
              , view =
                    \groupRow ->
                        Element.text (String.fromInt groupRow.d)
              }
            , { header = Element.text "L"
              , width = fillPortion 1
              , view =
                    \groupRow ->
                        Element.text (String.fromInt groupRow.l)
              }
            , { header = Element.text "GF"
              , width = fillPortion 1
              , view =
                    \groupRow ->
                        Element.text (String.fromInt groupRow.gf)
              }
            , { header = Element.text "GA"
              , width = fillPortion 1
              , view =
                    \groupRow ->
                        Element.text (String.fromInt groupRow.ga)
              }
            , { header = Element.text "GD"
              , width = fillPortion 1
              , view =
                    \groupRow ->
                        Element.text (String.fromInt groupRow.gd)
              }
            , { header = Element.text "Pts"
              , width = fillPortion 1
              , view =
                    \groupRow ->
                        Element.text (String.fromInt groupRow.pts)
              }
            ]
        }


viewMatchInput : Int -> HomeOrAway -> Maybe Int -> Element Msg
viewMatchInput matchId homeOrAway score =
    Element.Input.text [ width (px 100), Font.color (rgba 0 0 0 1) ]
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
