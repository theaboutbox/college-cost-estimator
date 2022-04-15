module Main exposing (..)

import Browser
import FormatNumber exposing (format)
import FormatNumber.Locales exposing (Decimals(..), Locale, usLocale)
import Html exposing (Attribute, Html, button, div, span, input, label, text, br, table, tr, td, th, h1, h2, p)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Time
import Time.Extra as Time
import Task
import Chart as C
import Chart.Attributes as CA
import Chart.Item as CI
import Chart.Events as CE

-- MAIN


main =
    Browser.element { init = init, update = update, view = view, subscriptions=subscriptions }

-- Subscriptions
subscriptions : Model -> Sub Msg
subscriptions model = Sub.none


-- MODEL


type alias Outcome =
    { month_number : Int
    , year : Int
    , month : Time.Month
    , contribution : Float
    , interest : Float
    , payment : Float
    , amount : Float
    }

type alias Form = 
    { starting_amount : String
    , contribution_lump_sum : String
    , contribution_monthly : String
    , percent_interest : String
    , contribution_start_yearmon : String
    , contribution_end_yearmon : String
    , payment_start_yearmon : String
    , payment_second_month : String
    , payment_amount : String
    , simulation_end_yearmon : String   
    }

type alias Parameters =
    { starting_amount : Float
    , contribution_lump_sum : Float
    , contribution_start_yearmon : YearMonth
    , contribution_end_yearmon : YearMonth
    , monthly_contribution : Float
    , percent_interest : Float
    , withdrawal_start_yearmon : YearMonth
    , withdrawal_second_month : Time.Month
    , withdrawal_amount : Float
    , simulation_end_yearmon : YearMonth
    }

type alias Model = 
    { starting_amount : String
    , contribution_lump_sum : String
    , contribution_monthly : String
    , percent_interest : String
    , contribution_start_yearmon : String
    , contribution_end_yearmon : String
    , payment_start_yearmon : String
    , payment_second_month : String
    , payment_amount : String
    , simulation_end_yearmon : String
    , current_yearmon : YearMonth
    , current_zone : Time.Zone
    , results : List Outcome
    , hovering : List (CI.One Outcome CI.Dot)
    }

type Msg
    = NoOp
    | GetCurrentTime
    | SetCurrentTime (YearMonth, Time.Zone)
    | SubmitForm
    | SetStartingAmount String
    | SetLumpSum String
    | SetMonthlyContribution String
    | SetInterestRate String
    | SetContributionStart String
    | SetContributionEnd String
    | SetPaymentStart String
    | SetPaymentSecondMonth String
    | SetPaymentAmount String
    | SetSimulationEnd String
    | OnHover (List (CI.One Outcome CI.Dot))


init : () -> (Model, Cmd Msg)
init _ =
    (
    { starting_amount = "95000"
    , contribution_lump_sum = "0"
    , contribution_start_yearmon = "3/2022"
    , contribution_end_yearmon = "7/2024"
    , contribution_monthly = "5000"
    , percent_interest = "5.0"
    , payment_amount = "35000"
    , payment_start_yearmon = "8/2024"
    , payment_second_month = "1"
    , simulation_end_yearmon = "5/2028"  
    , current_yearmon = {year=2022,month=Time.Mar}
    , current_zone = Time.utc
    , results = []
    , hovering = []
    }
    , Task.perform SetCurrentTime (Task.map2 get_year_month Time.here Time.now)
    )


first_month : Parameters -> Outcome
first_month model =
    {
        month_number = 0,
        year = model.contribution_start_yearmon.year,
        month = model.contribution_start_yearmon.month,
        amount = model.starting_amount + model.contribution_lump_sum,
        contribution = 0,
        interest = 0,
        payment = 0
    }

interest_payment : Parameters -> Outcome -> Float
interest_payment model previous =
    if previous.amount <= 0 then
        0
    else
        previous.amount * (model.percent_interest / 100 / 12)

date_increment : YearMonth -> YearMonth
date_increment current =
    case current.month of
        Time.Jan -> {month=Time.Feb,year=current.year}
        Time.Feb -> {month=Time.Mar,year=current.year}
        Time.Mar -> {month=Time.Apr,year=current.year}
        Time.Apr -> {month=Time.May,year=current.year}
        Time.May -> {month=Time.Jun,year=current.year}
        Time.Jun -> {month=Time.Jul,year=current.year}
        Time.Jul -> {month=Time.Aug,year=current.year}
        Time.Aug -> {month=Time.Sep,year=current.year}
        Time.Sep -> {month=Time.Oct,year=current.year}
        Time.Oct -> {month=Time.Nov,year=current.year}
        Time.Nov -> {month=Time.Dec,year=current.year}
        Time.Dec -> {month=Time.Jan,year=current.year+1}

date_is_before : YearMonth -> YearMonth -> Bool
date_is_before current target =
    if current.year < target.year then
        True
    else if current.year == target.year then
        month_number current.month < month_number target.month
    else
        False

contribution_amount : Parameters -> Outcome -> Float
contribution_amount model previous =
    let
        current_yearmon = {year=previous.year,month=previous.month}
    in
        if date_is_before current_yearmon model.contribution_end_yearmon then
            model.monthly_contribution
        else
            0

payment_amount : Parameters -> Outcome -> Float
payment_amount model previous =
    let
        next_yearmon = date_increment {year=previous.year,month=previous.month}
        payment_started = not (date_is_before next_yearmon model.withdrawal_start_yearmon)
    in
        if payment_started && (next_yearmon.month == model.withdrawal_start_yearmon.month || next_yearmon.month == model.withdrawal_second_month) then
            model.withdrawal_amount
        else
            0

next_month : Parameters -> Outcome -> Outcome
next_month model previous =
    let
        current_month_number = previous.month_number + 1
        interest = interest_payment model previous
        contribution = contribution_amount model previous
        payment = payment_amount model previous
        this_yearmon = date_increment {month=previous.month,year=previous.year}

    in
    { month_number = current_month_number
    , month = this_yearmon.month
    , year = this_yearmon.year
    , contribution = contribution
    , payment = payment
    , interest = interest
    , amount = previous.amount + interest + contribution - payment
    }

is_final_month : Parameters -> Outcome -> Bool
is_final_month model outcome =
    outcome.month == model.simulation_end_yearmon.month && outcome.year == model.simulation_end_yearmon.year

calculate_amounts : Parameters -> List Outcome -> List Outcome
calculate_amounts model outcomes =
    case outcomes of
        [] ->
            let
                initial_outcome = first_month model

                first =
                    next_month model initial_outcome
            in
                calculate_amounts model [ { first | contribution = first.contribution + model.contribution_lump_sum} ]

        x :: xs ->
            let
                next = next_month model x
            in
                if is_final_month model next then
                    List.reverse (next :: x :: xs)
                else
                    calculate_amounts model (next :: x :: xs)

final_amount : List Outcome -> Float
final_amount outcomes =
    case outcomes of
        x::[] -> x.amount
        _::xs -> final_amount(xs)
        [] -> 0

-- UPDATE

type alias YearMonth =
    { year : Int
    , month : Time.Month
    }
get_year_month : Time.Zone -> Time.Posix -> (YearMonth, Time.Zone)
get_year_month zone posix_time = 
    ({
        year = (Time.toYear zone posix_time)
        , month = (Time.toMonth zone posix_time)
    }, zone)

get_parameters : Model -> Parameters
get_parameters model =
    let
        starting_amount = String.toFloat model.starting_amount
        contribution_lump_sum = String.toFloat model.contribution_lump_sum
        contribution_monthly = String.toFloat model.contribution_monthly
        percent_interest = String.toFloat model.percent_interest
        contribution_start = parse_yearmon model.contribution_start_yearmon
        contribution_end = parse_yearmon model.contribution_end_yearmon
        payment_start = parse_yearmon model.payment_start_yearmon
        payment_second_month = Result.andThen number_to_month (Result.fromMaybe "Error parsing Month" (String.toInt model.payment_second_month))
        payment = String.toFloat model.payment_amount
        simulation_end = parse_yearmon model.simulation_end_yearmon
    in
        { starting_amount = Maybe.withDefault 0 starting_amount
        , contribution_lump_sum = Maybe.withDefault 0 contribution_lump_sum
        , contribution_start_yearmon = Result.withDefault {year=2022,month=Time.Mar} contribution_start
        , contribution_end_yearmon = Result.withDefault {year=2022,month=Time.Mar} contribution_end
        , monthly_contribution = Maybe.withDefault 0 contribution_monthly
        , percent_interest = Maybe.withDefault 0 percent_interest
        , withdrawal_start_yearmon = Result.withDefault {year=2022,month=Time.Mar} payment_start
        , withdrawal_second_month = Result.withDefault Time.Mar payment_second_month
        , withdrawal_amount = Maybe.withDefault 0 payment
        , simulation_end_yearmon = Result.withDefault {year=2022,month=Time.Mar} simulation_end
        }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        NoOp ->
            (model, Cmd.none)

        GetCurrentTime ->
            (model, Task.perform SetCurrentTime (Task.map2 get_year_month Time.here Time.now))

        SetCurrentTime ymt ->
            let 
                year_month = Tuple.first ymt
                zone = Tuple.second ymt
            in
                ({ model | current_yearmon = year_month, current_zone=zone }, Cmd.none)

        SubmitForm ->
            let
                params = get_parameters model
            in
                ({model | results = calculate_amounts params []}, Cmd.none)

        SetStartingAmount a ->
            ({ model | starting_amount = a }, Cmd.none)

        SetLumpSum a ->
            ({ model | contribution_lump_sum = a }, Cmd.none)

        SetMonthlyContribution a ->
            ({ model | contribution_monthly = a }, Cmd.none)

        SetInterestRate a ->
            ({ model | percent_interest = a }, Cmd.none)

        SetContributionStart a ->
            ({ model | contribution_start_yearmon = a }, Cmd.none)

        SetContributionEnd a ->
            ({ model | contribution_end_yearmon = a }, Cmd.none)

        SetPaymentStart a ->
            ({ model | payment_start_yearmon = a }, Cmd.none)

        SetPaymentSecondMonth a ->
            ({ model | payment_second_month = a }, Cmd.none)

        SetPaymentAmount a ->
            ({ model | payment_amount = a }, Cmd.none)

        SetSimulationEnd a ->
            ({ model | simulation_end_yearmon = a }, Cmd.none)

        OnHover hovering ->
            ({ model | hovering = hovering }, Cmd.none)

-- VIEW


view : Model -> Html Msg
view model =
    div [ style "padding" "1em" ]
        [ view_parameters model
        , view_results model
        ]

month_number : Time.Month -> Int
month_number month = 
    case month of
        Time.Jan -> 1
        Time.Feb -> 2
        Time.Mar -> 3
        Time.Apr -> 4
        Time.May -> 5
        Time.Jun -> 6
        Time.Jul -> 7
        Time.Aug -> 8
        Time.Sep -> 9
        Time.Oct -> 10
        Time.Nov -> 11
        Time.Dec -> 12

number_to_month : Int -> Result String Time.Month
number_to_month mn =
    case mn of
        1 -> Ok Time.Jan
        2 -> Ok Time.Feb
        3 -> Ok Time.Mar
        4 -> Ok Time.Apr
        5 -> Ok Time.May
        6 -> Ok Time.Jun
        7 -> Ok Time.Jul
        8 -> Ok Time.Aug
        9 -> Ok Time.Sep
        10 -> Ok Time.Oct
        11 -> Ok Time.Nov
        12 -> Ok Time.Dec
        _ -> Err "Month number needs to be between 1 and 12"

create_yearmon : Time.Month -> Int -> YearMonth
create_yearmon month year =
    {month=month,year=year}

parse_yearmon : String -> Result String YearMonth
parse_yearmon date_str =
    let
        date_list = String.split "/" date_str
    in
        case date_list of
            m::y::[] ->
                let
                    month_result = Result.andThen number_to_month (Result.fromMaybe "Error parsing Month" (String.toInt m))
                    year_result = Result.fromMaybe "Error parsing year" (String.toInt y)
                in
                    Result.map2 create_yearmon month_result year_result
            _ -> Err "Cannot parse date string"
                    
year_mon_format : Time.Month -> Int -> String
year_mon_format month year =
    (String.fromInt (month_number month)) ++ "/" ++ (String.fromInt year)    

label_style : List (Attribute msg)
label_style = [ style "width" "20em"
              , style "display" "inline-block"
              , style "text-align" "right"
              , style "margin-right" "0.5em"
              , style "font-weight" "bold" ]

view_parameters : Model -> Html Msg
view_parameters model =
    div [  ]
        [ h1 [] [ text "College Cost Calculator" ]
        , p [] [ text "Enter saving and expense parameters below and press 'calculate'" ] 
        , div []
            [ label label_style [ text "Starting Amount:" ] 
            , input [ type_ "text"
                    , placeholder "Starting Amount"
                    , onInput SetStartingAmount
                    , value model.starting_amount
                    ] []                
            , br [] []
            , label label_style [ text "Lump Sum Contribution:" ] 
            , input [ type_ "text"
                    , onInput SetLumpSum
                    , value model.contribution_lump_sum
                    ] []                
            , br [] []
            , label label_style [ text "Monthly Contribution: "]
            , input [ type_ "text"
                    , placeholder "Monthly Contribution"
                    , onInput SetMonthlyContribution
                    , value model.contribution_monthly
                    ] []
            , br [] []
            , label label_style [ text "Rate of return: "]
            , input
                [ type_ "text"
                , placeholder "Rate of return"
                , onInput SetInterestRate
                , value model.percent_interest
                ] []
            , br [] []
            ]
        , div []
            [ label label_style [ text "Contribution Start Date (MM/YYYY):" ]
            , input
                [ type_ "text"
                , onInput SetContributionStart
                , value model.contribution_start_yearmon
                ] []    
            , br [] []
            , label label_style [ text "Contribution End Date (MM/YYYY):" ]
            , input
                [ type_ "text"
                , onInput SetContributionEnd
                , value model.contribution_end_yearmon
                ] []    
            , br [] []
            , label label_style [ text "Month Payments Start (MM/YYYY):" ]
            , input
                [ type_ "text"
                , onInput SetPaymentStart
                , value model.payment_start_yearmon
                ] []                    
            , br [] []
            , label label_style [ text "Second Payment Month (MM):" ]
            , input
                [ type_ "text"
                , onInput SetPaymentSecondMonth
                , value model.payment_second_month
                ] []                     
            , br [] []
            , label label_style [ text "Payment Amount (Per semester):" ]
            , input
                [ type_ "text"
                , onInput SetPaymentAmount
                , value model.payment_amount
                ] []
            , br [] []
            , label label_style [ text "Simulaion End (MM/YYYY):" ]
            , input
                [ type_ "text"
                , onInput SetSimulationEnd
                , value model.simulation_end_yearmon
                ] []                          
            ]
        , div []
            [ button [ onClick SubmitForm ] [ text "Calculate" ]
            ]
        ]


outcome_item : Outcome -> Html Msg
outcome_item outcome =
    tr [] [
        td [style "text-align" "right"] [
            text (String.fromInt outcome.month_number)
        ]
      , td [style "text-align" "right"] [
            text (year_mon_format outcome.month outcome.year)
        ]
      , td [style "text-align" "right"] [
            let 
                amt_style = if outcome.amount < 0 then [style "color" "red"] else []
            in
                span amt_style [ text ("$" ++ (format usLocale outcome.amount)) ]
            
        ]
      , td [style "text-align" "right"] [
          if outcome.contribution > 0 then
              text ("$" ++ (format usLocale outcome.contribution))
          else text ""
      ]
      , td [style "text-align" "right"] [
          text ("$" ++ (format usLocale outcome.interest))
      ]
      , td [style "color" "red"
           ,style "text-align" "right" ] [
          if outcome.payment > 0 then
              text ("$" ++ (format usLocale outcome.payment))
          else
              text ""
      ]

    ]


view_results : Model -> Html Msg
view_results model =
    if List.isEmpty model.results then
        div [] []
    else
        div []
            [ h2 [] [ text "Results" ]
            , p [] [ text ("At the end, you will have " ++ (FormatNumber.format usLocale (final_amount model.results)))]
            , div [ style "height" "250px", style "width" "850px", style "padding-left" "100px"
                  , style "margin-bottom" "30px" ] [ 
              view_chart model ]
            , let 
                headers = tr [] [
                    th [] [ text "#" ]
                    , th [style "width" "5em"] [ text "Date" ]
                    , th [style "width" "8em"] [ text "Balance" ]
                    , th [style "width" "7em"] [ text "Contribution" ]
                    , th [style "width" "7em"] [ text "Interest" ]
                    , th [style "width" "7em"] [ text "Payment" ] ]
                rows = List.map outcome_item model.results
            in
                table [] (headers::rows)
            ]


outcome_to_time : Model -> Outcome -> Float
outcome_to_time model outcome =
    let posix = Time.partsToPosix model.current_zone (Time.Parts outcome.year outcome.month 1 0 0 0 0)
    in toFloat (Time.posixToMillis posix)
view_chart : Model -> Html Msg
view_chart model =
  C.chart
    [ CA.height 200
    , CA.width 800
    , CA.padding { top = 10, bottom = 5, left = 10, right = 10 }
    , CE.onMouseMove OnHover (CE.getNearest CI.dots)
    , CE.onMouseLeave (OnHover [])
    ]
    [ C.xAxis []
    , C.xTicks [ CA.times model.current_zone, CA.amount (List.length model.results) ]
    , C.xLabels [ CA.times model.current_zone, CA.withGrid, CA.rotate 90, CA.fontSize 10, CA.amount 24 ]
    , C.yAxis []
    , C.yLabels [ CA.withGrid
                , CA.fontSize 10
                , CA.format (\num -> "$" ++ FormatNumber.format usLocale num) ]
    , C.series (outcome_to_time model)
        [ C.interpolated .amount [ CA.width 1 ] [ CA.circle, CA.size 2 ]
            |> C.named "Balance"
        , C.interpolated .payment [ CA.stepped ] []
            |> C.named "Payment"
        ]
        model.results
    , C.each model.hovering <| \p dot ->
        let
            amount = CI.getY dot
            name = CI.getName dot
        in
            [C.tooltip dot [] 
                [] 
                [ text (name ++ ": $")
                , text (FormatNumber.format usLocale amount) ]
            ]
    ]