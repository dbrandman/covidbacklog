port module Main exposing (..)

{-
Covid backlog modeler

Written David Brandman, May 2020. 

Manuscript by:
David M. Brandman1, Erika Leck1, Sean Christie1

1. Division of Neurosurgery (Department of Surgery), Dalhousie University


See "modelAssumptions" for details about how the model works
For a definition and description of these variables, look at parameterInfo function. The goal was to make the project self-documenting.

-----------------
-- Implementation
-----------------

The meat of the parameters in the Model is a Parameters record. This contains all of the relevant modeling variables. The model contains a List Parameters entry; each Parameters record is corresponds to a time epoch. The baseline epoch is always 0 and cannot be deleted. The remaining Parameters records are all representing temporary epochs of time when variables are added or removed.

Baseline and epochs both are Parameters records, so they contain all of the same variables. But, the list of variables presented to the user are different. The list that can be changed by the user are contained in the baselineParametersList and slowdownParametersList. 

The meat of the code can be found in runTimestep. runSimulation initializes the model appropriately so that it can be fed into runTimestep. The goal of runTimestep is to move patients from one category to another. It does so based on the Parameters record. runTimestep knows whether the current timestep is within "baseline" or during an "epoch" and then gets the appropriate sets of parameters.  

I took a slightly unconventional approach to setting variables. There is a single setSimulationParameter function rather then many smaller setter functions which is more standard in elm. I did this to make it easier to add new variables of interest, since it minimized new function creation.

-----------------
-- Adding a new parameter
-----------------

1. Add it to the Parameters record. Anything the user can edit should be a Maybe type 
2. Be sure to give it initial values in both initEpoch and initBaseline
3. Add a new type in Parameter type corresponding to the variable
4. Add a new branch into parameterInfo
5. Add a new branch to setSimulationParameter
6. Add it to slowdownParametersList or baselineParametersList so that it is displayed

-----------------
-- Patient flow (State)
-----------------

A person falls into one of 4 categories:
nPreClinic : Number of people waiting to see a surgeon
nPostClinic : Number of people who have seen a surgeon 
nPreSurgery : Number of people waiting for surgery 
nPostSurgery : Number of people who have had operations

Patients who are backlogged are added to the nBacklogged list. They are moved to the nPreClinic category every timestep after things return to baseline.

Mentioned before about running simulation that at the bottom of the simulation it would give 
an estimated date to address the COVID backlog 

It's easy for the program to determine a trajectory based on the pre-covid stats. And then look at waht the impact of the COVID. If you get back to your pre-covid trajectory by such and such. 

1. The fields that we have on th eleft are good for back-of-the-envelope. In the mnaucsript, just be a bit more clear that people can use this not only to see waht their situation is, but to do very basic changes, mostof which, would coud claim, could be within the practitioner's capabiilites (to some extent). Using this system to predict the various options that we have is very useful. But be more strong with the paper. 

2. Supplementary information under the graph: we can calculate a predicted T1 or T2. T1 is the wait-time referral to see specialist. T2 is from clinic to surgery. Wait time doesn't distinguish between different entitites. 



-}


import Browser
import Browser.Dom

import Html exposing (..) 
import Html.Attributes exposing ( attribute, style, src, placeholder, type_, href, rel, class, value , classList , id)
import Html.Events exposing (onClick, onInput, onCheck)

import Task

import Date exposing (Date, Unit(..), Interval(..)) 
import Time exposing (Month(..))

import Json.Encode
import Json.Decode

import Process

import List.Extra

-- This library is used for convenient HTML rendering
import Markdown
import Markdown.Config exposing (Options, defaultOptions)


---------------------------------------------
-- Main
---------------------------------------------

main =
  Browser.element
    { init          = init
    , update        = update
    , subscriptions = subscriptions
    , view          = view
    }

subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none

---------------------------------------------
-- Ports and commands
---------------------------------------------

-- Send the information to Javascript
port toJS_plotly : List Json.Encode.Value -> Cmd msg

portJson : Model -> Cmd msg
portJson model =
    [ model |> plotlyJsonData
    , model |> plotlyJsonLayout
    ]
    |> toJS_plotly

---------------------------------------------
-- Model
---------------------------------------------

-- For a definition and description of these variables, look at parameterInfo function below
type alias Parameters =
    { name                     : String
    , nPreClinic               : Maybe Int
    , nPreSurgery              : Maybe Int

    , nSurgeons                : Maybe Int
    , nClinicsRate             : Maybe Float 
    , nConsultsPerClinic       : Maybe Int
    , nSurgicalDaysRate        : Maybe Float
    , nCasesPerSurgicalDay     : Maybe Int
    , percentSurgical          : Maybe Int
    , percentBumped            : Maybe Int

    , nConsultsRate            : Maybe Int
    , nPostCovidSpread         : Maybe Int
    , startDate                : Date
    , endDate                  : Date
    }


-- Keep a running tally of the values that are relevant
type alias State =
    { nPreClinic   : List Int
    , nPostClinic  : List Int
    , nPreSurgery  : List Int
    , nPostSurgery : List Int
    , nBacklogged  : List Int
    }

type alias Model = 
    { parameters  : List Parameters -- Recall: entry 0 is baseline, all others are epochs
    , state       : State           -- List of (x,y) coordinates for the model based on parameters
    , currentTab  : Tabs            -- which tab is currently being displayed
    , burgerFlag  : Bool            -- Do we open the burger
    , figureWidth : Float           -- What is the width of the figure we should display
    }

-- A convenience record that contains the information pretaining to a variable
-- The values of this are displayed to the user
type alias ParameterInfo =
    { value       : Maybe String
    , name        : String
    , description : String
    , valueType   : String
    }


initEpoch : Parameters
initEpoch =
    { name                     = ""
    , nPreClinic               = Nothing
    , nPreSurgery              = Nothing

    , nSurgeons                = Just 10
    , nClinicsRate             = Just 1.0
    , nConsultsPerClinic       = Just 1
    , nSurgicalDaysRate        = Just 1.0
    , nCasesPerSurgicalDay     = Just 2
    , percentSurgical          = Just 20
    , percentBumped            = Just 5

    , nConsultsRate            = Just 10
    , nPostCovidSpread         = Nothing
    , startDate                = Date.fromCalendarDate 2020 Mar 1
    , endDate                  = Date.fromCalendarDate 2020 Jun 1

    }

initBaseline: Parameters 
initBaseline = 
    { name                     = "Baseline (pre-COVID)"
    , nPreClinic               = Just 500
    , nPreSurgery              = Just 100

    , nSurgeons                = Just 10
    , nClinicsRate             = Just 1.0
    , nConsultsPerClinic       = Just 4
    , nSurgicalDaysRate        = Just 8.0
    , nCasesPerSurgicalDay     = Just 2
    , percentSurgical          = Just 20
    , percentBumped            = Just 5

    , nConsultsRate            = Just 30
    , nPostCovidSpread         = Just 4
    , startDate                = Date.fromCalendarDate 2020 Jan 1
    , endDate                  = Date.fromCalendarDate 2021 Jan 1

    }

initState : Parameters -> State 
initState p =
    { nPreClinic   = List.singleton <| Maybe.withDefault 0 p.nPreClinic
    , nPostClinic  = List.singleton 0
    , nPreSurgery  = List.singleton <| Maybe.withDefault 0 p.nPreSurgery
    , nPostSurgery = List.singleton 0
    , nBacklogged  = List.singleton 0
    } 

emptyState : State
emptyState =
    { nPreClinic   = List.singleton 0
    , nPostClinic  = List.singleton 0
    , nPreSurgery  = List.singleton 0
    , nPostSurgery = List.singleton 0
    , nBacklogged  = List.singleton 0
    } 


init : () -> (Model, Cmd Msg)
init _ = 
    ( { parameters = List.singleton initBaseline
      , state      = emptyState
      , currentTab = TabModel
      , burgerFlag = False
      , figureWidth = 700
      }
    , Cmd.none)



-----------------------------------------------
-- Msg and update
-----------------------------------------------

-- The Single page application is divided into tabs
-- To add a new tab, change displayContent and add it to the allTabs list

type Tabs
    = TabImplementation
    | TabModel
    | TabAssumptions

type Parameter  
    = Name
    | PreClinic                
    | PostClinic
    | PreSurgery               
    | PostSurgery              
    | Surgeons                 
    | ClinicsRate              
    | ConsultsPerClinic        
    | SurgicalDaysRate         
    | CasesPerSurgicalDay      
    | PercentSurgical          
    | PercentBumped            
    | ConsultsRate     
    | PostCovidSpread
    | StartDate
    | EndDate
    | Backlogged  

type Msg 
    = SetParameter Int Parameter String
    | SetState Parameter Int
    | RunSimulation 
    | ChangeTabs Tabs
    | ToggleBurger
    | AddEpoch
    | RemoveEpoch Int
    | GetColumnSize (Result Browser.Dom.Error Browser.Dom.Element)
    | Port

update : Msg -> Model -> ( Model , Cmd Msg )
update msg model =
    case msg of

        SetParameter ind parameter stringValue ->
            (setSimulationParameter ind parameter stringValue model, Cmd.none)

        SetState parameter intValue ->
            (setState parameter intValue model, Cmd.none)

        RunSimulation ->
            (runSimulation model , portCommand)

        ChangeTabs tab ->
            (changeTab tab model, Cmd.none)

        ToggleBurger ->
            (toggleBurger model, Cmd.none)

        AddEpoch ->
            (addEpoch model, Cmd.none)

        RemoveEpoch ind ->
            (removeEpoch ind model, Cmd.none)

        GetColumnSize result ->
            (setFigureWidth result model, Cmd.none)

        Port ->
            (model, portJson model)

-- This looks ugly but I couldn't figure out a better way to do this
-- If you don't do this delay runaround, then elm will fire the command
-- prior to the model being updated. 
-- https://github.com/elm/compiler/issues/1776
-- This is the workaround provided by evancz.
portCommand : Cmd Msg
portCommand =
    Process.sleep 1 
    |> Task.andThen (\_ -> Task.succeed Port)
    |> Task.perform (\_ -> Port)

-- Rather than making a ton of little setter functions, I chose to consolidate all of the
-- setting logic in one place. I think this makes things less error prone and means
-- the cost to adding a new variable to the model is lowered
-- When it comes to setting the startDate and endDate: we consider the following cases
-- Are you valid?
-- If you're the baseline, then set the date with impunity
-- If you're note the baseline, then make sure you don't come before/after the start/end Date of baseline

setSimulationParameter : Int -> Parameter -> String -> Model -> Model
setSimulationParameter ind parameter val model =
    let

        int : String -> Maybe Int
        int valString =
            valString
            |> String.toInt
            |> Maybe.andThen (\a -> if a < 0 then Nothing else Just (abs a))

        float : String -> Maybe Float
        float valString = 
            valString
            |> String.toFloat
            |> Maybe.andThen (\a -> if a < 0 then Nothing else Just (abs a))


        newParameters : Maybe Parameters
        newParameters =
            case List.Extra.getAt ind model.parameters of
                Nothing -> Nothing
                Just sp ->
                  case parameter of
                     Name                -> Just {sp | name                 =     val}
                     PreClinic           -> Just {sp | nPreClinic           = int val}
                     PreSurgery          -> Just {sp | nPreSurgery          = int val}
                     Surgeons            -> Just {sp | nSurgeons            = int val}
                     ClinicsRate         -> Just {sp | nClinicsRate         = float val}
                     ConsultsPerClinic   -> Just {sp | nConsultsPerClinic   = int val}
                     SurgicalDaysRate    -> Just {sp | nSurgicalDaysRate    = float val}
                     CasesPerSurgicalDay -> Just {sp | nCasesPerSurgicalDay = int val}
                     PercentSurgical     -> Just {sp | percentSurgical      = int val}
                     PercentBumped       -> Just {sp | percentBumped        = int val}
                     ConsultsRate        -> Just {sp | nConsultsRate        = int val}
                     PostCovidSpread     -> Just {sp | nPostCovidSpread     = int val}

                     StartDate ->
                         case (Date.fromIsoString val |> Result.toMaybe) of
                             Nothing -> Just sp
                             Just date -> 
                                 if ind == 0 then
                                     Just {sp | startDate = date}
                                 else if Date.compare date (baselineParameters model).startDate == LT then
                                     Just {sp | startDate = (baselineParameters model).startDate}
                                 else
                                     Just {sp | startDate = date}

                     EndDate ->
                         case (Date.fromIsoString val |> Result.toMaybe) of
                             Nothing -> Just sp
                             Just date -> 
                                 if ind == 0 then
                                     Just {sp | endDate = date}
                                 else if Date.compare date (baselineParameters model).endDate == GT then
                                     Just {sp | endDate = (baselineParameters model).endDate}
                                 else
                                     Just {sp | endDate = date}

                     _ -> Nothing
                 
    in
        case newParameters of
            Just justNewParameters ->
                {model | parameters = List.Extra.setAt ind justNewParameters model.parameters }

            Nothing -> model


setState : Parameter -> Int -> Model -> Model
setState parameter val model =
    let 
        state : State
        state = model.state 

        newState : State
        newState =
            case parameter of
                 PreClinic   -> {state | nPreClinic   = val :: state.nPreClinic}
                 PostClinic  -> {state | nPostClinic  = val :: state.nPostClinic}
                 PreSurgery  -> {state | nPreSurgery  = val :: state.nPreSurgery}
                 PostSurgery -> {state | nPostSurgery = val :: state.nPostSurgery}
                 Backlogged  -> {state | nBacklogged  = val :: state.nBacklogged}
                 _           ->  state
    in
        {model | state = newState}


-- This initializes the relevant data to execute runTimestep, a total of nSteps times

runSimulation : Model -> Model
runSimulation model =
    let
        initialState : State
        initialState =
            baselineParameters model
            |> initState

        initializedModel : Model
        initializedModel  =
            {model | state = initialState}

        nSteps : Int
        nSteps =
            Date.diff Weeks
                (model |> baselineParameters |> .startDate)
                (model |> baselineParameters |> .endDate)
    in
        List.foldl 
            (\a -> runTimestep a) 
            initializedModel
            (List.range 0 nSteps)

changeTab : Tabs -> Model -> Model
changeTab tab model =
    {model | currentTab = tab, burgerFlag = False}

toggleBurger : Model -> Model
toggleBurger model =
    {model | burgerFlag = not model.burgerFlag}

addEpoch : Model -> Model
addEpoch model =
    {model | parameters = List.append model.parameters (List.singleton initEpoch)}

removeEpoch : Int -> Model -> Model
removeEpoch ind model =
    {model | parameters = List.Extra.removeAt ind model.parameters}

setFigureWidth : (Result Browser.Dom.Error Browser.Dom.Element) -> Model -> Model
setFigureWidth result model =
    case result of
        Ok element -> {model | figureWidth = element.element.width}
        Err _ -> model

-----------------------------------------------
-- State function
-----------------------------------------------

runTimestep : Int -> Model -> Model 
runTimestep step model =
    let

        baseline : Parameters
        baseline = 
            List.Extra.getAt 0 model.parameters
            |> Maybe.withDefault initBaseline
            
        -- The question we want to answer is: does the current step fit within an epoch?
        -- Base case: we have exceeded the length of the model.parameters, so we use the baseline
        -- Otherwise, we check to see if the date corresponding to the timestep is within the
        -- start and end dates. If yes, then that's the ind we return. Otherwise, check the next ind

        epochInd : Int -> Int
        epochInd ind =
            let
                stepDate : Date
                stepDate = 
                    Date.add Weeks step baseline.startDate 

            in
                case List.Extra.getAt ind model.parameters of
                    Nothing -> 0
                    Just cp ->
                        if Date.isBetween cp.startDate cp.endDate stepDate then
                            ind
                        else
                            epochInd (ind+1)

        epoch : Parameters
        epoch =
            List.Extra.getAt (epochInd 1) model.parameters
            |> Maybe.withDefault initEpoch

        nPreClinic   = Maybe.withDefault 0 <| List.head model.state.nPreClinic
        nPostClinic  = Maybe.withDefault 0 <| List.head model.state.nPostClinic
        nPreSurgery  = Maybe.withDefault 0 <| List.head model.state.nPreSurgery
        nPostSurgery = Maybe.withDefault 0 <| List.head model.state.nPostSurgery
        nBacklogged  = Maybe.withDefault 0 <| List.head model.state.nBacklogged

        epochSurgeons            = Maybe.withDefault 0 epoch.nSurgeons |> toFloat
        epochConsultsRate        = Maybe.withDefault 0 epoch.nConsultsRate
        epochClinicsRate         = Maybe.withDefault 0.0 epoch.nClinicsRate
        epochConsultsPerClinic   = Maybe.withDefault 0 epoch.nConsultsPerClinic |> toFloat
        epochCasesPerSurgicalDay = Maybe.withDefault 0 epoch.nCasesPerSurgicalDay
        epochSurgicalDaysRate    = Maybe.withDefault 0.0 epoch.nSurgicalDaysRate

        baselinePercentSurgical  = Maybe.withDefault 0 baseline.percentSurgical
        baselineConsultsRate     = Maybe.withDefault 0 baseline.nConsultsRate
        baselinePostCovidSpread  = Maybe.withDefault 0 baseline.nPostCovidSpread



        -- How many new consults were not seen in this timestep. Is zero if epoch is baseline
        -- For it to be a true slowdown, ensure that epochConsults < baselineConsults
        newBacklogged : Int
        newBacklogged =
            max 0 (baselineConsultsRate - epochConsultsRate)

        -- If we are in a baseline, then add an exponential portion of nBacklogged into the system
        dumpedBacklogged : Int
        dumpedBacklogged = 
            if (epochInd 1 ) == 0 then
                (nBacklogged // baselinePostCovidSpread)
            else
                0

        -- The difference in the number of people waiting to be seen in clinic this timestep
        -- Is the epoch's new consult rate minute whoever gets seen in clinic, plus the dump
        preClinicDiff : Int
        preClinicDiff = 
            epochConsultsRate - postClinicDiff + dumpedBacklogged

        -- The number of people seen in clinic is nSurgeons * new people per clinic * num new consults
        postClinicDiff : Int
        postClinicDiff =
            min 
                (nPreClinic) 
                (round (epochSurgeons * epochClinicsRate * epochConsultsPerClinic))

        -- The number of new people who are waiting for surgery is the fraction of people who 
        -- would benefit from surgery who were seen in clinic minus those who got surgery
        preSurgeryDiff : Int
        preSurgeryDiff =
            ((postClinicDiff * baselinePercentSurgical) // 100 - postSurgeryDiff)
            
        -- The number of people who got surgery is the minimum of the number of cases that could go
        -- and the number of cases waiting for surgery
        postSurgeryDiff : Int
        postSurgeryDiff = 
            if nPreSurgery > 0 then
                min (nPreSurgery) (round ((toFloat epochCasesPerSurgicalDay * epochSurgicalDaysRate)))
            else
                0

    in
        model
        |> setState PreClinic   ( max 0 (nPreClinic   + preClinicDiff))
        |> setState PostClinic  ( max 0 (nPostClinic  + postClinicDiff))
        |> setState PreSurgery  ( max 0 (nPreSurgery  + preSurgeryDiff))
        |> setState PostSurgery ( max 0 (nPostSurgery + postSurgeryDiff))
        |> setState Backlogged  ( max 0 (nBacklogged  + newBacklogged - dumpedBacklogged))


-----------------------------------------------
-- View
-----------------------------------------------

view : Model -> Html Msg
view model = 
    main_ [] 
    [ displayHero model
    , displayBurger model
    , displayContent model
    ]

displayHero : Model -> Html Msg
displayHero model = 
    section [class "hero is-primary is-small"] 
    [ div [class "hero-body"] 
      [ div [class "container"] 
        [ h1 
          [ class "title"] 
          [ text "COVID backlog modeler" ]
        , h2 
          [class "subtitle"]
          [text "Version 0.2.1"] 
        ]
      ]
    ]

displayBurger : Model -> Html Msg
displayBurger model = 
    let
        displaySingleTab : Tabs -> Html Msg
        displaySingleTab thisTab =
            a [ class "navbar-item", onClick (ChangeTabs thisTab) ]
              [ text (tabString thisTab) ]

    in
        nav [ class "navbar"]
        [ div [ class "container" ]
          [ div [ class "navbar-brand" ]
            [ a [ class "navbar-item" , attribute "style" "font-weight:bold;" ]
              [ text (tabString model.currentTab)]
            , span [ classList [ ( "navbar-burger burger", True)
                   , ("is-active", model.burgerFlag)
                   ]
                   , onClick ToggleBurger ]
              [ span [] []
              , span [] []
              , span [] []
              ]
            ]
          , div 
            [ classList [ ("navbar-menu", True)
                        , ("is-active", model.burgerFlag) ]
            , id "navMenu" ]
            [ div [ class "navbar-end" ]
              <| List.map displaySingleTab allTabsList
            ]
          ]
        ]


displayContent : Model -> Html Msg
displayContent model =
    let
        content : Html Msg
        content =
            case model.currentTab of
                TabImplementation -> displayImplementation model
                TabModel          -> displayModel model
                TabAssumptions    -> displayAssumptions model
    in
        div [class "section"]
        [ div [class "container"]
          [ content ]
        ]

displayImplementation : Model -> Html Msg
displayImplementation model =
    div [class "container"] <| renderHTML modelImplementation  


displayAssumptions : Model -> Html Msg
displayAssumptions model =
    div [class "container"] <| renderHTML modelAssumptions  

displayModel : Model -> Html Msg
displayModel model =
    div [class "columns"]
    [ div [class "column is-one-third"]
      [ displayAddEpochButton model
      , displayParameters model ]
    , div [class "column is-two-thirds", id "rightColumn"] -- id here is used for getting size of DOM element
      [ displayFigure model ]
    ]
    
displayAddEpochButton : Model -> Html Msg
displayAddEpochButton model =
    button 
    [ class "button"
    , class "is-info"
    , onClick AddEpoch
    ] 
    [text "Add Epoch"
    ]

displayFigure : Model -> Html Msg
displayFigure model =
    div [class "isFixed"] -- Custom css entry defined in index.html
    [ button 
      [ class "button is-info"
      , onClick RunSimulation
      , Html.Attributes.disabled <| not (areAllParametersValid model)
      ] 
      [text "Run Simulation"]
    , div [id "plotly-div"] []
    ]


displayParameters : Model -> Html Msg
displayParameters model =
    let
        inputClass : ParameterInfo -> String
        inputClass pInfo =
            case pInfo.value of
                Nothing -> "input is-danger"
                Just _ -> "input"

        displaySingleInput :  Parameter -> Int -> ParameterInfo -> Html Msg
        displaySingleInput parameter ind pInfo =
            div [class "field"]
            [ label [class "label"] [text pInfo.name]
            , div [class "control"]
              [ input 
                [ class (inputClass pInfo)
                , value (Maybe.withDefault "" pInfo.value)
                , attribute "type" pInfo.valueType
                , onInput (SetParameter ind parameter)
                ] []
              ]
            , p [class "help"] [text pInfo.description]
            ]

        justParameters : Int -> Parameters
        justParameters ind =
            List.Extra.getAt ind model.parameters
            |> Maybe.withDefault initBaseline

        displayBaselineParameters : Html Msg
        displayBaselineParameters =
            article [class "message"]
            [ div [class "message-header"]
              [ p [] [text "Baseline (pre-COVID)"] ]
            , div [class "message-body"] 
                <| List.map  
                    (\a -> displaySingleInput a 0 (parameterInfo a (justParameters 0)))
                    baselineParametersList
            ]

        displayEpochs : Int -> Html Msg
        displayEpochs ind = 
            article [class "message is-primary"]
            [ div [class "message-header"]
              [ p [] [text (justParameters ind |> .name)]
              , button 
                [ class "delete"
                , attribute "aria-label" "delete"
                , onClick (RemoveEpoch ind)
                ] []
              ]
            , div [class "message-body"] 
                <| List.map  
                    (\a -> displaySingleInput a ind (parameterInfo a (justParameters ind)))
                    slowdownParametersList
            ]
                  
    in 
        div [] 
        [ div [] <| List.map displayEpochs <| List.reverse <| List.range 1 (List.length model.parameters - 1)
        , div [] [displayBaselineParameters]
        ]

-----------------------------------------------
-- Helpers
-----------------------------------------------

-- isValid goes through a list of all parameters, maps them to the parameterInfo and sees
-- if none of them are Nothing. Then we go through all of the model.parameters
areAllParametersValid : Model -> Bool
areAllParametersValid model =
    let
        isValid : Parameters -> List Parameter -> Bool
        isValid parameters parametersList =
            List.map (\a -> parameterInfo a parameters) parametersList 
            |> List.map .value 
            |> List.all (\a -> a /= Nothing)  -- Check if all .value is Nothing

        isBaselineValid : Bool
        isBaselineValid =
            isValid (baselineParameters model) baselineParametersList

        isSlowdownValid : Bool
        isSlowdownValid =
            List.map (\a -> isValid a slowdownParametersList) model.parameters
            |> List.all (\a -> a)

    in
        isBaselineValid && isSlowdownValid

baselineParameters : Model -> Parameters
baselineParameters model =
    List.Extra.getAt 0 model.parameters
    |> Maybe.withDefault initBaseline

-- What are we going to show in the burger?
allTabsList : List Tabs
allTabsList =
    [TabModel, TabAssumptions, TabImplementation]

-- What are we going to include as variables considered "baseline"
baselineParametersList : List Parameter
baselineParametersList =
    [ StartDate, EndDate, PreClinic, PreSurgery, ConsultsRate, Surgeons, ClinicsRate, ConsultsPerClinic, PostCovidSpread, SurgicalDaysRate, CasesPerSurgicalDay, PercentSurgical] 

-- What are we going to consider an epoch-specific variable
slowdownParametersList : List Parameter
slowdownParametersList =
    [ Name, StartDate, EndDate, ConsultsRate, Surgeons, ClinicsRate, ConsultsPerClinic, SurgicalDaysRate, CasesPerSurgicalDay] 


parameterInfo : Parameter -> Parameters -> ParameterInfo
parameterInfo p parameters =
    case p of
        Name ->
            { value = Just parameters.name
            , name = "Name" 
            , description = "Name describing the epoch"
            , valueType = "text"
            }

        PreClinic ->
            { value = parameters.nPreClinic |> Maybe.map String.fromInt
            , name = "Number of pre-existing consults" 
            , description = "Number of consults waiting to be seen by the surgical group at baseline"
            , valueType = "number"
            }
        PreSurgery ->
            { value = parameters.nPreSurgery |> Maybe.map String.fromInt
            , name = "Number of pre-surgical cases" 
            , description = "Number of people on the wait list for surgery"
            , valueType = "number"
            }
        ConsultsRate  ->
            { value =  parameters.nConsultsRate |> Maybe.map String.fromInt
            , name = "Consult rate" 
            , description = "Number of new consults that the surgical group gets per week"
            , valueType = "number"
            }
        PostCovidSpread ->
            { value =  parameters.nPostCovidSpread |> Maybe.map String.fromInt
            , name = "Slowdown reintroduction rate" 
            , description = "Number of weeks over which consults are introduced after a slowdown"
            , valueType = "number"
            }
        Surgeons ->
            { value = parameters.nSurgeons |> Maybe.map String.fromInt 
            , name =  "Surgeons" 
            , description = "Number of surgeons in the group available to see patients in clinic"
            , valueType = "number"
            }
        ClinicsRate ->
            { value =  parameters.nClinicsRate |> Maybe.map String.fromFloat
            , name =  "Clinic days per week" 
            , description = "Number of clinics that each surgeon holds per week"
            , valueType = "number"
            }
        ConsultsPerClinic ->
            { value =  parameters.nConsultsPerClinic |> Maybe.map String.fromInt
            , name = "New consults per clinic" 
            , description = "The number of new consults that a surgeon sees during a clinic" 
            , valueType = "number"
            }
        PercentSurgical ->
            { value =  parameters.percentSurgical |> Maybe.map String.fromInt
            , name = "Percent surgical" 
            , description = "The percentage of consults that a surgeon sees in clinic that require an operation"
            , valueType = "number"
            }
        SurgicalDaysRate ->
            { value =  parameters.nSurgicalDaysRate |> Maybe.map String.fromFloat
            , name = "OR days per week" 
            , description = "The total number of operating room lists per week for the surgical group" 
            , valueType = "number"
            }
        CasesPerSurgicalDay ->
            { value =  parameters.nCasesPerSurgicalDay |> Maybe.map String.fromInt
            , name = "Cases per day" 
            , description = "The average number of cases a surgeon within the group does per operative day"
            , valueType = "number"
            }
        StartDate ->
            { value = Just (Date.toIsoString parameters.startDate )
            , name = "Start Date" 
            , description = "Start date of time interval"
            , valueType = "date"
            }
        EndDate ->
            { value = Just (Date.toIsoString parameters.endDate )
            , name = "End Date" 
            , description = "End date of time interval" 
            , valueType = "date"
            }
        _ ->
            { value = Just ""
            , name = ""
            , description = ""
            , valueType = ""
            }


-- This is a helper that allows for rendering of text as HTML
renderHTML : String -> List (Html msg)
renderHTML theText =
    let
        myOptions : Options
        myOptions =
            { defaultOptions | rawHtml = Markdown.Config.ParseUnsafe}
    in
      Markdown.toHtml (Just myOptions) theText

tabString : Tabs -> String
tabString tab =
    case tab of
        TabImplementation -> "Implementation"
        TabModel          -> "Model"
        TabAssumptions    -> "Assumptions"



--------------------------------------------------
-- JSON output to plotlyjs
---------------------------------------------------

-- Build a JSON structure corresponding to data component of PLotly input
-- N.B. it expects an array

plotlyJsonData : Model -> Json.Encode.Value
plotlyJsonData model =
    let
        dateList : List String
        dateList = 
            Date.range Week 1 
                (model |> baselineParameters |> .startDate) 
                (model |> baselineParameters |> .endDate )
            |> List.map Date.toIsoString
            
        singlePlot : List Int -> String -> List (String, Json.Encode.Value)
        singlePlot data name =
            [ ("x", Json.Encode.list Json.Encode.string dateList)
            , ("y", Json.Encode.list Json.Encode.int (List.reverse data))
            , ("type"  , Json.Encode.string "date")
            , ("name" , Json.Encode.string name)
            ] 

    in
        List.map2 
            singlePlot 
            [model.state.nPreClinic, model.state.nPreSurgery, model.state.nPostSurgery]
            ["Referred", "Pre-surgery","Post-surgery"]
        |> Json.Encode.list Json.Encode.object 

-- Build a JSON structure corresponding to layout component of Plotly input
-- We want to make a shaded area for each epoch
-- Center the text between start and end dates of epochs
-- TODO: Why is the +1 required for offset for the shape?

plotlyJsonLayout : Model -> Json.Encode.Value
plotlyJsonLayout model =
    let
        singleShape : Parameters -> List (String, Json.Encode.Value)
        singleShape p =
            [ ("x0", Json.Encode.string (p.startDate |> Date.add Weeks 1 |> Date.toIsoString))
            , ("x1", Json.Encode.string (p.endDate   |> Date.add Weeks 1 |> Date.toIsoString))
            , ("y0", Json.Encode.float 0.05)
            , ("y1", Json.Encode.int 1)
            , ("type", Json.Encode.string "rect")
            , ("xref", Json.Encode.string "x")
            , ("yref", Json.Encode.string "paper")
            , ("opacity", Json.Encode.float 0.2)
            , ("fillcolor", Json.Encode.string "#d3d3d3")
            ]

        midDate : Parameters -> String
        midDate p = 
             Date.add 
                Weeks 
                ((Date.diff Weeks p.startDate p.endDate) // 2)
                p.startDate
            |> Date.toIsoString

        singleAnnotation: Int -> Parameters -> List (String, Json.Encode.Value)
        singleAnnotation ind p =
            [ ("x", Json.Encode.string <| midDate p)
            , ("y", Json.Encode.float (1.0 - 0.05*(toFloat (modBy 3 ind))))
            , ("text", Json.Encode.string p.name)
            , ("xref", Json.Encode.string "x")
            , ("yref", Json.Encode.string "paper")
            , ("showarrow", Json.Encode.bool False)
            , ("font", Json.Encode.object [("size", Json.Encode.int 14)])
            ]

        allShapes : Json.Encode.Value
        allShapes =
            List.drop 1 model.parameters
            |> List.map singleShape 
            |> Json.Encode.list Json.Encode.object

        allAnnotations : Json.Encode.Value
        allAnnotations =
            List.drop 1 model.parameters
            |> List.indexedMap singleAnnotation 
            |> Json.Encode.list Json.Encode.object

        xAxis : Json.Encode.Value
        xAxis = 
            [("title", Json.Encode.string "Date")
            ,("showgrid", Json.Encode.bool True)
            ,("zeroline", Json.Encode.bool True)
            ]
            |> Json.Encode.object

        yAxis : Json.Encode.Value
        yAxis = 
            [("title", Json.Encode.string "Number of Patients")
            ,("showgrid", Json.Encode.bool True)
            ,("zeroline", Json.Encode.bool True)
            ]
            |> Json.Encode.object


    in
        [ ("title", Json.Encode.string "COVID-backlog prediction")
        , ("shapes", allShapes)
        , ("annotations", allAnnotations)
        , ("xaxis", xAxis)
        , ("yaxis", yAxis)
        ]
    |> Json.Encode.object




--------------------------------------------------
-- Assumptions Text
---------------------------------------------------

modelAssumptions : String
modelAssumptions = """

<p class="is-size-3">Introduction</p> <BR>

The goal of this CovidBacklog model is to provide a back-of-the-envelope estimate of patient flow through a referral department. The model was originally designed to be used by a surgical service. Before using the model, make sure that the assumptions make sense for your clinical environment. 

<BR> <p class="is-size-3">Model setup</p> <BR>

The model assigns patients to being in one of three states:

<BR>

Referred : patients who have been referred and are waiting to see a surgeon in clinic <BR>
Pre-surgical : patients who have seen a surgeon and are waiting for an operation <BR>
Post-surgical : patients who have received their operations

<BR>

At the start of each week, newly referred patients are added to the pool. The number of patients seen by a surgeon are removed from the referred pool. A fraction of the patients seen by a surgeon are consented for surgery, and are then moved to the pre-surgical pool. Finally, a fraction of the pre-surgical patients undergo surgery and are moved to the post-surgical pool.

<BR>

The model recognizes two kinds of time epochs: a "baseline" epoch, and a "slowdown" epoch. The baseline epoch represents when the department is working at normal capacity. A slowdown epoch represents a defined period of time wherein the parameters are temporarily changed. For instance, in modeling the COVID crisis it's likely that there are less new referrals sent to the department, as well as less OR days per week. Multiple slowdown epochs can be added to the model.

<BR> <p class="is-size-4">Adding new referred patients</p> <BR>

Newly referred patients are added from one of two sources: (1) the referral rate specific to the epoch, and (2) referrals that arrive after a slowdown epoch. This step introduces the first major modeling assumption: <i>every consult not referred during a slowdown is referred once the slowdown ends</i>. For example, if a surgical group gets 50 referrals per week at baseline and this number drops to 20 during a 12-week slowdown, then (50-20)*12 = 360 additional referrals will be introduced after the practice returns to normal. Note that the 360 patients are introduced in addition to the weekly baseline referral rate. This assumption makes sense if you have a practice where most patients cannot self-refer to a clinic (see other modeling assumptions below).

<BR>

The slowdown patients are reintroduced when the slowdown epoch ends (i.e. the time returns back to baseline). A parameter controls the half-life of how quickly these patients are re-introduced (representing the delay in seeing the physician who will ultimately refer to the surgical group). For example, if the Slowdown Reintroduction Rate is 4, then (360/4) = 90 additional patients will be reintroduced the first week, and then (270/4) = 68 people are introduced the next week, and so forth. For this example, in the first week after returning to baseline, the surgical group will receive 50 + 90 = 140 new referrals.

<BR> <p class="is-size-4">Pre-surgical patients</p> <BR>

Each timestep, patients are moved from the referred to the pre-surgical group. The number of patients moved is:

<BR>

<center>(Number of surgeons) * (Number of clinics per timestep) * (Number of new patients per clinic) * (Fraction of new referrals considered surgical)</center>

<BR>

For instance, suppose that a surgical group has 10 surgeons, each surgeon runs a full-day clinic per week, each surgeon sees 5 new referrals per clinic, and the surgeon estimates that 40% of all new consults need surgery. Hence, (10 * 1 * 5 * 0.4) = 20 new patients will move from the referred to the pre-surgical group. 

<BR>

This patient state transition introduces the second critical assumption of the model: <i>The model cannot distinguish between patient priority</i>. The model is, instead, designed to compute a global wait time for all patients. Any form of prioritization based on patient needs is left up to the surgeon's internal referral triaging system, which is beyond the scope of this model. 

<BR> <p class="is-size-4">Post-surgical patients</p> <BR>

Each timestep, patients are moved from the pre-surgical group to the post-surgical group. The number of patients moved is:

<BR>

<center> (Number of operating rooms running per timestep) * (Number of operations per OR day)</center>

<BR> <p class="is-size-3">Other modeling assumptions</p> <BR>

Queue theory has developed terminology to describe how patients may react to existing lines:

<BR>

Balking: a patient may not join the queue if there are many patients already in line. For instance, a patient may find out from their primary care provider that there are long-wait times to see a surgeon. The patient may decide to go see a different practice or to go to their emergency room ahead of time. This model assumes that no patients will balk. This is a reasonable assumption in cases where patients are geographically restricted in their ability to see a surgeon, and if they do not invest in medical tourism to go elsewhere to be seen. 

<BR>

Reneging: a patient may exit the queue if there are many patients already in line. Similar to balking, except the patient has already joined the pool of patients. This model assumes that no patients will renege.


<BR>

Line jumping: a patient may join a different queue with the perception that they will be seen faster. In this model we assume that each surgeon is interchangeable, and so it doesn't make sense to explicitly incorporate this into a model.

"""

---------------------------------------------------
-- Implementation Text
---------------------------------------------------
modelImplementation : String
modelImplementation = """

<BR> <p class="is-size-3">Implementation details</p> <BR>

The Covid Backlog modeler was written in <a href='https://elm-lang.org/'>elm</a>, version 0.19. Elm is a functional language that transpiles into Javascript. The source code is freely available under an MIT license, and can be found on <a href='https://github.com/dbrandman/'>github</a>.

<BR>

We used the <a href='https://bulma.io/'>Bulma</a> open-source CSS framework. The figures were generated using <a href='https://plotly.com/javascript/'>plotly.js</a>.

"""
