import Html exposing (Html, button, div, text, input, Attribute, table, tr, td, thead, th)
import Html.Events exposing (onClick, onInput, on, keyCode)
import Html.Attributes exposing (id, autofocus, value, disabled)
import Time exposing (Time, second)
import Json.Decode
import Task
import Dom

main =
  Html.program
  { init = init
  , view = view
  , update = update
  , subscriptions = subscriptions
  }

-- MODEL
--
--     QuizzState - ( remainingVerbs, correctAnswers, wrongAnswers )
--
--     UiState - ( QuestionForm, ViewingResult, Scorecard }
--
--      (QuestionForm)
--         |     ^
--       Answer  |
--         |    TransitionToResult,
--         |    NextScreen
--         v     |
--      (ResultScreen)
--          |
--         TransitionToResult
--         NextScreen
--          |
--          v
--      (ScoreCard)
--
--

type alias Verb =
  { inSpanish : String
  , inEnglish: String
  }

type UserAnswer = Correct Int Verb | Incorrect Int String Verb

type AppScreen = StartScreen | QuizzForm | ResultScreen | ScoreCard

type alias Model =
  { screen : AppScreen
  , remainingVerbs : List Verb
  , currentVerb : Maybe Verb
  , answers : List UserAnswer
  , currentAnswer: String
  , qNum : Int
  }

initialVerbs =
  [ Verb "comer" "to eat"
  , Verb "hacer" "to do, to make"
  , Verb "aprender" "to learn"
  ]

model : Model
model = Model StartScreen initialVerbs Nothing [] "" 0

init : (Model, Cmd Msg)
init = (model, Cmd.none)

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  case model.screen of
    ResultScreen -> Time.every second TransiotionToNextScreen
    _ -> Sub.none


-- UPDATE

type Msg =
      StartQuizz
    | Input String
    | Answer
    | KeyDown Int
    | TransiotionToNextScreen Time
    | NextScreen
    | RetakeQuizz
    | FocusResult (Result Dom.Error ())

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    StartQuizz ->
      ( model
        |> nextQuestion
        |> \m -> { m | screen = QuizzForm }
      , Dom.focus "answerbar" |> Task.attempt FocusResult
      )

    Input text ->
      ( if model.screen == QuizzForm then
          { model | currentAnswer= text }
        else
          model
      , Cmd.none
      )

    Answer ->
      ( model
        |> processAnswer
        |> \m -> { m | screen = ResultScreen }
      , Cmd.none
      )

    KeyDown code ->
       if code == 13 then
         ( model
           |> processAnswer
           |> \m -> { m | screen = ResultScreen }
         , Dom.focus "answerbar" |> Task.attempt FocusResult
         )
       else
         ( model
         , Cmd.none
         )

    NextScreen ->
      ( model
        |> nextQuestion
        |> \m -> { m | screen = QuizzForm }
      , Dom.focus "answerbar" |> Task.attempt FocusResult
      )

    TransiotionToNextScreen time ->
      ( model
        |> nextQuestion
      , Cmd.none
      )

    RetakeQuizz ->
      ( Model StartScreen initialVerbs Nothing [] "" 0
      , Cmd.none
      )

    FocusResult result ->
      ( model
      , Cmd.none
      )

--
-- Scaffolding to handle the 'enter' key press event for the input field:
--
-- Ref: https://stackoverflow.com/questions/40113213/how-to-handle-enter-key-press-in-input-field
--
onKeyDown : (Int -> msg) -> Attribute msg
onKeyDown tagger =
  on "keydown" (Json.Decode.map tagger keyCode)

nextQuestion : Model -> Model
nextQuestion model =
  List.head model.remainingVerbs
  |> Maybe.map (\verb ->
       { model
       | screen = QuizzForm
       , currentVerb = Just verb
       , qNum = model.qNum + 1
       , currentAnswer = "" })
  |> Maybe.map (\model ->
       { model
       | remainingVerbs =
           List.tail model.remainingVerbs
             |> Maybe.map (\tail -> tail)
             |> Maybe.withDefault []
       })
  |> Maybe.withDefault { model | screen = ScoreCard }

processAnswer : Model -> Model
processAnswer model =
  Maybe.map (checkAnswer model.qNum model.currentAnswer) model.currentVerb
  |> Maybe.map (\answer ->
        { model
        | answers = answer :: model.answers
        , currentAnswer = ""
        })
  |> Maybe.withDefault model

checkAnswer: Int -> String -> Verb -> UserAnswer
checkAnswer qNum answer verb =
  if String.toLower answer == String.toLower verb.inSpanish then
    Correct qNum verb
  else
    Incorrect qNum answer verb

-- VIEW

view : Model -> Html Msg
view model =
  case model.screen of
    StartScreen -> renderStartScreen model

    QuizzForm -> renderQuizzForm model

    ResultScreen -> renderQuizzForm model

    ScoreCard -> renderScoreCard model

renderStartScreen model =
  div []
   [ div [] [ text "Start Screen"]
   , button [ onClick StartQuizz ] [ text "Start Quizz" ]
   ]

renderQuizzForm model =
  div []
   [ div []
     [ div [] [text ("Question " ++ toString model.qNum ++ ".")]
     , div [] [text ("'" ++ currentVerbInEnglish model ++ "'")]
     ]
   , div [] [ text "In Spanish: " ]
   , input
        [ id "answerbar"
        , value model.currentAnswer
        , onKeyDown KeyDown
        , onInput Input
        ]
        []
   ,
     if model.screen == ResultScreen then
       div []
       [ div []
         [ List.head model.answers
             |> Maybe.map showResult
             |> Maybe.withDefault (div [] [])
         , button [ onClick NextScreen ] [ text "Continue ..." ]
         ]
       ]
     else
       button [ onClick Answer ] [ text "Answer" ]
   ]

showResult: UserAnswer -> Html msg
showResult answer =
  case answer of
  Correct _ verb -> div [] [text "Correct!"]
  Incorrect _ answer verb -> div []
    [ div [] [ text "Incorrect!" ]
    , div [] [ text "The correct answer is: " ]
    , div [] [ text ("'" ++ verb.inSpanish ++ "'") ]
    ]

renderScoreCard model =
  div [] (
   [ div [] [ text "Score Card"]
   , div [] [ text "Your got " ]
   , div [] [ text (toString (numberOfCorrects model.answers)) ]
   , div [] [ text " out of " ]
   , div [] [ text (toString (List.length model.answers)) ]
   , if numberOfCorrects model.answers /= List.length model.answers then
        scoreCardAnswersTable model.answers
     else
        div [] [ text "All correct!" ]
   , button [ onClick RetakeQuizz ] [ text "Retake" ]
   ])

scoreCardAnswersTable: List UserAnswer -> Html msg
scoreCardAnswersTable answers =
  div []
    [ text "Score card table: "
    , table []
         (answers
          |> List.reverse
          |> List.filter isIncorrectAnswer
          |> List.map scoreCardRow)
    ]

scoreCardRow: UserAnswer -> Html msg
scoreCardRow answer =
  case answer of
    Incorrect qNum answer verb ->
      tr []
        [ td [] [ text (toString qNum ++ ".") ]
        , td [] [ text answer ]
        , td [] [ text "vs" ]
        , td [] [ text verb.inSpanish ]
        ]
    _ ->
      tr []
        [ td [] []
        , td [] []
        , td [] []
        , td [] []
        ]

numberOfCorrects: List UserAnswer -> Int
numberOfCorrects answers =
  answers
  |> List.filter isCorrectAnswer
  |> List.length

isCorrectAnswer answer =
  case answer of
    Correct _ x -> True
    _ -> False

isIncorrectAnswer answer =
  not (isCorrectAnswer answer)

currentVerbInEnglish: Model -> String
currentVerbInEnglish model =
    model.currentVerb
    |> Maybe.map (\verb -> verb.inEnglish)
    |> Maybe.withDefault ""
