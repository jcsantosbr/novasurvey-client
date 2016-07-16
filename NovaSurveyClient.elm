import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick, onInput)

type alias QuestionChoiceOption = String
type alias ChoiceQuestion =
  { description : String
  , options : List QuestionChoiceOption
  }

type Question
  = FreeText String
  | SingleChoice ChoiceQuestion
  | MultipleChoice ChoiceQuestion

type Response
  = FreeTextResponse String
  | SingleChoiceResponse Int
  | MultipleChoiceResponse (List Int)

type alias AnsweredQuestion =
  { question: Question
  , response: Response
  }

type Quiz
  = EmptyQuiz (List Question)
  | PartialQuiz (List Question) (List AnsweredQuestion)
  | CompletedQuiz (List AnsweredQuestion)

draw_response answer =
  div []
      [ p []
          [ text ((toString answer.question) ++ (toString answer.response)) ]
      ]

getFreeTextResponse response =
  case response of
  Just (FreeTextResponse text) ->
    text
  _ ->
    ""

draw_question q response =
  case q of
  FreeText description ->
    div []
        [ p [] [ text description ]
        , input [ name "response"
                , type' "text"
                , value (getFreeTextResponse response)
                , onInput UpdateFreeTextResponse
                ][]
        , button [ onClick NextQuestion ] [ text "Next" ]
        ]
  SingleChoice question ->
    div []
        [ p []
            [ text question.description ]
        , ul []
             (List.map
               (\option ->
                 li []
                    [ input [ id "response"
                            , name "response"
                            , type' "radio"
                            , value option
                            ]
                            []
                    , label [ for "response" ]
                            [ text option ]
                    ]
               )
               question.options
             )
          , button [] [ text "Next" ]
        ]
  MultipleChoice question ->
    div []
        [ p []
            [ text question.description ]
        , ul []
             (List.map
               (\option ->
                 li []
                    [ input [ id "response"
                            , name "response"
                            , type' "checkbox"
                            , value option
                            ]
                            []
                    , label [ for "response" ]
                            [ text option ]
                    ]
               )
               question.options
             )
          , button [] [ text "Next" ]
        ]

q1 = FreeText "What is your name?"
q2 = SingleChoice
  { description = "What is your city?"
  , options = ["Sao Paulo", "Rio de Janeiro", "Belo Horizonte"]
  }
q3 = MultipleChoice
  { description = "What are your favorite foods?"
  , options = [ "Ham", "Bacon", "Pork" ]
  }

q4 = FreeText "Do you want to participate?"
r4 = FreeTextResponse "Yep"
aq = { question=q4, response=r4}

q5 = FreeText "Where were you born?"
q6 = FreeText "What is your age?"

--quiz = EmptyQuiz [q1, q2, q3]
--quiz = PartialQuiz [q1, q2, q3] [aq]
--quiz = CompletedQuiz [aq]
quiz = EmptyQuiz [q1, q5, q6]

type alias Model =
  { quiz: Quiz
  , currentQuestion: Question
  , currentResponse: Maybe Response
  , replied: Bool
  , feedback: String
  }

model = Model quiz q1 Nothing False ""

main =
  beginnerProgram { model = model, view = view, update = update }

view model =
  case model.quiz of
  EmptyQuiz questions ->
    div []
        [ (draw_question model.currentQuestion model.currentResponse)
        , p [] [ text ("Feedback: " ++ model.feedback) ]
        ]
  PartialQuiz questions replied_questions ->
    div []
        (
          [(draw_question model.currentQuestion model.currentResponse)]
          ++ [ p [] [ text ("Feedback: " ++ model.feedback) ] ]
          ++ (List.map draw_response replied_questions)
        )
  CompletedQuiz replied_questions ->
    div [] [ p [] [ text "over" ] ]

type Msg
  = UpdateFreeTextResponse String
  | NextQuestion

update : Msg -> Model -> Model
update msg model =
  case msg of
  UpdateFreeTextResponse text ->
    { model |
      currentResponse = (Just (FreeTextResponse text))
      , feedback = "Replying..."
    }
  NextQuestion ->
    case model.currentResponse of
    Nothing ->
      { model | feedback = "No response" }
    Just response ->
      let
        repliedQuestion = AnsweredQuestion model.currentQuestion response
        newQuiz = updateQuiz model.quiz repliedQuestion
      in
        case newQuiz of
        Ok quiz ->
          { model |
              quiz = quiz
              , currentQuestion = findNextQuestion quiz
              , currentResponse = Nothing
              , feedback = "???"
          }
        Err feedback ->
          { model | feedback = feedback }

updateQuiz : Quiz -> AnsweredQuestion -> Result String Quiz
updateQuiz quiz answeredQuestion =
  case quiz of
  EmptyQuiz questions ->
    if (List.length questions) == 1 then
      Ok (CompletedQuiz [answeredQuestion])
    else
      Ok (PartialQuiz questions [answeredQuestion])
  PartialQuiz questions repliedQuestions ->
    if (List.length questions) == (List.length repliedQuestions) + 1 then
      Ok (CompletedQuiz (repliedQuestions ++ [answeredQuestion]))
    else
      Ok (PartialQuiz questions (repliedQuestions ++ [answeredQuestion]))
  CompletedQuiz replied_questions ->
    Err "Quiz is already completed!"

findNextQuestion : Quiz -> Question
findNextQuestion quiz =
  case quiz of
  EmptyQuiz questions ->
    case (List.head questions) of
    Just question ->
      question
    Nothing ->
      FreeText "Empty Question for now"
  PartialQuiz questions responses ->
    let
      repliedQuestions = List.map (\r -> r.question) responses
      unrepliedQuestions = List.filter (\q -> not (List.member q repliedQuestions))  questions
    in
      case (List.head unrepliedQuestions) of
      Just question ->
        question
      Nothing ->
        FreeText "Empty Question for now"
  CompletedQuiz replied_questions ->
    FreeText "Empty Question for now"
