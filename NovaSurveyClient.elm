import Html exposing (div, button, text, p, input, ul, li, label)
import Html.App exposing (beginnerProgram)
import Html.Events exposing (onClick)
import Html.Attributes exposing (name, placeholder, type', value, id, for)
import String exposing (concat)

type alias QuestionChoiceOption = String
type alias ChoiceQuestion =
    { description : String
    , options : List QuestionChoiceOption
    }

type Question 
  = FreeText String 
  | SingleChoice ChoiceQuestion
  | MultipleChoice ChoiceQuestion

type Response = FreeTextResponse String | SingleChoiceResponse Int | MultipleChoiceResponse (List Int)

type alias AnsweredQuestion = 
  { question: Question
  , response: Response
  }

type Quiz 
  = EmptyQuiz (List Question)
  | PartialQuiz (List Question) (List AnsweredQuestion)
  | CompletedQuiz (List AnsweredQuestion)

type AnsweredQuiz = AnsweredQuiz (List AnsweredQuestion)



draw_response answer =
  div [] 
      [ p [] 
          [ text ((toString answer.question) ++ (toString answer.response)) ]
      ]
          

draw_question i q =
  case q of
  FreeText description ->
    div []
        [ p [] [ text description ]
        , input [ name ("response" ++ (toString i)) , type' "text", value "" ][]
        , button [] [ text "Next" ]
        ]
  SingleChoice question ->
    div []
        [ p [] 
            [ text question.description ]
        , ul []
             (List.indexedMap 
               (\idx option -> 
                 li []
                    [ input [ id ( ("response" ++ (toString i)) ++ "_" ++ (toString idx) )
                            , name ("response" ++ (toString i))
                            , type' "radio"
                            , value option 
                            ]
                            []
                    , label [ for ("response" ++ (toString i)) ]
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
             (List.indexedMap 
               (\idx option -> 
                 li []
                    [ input [ id ( ("response" ++ (toString i)) ++ "_" ++ (toString idx) )
                            , name ("response" ++ (toString i))
                            , type' "checkbox"
                            , value option 
                            ]
                            []
                    , label [ for ("response" ++ (toString i)) ]
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

quiz = EmptyQuiz [q1, q2, q3]
--quiz = PartialQuiz [q1, q2, q3] [aq]
--quiz = CompletedQuiz [aq]

main =
  beginnerProgram { model = quiz, view = view, update = update }


view model =
  case model of 
  EmptyQuiz questions ->
--    div [] (List.indexedMap draw_question questions)
    div [] 
        [ case List.head questions of 
          Just q -> 
            (draw_question 0 q)
          Nothing ->
            p [] []
        ]
  PartialQuiz questions replied_questions -> 
    div [] 
        ((List.indexedMap draw_question questions) ++ (List.map draw_response replied_questions))
  CompletedQuiz replied_questions ->
    div [] [ p [] [ text "over" ] ] 


type Msg = Increment | Decrement | Reset

update msg model =
  model
