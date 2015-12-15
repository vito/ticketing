module Ticketing where

import Effects
import Html
import Html.Events exposing (onClick)
import StartApp
import Json.Decode exposing ((:=))
import Http
import Task
import Time

app = StartApp.start { init = init, view = view, update = update, inputs = [] }
main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks

type alias Model =
  { repos : Maybe (List Repo)
  }

type alias Repo =
  { name : String
  , openIssues : Int
  }

type Action =
  ReposFetched (Result Http.Error (List Repo))

init : (Model, Effects.Effects Action)
init =
  let
    model =
      { repos = Nothing
      }
  in
    (model, fetchRepos 0)

view : Signal.Address Action -> Model -> Html.Html
view address model =
  case model.repos of
    Just repos ->
      Html.div []
        [ Html.h1 []
          [ Html.text <| (toString <| List.sum <| List.map .openIssues repos) ++ " open issues" ]
        , Html.dl [] (List.concatMap viewRepo <| List.filter hasIssues repos)
        ]
    Nothing ->
      Html.text "Loading..."

hasIssues : Repo -> Bool
hasIssues repo = repo.openIssues > 0

viewRepo : Repo -> List Html.Html
viewRepo repo =
  [ Html.dt [] [Html.text repo.name]
  , Html.dd [] [Html.text (toString repo.openIssues)]
  ]

update : Action -> Model -> (Model, Effects.Effects Action)
update action model =
  let
    repoll = fetchRepos Time.minute
  in
    case action of
      ReposFetched (Ok repos) ->
        ( { model | repos = Just repos } , repoll)
      ReposFetched (Err err) ->
        (model, repoll)

fetchRepos : Time.Time -> Effects.Effects Action
fetchRepos delay =
  Task.sleep delay `Task.andThen` (always <| Http.get decodeRepos "https://api.github.com/orgs/concourse/repos")
    |> Task.toResult
    |> Task.map ReposFetched
    |> Effects.task

decodeRepos : Json.Decode.Decoder (List Repo)
decodeRepos = Json.Decode.list <| Json.Decode.object2 Repo
                                    ("name" := Json.Decode.string)
                                    ("open_issues" := Json.Decode.int)
