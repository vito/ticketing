module Ticketing where

import Html
import Http
import Json.Decode exposing ((:=))
import Pagination exposing (Paginated, Page)
import Task
import Time

import Effects
import StartApp

app = StartApp.start { init = init, view = view, update = update, inputs = [] }
main = app.html

port tasks : Signal (Task.Task Effects.Never ())
port tasks = app.tasks

type alias Model =
  { repos : Maybe (List Repo)
  , currentRepos : Maybe (List Repo)
  , openPullRequests : Maybe Int
  }

type alias Repo =
  { name : String
  , openIssues : Int
  }

type Action
  = ReposFetched (Result Http.Error (Paginated Repo))
  | PullRequestsFetched (Result Http.Error Int)

init : (Model, Effects.Effects Action)
init =
  let
    model =
      { repos = Nothing
      , currentRepos = Nothing
      , openPullRequests = Nothing
      }
  in
    (model, Effects.batch [fetchRepos 0 Nothing, fetchPRs 0])

view : Signal.Address Action -> Model -> Html.Html
view address model =
  case (model.repos, model.openPullRequests) of
    (Just repos, Just prCount) ->
      let
        openIssues = List.sum <| List.map .openIssues repos
        interestingRepos = List.filter hasIssues repos
        header = (toString openIssues) ++ " open issues of which " ++ (toString prCount) ++ " are pull requests"
      in
        Html.div []
          [ Html.h1 []
            [ Html.text header ]
          , Html.dl [] (List.concatMap viewRepo <| interestingRepos)
          ]

    (_, _) ->
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
  case action of
    ReposFetched (Ok repos) ->
      handleReposFetched repos model

    ReposFetched (Err err) ->
      Debug.crash (toString err)

    PullRequestsFetched (Ok count) ->
      handlePRsFetched count model

    PullRequestsFetched (Err err) ->
      Debug.crash (toString err)

repollInterval : Time.Time
repollInterval = 5 * Time.minute

handleReposFetched : Paginated Repo -> Model -> (Model, Effects.Effects Action)
handleReposFetched repoPage model =
  let
    repos = List.append (Maybe.withDefault [] model.currentRepos) repoPage.content
  in
    case repoPage.pagination.nextPage of
      Nothing ->
        ( { model | currentRepos = Nothing, repos = Just repos } , fetchRepos repollInterval Nothing)

      Just page ->
        ( { model | currentRepos = Just repos }, fetchRepos 0 (Just page))

handlePRsFetched : Int -> Model -> (Model, Effects.Effects Action)
handlePRsFetched count model =
  ( { model | openPullRequests = Just count } , fetchPRs repollInterval)

fetchRepos : Time.Time -> Maybe Page -> Effects.Effects Action
fetchRepos delay page =
  let
    url = "https://api.github.com/orgs/concourse/repos"
  in
    (Task.sleep delay `Task.andThen` (always <| Pagination.fetch decodeRepo url page))
      |> Task.toResult
      |> Task.map ReposFetched
      |> Effects.task

decodeRepo : Json.Decode.Decoder Repo
decodeRepo = Json.Decode.object2 Repo
               ("name" := Json.Decode.string)
               ("open_issues" := Json.Decode.int)

decodeResults : Json.Decode.Decoder Int
decodeResults = ("total_count" := Json.Decode.int)

fetchPRs : Time.Time -> Effects.Effects Action
fetchPRs delay =
  let
    url =
      "https://api.github.com/search/issues?q=user:concourse+state:open+type:pr"
    get =
      Http.send
        Http.defaultSettings
        { verb = "GET"
        , headers = [("Authorization", "token TOKEN-GOES-HERE")]
        , url = url
        , body = Http.empty
        }
  in
    (Task.sleep delay `Task.andThen` (\x -> Task.mapError promoteHttpError get) `Task.andThen` parsePRs decodeResults)
      |> Task.toResult
      |> Task.map PullRequestsFetched
      |> Effects.task

promoteHttpError : Http.RawError -> Http.Error
promoteHttpError rawError =
  case rawError of
    Http.RawTimeout -> Http.Timeout
    Http.RawNetworkError -> Http.NetworkError

parsePRs : Json.Decode.Decoder a -> Http.Response -> Task.Task Http.Error a
parsePRs decode response =
  let
    decoded =
      handleResponse response `Result.andThen` \body ->
        Json.Decode.decodeString decode body
          |> Result.formatError Http.UnexpectedPayload
  in
    case decoded of
      Err err ->
        Task.fail err

      Ok count ->
        Task.succeed count

handleResponse : Http.Response -> Result Http.Error String
handleResponse response =
  if 200 <= response.status && response.status < 300 then
    case response.value of
      Http.Text str ->
        Ok str

      _ ->
        Err (Http.UnexpectedPayload "Response body is a blob, expecting a string.")
  else
    Err (Http.BadResponse response.status response.statusText)
