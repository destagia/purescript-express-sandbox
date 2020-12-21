module Main where

import Prelude hiding (apply)

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log)
import Effect.Ref (Ref, modify', new, read)
import Node.Express.App (App, get, listenHttp, use)
import Node.Express.Handler (Handler, next)
import Node.Express.Request (getOriginalUrl, setUserData)
import Node.Express.Response (send)
import Node.HTTP (Server)

type AppStateData = { count :: Int }
type AppState = Ref AppStateData
type AppError = String

initState :: Effect AppState
initState = new { count: 0 }

logger :: AppState -> Handler
logger state = do
  count <- liftEffect $ read state
  url <- getOriginalUrl
  liftEffect $ log (">>> " <> url <> " count = " <> (show $ count.count))
  setUserData "logged" url
  next

indexHandler :: AppState -> Handler
indexHandler _ = do
  send "Hello, PureScript λ"

countUp :: AppStateData -> { state :: AppStateData, value :: Int }
countUp stateData =
  let newCount = stateData.count + 1
  in { state: { count: newCount }, value: newCount }

countUpHandler :: AppState -> Handler
countUpHandler state = do
  res <- liftEffect $ modify' countUp state
  send ("count = " <> (show $ res))

appSetup :: AppState -> App
appSetup state = do
  liftEffect $ log "Setting up"
  use (logger state)
  get "/" (indexHandler state)
  get "/count" (countUpHandler state)

main :: Effect Server
main = do
  state <- initState
  listenHttp (appSetup state) 8080 \_ ->
    log $ "Listening on 8080"