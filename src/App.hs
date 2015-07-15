{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}

module App where

import Snap
import Snap.Snaplet.SqliteSimple
import Snap.Snaplet.Heist
import Control.Applicative
import Control.Lens
import Database.SQLite.Simple.ToField
import qualified Database.SQLite.Simple as S
import Control.Concurrent
import Heist.Interpreted
import Heist.Internal.Types

import Data.Maybe
import Data.Word
import Data.Text as T
import Data.Text.Encoding as TE

import Data.Map.Syntax
import Data.Monoid

import qualified Data.ByteString.Char8 as BS

data Post = Post {
    _postId   :: Word32,
    _postUser :: Text,
    _postBody :: Text
  }

makeLenses ''Post

instance ToRow Post where
    toRow (Post a b c) = [toField a, toField b, toField c]

instance FromRow Post where
    fromRow = Post <$> field <*> field <*> field

data App = App {
    _heist :: Snaplet (Heist App),
    _db :: Snaplet Sqlite
  }

makeLenses ''App

instance HasHeist App where
    heistLens = subSnaplet heist

appInit :: SnapletInit App App
appInit = makeSnaplet "crashboard" "A temporary message board" Nothing $ do
    let hc = HeistConfig sc "" False
        sc = mempty { _scInterpretedSplices = defaultSplices }
    h <- nestSnaplet "heist" heist $ heistInit' "templates" hc
    d <- nestSnaplet "db" db sqliteInit
    addRoutes [
        ("/", indexHandler),
        ("/post", newPostHandler),
        ("/:postid/delete", deleteHandler)
        ]
    wrapSite (<|> heistServe)
    let c = sqliteConn $ d ^# snapletValue
    liftIO $ withMVar c $ \conn -> createTables conn
    return $ App h d

tableExists :: S.Connection -> String -> IO Bool
tableExists conn nam = do
    r <- S.query conn
        "SELECT name FROM sqlite_master WHERE type='table' AND name=?"
        (Only nam)
    case (r :: [Only String]) of
        [Only _] -> return True
        _ -> return False

createTables :: S.Connection -> IO ()
createTables conn = do
    ex <- tableExists conn "posts"
    unless ex $ S.execute_ conn (S.Query $ T.concat [
            "CREATE TABLE posts ("
           ,"id INTEGER PRIMARY KEY AUTOINCREMENT, "
           ,"user TEXT, "
           ,"body TEXT)"
        ])

allPosts :: Handler App Sqlite [Post]
allPosts = query "SELECT id, user, body FROM posts ORDER BY id ASC" ()

deletePost :: Word32 -> Handler App Sqlite [Only Int]
deletePost pid = query "DELETE FROM posts WHERE id = ?" (Only pid)

addPost :: Text -> Text -> Handler App Sqlite [Only Int]
addPost user body = query "INSERT INTO posts (user, body) VALUES (?, ?)"
    (toField user, toField body)

postSplice :: Monad m => Post -> Splice m
postSplice p = runChildrenWithText $ do
     "postid" ## (T.pack . show) $ p^.postId
     "postuser" ## p^.postUser
     "postbody" ## p^.postBody

postsSplice :: Splice (Handler App App)
postsSplice = do
    posts <- lift $ with db allPosts
    mapSplices postSplice posts

newPostSplice :: Splice (Handler App App)
newPostSplice = do
    user <- lift $ getUser
    runChildrenWithText $ do
        "user" ## user

indexHandler :: Handler App App ()
indexHandler = method GET $ gRender "index.tpl"

setUser :: Text -> Handler App App ()
setUser user = modifyResponse addCookie where
    addCookie = addResponseCookie cook
    cook = Cookie "user" (encodeUtf8 user) Nothing Nothing Nothing False False

getUser :: Handler App App Text
getUser = do
    cook <- getCookie "user"
    return $ maybe "" (decodeUtf8.cookieValue) cook

newPostHandler :: Handler App App ()
newPostHandler = method POST $ do
    user' <- fmap TE.decodeUtf8 <$> getPostParam "name"
    body' <- fmap TE.decodeUtf8 <$> getPostParam "body"
    case (user', body', fmap T.length user') of
        (Just _, Just body, Just 0) -> void $ with db $ addPost "anonymouse\x1F42D" body
        (Just user, Just body, _) -> do
            with db $ addPost user body
            setUser user
        _ -> return ()
    redirect "/#bottom"

deleteHandler :: Handler App App ()
deleteHandler = method POST $ do
    postid <- fmap (read . BS.unpack) <$> getParam "postid"
    case postid of
        Just p -> void $ with db $ deletePost p
        _ -> return ()
    redirect "/#bottom"

defaultSplices :: Splices (Splice (Handler App App))
defaultSplices = do
    "posts" ## postsSplice
    "newpost" ## newPostSplice

