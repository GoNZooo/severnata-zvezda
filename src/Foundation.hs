{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Foundation where

import Control.Monad.Logger (LogSource)
-- Used only when in "auth-dummy-login" setting is enabled.

import qualified Data.CaseInsensitive as CI
import Data.Kind (Type)
import qualified Data.Text.Encoding as TE
import Data.UUID (UUID)
import qualified Data.UUID.V4 as UUIDv4
import Database (ChallengeApprovalPayload, appApproveLoginRequest)
import Database.Persist.Sql (BackendKey (..), ConnectionPool, runSqlPool)
import Import.NoFoundation
import Plugin.AuthExternal (AuthExternal (..), authExternal)
import Text.Hamlet (hamletFile)
import Text.Jasmine (minifym)
import Yesod.Auth.Dummy
import Yesod.Auth.Message (AuthMessage (..))
import Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import Yesod.Default.Util (addStaticContentExternal)

-- | The foundation datatype for your application. This can be a good place to
-- keep settings and values requiring initialization before your application
-- starts running, such as database connections. Every handler will have
-- access to the data present here.
data App = App
  { appSettings :: AppSettings,
    -- | Settings for static file serving.
    appStatic :: Static,
    -- | Database connection pool.
    appConnPool :: ConnectionPool,
    appHttpManager :: Manager,
    appLogger :: Logger
  }

data MenuItem = MenuItem
  { menuItemLabel :: Text,
    menuItemRoute :: Route App,
    menuItemAccessCallback :: Bool
  }

data MenuTypes
  = NavbarLeft MenuItem
  | NavbarRight MenuItem

-- This is where we define all of the routes in our application. For a full
-- explanation of the syntax, please see:
-- http://www.yesodweb.com/book/routing-and-handlers
--
-- Note that this is really half the story; in Application.hs, mkYesodDispatch
-- generates the rest of the code. Please see the following documentation
-- for an explanation for this split:
-- http://www.yesodweb.com/book/scaffolding-and-the-site-template#scaffolding-and-the-site-template_foundation_and_application_modules
--
-- This function also generates the following type synonyms:
-- type Handler = HandlerFor App
-- type Widget = WidgetFor App ()
mkYesodData "App" $(parseRoutesFile "config/routes.yesodroutes")

-- | A convenient synonym for creating forms.
type Form x = Html -> MForm (HandlerFor App) (FormResult x, Widget)

-- | A convenient synonym for database access functions.
type DB a =
  forall (m :: Type -> Type).
  (MonadUnliftIO m) =>
  ReaderT SqlBackend m a

-- Please see the documentation for the Yesod typeclass. There are a number
-- of settings which can be configured by overriding methods here.
instance Yesod App where
  -- Controls the base of generated URLs. For more information on modifying,
  -- see: https://github.com/yesodweb/yesod/wiki/Overriding-approot
  approot :: Approot App
  approot = ApprootRequest $ \app req ->
    case appRoot $ appSettings app of
      Nothing -> getApprootText guessApproot app req
      Just root -> root

  -- Store session data on the client in encrypted cookies,
  -- default session idle timeout is 120 minutes
  makeSessionBackend :: App -> IO (Maybe SessionBackend)
  makeSessionBackend _ =
    Just
      <$> defaultClientSessionBackend
        (7 * 24 * 60) -- timeout in minutes
        "config/client_session_key.aes"

  -- Yesod Middleware allows you to run code before and after each handler function.
  -- The defaultYesodMiddleware adds the response header "Vary: Accept, Accept-Language" and performs authorization checks.
  -- Some users may also want to add the defaultCsrfMiddleware, which:
  --   a) Sets a cookie with a CSRF token in it.
  --   b) Validates that incoming write requests include that token in either a header or POST parameter.
  -- To add it, chain it together with the defaultMiddleware: yesodMiddleware = defaultYesodMiddleware . defaultCsrfMiddleware
  -- For details, see the CSRF documentation in the Yesod.Core.Handler module of the yesod-core package.
  yesodMiddleware :: Handler res -> Handler res
  yesodMiddleware = defaultYesodMiddleware

  defaultLayout :: Widget -> Handler Html
  defaultLayout widget = do
    master <- getYesod
    message <- getMessage

    user <- maybeAuthPair
    currentRoute <- getCurrentRoute

    -- Get the breadcrumbs, as defined in the YesodBreadcrumbs instance.
    (title, parents) <- breadcrumbs

    -- Define the menu items of the header.
    let menuItems =
          [ NavbarLeft $
              MenuItem
                { menuItemLabel = "Home",
                  menuItemRoute = HomeR,
                  menuItemAccessCallback = True
                },
            NavbarLeft $
              MenuItem
                { menuItemLabel = "Posts",
                  menuItemRoute = PostsR,
                  menuItemAccessCallback = True
                },
            NavbarRight $
              MenuItem
                { menuItemLabel = "Login",
                  menuItemRoute = AuthR LoginR,
                  menuItemAccessCallback = isNothing user
                },
            NavbarRight $
              MenuItem
                { menuItemLabel = "Logout",
                  menuItemRoute = AuthR LogoutR,
                  menuItemAccessCallback = isJust user
                }
          ]

    let navbarLeftMenuItems = [x | NavbarLeft x <- menuItems]
    let navbarRightMenuItems = [x | NavbarRight x <- menuItems]

    let navbarLeftFilteredMenuItems = [x | x <- navbarLeftMenuItems, menuItemAccessCallback x]
    let navbarRightFilteredMenuItems = [x | x <- navbarRightMenuItems, menuItemAccessCallback x]

    -- We break up the default layout into two components:
    -- default-layout is the contents of the body tag, and
    -- default-layout-wrapper is the entire page. Since the final
    -- value passed to hamletToRepHtml cannot be a widget, this allows
    -- you to use normal widget features in default-layout.

    pc <- widgetToPageContent $ do
      addStylesheet $ StaticR css_bootstrap_css
      $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")

  -- The page to be redirected to when authentication is required.
  authRoute ::
    App ->
    Maybe (Route App)
  authRoute _ = Just $ AuthR LoginR

  isAuthorized ::
    -- | The route the user is visiting.
    Route App ->
    -- | Whether or not this is a "write" request.
    Bool ->
    Handler AuthResult
  -- Routes not requiring authentication.
  isAuthorized (AuthR _) _ = pure Authorized
  isAuthorized HomeR _ = pure Authorized
  isAuthorized PostsR _ = pure Authorized
  isAuthorized FaviconR _ = pure Authorized
  isAuthorized RobotsR _ = pure Authorized
  isAuthorized (StaticR _) _ = pure Authorized
  isAuthorized (PostR _) False = pure Authorized
  isAuthorized (PostR _) True = isAuthenticated
  isAuthorized (EditPostR _) _ = isAuthenticated

  -- This function creates static content files in the static folder
  -- and names them based on a hash of their content. This allows
  -- expiration dates to be set far in the future without worry of
  -- users receiving stale content.
  addStaticContent ::
    -- | The file extension
    Text ->
    -- | The MIME content type
    Text ->
    -- | The contents of the file
    LByteString ->
    Handler (Maybe (Either Text (Route App, [(Text, Text)])))
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      -- Generate a unique filename based on the content itself
      genFileName lbs = "autogen-" ++ base64md5 lbs

  -- What messages should be logged. The following includes all messages when
  -- in development, and warnings and errors in production.
  shouldLogIO :: App -> LogSource -> LogLevel -> IO Bool
  shouldLogIO app _source level =
    return $
      appShouldLogAll (appSettings app)
        || level == LevelWarn
        || level == LevelError

  makeLogger :: App -> IO Logger
  makeLogger = return . appLogger

-- Define breadcrumbs.
instance YesodBreadcrumbs App where
  -- Takes the route that the user is currently on, and returns a tuple
  -- of the 'Text' that you want the label to display, and a previous
  -- breadcrumb route.
  breadcrumb :: Route App -> Handler (Text, Maybe (Route App))
  breadcrumb HomeR = pure ("Home", Nothing)
  breadcrumb (AuthR _) = pure ("Login", Just HomeR)
  breadcrumb PostsR = pure ("Posts", Just HomeR)
  breadcrumb (PostR (BlogPostKey (SqlBackendKey id'))) = pure ("Post " <> tshow id', Just PostsR)
  breadcrumb (EditPostR (BlogPostKey (SqlBackendKey id'))) = pure ("Edit Post " <> tshow id', Just PostsR)
  breadcrumb _ = pure ("home", Nothing)

-- How to run database actions.
instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB :: SqlPersistT Handler a -> Handler a
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner :: Handler (DBRunner App, Handler ())
  getDBRunner = defaultGetDBRunner appConnPool

instance YesodAuth App where
  type AuthId App = UserId

  -- Where to send a user after successful login
  loginDest :: App -> Route App
  loginDest _ = PostsR

  -- Where to send a user after logout
  logoutDest :: App -> Route App
  logoutDest _ = HomeR

  -- Override the above two destinations when a Referer: header is present
  redirectToReferer :: App -> Bool
  redirectToReferer _ = True

  authenticate ::
    (MonadHandler m, HandlerSite m ~ App) =>
    Creds App ->
    m (AuthenticationResult App)
  authenticate credentials = liftHandler $ do
    runDB $ do
      x <- getBy $ UniqueUsername $ credsIdent credentials
      case x of
        Just (Entity uid _) -> pure $ Authenticated uid
        Nothing -> pure $ UserError AuthError

  -- You can add other plugins like Google Email, email or OAuth here
  authPlugins :: App -> [AuthPlugin App]
  authPlugins app = extraAuthPlugins
    where
      -- Enable authDummy login if enabled.
      extraAuthPlugins = [authExternal] <> [authDummy | appAuthDummyLogin $ appSettings app]

instance AuthExternal App where
  type RequestId App = UUID
  type LoginUser App = User
  type ChallengePayload App = ChallengeApprovalPayload
  isLoginRequestApproved = runDB . checkLoginRequest
  getUserForLoginRequest = runDB . appGetUserForLoginRequest
  encodeUser (User username' _) = username'
  createLoginRequest = runDB . appCreateLoginRequest
  getUserByUsername = runDB . appGetUserByUsername
  approveLoginRequest = runDB . appApproveLoginRequest
  markLoginRequestAsFollowed = runDB . appMarkLoginRequestAsFollowed

appCreateLoginRequest :: User -> DB (Maybe UUID)
appCreateLoginRequest user = do
  maybeUserEntity <- getByValue user
  case maybeUserEntity of
    Just (Entity userId' _) -> newLoginRequest userId'
    Nothing -> pure Nothing

appGetUserByUsername :: Text -> DB (Maybe User)
appGetUserByUsername username' = do
  maybeUserEntity <- getBy $ UniqueUsername username'
  pure $ case maybeUserEntity of
    Just (Entity _userId' user') -> Just user'
    Nothing -> Nothing

newLoginRequest :: UserId -> DB (Maybe UUID)
newLoginRequest userId' = do
  newUuid <- liftIO UUIDv4.nextRandom
  maybeId' <- insertUnique $ LoginRequest newUuid userId' Nothing False
  pure $ case maybeId' of
    Just _ -> Just newUuid
    Nothing -> Nothing

appGetUserForLoginRequest :: UUID -> DB (Maybe User)
appGetUserForLoginRequest uuid = do
  maybeLoginRequest <- getBy $ UniqueUuid uuid
  case maybeLoginRequest of
    Just (Entity _loginRequestId (LoginRequest _ userId' _ _)) -> do
      get userId'
    Nothing -> pure Nothing

checkLoginRequest :: UUID -> DB Bool
checkLoginRequest uuid = do
  maybeLoginRequest <- getBy $ UniqueUuid uuid
  pure $ case maybeLoginRequest of
    Just (Entity _loginRequestId (LoginRequest _ _ (Just _approvedTime) followed)) -> not followed
    _ -> False

appMarkLoginRequestAsFollowed :: UUID -> DB ()
appMarkLoginRequestAsFollowed uuid = do
  maybeLoginRequest <- getBy $ UniqueUuid uuid
  case maybeLoginRequest of
    Just (Entity loginRequestId' _loginRequest') ->
      update loginRequestId' [LoginRequestFollowed =. True]
    _ -> pure ()

-- | Access function to determine if a user is logged in.
isAuthenticated :: Handler AuthResult
isAuthenticated = do
  maybeUid <- maybeAuthId
  return $ case maybeUid of
    Nothing -> Unauthorized "You must login to access this page"
    Just _ -> Authorized

instance YesodAuthPersist App

-- This instance is required to use forms. You can modify renderMessage to
-- achieve customized and internationalized form validation messages.
instance RenderMessage App FormMessage where
  renderMessage :: App -> [Lang] -> FormMessage -> Text
  renderMessage _ _ = defaultFormMessage

-- Useful when writing code that is re-usable outside of the Handler context.
-- An example is background jobs that send email.
-- This can also be useful for writing code that works across multiple Yesod applications.
instance HasHttpManager App where
  getHttpManager :: App -> Manager
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger

-- Note: Some functionality previously present in the scaffolding has been
-- moved to documentation in the Wiki. Following are some hopefully helpful
-- links:
--
-- https://github.com/yesodweb/yesod/wiki/Sending-email
-- https://github.com/yesodweb/yesod/wiki/Serve-static-files-from-a-separate-domain
-- https://github.com/yesodweb/yesod/wiki/i18n-messages-in-the-scaffolding
