--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll
import Debug.Trace
import Data.String
import Data.List
import Control.Applicative
import Data.Maybe
import Control.Monad

--------------------------------------------------------------------------------
contentContext :: Compiler (Context String)
contentContext = do
  menu <- getMenu
  return $
    defaultContext <>
    constField "menu" (traceId menu)

getMenu :: Compiler String
getMenu = do
  menu <- map itemBody <$> loadAll (fromVersion $ Just "menu")
  myRoute <- getRoute =<< getUnderlying
  return $ case myRoute of
             Nothing -> showMenu "" menu
             Just me -> showMenu me menu

showMenu :: FilePath -> [FilePath] -> String
showMenu this items = "<ul>"++concatMap li items++"</ul>"
  where li item = "<li><a href=\"/"++item++"\">"++name item++"</a></li>"
        name item | item == this = "<strong>"++item++"</strong>"
                  | otherwise    = item

compileTemplates :: Rules ()
compileTemplates = match "template.html" $ compile templateCompiler

main :: IO ()
main = hakyll $ do
    match "images/*" $ do
        route   idRoute
        compile copyFileCompiler

    match "css/*" $ do
        route   idRoute
        compile compressCssCompiler

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "posts/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/post.html"    postCtx
            >>= loadAndApplyTemplate "templates/default.html" postCtx
            >>= relativizeUrls

    create ["archive.html"] $ do
        route idRoute
        compile $ do
            posts <- recentFirst =<< loadAll "posts/*"
            let archiveCtx =
                    listField "posts" postCtx (return posts) <>
                    constField "title" "Archives"            <>
                    defaultContext

            makeItem ""
                >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
                >>= loadAndApplyTemplate "templates/default.html" archiveCtx
                >>= relativizeUrls


    -- match "index.html" $ do
    --     route idRoute
    --     compile $ do
    --         -- posts <- recentFirst =<< loadAll "posts/*"
    --         -- let indexCtx =
    --         --         listField "posts" postCtx (return posts) <>
    --         --         constField "title" "SF Bay Siberian and Neva Masquerade Cat" <>
    --         --         defaultContext

    --         getResourceBody
    --             >>= applyAsTemplate indexCtx
    --             >>= loadAndApplyTemplate "templates/default.html" indexCtx
    --             >>= relativizeUrls

    -- match "templates/menu-items.html" $ do
    --   route idRoute
    --   compile $ do
    --     menus <- menuItems
    --     let indexCtx =
    --           listField "menuItems" defaultContext (return menus) <>
    --           constField "title" "SF Bay Siberian and Neva Masquerade Cat" <>
    --           defaultContext

    --     getResourceBody
    --       >>= applyAsTemplate indexCtx
    --       >>= loadAndApplyTemplate "templates/default.html" indexCtx
    --       >>= relativizeUrls

    match "templates/*" $ do
      compile templateBodyCompiler

    -- match "pages/*" $ version "preLoad" $ do
    --   route $ setExtension "html" `composeRoutes` gsubRoute "pages/" (const "preLoad/")
    --   -- route idRoute
    --   compile pandocCompiler

    match "pages/*" $ do
      let (+~+) = composeRoutes
      route $ setExtension "html" +~+ gsubRoute "pages/" (const "")
      -- route idRoute
      compile $ do
        -- posts <- recentFirst =<< loadAll "posts/*"
        -- let indexCtx =
        --       listField "posts" postCtx (return posts) <>
        --       defaultContext

        getResourceBody
          >>= saveSnapshot "preload"
          >>= applyAsTemplate indexCtx
          >>= renderPandoc
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    -- compileTemplates


--------------------------------------------------------------------------------
postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    defaultContext

menuCtx :: Context String
menuCtx =
  urlField "url" <>
  metadataField <>
  titleField "title" <>
  field "me" isMe
  where isMe item = do
          myRoute   <- getRoute =<< getUnderlying
          thisRoute <- getRoute $ itemIdentifier item
          let result = do
                myP   <- myRoute
                thisP <- thisRoute
                return $ myP == thisP
          case result of
            Just True -> return "me"
            _         -> return ""


defaultMenuItems =
  menuItemsFrom [ "kittens"
                , "index"
                , "sires"
                , "dames"
                , "gallery"
                , "retired"
                , "contact"
                , "breed"
                ]

indexMenuItems :: Compiler [Item String]
indexMenuItems = do
  index <- loadAllSnapshots "pages/index.*" "preload" :: Compiler [Item String]
  df <- mapM (flip getMetadataField "menu" . itemIdentifier) $ listToMaybe index
  case join df of
    Nothing -> return []
    Just  s -> menuItemsFrom $ words s


menuItemsFrom :: [String] -> Compiler [Item String]
menuItemsFrom menuElements = do
  cc <- mapM (\i -> loadAllSnapshots i "preload") menuFiles
  return $ concat cc
  where
        menuFiles :: [Pattern]
        menuFiles = map (\s -> fromGlob ("pages/"++ s ++".*")) menuElements

indexCtx :: Context String
indexCtx =
  listField "menuItems" menuCtx indexMenuItems <>
  listField "posts" postCtx (recentFirst =<< loadAll "posts/*") <>
  defaultContext
