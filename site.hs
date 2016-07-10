--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll
import Debug.Trace
import Data.String
import Data.String.Utils (replace, strip)
import Data.List --(intercalate, head, isPrefixOf)
import Control.Applicative ((<$>), empty)
import Data.Maybe
import Control.Monad (join)
import qualified Data.HashMap.Strict            as HMS
import qualified Data.Text                      as T
import System.FilePath (takeDirectory)
import System.FilePath.Find as FF
import System.IO.Unsafe (unsafePerformIO)

--------------------------------------------------------------------------------
(+~+) = composeRoutes

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
    match "images/**" $ do
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

    match "cats/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/cat.html"     indexCtx
          >>= saveSnapshot "preload"
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
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

    match "pages/*" $ do
      route $ setExtension "html" +~+ gsubRoute "pages/" (const "")
      compile $ do

        getResourceBody
          >>= saveSnapshot "preload"
          >>= applyAsTemplate indexCtx
          >>= renderPandoc
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

    -- compileTemplates
    compileGallery


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
  df    <- mapM (flip getMetadataField "menu" . itemIdentifier) $ listToMaybe index
  case join df of
    Nothing -> return []
    Just  s -> menuItemsFrom $ words s

menuItemsFrom :: [String] -> Compiler [Item String]
menuItemsFrom menuElements = do
  cc <- mapM (\i -> loadAllSnapshots i "preload") menuFiles
  return $ concat cc
  where
        menuFiles :: [Pattern]
        menuFiles = map (\s -> fromGlob (s ++ ".*")) menuElements

indexCtx :: Context String
indexCtx =
  includeCtx <>
  groupCtx <>
  listField "menuItems" menuCtx indexMenuItems <>
  listField "posts" postCtx (recentFirst =<< loadAll "posts/*") <>
  defaultContext


--------------------------------------------------------------------------------
-- | Parse a "group_*: ..." metadata field into a ListField
groupCtx :: Context String
groupCtx = Context $ \k _ i -> do
  item <- getUnderlying
  md   <- getMetadata item
  let values = map (Item item) . concatMap words $ lookupString k md
  case (isPrefixOf "group_" k) of
    False -> empty
    True  -> return $ ListField defaultContext values

-- | Include an arbitrary page: '$include("foo/bar")'
includeCtx :: Context String
includeCtx = functionField "include" doInc
  where doInc (arg:_) _ = do
          newItems <- flip loadAllSnapshots "preload" . fromGlob . (++ ".*") $ arg
          return . fromMaybe (arg ++ " --not found--") . listToMaybe . map (lstrip . itemBody) $ newItems
        doInc [] _ = empty
        lstrip = unlines . map strip . lines

--------------------------------------------------------------------------------

getPicsInDir :: Compiler [Item CopyFile]
getPicsInDir = do
    postPath <- toFilePath <$> getUnderlying
    let pattern = fromGlob $ postPath ++ "/*.jpg"
    loadAll pattern

getSubGalleries :: Compiler [Item String]
getSubGalleries = do
    postPath <- toFilePath <$> getUnderlying
    let pattern = fromGlob $ postPath ++ "/*"
    loadAllSnapshots pattern "gallery"

compileGallery :: Rules ()
compileGallery = do
  let galleryDirs =
        unsafePerformIO $
        FF.find always (depth ==? 2 &&? FF.fileType ==? Directory) "images/gallery"

  create (traceShowId $ fromFilePath <$> galleryDirs) $ do
    route $ setExtension ".html" +~+ gsubRoute "images/" (const "")
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/gallery.html" picsCtx
        >>= saveSnapshot "preload"
        >>= saveSnapshot "gallery"
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

picContext :: Context CopyFile
picContext = urlField "url"

subGalleryContext :: Context [String]
subGalleryContext =
  urlField "url" <>
  capTitleField "title" <>
  (field "head" $ \item -> do
      first <- getMetadataField' (itemIdentifier item) "pics"
      return undefined)

picsCtx :: Context String
picsCtx =
    listField "pics" picContext getPicsInDir <>
    capTitleField "title" <>
    defaultContext

capTitleField = mapContext (T.unpack . T.toTitle . T.pack) . titleField
