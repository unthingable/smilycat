--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend, (<>))
import           Hakyll
import Debug.Trace
import Data.Function (on)
import Data.String
import Data.String.Utils (replace, strip)
import Data.List --(intercalate, head, isPrefixOf)
import Control.Applicative ((<$>), empty)
import Data.Maybe
import Control.Monad --(join, msum)
import qualified Data.HashMap.Strict            as HMS
import qualified Data.Text                      as T
import System.FilePath --(takeDirectory, takeBaseName)
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

    compileGallery

    match (fromList ["about.rst", "contact.markdown"]) $ do
        route   $ setExtension "html"
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "templates/default.html" defaultContext
            >>= relativizeUrls

    match "cats/*" $ do
        route $ setExtension "html"
        compile $ pandocCompiler
          >>= loadAndApplyTemplate "templates/cat.html"     indexCtx
          >>= saveSnapshot "preload"
          >>= loadAndApplyTemplate "templates/default.html" indexCtx
          >>= relativizeUrls

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


--------------------------------------------------------------------------------
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
  where
    doInc (arg:_) _ = do
      newItems <- flip loadAllSnapshots "preload" . fromGlob $ arg
      return . maybe (arg ++ " --not found--") (lstrip . itemBody) $ listToMaybe newItems
    doInc _ _ = empty
    lstrip = unlines . map strip . lines

--------------------------------------------------------------------------------

getPicsInDir :: Compiler [Item CopyFile]
getPicsInDir = do
    path <- toFilePath <$> getUnderlying
    let pattern = foldl1 (.||.) $ map (fromGlob . (path ++)) ["/*.jpg", "/*.JPG"]
    -- liftM (sortOn itemBody) $
    loadAll pattern

getSubPics :: Compiler [Item CopyFile]
getSubPics = do
    path <- toFilePath <$> getUnderlying
    let pattern = foldl1 (.||.) $ map (fromGlob . (path ++)) ["/*/*.jpg", "/*/*.JPG"]
    items <- loadAll pattern
    -- get one from each
    let groups = groupBy ((==) `on` (takeBaseName . takeDirectory . toFilePath . itemIdentifier)) items
        heads  = catMaybes . map listToMaybe $ groups
    return $ heads

-- a little fragile, but works
getCrumbs :: Compiler [Item String]
getCrumbs = do
  path <- toFilePath <$> getUnderlying
  -- drop 2 because "image/gallery"
  return . map pathItem . drop 2 $ crumbs path []
  where crumbs "" ps = ps
        crumbs s ps  = crumbs (minusOne s) (s : ps)
        minusOne     = dropTrailingPathSeparator . joinPath . init . splitPath

compileGallery :: Rules ()
compileGallery = do
  let galleryDirs =
        unsafePerformIO $
        FF.find always (depth >=? 2 &&? FF.fileType ==? Directory) "images/gallery"

  create (traceShowId $ fromFilePath <$> reverse galleryDirs) $ do
    route $ gsubRoute "images/" (const "") +~+ setExtension ".html"
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/gallery.html" picsCtx
        >>= saveSnapshot "preload"
        >>= loadAndApplyTemplate "templates/default.html" indexCtx
        >>= relativizeUrls

picContext :: Context CopyFile
picContext = urlField "url"

subGalleryContext :: Context CopyFile
subGalleryContext =
  urlField "url" <>
  (field "subTitle" $ \item -> do
      return . subGalleryTitle . toFilePath $ itemIdentifier item) <>
  (field "subUrl" $ \item -> do
      return . subGalleryUrl . toFilePath $ itemIdentifier item)
  where
    -- Careful with that path, Eugene - has to match the routes in compileGallery exactly
    subGalleryTitle = takeBaseName . takeDirectory
    subGalleryUrl = (\s -> "/" ++ s ++ ".html") . dropTrailingPathSeparator . joinPath . tail . init . splitPath

picsCtx :: Context String
picsCtx =
  listField "subs" subGalleryContext getSubPics <>
  listField "pics" picContext getPicsInDir <>
  listField "crumbs" defaultContext getCrumbs <>
  titleField "title" <>
  defaultContext

pathItem x = Item (fromFilePath x) x
