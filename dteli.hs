--------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend, (<>))
import Hakyll

import qualified Text.Pandoc.Options as Pdo

import qualified Data.Set            as S
import qualified Data.Map            as M

--------

configuratie :: Configuration
configuratie = defaultConfiguration
  {
    deployCommand = "rsync -cave ssh _site/ chartrex_dteli@ssh.phx.nearlyfreespeech.net:/home/public/",
    previewHost = "192.168.1.80",
    previewPort = 8448
  }

-- ===========================================================

main :: IO ()
main = hakyllWith configuratie $ do


    match "css/*.css" $ do
        route       idRoute
        compile   $ getResourceString
                 >>= withItemBody (unixFilter "/home/winfield/.node_modules/bin/postcss" ["--use", "autoprefixer"])
                 >>= return . fmap compressCss

    match ("img/*.png" .||. "img/*.jpg" .||. "img/*.gif"
           .||. "img/grounds/*.JPG" .||. "img/grounds/grndex.json"
           .||. "img/posts/*"
           .||. "aanvullend/*"
           .||. "robots.txt" .||. "favicon.ico"
           .||. "res/res0.1.html" .||. "res/res.css") $ do
        route    idRoute
        compile  copyFileCompiler

    match "templates/*" $ compile $ templateCompiler




    match ("index.html" .||. "contact.html" .||. "res/res_if.html") $ do
        route      idRoute
        compile  $ do
            getResourceBody
                >>= loadAndApplyTemplate "templates/staticcontent.html" staticContext
                >>= loadAndApplyTemplate "templates/staticpage.html" staticContext
                >>= relativizeUrls

    match ("about.md" .||. "rte.md") $ do
        route    $ setExtension "html"
        compile  $ do
            pandocCompilerWith pandocReaderOptions pandocWriterOptions
                >>= loadAndApplyTemplate "templates/staticcontent.html" staticContext
                >>= loadAndApplyTemplate "templates/staticpage.html" staticContext
                >>= relativizeUrls


    -- match ("yield/*.md") $ do             -- yindex, scrawls, ..  mixes, tapas
    --     route    $ setExtension "html"
    --     compile  $ do
    --         pandocCompilerWith pandocReaderOptions pandocWriterOptions
    --             >>= loadAndApplyTemplate "templates/barecontent.html" yieldContext
    --             >>= loadAndApplyTemplate "templates/yieldpage.html" yieldContext
    --             >>= relativizeUrls



    -- match ("lps/*.md") $ do
    --   route $ setExtension "html"
    --   compile $ do
        


    -- match (    ) $ do
    --     route    $ setExtension "html"
    --     compile  $ do
    --         pandocCompilerWith pandocReaderOptions pandocWriterOptions
    --             >>= loadAndApplyTemplate "templates/protobankcontent.html" protobankContext
    --             >>= loadAndApplyTemplate "templates/protobankpage.html" protobankContext
    --             >>= relativizeUrls



--  match ("meta/ntc.md" .||. "meta/to-implement.md") $ do
    match ("meta/*.md") $ do
        route    $ setExtension "html"
        compile  $ do
            pandocCompilerWith pandocReaderOptions pandocWriterOptions
                >>= loadAndApplyTemplate "templates/barecontent.html" siteContext
                >>= loadAndApplyTemplate "templates/sitepage.html" siteContext
                >>= relativizeUrls

    match ("meta/*.html") $ do
        route      idRoute
        compile  $ do
            getResourceBody
                >>= loadAndApplyTemplate "templates/barecontent.html" siteContext
                >>= loadAndApplyTemplate "templates/sitepage.html" siteContext
                >>= relativizeUrls      




    match "posts/*.md" $ do
        route    $ setExtension "html"
        compile  $ do
            posts <- recentFirst =<< recentPosts
            let postContextPlusPosts =
                    listField "posts" postContext (return posts)
                 <> postContext
            pandocCompilerWith pandocReaderOptions pandocWriterOptions
                >>= loadAndApplyTemplate "templates/postcontent.html" postContextPlusPosts
                >>= loadAndApplyTemplate "templates/postpage.html" postContextPlusPosts
                >>= relativizeUrls






-- ===========================================================

globalContext :: Context String
globalContext = field "lpUrl" (\_ -> latestPostUrl)
    <> defaultContext

staticContext :: Context String
staticContext = constField "themecolor" "#F74D4D"  -- red
    <> constField "sector" "static"
    <> globalContext

-- yieldContext :: Context String
-- yieldContext = constField "themecolor" "#FF872B"  -- orange
--     <> constField "sector" "yield"
--     <> globalContext

-- vinylContext :: Context String
-- vinylContext = constField "themecolor" "#FFED47"  -- yellow
--     <> dateField "date" "%Y %B %e"
--     <> constField "sector" "vinyl"
--     <> globalContext

-- protobankContext :: Context String
-- protobankContext = field "listlist" (\_ ->  protobankIndex  )
--     <> constField "themecolor" "#2BFF87"  -- green
--     <> constField "sector" "protobank"
--     <> globalContext

siteContext :: Context String
siteContext = constField "themecolor" "#2BA0CB"  -- blue
    <> constField "sector" "site"
    <> globalContext

postContext :: Context String
postContext = field "entries" (\_ -> recentPostList)
    <> dateField "date" "%Y %B %e"
    <> constField "themecolor" "#872BFF"  -- purple
    <> constField "sector" "posts"
    <> globalContext



-- ===========================================================

-- thank you Danny Su for some of these
-- (http://dannysu.com/2013/03/20/hakyll-4/)

recentPosts :: Compiler [Item String]
recentPosts = do
    identifiers <- getMatches "posts/*.md"
    return [Item identifier "" | identifier <- identifiers]

recentPostList :: Compiler String
recentPostList = do
    posts <- recentFirst =<< recentPosts
    itemTpl <- loadBody "templates/postli.html"
    list <- applyTemplateList itemTpl postContext posts
    return list

latestPostUrl :: Compiler String
latestPostUrl = do
    latestPost <- fmap (return . head) . recentFirst =<< recentPosts
    itemTpl <- loadBody "templates/posturl.html"
    lpUrl <- applyTemplateList itemTpl defaultContext latestPost
    return lpUrl

-- ==========================================================


pandocReaderOptions :: Pdo.ReaderOptions
pandocReaderOptions = defaultHakyllReaderOptions
                      -- { Pdo.readerExtensions = S.union (Pdo.readerExtensions Pdo.def) (S.fromList [])
                      -- { Pdo.readerExtensions = Pdo.readerExtensions Pdo.def,
                      { Pdo.readerExtensions = Pdo.pandocExtensions
                      }


pandocWriterOptions :: Pdo.WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions
                      { Pdo.writerHTMLMathMethod = Pdo.MathJax "https://cdn.mathjax.org/mathjax/2.7-latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
                      -- , writerHTMLMathMethod = MathJax "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
                      , Pdo.writerExtensions = Pdo.pandocExtensions
                      }

-- -----------------------------------------------------------


-- [textual substitution]
