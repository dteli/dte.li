--------
{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend, (<>))
import Hakyll

--import qualified Text.Pandoc.Options as Pandocptions        -- couldn't resist
import qualified Text.Pandoc.Options as Pdo

import qualified Data.Set            as S
import qualified Data.Map            as M

-- import my color style?
--import qualified Text.Highlighting.Kate.Types as Katetypes

--------

configuratie :: Configuration
configuratie = defaultConfiguration
            {
              deployCommand = "rsync -cave ssh _site/ chartrex_dteli@ssh.phx.nearlyfreespeech.net:/home/public/",
              previewHost = "192.168.1.80",
              previewPort = 8448
            }

main :: IO ()
main = hakyllWith configuratie $ do


-- ===========================================================

    match ("images/*" .||. "aanvullend/*") $ do
        route    idRoute
        compile  copyFileCompiler

    match ("robots.txt" .||. "favicon.ico") $ do
        route    idRoute
        compile  copyFileCompiler

-- ===========================================================

    match "css/*.css" $ do
        route       idRoute
        compile   $ getResourceString
                 >>= withItemBody (unixFilter "/home/winfield/.node_modules/bin/postcss" ["--use", "autoprefixer"])
                >>= return . fmap compressCss

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


    match ("about.md" .||. "rte.md") $ do
        route    $ setExtension "html"
        compile  $ do
            pandocCompilerWith pandocReaderOptions pandocWriterOptions
                >>= loadAndApplyTemplate "templates/staticcontent.html" staticContext
                >>= loadAndApplyTemplate "templates/staticpage.html" staticContext
                >>= relativizeUrls


    -- match (    ) $ do             -- yindex, scrawls, ..  mixes, tapas
    --     route    $ setExtension "html"
    --     compile  $ do
    --         pandocCompilerWith pandocReaderOptions pandocWriterOptions
    --             >>= loadAndApplyTemplate "templates/barecontent.html" yieldContext
    --             >>= loadAndApplyTemplate "templates/yieldpage.html" yieldContext
    --             >>= relativizeUrls


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

--    match ("meta/*.html") $ do

    match ("index.html" .||. "contact.html") $ do
        route      idRoute
        compile  $ do
            getResourceBody
                >>= loadAndApplyTemplate "templates/staticcontent.html" staticContext
                >>= loadAndApplyTemplate "templates/staticpage.html" staticContext
                >>= relativizeUrls


    match "templates/*" $ compile $ templateCompiler




-- ===========================================================

globalContext :: Context String
globalContext = field "lpUrl" (\_ -> latestPostUrl)
    <> defaultContext

staticContext :: Context String
staticContext = constField "themecolor" "#F74D4D"
    <> globalContext

yieldContext :: Context String
yieldContext = constField "themecolor" "#FF872B"
    <> globalContext

protobankContext :: Context String
protobankContext = constField "themecolor" "#2BFF87"
    <> globalContext

siteContext :: Context String
siteContext = constField "themecolor" "#2BA0CB"
    <> globalContext

postContext :: Context String
postContext = field "entries" (\_ -> recentPostList)
    <> dateField "date" "%Y %B %e"
    <> constField "themecolor" "#872BFF"
    <> constField "sector" "posts"
    <> globalContext




-- ===========================================================


--------
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



--autoprefixer :: Compiler String
--autoprefixer = getResourceString >>= withItemBody (unixFilter "autoprefixer" [])




-- ===========================================================
-- ===========================================================


pandocReaderOptions :: Pdo.ReaderOptions
pandocReaderOptions = defaultHakyllReaderOptions
                      -- { Pdo.readerExtensions = S.union (Pdo.readerExtensions Pdo.def) (S.fromList [])
                      -- { Pdo.readerExtensions = Pdo.readerExtensions Pdo.def,
                      { Pdo.readerExtensions = Pdo.pandocExtensions
                      }


pandocWriterOptions :: Pdo.WriterOptions
pandocWriterOptions = defaultHakyllWriterOptions
                      { Pdo.writerHtml5 = True
                      , Pdo.writerHTMLMathMethod = Pdo.MathJax ""
                      -- , writerHTMLMathMethod = MathJax "https://cdn.mathjax.org/mathjax/latest/MathJax.js?config=TeX-AMS-MML_HTMLorMML"
                      -- , writerExtensions =  ...
                      }

-- -----------------------------------------------------------


-- txtsubs :: M.Map String String
-- txtsubs :: M.fromList
--     [ ("LaTeX", "<span class=\"tex\">L<sup>a</sup>T<sub>e</sub>X</span>"),
--     ]


--theGreatFilter :: String → String
























-- -----------------------------------------------------------


-- csl, bib
-- match "references.bib" $ compile biblioCompiler
-- match "ieee-trigraph.csl" $ compile cslCompiler
--   (alt. "ieee-with-url.csl")
--   (include [..].csl in etc directory?
--    or is that incompatible with routing etc/* above?)


-- -----------------------------------------------------------

-- tags, categories


-- -----------------------------------------------------------


-- feed config?

-- comment system?














-- -----------------------------------------------------------

-- v2palette :: Style
-- v2palette = Style {
--     backgroundColor = toColor "#060606"
--   , defaultColor = toColor "#F8F8F8"
--   , lineNumberColor = toColor "#888888"
--   , lineNumberBackgroundColor = Nothing
--   , tokenStyles =
--     [ (KeywordTok, defStyle{ tokenColor = toColor "#F74D4D", tokenBold = True })
--     , (DataTypeTok, defStyle{ tokenColor = toColor "#FA8585" })
--     , (DecValTok, defStyle{ tokenColor = toColor "#FF872B" })
--     , (BaseNTok, defStyle{ tokenColor = toColor "#FFAD6E" })
--     , (FloatTok, defStyle{ tokenColor = toColor "#FFFB18" })
--     , (CharTok, defStyle{ tokenColor = toColor "#FFFD9C" })
--     , (StringTok, defStyle{ tokenColor = toColor "#2BFF87" })
--     , (CommentTok, defStyle{ tokenColor = toColor "#A1FFCA", tokenItalic = True })
--     , (OtherTok, defStyle{ tokenColor = toColor "#2BA0CB" })
--     , (AlertTok, defStyle{ tokenColor = toColor "#98D3E9", tokenBold = True })
--     , (FunctionTok, defStyle{ tokenColor = toColor "#872BFF" })
--     , (ErrorTok, defStyle{ tokenColor = toColor "#CCA6FF", tokenBold = True })
--     ]
--   }


