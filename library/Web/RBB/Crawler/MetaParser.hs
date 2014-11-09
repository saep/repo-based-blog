{- |
Module      :  RBB.Crawler.MetaParser
Description :  Extract meta data from the repository
Copyright   :  (c) Sebastian Witte
License     :  BSD3

Maintainer  :  woozletoff@gmail.com
Stability   :  experimental

-}
module RBB.Crawler.MetaParser
    where

import Control.Applicative (pure, (*>), (<$>), (<*), (<*>))
import Control.Monad
import Data.Char
import Data.Text           (Text, pack)
import Text.Parsec

-- | The 'Meta' data type represents a model of all the meta data that can be
-- embedded into a commit.
data Meta =
    Tags [(TagQuantifier, Text)]
    -- ^ $tag
    | Title Text
    -- ^ $title
    | Context FilePath
    -- ^ Context entry, usually a relative path for the blog entry repository.
    --
    -- There is no validity check performed.
    | None
    -- ^ Everything that is not a tag quantified in pieces of text followed by
    -- two newlines or the end of input.
    deriving (Eq, Show)

-- | Data type representing a the prefix of a tag.
data TagQuantifier
    = TagAdd
    -- ^ Add the tag to the current set of tags.
    | TagRemove
    -- ^ Remove the tag from the current set of tags.
    | TagReplace
    -- ^ Replace the current set of tags with all the tags given in this tag
    -- definition.
    deriving (Eq, Show, Read, Ord, Enum, Bounded)

-- | Helper parser for a case-insensitive character.
ciChar :: Char -> Parsec String u Char
ciChar c = char (toLower c) <|> char (toUpper c)

-- | Helper parser for a case-insensitive string.
ciString :: String -> Parsec String u String
ciString = mapM ciChar

-- | Parse meta data from the given 'String'. The order of the list items is
-- the same as the order in which the meta data appears in the input.
parseMeta :: String -> Either ParseError [Meta]
parseMeta inp =
    parse (many pSplice) "slice" (inp++"\n\n")
        >>= mapM (parse pMeta "meta parser") . filter (not . all isSpace)

pSplice :: Parsec String u String
pSplice = blankLines
    <|> unwords . words . unlines <$> many1 (notFollowedBy blankLine *> anyLine)

skipSpaces :: Parsec String u ()
skipSpaces = skipMany $ char ' ' <|> char '\t'

blankLine :: Parsec String u Char
blankLine = skipSpaces *> newline

blankLines :: Parsec String u String
blankLines = many1 blankLine

anyLine :: Parsec String u String
anyLine = skipSpaces *> (anyChar `manyTill` newline)

pMeta :: Parsec String u Meta
pMeta = try pTags <|> try pTitle <|> try pContext <|> pNone

pNone :: Parsec String u Meta
pNone = const None <$> many1 anyChar

-- $tag
--
-- A tag can be applied to any published document. For now they are associated
-- with the changed files of a commit. This means that you have to edit a file
-- in order to make update the tags. (Adding/Removing an empty line at the end
-- will do.) This limitation lies within the technical choice of meta data
-- representation. In the future, features from within the blog could be used to
-- do such simple tasks.
--
-- The sytax of tags is quite simple:
-- @tag[s]:@ in any case followed by a space or newline separated list of tags.
-- Tags which spaces can be escaped with quotation marks.
--
-- >>> parseMeta "tAgS   :  foo \"bar'mitz wa\" +'quz''"
--
-- Tags can also be prefixed with @+@ or @-@. If only tags with a prefix are
-- used, they will be added or removed from the current state of the blog's
-- entry. However, if at leas one of the provided tags has no prefix, the tags
-- will be overwritten by the given tags.
--

pTags :: Parsec String u Meta
pTags = Tags
    <$> (try (ciString "tag" *> optional (ciChar 's') *> try spaces *> char ':')
    *> spaces *> pSpaceElements <* spaces)

pQuantifierPrefix :: Parsec String u TagQuantifier
pQuantifierPrefix = quantify <$> (try (char '+') <|> try (char '-') <|> pure '=')
  where
    quantify c
        | c == '+' = TagAdd
        | c == '-' = TagRemove
        | otherwise = TagReplace

pSpaceElements :: Parsec String u [(TagQuantifier, Text)]
pSpaceElements = many pSpaceDelimitedElement

pSpaceDelimitedElement :: Parsec String u (TagQuantifier, Text)
pSpaceDelimitedElement = (\q t -> (q, pack t))
    <$> pQuantifierPrefix
    <*> (char '"' *> anyChar `manyTill` char '"' <* tryEndOfTag
        <|> char '\'' *> anyChar `manyTill` try (char '\'' *> tryEndOfTag)
        <|> ((:) <$> satisfy (not . isSpace)
                 <*> anyChar `manyTill` tryEndOfTag))

tryEndOfTag :: Parsec String u ()
tryEndOfTag = (try (void space) *> spaces) <|> eof

-- $title
-- A title is a String delimited by 2 newlines or the end of input. Newlines
-- withing the title defition are replaced by spaces.
--
-- >>> parseMeta "title: This is an example\nwith a newline within it"
--
pTitle :: Parsec String u Meta
pTitle = Title . pack . unwords
    <$> (try (ciString "title") *> spaces *> char ':' *> spaces
    *> many ((:) <$> anyChar <*> anyChar `manyTill` tryEndOfTag))

pContext :: Parsec String u Meta
pContext = Context . unwords
    <$> (try (ciString "context") *> spaces *> char ':' *> spaces
    *> many ((:) <$> anyChar <*> anyChar `manyTill` tryEndOfTag))

