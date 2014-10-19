{-# LANGUAGE OverloadedStrings #-}
module Web.Saeplog.Crawler.MetaParserSpec
    where

import Web.Saeplog.Crawler.MetaParser

import Test.Hspec

import Data.Function (on)
import Control.Monad
import Text.Parsec (parse, ParseError)
import Data.List (intercalate, permutations, sortBy)

instance Eq ParseError where
    (==) = (==) `on` show

isLeft :: Either l r -> Bool
isLeft (Left _) = True
isLeft _        = False

spec :: Spec
spec = parallel $ do
    let ptest p = parse p "MetaSpec"
    describe "pSpaceDelimitedElement" $ do
        let p = ptest pSpaceDelimitedElement
        it "should parse a spaceless string" $ do
            p "foo" `shouldBe` Right (TagReplace, "foo")
            p "-foo" `shouldBe` Right (TagRemove, "foo")
            p "+foo" `shouldBe` Right (TagAdd, "foo")

        it "should fail on a space at the beginning" $ do
            p " bar" `shouldSatisfy` isLeft

        it "should parse a double quoted element" $ do
            p "\"foo\"" `shouldBe` Right (TagReplace, "foo")
            p "-\"foo\"" `shouldBe` Right (TagRemove, "foo")
            p "+\"foo\"" `shouldBe` Right (TagAdd, "foo")

            p "\"foo bar quz\"" `shouldBe` Right (TagReplace, "foo bar quz")
            p "-\"foo bar quz\"" `shouldBe` Right (TagRemove, "foo bar quz")
            p "+\"foo bar quz\"" `shouldBe` Right (TagAdd, "foo bar quz")

        it "should parse a single quoted element" $ do
            p "'foo'" `shouldBe` Right (TagReplace, "foo")
            p "-'foo'" `shouldBe` Right (TagRemove, "foo")
            p "+'foo'" `shouldBe` Right (TagAdd, "foo")

            p "'foo bar quz'" `shouldBe` Right (TagReplace, "foo bar quz")
            p "-'foo bar quz'" `shouldBe` Right (TagRemove, "foo bar quz")
            p "+'foo bar quz'" `shouldBe` Right (TagAdd, "foo bar quz")

    let cmp = compare `on` snd
        complexCase = sortBy cmp [ (TagAdd, "'bar mit'zwa")
                                 , (TagReplace, "foo")
                                 , (TagRemove, "quz'")
                                 ]
    describe "pSpaceElements" $ do
        let p = ptest pSpaceElements
        it "should parse simple elements correctly" $ do
            p "foo -bar +quz"
                `shouldBe` Right [(TagReplace, "foo"), (TagRemove, "bar"),(TagAdd, "quz")]

        it "should parse a mixture of different elements" $ do
            let perms = permutations ["foo", "+\"'bar mit'zwa\"", "-\'quz'\'"]
                intercalations = [" ", "      ", "  \n \n \n"]

            forM_ perms $ \perm -> forM intercalations $ \ic ->
                case (p . intercalate ic) perm of
                    Right xs -> sortBy cmp xs `shouldBe` complexCase
                    Left err -> error $ show err

    describe "parseMeta" $ do
        context "tags with a complex result case" $ do
            let complexCase' = Right [Tags complexCase]
            it "should parse the case without newlines" $ do
                parseMeta "taG:+\"'bar mit'zwa\" foo -quz'"
                    `shouldBe` complexCase'
            it "should parse the case with newlines everywhere" $ do
                parseMeta "tAgS:\n+\"'bar mit'zwa\"\n \nfoo     \n -quz'\n\n\n"
                    `shouldBe` complexCase'


        context "title" $ do
            it "should handle an eof terminated single line case" $ do
                let result = Right [Title "foo bar quz"]
                parseMeta "tiTLE: foo bar quz" `shouldBe` result
                parseMeta "titLe  :  foo bar quz" `shouldBe` result
                parseMeta "title:  foo bar quz" `shouldBe` result
                parseMeta "titlE  :foo bar quz" `shouldBe` result
                parseMeta "Title:foo bar quz" `shouldBe` result
                parseMeta "TItlE:foo\nbar\nquz\n\n" `shouldBe` result

        context "context" $ do
            it "should handle an eof terminated single line case" $ do
                let result = Right [Context "foo bar quz"]
                parseMeta "context: foo bar quz" `shouldBe` result
                parseMeta "context  :  foo bar quz" `shouldBe` result
                parseMeta "context:  foo bar quz" `shouldBe` result
                parseMeta "context  :foo bar quz" `shouldBe` result
                parseMeta "context:foo bar quz" `shouldBe` result
                parseMeta "context:foo\nbar\nquz\n\n" `shouldBe` result


