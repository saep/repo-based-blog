{-# LANGUAGE OverloadedStrings #-}
module Web.RBB.Crawler.MetaCombinerSpec
    where

import           Control.Applicative
import           Control.Lens                 hiding (elements)
import qualified Data.IxSet                   as IxSet
import           Data.Monoid
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text                    as T (Text, length, pack)
import           Data.Time
import           System.FilePath              ((</>))
import           Web.RBB.Crawler.MetaCombiner as M
import           Web.RBB.Crawler.MetaParser   as M
import           Web.RBB.Types.Entry
import           Web.RBB.Types.FileType

import Test.Hspec      as Test
import Test.QuickCheck

noNewline c = c `notElem` ("\n\r" :: String)

instance Arbitrary Text where
    arbitrary = do
        r <- pack . filter noNewline <$> arbitrary
        return $ case T.length r of
            0 -> "foo"
            _ -> r

instance Arbitrary Day where
    arbitrary = ModifiedJulianDay . getNonNegative <$> arbitrary

instance Arbitrary DiffTime where
    arbitrary = picosecondsToDiffTime . getNonNegative <$> arbitrary

instance Arbitrary UTCTime where
    arbitrary = UTCTime <$> arbitrary <*> arbitrary

instance Arbitrary EntryUpdate where
    arbitrary = EntryUpdate <$> arbitrary <*> (getNonEmpty <$> arbitrary)

instance Arbitrary Entry where
    arbitrary = do
        i <- getNonNegative <$> arbitrary
        t <- arbitrary
        a <- arbitrary
        m <- arbitrary
        ts <- arbitrary
        ft <- arbitraryBoundedEnum
        u <- arbitrary
        (fp,rp) <- (\fp rp -> ("" </> getNonEmpty fp </> getNonEmpty rp, getNonEmpty rp))
                    <$> arbitrary <*> arbitrary
        return $ Entry
            { _entryId      = i
            , _title        = t
            , _author       = a
            , _authorEmail  = m
            , _tags         = ts
            , _fileType     = ft
            , _relativePath = rp
            , _fullPath     = fp
            , _updates      = IxSet.fromList [u]
            , _lastUpdate   = u
            }

instance Arbitrary TagQuantifier where
    arbitrary = arbitraryBoundedEnum

instance Arbitrary Meta where
    arbitrary = do
        i <- elements [0..3] :: Gen Int
        case () of
            _ | i == 0 -> M.Tags <$> arbitrary
            _ | i == 1 -> M.Title <$> arbitrary
            _ | i == 2 -> M.Context . filter noNewline <$> arbitrary
            _ -> return None

spec :: Spec
spec = do
    let t = UTCTime (ModifiedJulianDay 1337) (picosecondsToDiffTime 42)
    let eu = EntryUpdate t "foo"
    let mdMock = Entry
            { _entryId = 7
            , _title = "title"
            , _author = "me"
            , _authorEmail = "not@defin.ed"
            , _tags = mempty
            , _fileType = PandocMarkdown
            , _relativePath = "some/path"
            , _fullPath = "/root/for/some/path"
            , _updates = IxSet.fromList [eu]
            , _lastUpdate = eu
            }
        md1 = mdMock & title .~ "Foo Bar"
                     & tags  .~ Set.fromList ["foo", "bar", "quz"]
        md2 = mdMock & title .~ "Quz"
                     & tags  .~ Set.fromList ["one", "two"]
        md3 = mdMock & tags  .~ Set.fromList ["only one"]

    describe "updateTags" $ do
        it "should remain unchanged for an empty tag list" $ property $ do
            \x -> updateTags [] x `shouldBe` x

        it "should ovewrite any previous value if at least one tag is without\
           \ a prefix" $ property $ do
               \x ts -> (not . null . filter ((== TagReplace) . fst)) ts
                    ==> updateTags ts x `shouldBe` updateTags ts mempty

        Test.context "unit tests" $ do
            it "1)" $ do
                updateTags [(TagAdd, "added")] (md1^.tags)
                    `shouldBe` Set.insert "added" (md1^.tags)

            it "2)" $ do
                updateTags [(TagReplace, "replaced")] (md1^.tags)
                    `shouldBe` Set.singleton "replaced"

            it "3)" $ do
                updateTags [(TagRemove, "foo"),(TagRemove, "bar")] (md1^.tags)
                    `shouldBe` Set.singleton "quz"

            it "4)" $ do
                updateTags [(TagRemove, "only one")] (md3^.tags)
                    `shouldBe` mempty


    describe "contract" $ do
        let notContext m = case m of
                (M.Context _) -> False
                _           -> True

        it "should create an empy map if no context is given" $ property $ do
            \x -> let meta = filter notContext x
                  in contract Nothing meta mempty `shouldBe` mempty

        it "should match the case 1" $ do
            let p = md1^.relativePath
                meta = [ M.Context p
                       , M.Tags [(TagReplace, "foo"), (TagAdd, "quz")]
                       , M.Title "Foo Bar"
                       , M.Tags [(TagAdd, "bar")]
                       ]
            contract Nothing meta (IxSet.fromList [mdMock])
                `shouldBe` IxSet.fromList [md1]

        it "should match the case 2" $ do
            let p = md2^.relativePath
                md = mdMock & relativePath .~ p
                meta = [ M.Title "Quz"
                       , M.Tags [(TagAdd, "three")]
                       , M.Tags [(TagAdd, "one")]
                       , M.Tags [(TagAdd, "two")]
                       , M.Tags [(TagRemove, "three")]
                       ]
            contract (Just p) meta (IxSet.fromList [md])
                `shouldBe` IxSet.fromList [md2]

        it "should match the case 3" $ do
            let p = md3^.relativePath
                md = mdMock & relativePath .~ p
                meta = [ M.Title "bar"
                       , M.Tags [(TagReplace, "only one")]
                       , M.Title "title"
                       ]
            contract (Just p) meta (IxSet.fromList [md])
                `shouldBe` IxSet.fromList [md3]

