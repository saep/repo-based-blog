module Web.Saeplog.Blog.CrawlerSpec
    where

import Web.Saeplog.Blog.Crawler
import Web.Saeplog.Blog.Types

import           Control.Monad.Trans.Except
import           Data.FileStore
import qualified Data.Set                   as Set
import           System.Directory
import           System.FilePath

import Test.Hspec

-- Note: This has been added in base >=4.7 to Data.Either
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

instance Show FileStore where
    show _ = "FileStore dummy"

instance Show FileStoreData where
    show _ = "FileStoreData dummy"

spec :: Spec
spec = do
    describe "findDirInParents" $ do
        it "should detect this git repository as the directory for .git" $ do
            repo <- findDirInParents "test-resources" ".git"
            cwd <- getCurrentDirectory >>= canonicalizePath
            repo `shouldBe` Just cwd

        it "should find the pseudo _darcs directory" $ do
            cwd <- getCurrentDirectory >>= canonicalizePath
            findDirInParents ("test-resources" </> "_darcs" </> ".hg") "_darcs"
                `shouldReturn` Just (cwd </> "test-resources")

        it "should find the pseudo .hg directory" $ do
            cwd <- getCurrentDirectory >>= canonicalizePath
            let hgPath = "test-resources" </> "_darcs"
            findDirInParents hgPath ".hg"
                `shouldReturn` Just (cwd </> hgPath)

        it "should fail in this case" $ do
            findDirInParents "test-resources" ".hg"
                `shouldReturn` Nothing

    describe "initializeFileStore" $ do
        it "should find one in the current repository" $ do
            fs <- runExceptT (initializeFileStore "test-resources")
            fs `shouldSatisfy` isRight

        it "should find a commit with the message \"Initial Commit\"" $ do
            Right fsd <- runExceptT (initializeFileStore "test-resources")
            rev <- revision (fileStore fsd) "52114b620cf80ee01501417cfaf698c035437915"
            revDescription rev `shouldBe` "Initial commit"
            contentRelativePath fsd `shouldBe` "test-resources"

    describe "collectEntryData" $ do
        it "should match this test case" $ do
            entries <- collectEntryData "test-resources"
            Set.size entries `shouldBe` 2
            Set.map fileType entries
                `shouldBe` Set.fromList [ PandocMarkdown, LiterateHaskell ]
            Set.map relativePath entries
                `shouldBe` Set.fromList [ "test-resources/toplevel.md"
                                        , "test-resources/nested/test/file.lhs" ]

            Set.map (Set.size . updates) entries
                `shouldBe` Set.fromList [1,2]



