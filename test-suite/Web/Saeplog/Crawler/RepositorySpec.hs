{-# LANGUAGE OverloadedStrings #-}
module Web.Saeplog.Crawler.RepositorySpec
    where

import Web.Saeplog.Crawler.Repository
import Web.Saeplog.Types

import Control.Monad.Trans.Except
import Data.FileStore
import Data.IxSet
import Data.List                  (sort)
import System.Directory
import System.FilePath

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

            size entries `shouldBe` 2

            size (entries @= AuthorName "Sebastian Witte")
                `shouldBe` 2

            map _fileType (toList entries)
                `shouldBe` [ PandocMarkdown, LiterateHaskell ]
            map _relativePath (toList entries)
                `shouldBe` [ "test-resources/toplevel.md"
                           , "test-resources/nested/test/file.lhs" ]

            (sort . map (size . _updates) . toList) entries
                `shouldBe` [1,2]





