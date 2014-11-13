{-# LANGUAGE OverloadedStrings #-}
module Web.RBB.Crawler.RepositorySpec
    where

import Web.RBB.Crawler.Repository
import Web.RBB.Templates.Default
import Web.RBB.Types
import Web.RBB.Types.Blog

import Control.Lens
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

instance Show (Blog m) where
    show _ = "Blog dummy"

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

        it "should find a commit with the message \"Initial commit\"" $ do
            Right (_,crp,fs) <- runExceptT (initializeFileStore "test-resources")
            rev <- revision fs "52114b620cf80ee01501417cfaf698c035437915"
            revDescription rev `shouldBe` "Initial commit"

            crp `shouldBe` "test-resources"

    describe "collectEntryData" $ do
        it "should match this test case" $ do
            Right blog <- runExceptT $ initBlog $ createDefaultBlogConfig "test-resources"
            let b = blog :: Blog Identity

            size (b^.entries) `shouldBe` 2

            size ((b^.entries) @= AuthorName "Sebastian Witte")
                `shouldBe` 2

            sort (map _fileType (toList (b^.entries)))
                `shouldBe` sort [ PandocMarkdown, LiterateHaskell ]
            sort (map _relativePath (toList (b^.entries)))
                `shouldBe` sort [ "test-resources/toplevel.md"
                                , "test-resources/nested/test/file.lhs" ]

            (sort . map (size . _updates) . toList) (b^.entries)
                `shouldBe` [1,2]





