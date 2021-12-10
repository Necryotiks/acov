import Development.Shake
  ( Lint (LintBasic),
    ShakeOptions (shakeFiles, shakeLint, shakeThreads),
    cmd_,
    getDirectoryFiles,
    need,
    phony,
    putInfo,
    removeFilesAfter,
    shakeArgs,
    shakeOptions,
    want,
    (%>),
  )
import Development.Shake.Command (cmd_)
import Development.Shake.FilePath ((</>))
import System.Directory.Extra (removeDirectoryRecursive)

main :: IO ()
main = shakeArgs shakeOptions {shakeThreads = 0, shakeLint = Just LintBasic, shakeFiles = "build"} $ do
  want ["build/lib/dpi-64/libacovdpi.so", "bin/acov"]

  phony "clean" $ do
    putInfo "Removing build objects(*.o,*.hi,*.dyn_o,*.dyn_hi)"
    removeFilesAfter "./" ["//*.o", "//*.hi", "//*.dyn_*"]

  phony "nuke" $ do
    need ["clean"]
    putInfo "Purging build directory"
    removeFilesAfter "build" ["//*"]

  --TODO: Add install
  "cover64.o" %> \out -> do
    src <- getDirectoryFiles "dpi" ["//*.cc"]
    let psrc = map ("dpi" </>) src
    cmd_ "gcc -c -fpic -Os -Idata" psrc "-m64" "-o" out

  -- "cover32.o" %> \out -> do
  --   src <- getDirectoryFiles "dpi" ["//*.cc"]
  --   let psrc = map ("dpi" </>) src
  --   cmd_ "gcc -c -fpic -Os -Idata" psrc "-m32" "-o" out

  "build/lib/dpi-64/libacovdpi.so" %> \out -> do
    let src = "cover64.o"
    need [src]
    cmd_ "gcc -shared -m64 -Os -Idata -o" out src

  -- "lib/dpi-32/libacovdpi.so" %> \out -> do
  --   let src = "cover32.o"
  --   need [src]
  --   cmd_ "gcc -shared -m32 -Os -Idata -o" out src

  "bin/acov" %> \out -> do
    src <- getDirectoryFiles "src" ["//*.hs"]
    let psrc = map ("src" </>) src
    need psrc
    cmd_ "ghc -O2 -Wall -XStrictData -fwrite-ide-info -hiedir=.hie" psrc "-o" out

  -- "bin/acov-test" %> \out -> do
  --   src <- getDirectoryFiles "src" ["frontend/*.hs"]
  --   test <- getDirectoryFiles "test" ["test/*Spec.hs"]
  --   let psrc = map ("src" </>) src
  --   let ptest = map ("test" </>) test
  --   need psrc
  --   need test
  --   cmd_ "ghc -O2 -Wall" psrc test "-o" out 
