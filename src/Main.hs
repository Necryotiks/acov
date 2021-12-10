{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Acov.Core.Verilog as Verilog
import qualified Acov.Frontend.Interface as Frontend
import qualified Acov.Report.CountPass as CountPass
import qualified Acov.Report.Merge as Merge
import qualified Acov.Report.Raw as Raw
import Acov.Report.Report (report)
import Control.Monad (Monad (return, (>>), (>>=)), foldM, when)
import Data.ByteString (ByteString, writeFile)
import Data.FileEmbed (embedFile)
import Options.Applicative
  ( Applicative ((<*>)),
    Parser,
    command,
    execParser,
    fullDesc,
    header,
    help,
    helper,
    hsubparser,
    info,
    long,
    metavar,
    progDesc,
    short,
    strOption,
    switch,
    value,
    (<$>),
  )
import System.Directory (createDirectoryIfMissing)
import System.Exit (exitFailure, exitSuccess)
import System.FilePath ((</>))
import System.IO
  ( FilePath,
    IO,
    IOMode (WriteMode),
    getContents,
    hPutStr,
    stderr,
    withFile,
  )
import Prelude hiding (writeFile)

data Command
  = Generate {input :: !FilePath, odir :: !FilePath}
  | Report {input :: !FilePath, odir :: !FilePath, ignoreHash :: !Bool}

subCommand :: Parser Command
subCommand =
  hsubparser
    ( command "generate" (info generateCmd (progDesc "Generate functional coverage bindings"))
        <> command "report" (info reportCmd (progDesc "Report functional coverage"))
    )

generateCmd :: Parser Command
generateCmd =
  Generate
    <$> strOption
      ( long "input"
          <> short 'c'
          <> metavar "FILE"
          <> value "input.acov"
          <> help "Input .acov file location"
      )
    <*> strOption
      ( long "odir"
          <> short 'o'
          <> metavar "DIR"
          <> value "./"
          <> help "Output directory"
      )

reportCmd :: Parser Command
reportCmd =
  Report
    <$> strOption --TODO: This takes logs as well
      ( long "input"
          <> short 'c'
          <> metavar "FILE"
          <> value "input.acov"
          <> help "Input .acov file location"
      )
      <*> strOption
        ( long "odir"
            <> short 'o'
            <> metavar "DIRFILE"
            <> value "./"
            <> help "Output directory"
        )
      <*> switch
        ( long "ignore-hash"
            <> help "Whether to ignore report hashes"
        )

reportRecov :: (Raw.Coverage, Maybe String) -> IO Raw.Coverage
reportRecov (cov, Just strn) =
  hPutStr stderr ("Warning: " ++ strn ++ "\n")
    >> return cov
reportRecov (cov, Nothing) = return cov

reportFatal :: Either String a -> IO a
reportFatal (Left err) =
  hPutStr stderr ("Error: " ++ err ++ "\n")
    >> exitFailure
reportFatal (Right a) = return a

readCoverage :: Maybe Int -> IO Raw.Coverage
readCoverage hash =
  do
    paths <- words <$> getContents
    when (null paths) $ hPutStr stderr "Warning: No coverage files.\n"
    foldM f Raw.emptyCoverage paths
  where
    f cov path = Raw.updateCoverage cov hash path >>= reportRecov

embeddedCSSFile :: ByteString
embeddedCSSFile = $(embedFile "data/acov.css")

{- Execute either the generate or report command. -}
run :: Command -> IO ()
run Generate {input = ifile, odir = outdir} = Frontend.run ifile >>= Verilog.run outdir
run Report {input = iflist, odir = outdir, ignoreHash = ih} = do
  (hash, mods) <- Frontend.run iflist
  cov <- readCoverage (if ih then Nothing else Just hash)
  mcov <- reportFatal (Merge.mergeCoverage mods cov)
  createDirectoryIfMissing False outdir
  withFile (outdir </> "index.html") WriteMode (\h -> report h $ CountPass.run mcov)
  writeFile (outdir </> "acov.css") embeddedCSSFile
  exitSuccess

main :: IO ()
main = execParser opt >>= run
  where
    opt = info (helper <*> subCommand) (fullDesc <> header "acov - A functional coverage generator")
