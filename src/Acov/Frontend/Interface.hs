module Acov.Frontend.Interface
  ( run,
  )
where

import Acov.Frontend.ErrorsOr (ErrorsOr, reportEO)
import qualified Acov.Frontend.Expressions as Expressions
import qualified Acov.Frontend.Grouping as Grouping
import Acov.Frontend.Parser (parseScript)
import qualified Acov.Frontend.Symbols as Symbols
import qualified Acov.Frontend.Width as Width

runPass :: FilePath -> (a -> ErrorsOr b) -> a -> IO b
runPass path pass a = reportEO path (pass a)

run :: FilePath -> IO (Int, [Width.Module])
run path =
  readFile path
    >>= runPass path (parseScript path)
    >>= runPass path Grouping.run
    >>= runPass path Symbols.run
    >>= runPass path Expressions.run
    >>= runPass path Width.run
