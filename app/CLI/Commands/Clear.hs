module CLI.Commands.Clear where
import System.Directory
import System.FilePath

clearCmd :: IO ()
clearCmd = do
  files <- listDirectory "./cache"
  mapM_ (removeFile . ("./cache" </>)) files