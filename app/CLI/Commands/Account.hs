module CLI.Commands.Account where
import System.Info
import System.Process.Extra

accountCmd :: IO ()
accountCmd = do
  let command = case os of
                "linux" -> "xdg-open"
                "darwin" -> "open"
                "mingw32" -> "start"
                _ -> error "Unsupported operating system"
  callCommand $ command ++ " " ++ "./scripts/truffle-config.js"

