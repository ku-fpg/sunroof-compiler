import Development.Shake as Shake
import System.Directory
import System.Process
import System.Environment
import Development.Shake.FilePath

main = shake (shakeOptions) $ do

	"*/Main" *> \ out -> do
                need [out ++ ".hs"]
                files <- getDirectoryFiles "../Language" ["//*.hs"]
                need ["../Language" </> file | file <- files ]
                need ["../sunroof.cabal"]
                liftIO $ putStrLn $ "Building: " ++ out
                systemCwd (takeDirectory out)
                          "ghc"
                          ["--make","Main.hs","-threaded",
                           "-dcore-lint","-i../..:../../dist/build/autogen/"]
		liftIO $ print out

	action $ do
		files <- getDirectoryFiles "" ["*/Main.hs"]
                need [ takeDirectory file </> "Main"
                     | file <- files
                     ]


-- to clean, rm */Main

