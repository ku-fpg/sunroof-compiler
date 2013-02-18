import Development.Shake as Shake
import System.Directory
import System.Process
import System.Environment
import Development.Shake.FilePath

main = shake (shakeOptions) $ do

	"*/Main" *> \ out -> do
		files <- getDirectoryFiles "../Language" ["//*.hs"]
                need ["../Language" </> file | file <- files ]
                need ["../sunroof.cabal"]
                systemCwd (takeDirectory out)
                          "ghc"
                          ["--make","Main.hs","-threaded","-fforce-recomp","-O2",
                           "-dcore-lint","-i..:../dist/build/autogen/"]
		liftIO $ print out

	action $ do
		files <- getDirectoryFiles "" ["*/Main.hs"]
                need [ takeDirectory file </> "Main"
                     | file <- files
                     ]


-- to clean, rm */Main

