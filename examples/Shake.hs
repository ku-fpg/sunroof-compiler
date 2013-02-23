import Development.Shake as Shake
import System.Directory
import System.Process
import System.Environment
import Development.Shake.FilePath
import Control.Monad

main = do
        args <- getArgs
        main2 args


main2 ["clean"] = do
        system "rm ./*/Main"
        system "rm -R ./*/cache/"
        return ()

main2 xs = shake (shakeOptions) $ do

        -- Use this if you want to also compile comet with each build
        let comet = ""
--        let comet = ":../../../kansas-comet:../../../kansas-comet/dist/build/autogen/"

        want xs

	when (null xs) $ action $ do
		files <- getDirectoryFiles "" ["*/Main.hs"]
                need [ takeDirectory file </> "Main"
                     | file <- files
                     ]

	"*/Main" *> \ out -> do
                need [out ++ ".hs"]
                files <- getDirectoryFiles "../Language" ["//*.hs"]
                need ["../Language" </> file | file <- files ]
                need ["../sunroof.cabal"]
                liftIO $ putStrLn $ "Building: " ++ out
                systemCwd (takeDirectory out)
                          "ghc"
                          ["--make","Main.hs","-threaded",
                           "-hidir","cache","-odir","cache",
                           "-dcore-lint","-i../..:../../dist/build/autogen/" ++ comet ]

		liftIO $ print out
