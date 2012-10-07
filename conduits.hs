import Data.Conduit -- the core library
import qualified Data.Conduit.List as CL -- some list-like functions
import qualified Data.Conduit.Binary as CB -- bytes
import qualified Data.Conduit.Text as CT
import System.IO

copyFile :: FilePath -> FilePath -> IO ()
copyFile src dest = runResourceT $ CB.sourceFile src $$ CB.sinkFile dest

main = do
    output <- openFile "output.txt" WriteMode
    input  <- openFile "input.txt"  ReadMode
    hGetContents input >>= hPutStr output
    hClose input
    hClose output

--main =
--    withFile "output.txt" WriteMode $ \output ->
--    withFile "input.txt" ReadMode $ \input ->
--    hGetContents input >>= hPutStr output
