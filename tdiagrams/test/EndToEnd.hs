import CCO.Component (Component, component, ioRun, printer)
import CCO.Diag (parser)
import CCO.Tree (ATerm, Tree (fromTree, toTree))
import CCO.TypeCheck (typeCheckATerm)
import CCO.Diag2Picture (diag2pictureATerm)
import CCO.Picture (Picture)

import Control.Arrow (Arrow (arr), (>>>))
import System.IO (hClose, hPutStrLn, openFile, Handle, IOMode(WriteMode))

main = do
    handle <- openFile "inference rules/diagrams.tex" WriteMode
    sequence $ map (run handle) correctPrograms
    hClose handle

run :: Handle -> String -> IO ()
run handle input = do
    hPutStrLn handle ""
    hPutStrLn handle "\\bigskip"
    hPutStrLn handle $ "\\noindent " ++ input
    hPutStrLn handle "\\bigskip"
    hPutStrLn handle ""
    picture handle input


picture :: Handle -> String -> IO ()
picture handle input = do
    parsed <- ioRun (parser >>> arr fromTree) input
    typeChecked <- ioRun typeCheckATerm parsed
    picture <- ioRun diag2pictureATerm typeChecked
    latex <- ioRun (component toTree :: Component ATerm Picture) picture
    output <- ioRun printer latex
    hPutStrLn handle output


correctPrograms :: [String]
correctPrograms = [
    "program hello in Haskell",
    "platform x64-windows",
    "interpreter hugs for Haskell in i686-windows",
    "execute program hello in Haskell on interpreter hugs for Haskell in x86-windows end",
    "execute interpreter hugs for Haskell in x86-windows on platform x86-windows end",
    "execute program hello in Haskell on interpreter hugs for Haskell in i686-windows end",
    "compile program hello in UUAG with compiler uuagc from UUAG to Haskell in i686-windows end",
    "execute compile program hello in UUAG with compiler uuagc from UUAG to Haskell in i686-windows end on interpreter hugs for Haskell in i686-windows end",
    "execute execute compile program hello in UUAG with compiler uuagc from UUAG to Haskell in i686-windows end on platform i686-windows end on interpreter hugs for Haskell in i686-windows end" ]
