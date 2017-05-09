module CCO.Diag2Picture (
    diag2picture,
    diag2pictureATerm
) where

import CCO.Component (component, printer, Component)
import CCO.Tree (parser, ATerm, Tree (fromTree, toTree))
import CCO.Diag2Picture.AG
import qualified CCO.Diag2Picture.Util as U
import CCO.Feedback (errorMessage, Feedback)
import CCO.Printing (text)
import Control.Arrow (Arrow (arr), (>>>))

diag2picture' :: Diag -> Feedback Picture
diag2picture' d = return $ sem_Root (Root d)

diag2picture :: Component String String
diag2picture = parser >>> diag2pictureATerm >>> printer

diag2pictureATerm :: Component ATerm ATerm
diag2pictureATerm = component toTree >>> component diag2picture' >>> arr fromTree
