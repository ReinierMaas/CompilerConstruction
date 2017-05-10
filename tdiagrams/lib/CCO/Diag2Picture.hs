module CCO.Diag2Picture (
    diag2picture,
    diag2pictureATerm
) where

import CCO.Component (component, printer, Component)
import CCO.Tree (parser, ATerm, Tree (fromTree, toTree))
import CCO.Diag2Picture.AG
import CCO.Feedback (errorMessage, Feedback)
import CCO.Printing (text)
import Control.Arrow (Arrow (arr), (>>>))

diag2gen :: Diag -> Feedback DiagGen
diag2gen d = return $ sem_Diag d

gen2picture :: DiagGen -> Feedback Picture
gen2picture g = return $ sem_Root (Root g)

diag2picture :: Component String String
diag2picture = parser >>> diag2pictureATerm >>> printer

diag2pictureATerm :: Component ATerm ATerm
diag2pictureATerm = component toTree >>> component diag2gen >>> component gen2picture >>> arr fromTree
