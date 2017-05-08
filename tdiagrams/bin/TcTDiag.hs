import CCO.Component (component, printer, ioWrap, Component)
import CCO.Diag (Diag)
import CCO.Feedback (Feedback)
import CCO.Tree (parser, Tree (fromTree, toTree))
import Control.Arrow (Arrow (arr), (>>>))

typeChecker :: Diag -> Feedback Diag
typeChecker = undefined

main = ioWrap (parser >>> component toTree >>> component typeChecker >>> arr fromTree >>> printer)
