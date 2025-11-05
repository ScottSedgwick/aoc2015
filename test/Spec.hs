import Test.Framework
import Spec_Day02
import Spec_Day03

main :: IO ()
main = defaultMainWithOpts
    (  day02
    <> day03
    )
    mempty