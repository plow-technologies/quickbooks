import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "src/QuickBooks.hs"]
