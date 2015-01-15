import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "test/Data.hs", "src/QuickBooks.hs"]
