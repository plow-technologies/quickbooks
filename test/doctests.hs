import Test.DocTest

main :: IO ()
main = doctest ["-isrc", "test/Data.hs", "test/QuickBooksSpec.hs", "src/QuickBooks.hs"]
