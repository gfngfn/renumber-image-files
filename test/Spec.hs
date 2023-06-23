import Test.HUnit

main :: IO ()
main = do
  _ <- runTestTT test1
  putStrLn "finished."

test1 =
  TestCase (assertEqual "small test for addition" 3 (1 + 2))
