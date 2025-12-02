
import Test.HUnit
import LibIO qualified
import Data.Set qualified as Set

main :: IO ()
main = do
  _ <- runTestTT $ TestList showFileTests
  putStrLn "finished."

showFileTests :: [Test]
showFileTests =
  [ TestCase (assertEqual title expected $ LibIO.showFile tag number index (Set.fromList classes, ext)) |
    (title, expected, tag, number, index, classes, ext) <-
      [ ("no index, no class",
           "foo042.jpg",
           "foo", 42, Nothing, [], "jpg"),
        ("indexed, no class",
           "foo042_03.jpg",
           "foo", 42, Just 3, [], "jpg"),
        ("no index, classified",
           "foo042__extr_star.jpg",
           "foo", 42, Nothing, ["star", "extr"], "jpg"),
        ("indexed, classified",
           "foo042_03__extr_st.jpg",
           "foo", 42, Just 3, ["star", "extr"], "jpg")
      ]
  ]
