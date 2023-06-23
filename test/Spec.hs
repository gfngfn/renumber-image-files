import Test.HUnit
import qualified LibIO

main :: IO ()
main = do
  _ <- runTestTT $ TestList showFileTests
  putStrLn "finished."

showFileTests :: [Test]
showFileTests =
  [ TestCase (assertEqual title expected $ LibIO.showFile tag number index (classes, ext)) |
    (title, expected, tag, number, index, classes, ext) <-
      [ ("no index, no class",
           "foo042.jpg",
           "foo", 42, Nothing, [], "jpg"),
        ("indexed, no class",
           "foo042_03.jpg",
           "foo", 42, Just 3, [], "jpg"),
        ("no index, classified",
           "foo042__star_extr.jpg",
           "foo", 42, Nothing, ["star", "extr"], "jpg"),
        ("indexed, classified",
           "foo042_03__star_extr.jpg",
           "foo", 42, Just 3, ["star", "extr"], "jpg")
      ]
  ]
