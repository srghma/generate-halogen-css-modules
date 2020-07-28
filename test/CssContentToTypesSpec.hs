module CssContentToTypesSpec where

import           Protolude

import           Test.Hspec

import qualified Data.Map as Map
import Data.Text (Text)
import Data.String.QQ

import Text.Regex.Base
import Text.RE.PCRE.Text

import           CssContentToTypes

input :: Text
input = [s|
.myButton {
  color: green;
}

.myButton > a {
  color: green;
}

.myButton2 > a {
  color: green;
}

#myButton3 > a {
  color: green;
}

#myButton4 > a {
  color: green;
}

a > .myButton5 {
  color: green;
}

.classInOneLine{color: green;}
#idInOneLine{color: green;}

@media print {
  * {
    text-shadow: none !important;
    color: #000 !important;
    background-color: #fff !important;
  }

  a, a:visited { text-decoration: underline; }

  .myButton3 > a {
    color: green;
  }
}
|]

spec :: Spec
spec = do
  it "HistoryToInputsSpec" $ do
    let (expected :: [Text]) = ["myButton", "myButton2", "myButton3", "myButton4", "myButton5", "classInOneLine", "idInOneLine"]
    cssContentToTypes input `shouldBe` expected
