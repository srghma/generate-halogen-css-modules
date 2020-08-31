module CssContentToTypesSpec where

import           Protolude

import           Test.Hspec

import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.List as List
import Data.String.QQ

import Text.Regex.Base
import Text.RE.PCRE.Text
import CssContentToTypes
import Control.Arrow

cssContent :: Text
cssContent = [s|
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

.classInsideClass .classInsideClass2 {
  color: green;
}

.classWithBefore1:before {
  color: green;
}

.classWithBefore2Pre .classWithBefore2:before {
  color: green;
}

.classWithBefore3:before .classWithBefore3Post {
  color: var(--mycolor);
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

  .myButtonInsideOfPrint > a {
    color: green;
  }
}

@use "@material/button";

.my_button_dense {
  --mdc-theme-primary: #FEDBD0;
  --mdc-theme-on-primary: #442C2E;

  @include button.ink-color(#84565E);
  @include button.density(-3);
}
|]

spec :: Spec
spec = do
  it "HistoryToInputsSpec" $ do
    let (expected :: [Text]) = ["myButton","myButton2","myButton3","myButton4","myButton5","classInsideClass","classInsideClass2","classWithBefore1","classWithBefore2Pre","classWithBefore2","classWithBefore3","classWithBefore3Post","classInOneLine","idInOneLine", "myButtonInsideOfPrint", "my_button_dense"]
    cssContentToTypes cssContent `shouldBe` expected
