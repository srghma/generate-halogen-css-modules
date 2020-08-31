module CssContentToTypes
    ( cssContentToTypes
    ) where

import "protolude" Protolude
import "text" Data.Text hiding (map)
import Text.Regex.Base
import Text.RE.PCRE.Text
import Language.Scss.Parser
import Data.String.QQ
import qualified Data.Text as Text
import qualified Data.List as List
import Text.Megaparsec (parseMaybe)

-- [".myButton",".myButton",".myButton2","#myButton3","#myButton4",".myButton5",".classInsideClass",".classInsideClass2",,".classWithBefore2Pre",".classWithBefore2:before",".classWithBefore3:before",".classWithBefore3Post",".classInOneLine","#idInOneLine",".myButton3"]

-- from ".classWithBefore1:before" to "classWithBefore1"

-- e.g. ".myButton3 > a"
extractClassesAndIds :: Text -> [Text]
extractClassesAndIds = List.filter (\t -> Text.isPrefixOf "#" t || Text.isPrefixOf "." t) . Text.words

extractClassOrId :: Text -> Maybe Text
extractClassOrId css = join $ map (flip atMay 1) $ flip atMay 0 $ (css =~ [re|[\.#]([\w\-]+)|] :: [[Text]])

valueToSelectors :: Value -> [Text]
valueToSelectors (Selector selector values) = [selector] <> (join $ map valueToSelectors values)
valueToSelectors (AtRule name body values) = join $ map valueToSelectors values
valueToSelectors _ = []

cssContentToTypes :: Text -> [Text]
cssContentToTypes cssContent = ordNub $ catMaybes $ map extractClassOrId $ join $ map extractClassesAndIds $ either (const []) (join . map valueToSelectors) $ parse cssContent
