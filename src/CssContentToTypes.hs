module CssContentToTypes
    ( cssContentToTypes
    ) where

import "protolude" Protolude
import "text" Data.Text hiding (map)
import Text.Regex.Base
import Text.RE.PCRE.Text
import Text.CSS.Parse
import Data.String.QQ
import qualified Data.Text as Text
import qualified Data.List as List

-- [".myButton",".myButton",".myButton2","#myButton3","#myButton4",".myButton5",".classInsideClass",".classInsideClass2",,".classWithBefore2Pre",".classWithBefore2:before",".classWithBefore3:before",".classWithBefore3Post",".classInOneLine","#idInOneLine",".myButton3"]
--
-- from ".classWithBefore1:before" to "classWithBefore1"

type CssBlock = (Text, [(Text, Text)])

collectCssBlocks :: NestedBlock -> [CssBlock]
collectCssBlocks (NestedBlock mediaQuery nestedBlocks) = join $ map collectCssBlocks nestedBlocks
collectCssBlocks (LeafBlock cssBlock) = [cssBlock]

extractName :: CssBlock -> Text
extractName = fst

-- e.g. ".myButton3 > a"
extractClassesAndIds :: Text -> [Text]
extractClassesAndIds = List.filter (\t -> Text.isPrefixOf "#" t || Text.isPrefixOf "." t) . Text.words

extractClassOrId :: Text -> Maybe Text
extractClassOrId css = join $ map (flip atMay 1) $ flip atMay 0 $ (css =~ [re|[\.#]([\w\-]+)|] :: [[Text]])

cssContentToTypes :: Text -> [Text]
cssContentToTypes cssContent = ordNub $ catMaybes $ map extractClassOrId $ join $ map extractClassesAndIds $ map extractName $ join $ map collectCssBlocks $ either (const []) identity $ parseNestedBlocks cssContent
