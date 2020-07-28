module CssContentToTypes
    ( cssContentToTypes
    ) where

import "protolude" Protolude
import "text" Data.Text hiding (map)
import Text.Regex.Base
import Text.RE.PCRE.Text

-- from regex tutorial http://regex.uk/re-tutorial.html
cssRegex = [re|[\.#]([A-Za-z\_][\w\-]*)[^\;]*\{|]

cssContentToTypes :: Text -> [Text]
cssContentToTypes cssContent = ordNub $ mapMaybe (\xs -> atMay xs 1) $ (cssContent =~ cssRegex :: [[Text]])
