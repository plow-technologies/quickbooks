module QuickBooks.QBText ( QBText
                         , QBTextConstructionError
                         , filterTextForQB
                         , textFromQBText) where

import           Data.Text
import qualified Data.List.Utils as L
import           Data.Aeson

-- | Text that has been filtered to be valid in all quickbooks context
newtype QBText = QBText {unQBText :: Text}
  deriving (Eq, Show)

instance ToJSON QBText where
    toJSON (QBText txt) = toJSON txt

instance FromJSON QBText where
    parseJSON (String txt) = return $ QBText txt
    parseJSON _            = fail "expecting String in FromJSON instance for QBText"

data QBTextConstructionError = QBTextErrorBadCharacter
  deriving (Show, Eq, Ord)

filterTextForQB :: Text -> Either QBTextConstructionError QBText
filterTextForQB inputText = do
  Right $ QBText $ replaceColon $ trimWhiteSpace inputText

textFromQBText :: QBText -> Text
textFromQBText qbText = unQBText qbText

trimWhiteSpace :: Text -> Text
trimWhiteSpace text = do
  Data.Text.reverse . Data.Text.dropWhile (== ' ') . Data.Text.reverse $ Data.Text.dropWhile (== ' ') text

replaceColon :: Text -> Text
replaceColon text =
  pack $ L.replace ":" "" (unpack $ text)
