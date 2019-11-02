module PrettifierQC where

import Prettifier
import Test.QuickCheck

instance Arbitrary Doc where
  arbitrary = oneof [
    text <$> arbitrary,
    char <$> arbitrary,
    return line,
    (</>) <$> arbitrary <*> arbitrary,
    (<>)  <$> arbitrary <*> arbitrary,
    return empty,
    return indent,
    return outdent
    ]
