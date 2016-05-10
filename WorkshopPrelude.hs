module WorkshopPrelude (module P, sum, _YOUR_CODE_HERE) where

import Prelude as P hiding (sum)
import qualified Prelude as P

-- export a `sum` specialized for lists to avoid problems when having
-- under-specified code with `_YOUR_CODE_HERE` placeholders
sum :: Num a => [a] -> a
sum = P.sum

_YOUR_CODE_HERE = error "Not implemented"
