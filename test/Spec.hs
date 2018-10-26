import qualified CheckoutSpec     as CS
import qualified JsonConfigSpec   as JCS
import qualified ShoppingCartSpec as SCS

main :: IO ()
main = do
  CS.spec
  SCS.spec
  JCS.spec
