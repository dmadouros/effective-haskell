{-# LANGUAGE RecordWildCards #-}

module Main where
  main = print ""

  data CustomerInfo = CustomerInfo
    { customerInfoFirstName :: String
    , customerInfoLastName :: String
    , customerInfoWidgetCount :: Int
    , customerInfoBalance :: Int
    }

  customerGeorge :: CustomerInfo
  customerGeorge =
    let customerInfoFirstName = "George"
        customerInfoLastName = "Bird"
        customerInfoWidgetCount = 10
        customerInfoBalance = 100
    in CustomerInfo {..}

  showCustomer :: CustomerInfo -> String    
  showCustomer CustomerInfo{..} =
    customerInfoFirstName
    <> " "
    <> customerInfoLastName
    <> " "
    <> show customerInfoWidgetCount
    <> " "
    <> show customerInfoBalance

  applyDiscount :: CustomerInfo -> CustomerInfo
  applyDiscount customer =
    case customer of
      (CustomerInfo "George" "Bird" count customerInfoBalance) ->
        CustomerInfo "George" "Bird" count (customerInfoBalance `div` 4)
      (CustomerInfo "Porter" "Pupper" count customerInfoBalance) ->
        CustomerInfo "Porter" "Pupper" count (customerInfoBalance `div` 2)
      otherCustomer -> otherCustomer

  emptyCart :: CustomerInfo -> CustomerInfo
  emptyCart customer =
    customer { customerInfoWidgetCount = 0
             , customerInfoBalance = 0
             }
