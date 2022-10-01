module Sums where

  data PreferredContactMethod =
    Email String
    | TextMessage String
    | Mail String String String Int

  emailContact :: PreferredContactMethod
  emailContact = Email "me@example.com"

  textContact :: PreferredContactMethod
  textContact = TextMessage "+1 307 555 0100"

  mailContact :: PreferredContactMethod
  mailContact = Mail "1123 S. Road St." "Suite 712" "Examplesville, OH" 98142

  confirmContact :: PreferredContactMethod -> String
  confirmContact contact =
    case contact of
      Email emailAddress ->
        "Okay, I'll email ou at " <> emailAddress
      TextMessage number ->
        "Okay, I'll text you at " <> number
      Mail street1 street2 citystate zip ->
        "Okay, I'll send a letter to\n"
        <> street1 <> "\n"
        <> street2 <> "\n"
        <> citystate <> " " <> show zip

  data CustomerInfo = CustomerInfo
    { customerName :: String
    , customerBalance :: Int
    }
  data EmployeeInfo = EmployeeInfo
    { employeeName :: String
    , employeeManagerName :: String
    , employeeSalary :: Int
    }
  data Person
    = Customer CustomerInfo
    | Employee EmployeeInfo
  george = Customer 
    $ CustomerInfo
      { customerName = "Georgie Bird"
      , customerBalance = 100
      }
  porter = Employee $
    EmployeeInfo
      { employeeName = "Porter P. Pupper"
      , employeeManagerName = "Remi"
      , employeeSalary = 10
      }

  getPersonName :: Person -> String
  getPersonName person =
    case person of
      Employee employee -> employeeName employee
      Customer customer -> customerName customer

  getPersonManager :: Person -> Maybe String
  getPersonManager person =
    case person of
      Employee employee -> Just $ employeeManagerName employee
      Customer _customer -> Nothing

  getPersonBalance :: Person -> Maybe Int
  getPersonBalance person =
    case person of
      Employee _employee -> Nothing
      Customer customer -> Just $ customerBalance customer

  getPersonSalary :: Person -> Maybe Int
  getPersonSalary person =
    case person of
      Employee employee -> Just $ employeeSalary employee
      Customer _customer -> Nothing

