{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
import Data.Char (toUpper)

-- client algebraic data type
-- different ctors are used to represent completely different alternatives

data Client = GovOrg     String -- name
            | Company    String Integer Person String -- name, id, contact, contact position
            | Individual Person Bool -- name, last name, receive offers
            deriving Show -- let haskell override toString automatically
                          -- Show is a class

--   type     ctor   constructor args
data Person = Person String String Gender
              deriving Show

-- enum
data Gender = Male | Female | Unknown
              deriving Show

responsibility :: Client -> String
responsibility (Company _ _ _ resp) = resp
responsibility _                    = "Unknown"

specialClient :: Client -> Bool
specialClient (responsibility -> "Director") = True
specialClient _                              = False

-- records: giving adts named values (like member variables)
data ClientRecord = GovOrgRecord { clientRecordName :: String } -- we can use same field name given they're the SAME TYPE
                  | CompanyRecord { clientRecordName :: String
                                  , companyId :: Integer
                                  , person :: PersonRecord
                                  , duty :: String }
                  | IndividualRecord { person :: PersonRecord }
                  deriving Show

data PersonRecord = PersonRecord { firstName :: String
                                 , lastName :: String
                                 } deriving Show

-- wildcard matching: the '..' operator will automatically do punning for records.
-- so instead of greet GovOrgRecord { clientRecordName = crn }, clientRecordName automatically gets captured
greet :: ClientRecord -> String
greet IndividualRecord { person = PersonRecord { .. } } = "Hello individual " ++ lastName 
greet GovOrgRecord     { .. } = "GREETINGS SUPREME OVERLORD " ++ clientRecordName
greet CompanyRecord    { .. } = "salutations businessman " ++ clientRecordName

nameInCapitals :: PersonRecord -> PersonRecord
nameInCapitals p@(PersonRecord { firstName = initial:rest, lastName = linitial:lrest }) =  -- record syntax is used to match on PersonRecord
  let newName = (toUpper initial):rest
      newLastName = (toUpper linitial):lrest            -- x:xs used to match on list
  in p { firstName = newName, lastName = newLastName }  -- since we want to refer to the value to update it, "as" pattern binds the pattern to p
nameInCapitals p@(PersonRecord { firstName = "" }) = p
