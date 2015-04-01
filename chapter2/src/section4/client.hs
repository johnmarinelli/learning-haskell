-- client algebraic data type
-- different ctors are used to represent completely different alternatives

data Client = GovOrg     String -- name
            | Company    String Integer Person -- name, id, contact, contact position
            | Individual Person Bool -- name, last name, receive offers
            deriving Show -- let haskell override toString automatically
                          -- Show is a class

--   type     ctor   constructor args
data Person = Person String String Gender
              deriving Show

-- enum
data Gender = Male | Female | Unknown
              deriving Show
