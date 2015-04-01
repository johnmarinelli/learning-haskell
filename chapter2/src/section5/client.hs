data Client = GovOrg String
            | Company String Integer Person Bool
            | Individual Person Bool
            deriving Show
data Person = Person String String
            deriving Show

{-
-- pattern matching:
clientName :: Client -> String
                    -- finds type of client based on data Client's ctors
clientName client = case client of
                      -- 'name' binds to the first GovOrg arg
                      GovOrg name                 -> name
                      -- 'name id person resp' binds to Company args
                      Company name id person resp -> name
                      -- Person fname lname binds to the Person ctor arg
                      -- of Individual.  the underscore means we don't
                      -- need to bind that value to anything, because
                      -- we don't use it.
                      Individual (Person fname lname) _ -> fname ++ lname
-}

-- Encode the pattern directly in definition of function
clientName (GovOrg name)                       = name
clientName (Company name _ _ _)                = name
clientName (Individual (Person fname lname) _) = fname ++ lname

-- 'partial' functions; not defined over complete domain of their argument
-- Maybe is (hilariously) a type that is parameterized by the value it holds
-- use cases for Maybe: when something might fail
companyName :: Client -> Maybe String
companyName client = case client of
                    -- Values of type Maybe T
                    -- Just v, where v is the value of T
                    Company name _ _ _ -> Just name
                    -- or Nothing.  which is nothing
                    _                  -> Nothing

