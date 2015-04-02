data Client = GovOrg String
            | Company String Integer Person Bool
            | Individual Person Bool
            deriving Show
data Person = Person String String Gender
            deriving Show

data Gender = Male | Female | Unknown
            deriving (Show, Eq)
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
clientName (Individual (Person fname lname _) _) = fname ++ lname

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

-- returns the gender of client.
clientGender :: Client -> Maybe Gender
clientGender client = case client of
                      Company _ _ (Person _ _ gender)  _ -> Just gender
                      Individual (Person _ _ gender) _   -> Just gender
                      _                                  -> Nothing

-- just an exercise for count genders
countBools :: [Bool] -> Integer
countBools lst = if null lst
                 then 0
                 else
                   if True == head lst
                   then 1 + countBools (tail lst)
                   else 0 + countBools (tail lst)

-- for accessing triples of Integer
first :: (Integer, Integer, Integer) -> Integer
first (x, _, _) = x

second :: (Integer, Integer, Integer) -> Integer
second (_, x, _) = x

third :: (Integer, Integer, Integer) -> Integer
third (_, _, x)  = x

-- counts gender of a list of clients
countGenders :: [Client] -> (Integer, Integer, Integer)
countGenders clients = if null clients
                       then (0, 0, 0)
                       else
                         -- foreach (client in clients)
                         let client = head clients
                         in if (clientGender client) == Just Female
                            then (1 + (first genders), 0 + (second genders), 0 + (third genders))
                            else if (clientGender client) == Just Male
                            then (0 + (first genders), 1 + (second genders), 0 + (third genders))
                            else (0 + (first genders), 0 + (second genders), 1 + (third genders))
                            -- recursive call
                            where genders = countGenders (tail clients)

