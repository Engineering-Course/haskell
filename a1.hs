-- a person is Male or Female, Abdicated, Dead or Alive, and has a  name
data Sex = Male | Female deriving (Eq, Show)
data Status = Alive | Dead | Abdicated deriving (Eq, Show)
data Person = Person Sex Status String deriving (Eq, Show)

-- a Dynasty is headed by a Person and indicates the descendants
-- oldest first; a Dull Person doesn't have any recorded descendants
data Dynasty = Descend Person [Dynasty] | Dull Person deriving (Eq, Show)

successors :: String -> Dynasty -> [String]
successors name dynasty = aliveafter name (linefrom dynasty)

linefrom :: Dynasty -> [Person]
linefrom (Dull (Person _ Abdicated _)) = []
linefrom (Dull person) = [person]
linefrom dy 
		| getStatus (getdyPerson dy) == Abdicated = []
		| otherwise = [person] ++ concat (map linefrom dys)
		where (Descend person dys) = reorder dy
		
reorder :: Dynasty -> Dynasty
reorder (Dull person) = Dull person
reorder (Descend person dys) = Descend person (sortds dys)

sortds :: [Dynasty] -> [Dynasty]
sortds = foldr insertd []

insertd :: Dynasty -> [Dynasty] -> [Dynasty]
insertd dy [] = dy : []
insertd dy (x:dys) 
		| getSex (getdyPerson dy) == Male = [dy] ++ [x] ++ dys
		| otherwise =  
				if getSex (getdyPerson x) == Male
				then x : insertd dy dys
				else [dy] ++ [x] ++ dys 

aliveafter :: String -> [Person] -> [String]
aliveafter name [] = []
aliveafter name (p:ps) 
		| getName p == name = alivein ps
		| otherwise         = aliveafter name ps

alivein :: [Person] -> [String]
alivein [] = []
alivein (p:ps)
		| getStatus p == Alive = getName p : alivein ps
		| otherwise            = alivein ps

--get the person by which the dynasty is headed
getdyPerson :: Dynasty -> Person
getdyPerson (Descend person _) = person
getdyPerson (Dull person) = person

--get the status of the person		
getStatus :: Person -> Status
getStatus (Person _ status _) = status

--get the name of the person
getName :: Person -> String
getName (Person _ _ name) = name

--get the sex of the person		
getSex :: Person -> Sex
getSex (Person sex _ _) = sex

exdyn =
    Descend (Person Male Dead "George5")
    [
        Descend (Person Male Abdicated "Edward8") [],
        Descend (Person Male Dead "George6")
        [
            Descend (Person Female Alive "Elizabeth2")
            [
                Descend (Person Male Alive "Charles")
                [
                    Descend (Person Male Alive "William")
                    [
                        Descend (Person Male Alive "George") []
                    ],
                    Descend (Person Male Alive "Harry") []
                ],
                Descend (Person Female Alive "Anne")
                [
                    Descend (Person Male Alive "Peter")
                    [
                        Dull (Person Female Alive "Savannah"),
                        Dull (Person Female Alive "Isla")
                    ],
                    Dull (Person Female Alive "Zarah")
                ],
                Descend (Person Male Alive "Andrew")
                [
                    Dull (Person Female Alive "Beatrice"),
                    Dull (Person Female Alive "Eugenie")
                ],
                Descend (Person Male Alive "Edward")
                [
                    Dull (Person Female Alive "Louise"),
                    Dull (Person Male Alive "James")
                ]
            ],
            Descend (Person Female Dead "Margaret")
            [
                Dull (Person Male Alive "David"),
                Dull (Person Female Alive "Sarah")
            ]
        ],
        Dull (Person Female Dead "Mary"),
        Dull (Person Male Dead "Henry"),
        Dull (Person Male Dead "George"),
        Dull (Person Male Dead "John")
    ]