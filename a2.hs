data Sex = Male | Female deriving (Eq, Show)
instance Ord Sex where
    compare Male Male = EQ
    compare Male Female = LT
    compare Female Male = GT
    compare Female Female = EQ

data Status = Alive | Dead | Abdicated deriving (Eq, Show)

data Person = Person Sex Status String deriving (Eq, Show)
instance Ord Person where
    compare (Person sx1 _ _) (Person sx2 _ _) = compare sx1 sx2
data Dynasty =
    Descend Person [Dynasty] | Dull Person deriving (Eq, Show)
instance Ord Dynasty where
    compare (Descend p1 _) (Descend p2 _) = compare p1 p2
    compare (Descend p1 _) (Dull p2 ) = compare p1 p2
    compare (Dull p1 ) (Descend p2 _) = compare p1 p2
    compare (Dull p1) (Dull p2 ) = compare p1 p2

successors :: String -> Dynasty -> [String]
successors name dynasty = aliveafter name (linefrom dynasty)

-- the catamorphism cataD on Dynasty
cataD :: (Person -> [t] -> t) -> (Person -> t) -> Dynasty -> t
cataD des dul (Dull person) = dul person
cataD des dul (Descend person dys) = des person (map (cataD des dul) dys)

-- linefrom using cataD
linefrom :: Dynasty -> [Person]
linefrom dy = cataD des dul (reorder dy)
			  where
			  des (Person _ Abdicated _) dys = []
			  des person dys = person : concat dys
			  dul (Person _ Abdicated _) = []
			  dul person = [person]			
			  
-- reorder using cataD and 
-- all sub-dynasties are sorted with Males before Females
reorder :: Dynasty -> Dynasty
reorder dy = cataD (\p dys-> Descend p (sortds dys)) (\p -> Dull p) dy

-- sortds using new insertd and flatten below			 
sortds :: [Dynasty] -> [Dynasty]
sortds dys = flatten (foldr insertd Dnull dys)

-- a type of binary trees for Dynasty
data BTD = Dnode BTD Dynasty BTD | Dnull

-- the catamorphism cataBTD on the above type
cataBTD::(t -> Dynasty -> t -> t) -> t -> BTD -> t
cataBTD nod nul Dnull = nul
cataBTD nod nul (Dnode btd1 dy btd2) = nod (cataBTD nod nul btd1) dy (cataBTD nod nul btd2)

-- an in-order traversal
flatten :: BTD -> [Dynasty]
flatten btd = cataBTD (\btd1 dy btd2 -> btd1 ++ [dy] ++ btd2) [] btd

insertd :: Dynasty -> BTD -> BTD
insertd dy (Dnull) = Dnode Dnull dy Dnull
insertd dy (Dnode btd1 thedy btd2) =
							if dy > thedy then Dnode btd1 thedy (insertd dy btd2)
							else if dy == thedy then Dnode btd1 dy btd2
							else Dnode (insertd dy btd1) thedy btd2

aliveafter :: String -> [Person] -> [String]
aliveafter name ps =
    let fromnam = dropWhile (\(Person _ _ pname)-> name /= pname) ps
    in if null fromnam then [] else alivein (tail fromnam)

alivein :: [Person] -> [String]
alivein ps =
    map
    (\(Person _ _ name) -> name)
    (filter (\(Person _ st _) -> st == Alive) ps)

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