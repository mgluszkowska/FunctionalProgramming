-- funkcja, ktora zwraca swoj argument
toSamo :: a -> a
toSamo x = x

-- funkcja, ktora podnosi liczbe do kwadratu
kwadrat :: Num a => a -> a 
kwadrat x = x^2

-- funkcja sprawdzajaca, czy lista jest pusta
pusta :: Eq a =>[a] -> Bool 
pusta x = if x == [] then True
          else False
		  
-- silnia
silnia ::  Integral a => a -> a
silnia 0 = 1
silnia x = x * silnia (x-1)

-- ciag fibonacciego
fibonacci :: Integral a => a -> a
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2) 

-- funkcja obliczajaca ile razy liczba y jest podzielna przez x
ileRazyPrzez ::  Integral a => a -> a -> a 
ileRazyPrzez x y = if mod y x /= 0 then 0
                   else 1 + ileRazyPrzez x (div y x)

-- funkcja obliczajaca dlugosc listy
dlugosc :: [a] -> Int
dlugosc [] = 0
dlugosc (h:t) = 1 + dlugosc t

-- funkcja sumujaca wszystkie elementy listy
suma :: Num a => [a] -> a
suma [] = 0
suma (h:t) = h + suma t

-- funkcja zwracajaca sume logiczna wszystkich elementow listy typu Bool
sumaLog :: [Bool] -> Bool
sumaLog [] = False
sumaLog (h:t) = if h == True then True
                else sumaLog t

-- funkcja zwracajaca najwiekszy element listy lub najmniejsza wartosc tego typu dla pustej listy
-- ta funkcja dziala tylko wtedy, kiedy powie sie programowi, jakiego typu sa dane wejsciowe
maxB :: (Bounded a, Ord a) => [a] -> a
maxB [] = minBound
maxB (h:t) = max h (maxB t)

-- funkcja zamieniajaca kazdy element listy jej nastepnikiem
nastepne :: Enum a => [a] -> [a]
nastepne [] = []
nastepne (h:t) = succ h : nastepne t

-- funkcja zamieniajaca kazdy element na wartosc typu Bool mowiaca o tym, czy element jest parzysty
parzyste :: Integral a => [a] -> [Bool]
parzyste [] = []
parzyste (h:t) = if mod h 2 == 0 then True: parzyste t
                 else False : parzyste t

-- funkcja, ktora bierze funkcje i liste argumentow, na ktorych ma wykonac ta funkcje
mapowanie :: (a -> b) -> [a] -> [b]
mapowanie f [] = [] 
mapowanie f (h:t) = f h : mapowanie f t

-- funkcja nastepne z wykorzystaniem funkcji mapowanie
nastepne' :: Enum a => [a] -> [a]
nastepne' x = mapowanie succ x

-- funkcja parzyste z wykorzystaniem funkcji mapowanie
parzyste' :: Integral a => [a] -> [Bool]
parzyste' = mapowanie even

-- funkcja, ktora ma wykonac jakas funkcje rekurencyjnie na wszystkich elementach listy zaczynajac od podanego elementu startowego
redukcja :: (a -> b -> b) -> b -> [a] -> b 
redukcja f p [] = p
redukcja f p (h:t) = f h (redukcja f p t)

-- funkcja dlugosc z wykorzystaniem funkcji redukcja
dlugosc' :: [a] -> Int
dlugosc' x = redukcja (\_ n -> n+1) 0 x  

-- funkcja suma z wykorzystaniem funkcji redukcja
suma' :: Num a => [a] -> a
suma' x = redukcja (+) 0 x

-- funkcja sumaLog z wykorzystaniem funkcji redukcja
sumaLog' :: [Bool] -> Bool
sumaLog' x = redukcja (||) False x


-- funkcja iloczynLog z wykorzystaniem funkcji redukcja
iloczynLog' :: [Bool] -> Bool
iloczynLog' x = redukcja (&&) True x

-- funkcja maxB z wykorzystaniem funkcji redukcja
maxB' :: (Bounded a, Ord a) => [a] -> a
maxB' x = redukcja max minBound x

-- funkcja sprawdzajaca, czy wszystkie elementy listy sa parzyste (zlozenie funkcji)
wszystkieParzyste :: Integral a => [a] -> Bool
wszystkieParzyste = iloczynLog' . parzyste'

-- wlasna implementacja zlozenia funkcji
compose :: (a -> b) -> (b -> c) -> a -> c
compose f g x = g (f x)

-- funkcja, ktora zwraca funkcje jednoragumentowa, ktora dla kazdego argumentu zwraca ten sam wynik
funkcjastala :: a -> (b -> a)
funkcjastala x = f
                 where f y = x

-- funkcja, ktora zwraca funkcje o zmienionej kolejnosci argumentow
odwroc :: (a -> b -> c) -> (b -> a -> c)
odwroc f = x
           where x a b = f b a

