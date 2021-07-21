{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}

{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Basics where
{-
    Expune funcțiile necesare reprezentării jocului.
-}

import ProblemState
import Data.List ()
import Data.Maybe (fromMaybe)

{-
    Sinonim tip de date pentru reprezetarea unei perechi (Int, Int)
    care va reține coordonatele celulelor de pe tabla de joc.
    Colțul stânga-sus este (0, 0).
-}
type Position = (Int, Int)

{-
    Tip de date pentru reprezentarea Target-urilor.
    Acestea conțin informații atât despre poziția curentă a
    Target-ului cât și despre comportamentul acestuia.
    Tipul Behavior este definit mai jos.
-}
data Target = Target {
    position :: Position,
    behavior :: Behavior
}

instance Eq Target where
    Target p1 _ == Target p2 _ = p1 == p2

instance Ord Target where
    Target p1 _ <= Target p2 _ = p1 <= p2

{-
    Tip de date pentru reprezentarea comportamentului unui Target.
    Tipul Behavior este utilizat pentru a modela tranziția Target-urilor
    din starea curentă în starea următoare. Primul parametru este poziția
    actuală a target-ului, iar al doilea, starea curentă a jocului.
    Tipul Game este definit mai jos.
    
    Observați că, din moment ce un Behavior produce un Target nou,
    acesta din urmă ar putea fi caracterizat de un alt Behavior
    decât cel anterior.
-}
type Behavior = Position -> Game -> Target

{-
    Direcțiile de deplasare pe tablă
-}
data Direction = North | South | West | East
    deriving (Eq, Show)

{-
    *** TODO ***
    
    Tip de date pentru reprezentarea stării jocului, la un anumit
    moment. Completați-l cu orice informație aveți nevoie pentru
    stocarea stării jocului (hunter, target, obstacole, gateways).
-}
data Game = Game {
    hunter :: Position
    , target :: [Target]
    , obstacol :: [Position]
    , gateway :: [(Position, Position)]
    , liness :: Int
    , columns :: Int
} deriving (Eq, Ord)

{-
    *** Optional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară,
    instantiați explicit clasele Eq și Ord pentru Game.
    În cazul acesta, eliminați deriving (Eq, Ord) din Game.
-}

{-
    *** TODO ***

    Reprezentați starea jocului ca șir de caractere, pentru afișarea
    la consolă.
    
    Atenție! Fiecare linie, mai puțin ultima, este urmată de \n.
    Celule goale vor fi reprezentate ca ' '.
    Hunter-ul va fi reprezentat ca '!'.
    Target-urile vor fi reprezentate ca '*'
    Gateways-urile vor fi reprezentate ca '#'.
    Obstacolele vor fi reprezentate de '@'.

    Hint: S-ar putea să vă fie utile list comprehensions,
    precum și funcțiile elem, any și intercalate din Data.List.
-}

search :: Position -> Game -> String
search (x, y) (Game hunter target obstacol gateway lines columns)
    | (x, y) == hunter && y== columns - 1 && x /= lines - 1 = "!\n"
    | (x, y) == hunter = "!"
    | (x, y) `elem` foldl(\acc x -> acc ++ [position x]) [] target && y == columns - 1 && x /= lines - 1 = "*\n"
    | (x, y) `elem` foldl(\acc x -> acc ++ [position x]) [] target = "*"
    | (x, y) `elem` foldl(\acc x -> acc ++ [fst x]) [] gateway  && columns - 1 == y && x /= lines - 1 = "#\n"
    | (x, y) `elem` foldl(\acc x -> acc ++ [fst x]) [] gateway = "#"
    | (x, y) `elem` obstacol && columns - 1 == y && x /= lines - 1 = "@\n"
    | (x,y) `elem` obstacol = "@"
    | otherwise = " "

allPositions :: Int -> Int -> [Position]
allPositions lines columns = [(x,y)| x <- [0..lines - 1], y <- [0..columns - 1]]

gameAsString :: Game -> String
-- gameAsString = undefined 
gameAsString game@(Game hunter target obstacol gateway lines columns) =
    s ++ foldl(\acc x -> acc ++ search x game) "" (allPositions lines columns)
    where
        s = ""

instance Show Game where
    show = gameAsString

{-
    *** TODO ***
    
    Primește numărul de linii și numărul de coloane ale tablei de joc.
    Intoarce un obiect de tip Game în care tabla conține spații goale în interior, fiind
    împrejmuită de obstacole pe toate laturile. Implicit, colțul din stânga sus este (0,0),
    iar Hunterul se găsește pe poziția (1, 1).
-}

margin :: Position -> Int -> Int -> Bool
margin (x, y) lines columns
    | x == 0 || x == lines - 1 || y == 0 || y == columns - 1 = True
    | otherwise  = False


emptyGame :: Int -> Int -> Game
emptyGame lines columns = Game (1,1) [] obstacol [] lines columns
    where
        obstacol = foldl(\acc x -> if margin x lines columns then acc ++ [x]
                                                               else acc) [] (allPositions lines columns)

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, cu Hunter-ul pus
    pe poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adaugat Hunter-ul
    Daca poziția este invalidă (ocupată sau în afara tablei de joc) se va întoarce
    același joc.
-}

addHunter :: Position -> Game -> Game
addHunter (x,y) (Game hunter target obstacol gateway lines columns) =
    if  x < 0 || y < 0 || x > lines - 1 ||
        y > columns - 1 ||
        (x,y) `elem` foldl(\acc x -> acc ++ [position x]) [] target ||
        (x,y) `elem` obstacol ||
        (x,y) `elem` foldl(\acc x -> acc ++ [fst x]) [] gateway
        then Game hunter target obstacol gateway lines columns
        else Game (x,y) target obstacol gateway lines columns

{-
    *** TODO ***

    Primește un comportament, o poziție și un joc și întoarce un nou joc, în care a fost
    adăugat Target-ul descris de comportament și poziție.
    Parametrul Behavior reprezintă comportamentul Hunter-ului care va fi adăugat.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat Target-ul.
-}

addTarget :: Behavior -> Position -> Game -> Game
addTarget b (x,y) (Game hunter target obstacol gateway lines columns) =
    Game hunter (target ++ [t]) obstacol gateway lines columns
    where
        t = Target (x,y) b

{-
    *** TODO ***

    Primește o pereche de poziții și un joc și întoarce un nou joc, în care au fost adăugate
    cele două gateway-uri interconectate.
    Parametrul (Position, Position) reprezintă pozițiile de pe hartă la care vor fi adăugate 
    cele două gateway-uri interconectate printr-un canal bidirecțional.
-}

addGateway :: (Position, Position) -> Game -> Game
addGateway (a, b) (Game hunter target obstacol gateway lines columns) =
    Game hunter target obstacol (gateway ++ [(a,b)] ++ [(b,a)]) lines columns

{-
    *** TODO ***

    Primește o poziție și un joc și întoarce un nou joc, în care a fost adăugat un obstacol
    la poziția specificată.
    Parametrul Position reprezintă poziția de pe hartă la care va fi adăugat obstacolul.
-}

addObstacle :: Position -> Game -> Game
addObstacle (x,y) (Game hunter target obstacol gateway lines columns) =
    Game hunter target (obstacol ++ [(x,y)]) gateway lines columns

{-
    *** TODO ***
    
    Primește o poziție destinație înspre care vrea să se deplaseze o entitate (Hunter sau Target)
    și verifică daca deplasarea este posibilă, întorcând noua poziție, luând în considerare
    și Gateway-urile.
    Avem următoarele cazuri:
    - dacă poziția corespunde unui spațiu gol, se întoarce acea poziție;
    - dacă poziția corespunde unui gateway, se întoarce poziția gateway-ului pereche;
    - dacă poziția corespunde unui obstacol, se întoarce Nothing.
    Parametrul Position reprezintă poziția destinație.
-}

attemptMove :: Position -> Game -> Maybe Position
attemptMove poz (Game hunter target obstacol gateway lines columns)
    | poz `elem` obstacol = Nothing
    | poz `elem` foldl(\acc x -> acc ++ [fst x]) [] gateway =
        Just (foldl(\acc x -> if poz == fst x then snd x else acc) (0,0) gateway)
    | otherwise = Just poz

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre est. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
    
    Conform definiției, tipul Behavior corespunde tipului funcție
    Position -> Game -> Target.
    
    Având în vedere că cele patru funcții definite în continuare (goEast, goWest,
    goNorth, goSouth) sunt foarte similare, încercați să implementați o funcție
    mai generală, pe baza căreia să le definiți apoi pe acestea patru.
-}
goEast :: Behavior
goEast (x,y) (Game hunter target obstacol gateway lines columns)
  | x < lines - 1 && y + 1 < columns - 1 && x >= 0 && y - 1 >= 0 =
       Target p goEast
  | (x,y+1) `elem` obstacol && (x,y) `elem` foldl(\acc e -> acc ++ [fst e]) [] gateway =
      Target (foldl(\acc e -> if (x,y) == fst e then snd e else acc) (0,0) gateway) goEast
  | otherwise =
      Target (x,y) goEast
  where
      p = fromMaybe
            (x, y)
            (attemptMove
               (x, y + 1) (Game hunter target obstacol gateway lines columns))

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre vest. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goWest :: Behavior
goWest (x,y) (Game hunter target obstacol gateway lines columns)
  | x < lines - 1 && y - 1 < columns - 1 && x >= 0 && y - 1 >= 0 =
       Target p goWest
  | (x,y - 1) `elem` obstacol && (x,y) `elem` foldl(\acc e -> acc ++ [fst e]) [] gateway =
       Target (foldl(\acc e -> if (x,y) == fst e then snd e else acc) (0,0) gateway) goEast
  | otherwise =
      Target (x,y) goWest
  where
      p = fromMaybe
            (x, y)
            (attemptMove
               (x, y - 1) (Game hunter target obstacol gateway lines columns))

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre nord. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goNorth :: Behavior
goNorth (x,y) (Game hunter target obstacol gateway lines columns) =
    if x - 1 < lines - 1 && y < columns - 1 && x - 1 >= 0 && y >= 0
        then Target p goNorth
        else Target (x,y) goNorth
    where
        p = fromMaybe (x,y) (attemptMove (x - 1, y) (Game hunter target obstacol gateway lines columns))

{-
    *** TODO ***

    Comportamentul unui Target de a se deplasa cu o casuță înspre sud. 
    Miscarea se poate face doar daca poziția este validă (se află pe tabla de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul va rămâne 
    pe loc.
-}
goSouth :: Behavior
goSouth (x,y) (Game hunter target obstacol gateway lines columns) =
    if x + 1 < lines - 1 && y < columns - 1 && x + 1 >= 0 && y >= 0
        then Target p goSouth
        else Target (x,y) goSouth
    where
        p = fromMaybe (x,y) (attemptMove (x + 1, y) (Game hunter target obstacol gateway lines columns))

{-
    *** TODO ***

    Comportamentul unui Target de a-și oscila mișcarea, când înspre nord, când înspre sud. 
    Mișcarea se poate face doar dacă poziția este validă (se află pe tablă de
    joc) și nu este ocupată de un obstacol. In caz contrar, Target-ul iși va schimba
    direcția de mers astfel:
    - daca mergea inspre nord, își va modifica direcția miscării înspre sud;
    - daca mergea inspre sud, își va continua mișcarea înspre nord.
    Daca Target-ul întâlneste un Gateway pe traseul său, va trece prin acesta,
    către Gateway-ul pereche conectat și își va continua mișcarea în același sens la ieșire
    din acesta.
    Puteți folosit parametrul Int pentru a surprinde deplasamentul Target-ului (de exemplu,
    1 pentru sud, -1 pentru nord).
-}
bounce :: Int -> Behavior
bounce n (x,y) (Game hunter target obstacol gateway lines columns)
    | (x + n, y) `notElem` obstacol &&
      (x + n, y) `notElem` foldl(\acc e -> acc ++ [fst e]) [] gateway &&
      (x + n, y) `notElem` foldl(\acc e -> acc ++ [snd e]) [] gateway =
            Target (x + n, y) (bounce n)
    | (x + n, y) `elem` foldl(\acc e -> acc ++ [fst e]) [] gateway =
            Target (foldl(\acc e -> if (x + n, y) == fst e then snd e else acc) (0,0) gateway) (bounce n)
    | (x + n, y) `elem` obstacol =
            Target (x - n, y) (bounce (negate n))
    | otherwise = Target (x,y) (bounce n)

{-
    *** TODO ***
    Funcție care mută toate Target-urile din Game-ul dat o poziție, în functie
    de behavior-ul fiecăreia și întoarce noul Game în care pozițiile Target-urilor
    sunt actualizate.
    
-}
moveTargets :: Game -> Game
moveTargets game@(Game hunter target obstacol gateway lines columns) =
    Game hunter (foldl(\acc x -> acc ++ [behavior x (position x) game]) [] target) obstacol gateway lines columns

{-
    *** TODO ***

    Verifică dacă Targetul va fi eliminat de Hunter.
    Un Target este eliminat de Hunter daca se află pe o poziție adiacentă
    cu acesta.
    Parametrul Position reprezintă poziția Hunterului pe tabla
    de joc.
    Parametrul Target reprezintă Targetul pentru care se face verificarea.
-}
isTargetKilled :: Position -> Target -> Bool
isTargetKilled (x,y) (Target (a,b) be)
    | (x,y) == (a- 1, b) || (x,y) == (a + 1, b) || (x,y) == (a, b - 1) || (x,y) == (a, b + 1) = True
    | otherwise = False


{-
    *** TODO ***

    Avansează starea jocului curent, rezultând starea următoare a jocului.
    Parametrul Direction reprezintă direcția în care se va deplasa Hunter-ul.
    Parametrul Bool specifică dacă, după mutarea Hunter-ului, vor fi
    mutate și Target-urile sau nu, și dacă vor fi eliminate din joc sau nu.
    Este folosit pentru a distinge între desfășurarea reală a jocului (True)
    și planificarea „imaginată” de hunter (False) în partea a doua a temei.

    Avansarea stării jocului respectă următoarea ordine:
    1. Se deplasează Hunter-ul.
    2. În funcție de parametrul Bool, se elimină Target-urile omorâte de către Hunter.
    3. In funcție de parametrul Bool, se deplasează Target-urile rămase pe tablă.
    4. Se elimină Targeturile omorâte de către Hunter și după deplasarea acestora.
    
    Dubla verificare a anihilării Target-urilor, în pașii 2 și 4, îi oferă Hunter-ului
    un avantaj în prinderea lor.
-}

move_hunter :: Direction -> Game -> Position
move_hunter d (Game (x,y) target obstacol gateway lines columns)
    | d == East = fromMaybe (x,y) (attemptMove (x, y + 1) (Game (x,y) target obstacol gateway lines columns))
    | d == West = fromMaybe (x,y) (attemptMove (x, y - 1) (Game (x,y) target obstacol gateway lines columns))
    | d == North = fromMaybe (x,y) (attemptMove (x-1, y) (Game (x,y) target obstacol gateway lines columns))
    | otherwise  = fromMaybe (x,y) (attemptMove (x+1, y) (Game (x,y) target obstacol gateway lines columns))

first_move :: Direction -> Game -> [Target]
first_move d game@(Game (x,y) target obstacol gateway lines columns) =
    foldl(\acc e -> if isTargetKilled (move_hunter d  game) e then acc else  acc ++ [e]) [] target

advanceGameState :: Direction -> Bool -> Game -> Game
advanceGameState d bool game@(Game (x,y) target obstacol gateway lines columns)

    | bool = Game (move_hunter d  game)
            (first_move d
            (moveTargets (Game (move_hunter d  game)
            (first_move d (Game (move_hunter d  game) target obstacol gateway lines columns))
            obstacol gateway lines columns)))
            obstacol gateway lines columns

    | otherwise = Game (move_hunter d  game) target obstacol gateway lines columns

{-
    ***  TODO ***

    Verifică dacă mai există Target-uri pe table de joc.
-}
areTargetsLeft :: Game -> Bool
areTargetsLeft (Game hunter target obstacol gateway lines columns) = null target

{-
    *** BONUS TODO ***

    Comportamentul unui Target de a se deplasa în cerc, în jurul unui Position, având
    o rază fixată.
    Primul parametru, Position, reprezintă centrul cercului.
    Parametrul Int reprezintă raza cercului.
    Puteți testa utilizând terenul circle.txt din directorul terrains, în conjuncție
    cu funcția interactive.
-}
circle :: Position -> Int -> Behavior
circle = undefined


instance ProblemState Game Direction where
    {-
        *** TODO ***
        
        Generează succesorii stării curente a jocului.
        Utilizați advanceGameState, cu parametrul Bool ales corespunzător.
    -}
    successors game@(Game (x,y) target obstacol gateway lines columns) =
        [(North, advanceGameState North False game),
        (South, advanceGameState South False game),
        (East, advanceGameState East False game),
        (West, advanceGameState West False game)]

    {-
        *** TODO ***
        
        Verifică dacă starea curentă este un în care Hunter-ul poate anihila
        un Target. Puteți alege Target-ul cum doriți, în prezența mai multora.
    -}
    isGoal (Game hunter target obstacol gateway lines columns)  =
        foldl(\acc x -> isTargetKilled hunter x || acc) False target

    {-
        *** TODO ***
        
        Euristica euclidiană (vezi hEuclidian mai jos) până la Target-ul ales
        de isGoal.
    -}
    h game@(Game hunter target obstacol gateway lines columns) =
        if not (isGoal game) then 0.0
        else hEuclidean hunter (position (head target))

{-
     ** NU MODIFICATI **
-}
hEuclidean :: Position -> Position -> Float
hEuclidean (x1, y1) (x2, y2) = fromIntegral $ ((x1 - x2) ^ pow) + ((y1 - y2) ^ pow)
  where
    pow = 2 :: Int

{-
    *** BONUS ***

    Acesta reprezintă un artificiu necesar pentru testarea bonusului,
    deoarece nu pot exista două instanțe diferite ale aceleiași clase
    pentru același tip.

    OBSERVAȚIE: Testarea bonusului pentru Seach este făcută separat.
-}

newtype BonusGame = BonusGame Game
    deriving (Eq, Ord, Show)

{-
    *** BONUS TODO ***

    Folosind wrapper-ul peste tipul Game de mai sus instanțiați
    ProblemState astfel încât să fie folosită noua euristică. 
-}
instance ProblemState BonusGame Direction where
    {-
        *** BONUS TODO ***

        Pentru a ne asigura că toțî succesorii unei stări sunt de tipul
        BonusGame și folosesc noua euristică trebuie să aplicăm wrapper-ul
        definit mai sus peste toți succesorii unei stări.

        Hint: Puteți să folosiți funcția fmap pe perechi pentru acest lucru.
        https://wiki.haskell.org/Functor
    -}
    successors = undefined

    {-
        *** BONUS TODO ***

        Definiți funcția isGoal pentru BonusGame.

        Hint: Folosiți funcția isGoal deja implementată pentru tipul Game.
    -}
    isGoal = undefined

    {-
        *** BONUS TODO ***

        Definiți o funcție euristică care este capabilă să găsească un drum mai scurt
        comparativ cu cel găsit de euristica implementată pentru Game.

        ATENȚIE: Noua euristică NU trebuie să fie una trivială.

        OBSERVAȚIE: Pentru testare se va folosi fișierul terrains/game-6.txt.
    -}
    h = undefined
