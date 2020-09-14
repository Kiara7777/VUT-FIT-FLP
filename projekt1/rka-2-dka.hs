-- Projekt: Prevod rozsireneho konecneho automatu a deterministicky konecny automat do predmetu FLP
-- Nazev: rka-2-dka
-- Autor: Sara Skutova, xskuto00@stud.fit.vutbr.cz

import System.Environment   
import System.IO
import Data.List
import Data.List.Split 

-- --------------------STRUKTURY--------------------
-- Struktura pro parametry
data Params = Params 
	{ mode :: Bool
	, file :: String
	} deriving (Show)
					 
-- STRUKTURY A TYPY PRO AUTOMAT
type KAStav = String
type KASymbol = String

-- Struktura pro prechod
data KAPrechod = KAPrechod 
	{ startState :: KAStav
	, symbolPr :: KASymbol
	, endState :: KAStav
	}
	
-- Pomocna struktura pro novy prechod, misto zakladnich stavu tam jsou makrostavy - e-uzavery
data NewPrechod = NewPrechod
	{ startStateN :: [KAStav]
	, symbolPrN :: KASymbol
	, endStateN :: [KAStav]
	} deriving Show

-- predefinovani Show pro prechod - at se to pekne vypise
instance Show KAPrechod where
	show (KAPrechod sS sP eS) = intercalate "," [sS, sP, eS]
	
-- Struktura pro automat
data KAutomat = KAutomat
	{ stavy :: [KAStav]
	, abeceda :: [KASymbol]
	, prechody :: [KAPrechod]
	, start :: KAStav
	, ends :: [KAStav]
	}
	
-- predefinovani Show pro automat - at se to pekne vypise
instance Show KAutomat where
	show (KAutomat q a p s f) = unlines $ 
		[intercalate "," q, s]
		++ ([intercalate "," f]) 
		++ (map show p)
		
-- Pomocna struktura pro zapamatovani stavu na konci pravidla a symbolu abecedy pres ktery se k nemu dostane
-- pouziva se taky pro e-uzavery		
data PreStav = PreStav
	{ preStavy :: [KAStav]
	, symbolAbc :: KASymbol
	} deriving Show

-- Pomocna struktura, pomoci ni se pak prejmenovavaji jednotlive makrostavy	
data RenameStav = RenameStav
	{ puvodni :: [KAStav]
	, nove :: KAStav
	} deriving Show


-- --------------------PROGRAM--------------------------

-- Hlavni funkce, spousti celou aplikaci
main :: IO() 
main = do  
   args <- getArgs 
   let param = argsParse args
   automat <- getKAutomat (getFile param)
   --ROZSIRENI
   runByParams param automat
   
   --puvodni
   -- if (mode param) then rka2dka automat
                   -- else printKAutomat automat
				   
				   
				   
-----------------------------------------PARAMETRY-----------------------------------   
-- Zkontroluje parametry a vytvori strukturu pro jejich ulozeni
-- @param [String] seznam s parametry
-- @return Params, struktura s parametry, kterou funkce vrati
argsParse :: [String] -> [Params]
argsParse [] = error "Nedostatecny pocet parametru"
argsParse (x:[]) = argsParse [x, ""]
argsParse (x:y:[])
   | (x == "-i" || x == "-t" ) && (y == "-i" || y == "-t") = argsParse' x y ""
   | x == "-i" = [Params False y]
   | x == "-t" = [Params True y]
   | otherwise = error "Neznama kombinace"
argsParse (x:y:z:[]) = argsParse' x y z
argsParse (x:y:z:_) = error "Prilis moc parametru"

-- Pomocna funkce na zpracovani parametru - ROZSIRENI
argsParse' :: String -> String -> String -> [Params]
argsParse' x y z
	| x == "-i" && y == "-t" = Params False z : Params True z : []
	| x == "-t" && y == "-i" = Params True z : Params False z : []
	| otherwise = error "Neznama kombinace"
	
-- Funkce co ze struktury parametru dostane jmeno souboru - ROZSIRENI
getFile :: [Params] -> String
getFile (x:xs) = file x

-- Funkce so prochazi seznam parametru a podle toho take tvori vystup programu - ROZSIRENI
-- -i a pak -t nebo -t a az pak -if
runByParams :: [Params] -> KAutomat -> IO()
runByParams (x:xs) automat
	| (length xs) > 0 = -- jeste tam neco je, vytiskne se o 1 radek navic pro oddeleni
		if (mode x) 
			then do
				rka2dka automat
				putStrLn ""
				runByParams xs automat
			else do
				printKAutomat automat
				putStrLn ""
				runByParams xs automat
	| otherwise =	 -- nic uz tam neni, jenom jeden prvek, normalni tisk
		if (mode x) 
			then rka2dka automat
			else printKAutomat automat
--------------------------------------------------------------------------------------

-----------------------------------NACTENI DO VNITRNI REPREZENTACE---------------------
-- Nacte konecny automat
-- @param String, cesta k souboru, pokud "" tak se cte z stdin
-- @return IO KAutomat, vnitrni reprezentace automatu zabalena do IO
getKAutomat :: String -> IO KAutomat
getKAutomat "" = do 
	content <- getContents
	return (toKAutomat (lines content))
getKAutomat path = do
	content <- readFile path
	return (toKAutomat (lines content))


-- Nacteny automat prevede do vnitrni reprezenatce
-- @param [String] seznam retezcu obsahujici nacteny automat
-- @return KAutomat vnitrvni reprezenatce automatu
toKAutomat :: [String] -> KAutomat
toKAutomat (stavy : pStav : kStavy : pravidla) = 
	if null pravidla
		then error "Zadna pravidla"
		else KAutomat
			(getStates stavy)
			(sort (removeDuplEps (map getAlph pravidla)))
			(map getRule pravidla)
			pStav
			(getStates kStavy)
toKAutomat _ = error "Spatny syntax vstupu"

-- Funkce odstrani duplikaty a prazdny znak z abecedy
-- @param [String] seznam se symboly abecedy
-- @return [String] seznam se symboly abecedy bez duplikatu a prazdneho znaku
removeDuplEps :: [String] -> [String]
removeDuplEps abeceda = nub (filter (/="") abeceda)

-- Funkce co rozdeli pravidlo na jednotlive clanky a vola funkci do z toho dostane symbol abecedy
-- @param String pravidlo, prechod
-- @return KASymbol symbol abecedy z daneho pravidla
getAlph :: String -> KASymbol
getAlph pravidlo = getAlph' (splitOn "," pravidlo)

-- Pomocna funkce pro dostani abecedy z pravidel
-- @param [String] jednotlive clanky parvidla
-- @return KASymbol symbol abecedy z daneho pravidla
getAlph' :: [String] -> KASymbol
getAlph' [q1, aSymbol, q2] = aSymbol
getAlph' _ = error "Spatna syntaxe pravidla"

-- Prevede retezec stavu na seznam stavu
-- @param String retezec se stavy
-- @return [KAStav] seznam stavu
getStates :: String -> [KAStav]
getStates stavy = sort $ splitOn "," stavy

-- Funkce rozdeli retezec s pravidlem na jenotlive clanky a vola funkci co to prevede na vnitrni reprezentaci
-- @param String retezec s pravidlem/prechodem
-- @return KAPrechod vnitrni reprezentace jednoho prechodu
getRule :: String ->  KAPrechod
getRule pravidlo = getRule' (splitOn "," pravidlo)

-- Pomocna funkce pro zpracovani prechodu a jeho prevodu na vnitrni reprezentaci
-- @param [String] seznam s clanky prechodu
-- @return KAPrechod vnitrni reprezentace jednoho prechodu
getRule' :: [String] -> KAPrechod
getRule' [q1, aSymbol, q2] = KAPrechod q1 aSymbol q2
getRule' _ = error "Spatna syntaxe pravidla"
-----------------------------------------------------------------------------------

------------------------VYPIS----------------------------------------------------
-- Funkce vytiskne konecny automat na stdout
printKAutomat :: KAutomat -> IO ()
printKAutomat automat = do
	putStr $ show automat -- nepouzivej print, da na konec o 1 novy radek navic
	
-- Jenom testovaci funkce pro vypis vypocteneho epsilon uzaveru
printEplilonUzaver :: [KAStav] -> IO()
printEplilonUzaver stavy = putStr $ unlines stavy
--------------------------------------------------------------------------------

-------------------------------------PREVOD RKA NA DKA---------------------------	
rka2dka :: KAutomat -> IO ()
rka2dka automat = do
	let epsilonStart = sort $ nub $ eplisolUzaver (start automat) (prechody automat) (prechody automat)-- epsilon uzaver pro statrovaci stav - seznam stavu [KAStav]
	let pechodyAbc = allStAllSymb epsilonStart (abeceda automat) (prechody automat) -- prechody pro epsilon uzaver ze startovaciho stavu - [PreStav], Prestav = [KAStav] + smbol
	let epsilonNew = pre2Eps pechodyAbc (prechody automat) --epsilon uzavery pro nove prvky - [PreStav]
	--muzu zacit vytvaret nove pravidlo
	
	let newRules = createNewRules epsilonNew epsilonStart
	let hotovo = [epsilonStart]
	let rozpracovano = removeDuplicates hotovo (nub $ preStav2KAStav epsilonNew)
	
	-- let test = sort $ nub $ eplisolUzaver (start automat) (prechody automat) (prechody automat)
	-- let pechodyAbcT = allStAllSymb test (abeceda automat) (prechody automat)
	-- let epsilonNewT = pre2Eps pechodyAbcT (prechody automat)
	-- let newRulesT = createNewRules epsilonNewT test 
	-- let hotovoT = [test]
	-- let rozpracovanoT = removeDuplicates hotovoT (nub $ preStav2KAStav epsilonNewT)
	
	-- print test
	-- print pechodyAbcT
	-- print epsilonNewT
	-- print newRulesT
	-- print hotovoT
	-- print rozpracovanoT
	
	simulate hotovo rozpracovano newRules automat
	

	
-- Simuluje cely prevod... zatim nejak
-- @param [[KAStav]] seznam zpracovanych makrostavu
-- @param [[KAStav]] seznam rozpracovanych makrostavu
-- @param [NewPrechod] seznam novych prechodu
-- @param KAutomat puvodni rka automat
simulate :: [[KAStav]] -> [[KAStav]] -> [NewPrechod] -> KAutomat -> IO ()
simulate hotovo [] pravidlaN automat = do 
	let st = startStates hotovo automat
	let fin = finalStates hotovo (ends automat)
	
	let ren = renameStates hotovo 1
	let dka = KAutomat (getNewStates ren) (abeceda automat) (renamePrechody pravidlaN ren) (renameSt st ren) (renameFin fin ren)
	
	printKAutomat dka
	-- print hotovo
	-- print st
	-- print fin
	-- print pravidlaN

	-- let allN = getNewStates ren
	-- let stN = renameSt st ren
	-- let finN = renameFin fin ren
	-- let prN = renamePrechody pravidlaN ren
	-- print ren
	-- print stN
	-- print finN
	-- print prN
	-- print allN
simulate hotovo (x:xs) pravidlaN automat = do
	let pechodyAbc = allStAllSymb x (abeceda automat) (prechody automat) -- stavy pres abecedu
	let epsilonNew = pre2Eps pechodyAbc (prechody automat) --epsilon uzavery pro stavy pres abecedu
	let newRules = createNewRules epsilonNew x
	let hotovoN = hotovo ++ [x]
	let rozpracovano = nub $ xs ++ removeDuplicates hotovoN (nub $ preStav2KAStav epsilonNew)
	simulate hotovoN rozpracovano (pravidlaN ++ newRules) automat
	
-------------------------------------------------PREJMENOVANI---------------------------
-- Prejmenuje vsechny stavy do pozadovane podoby, podle toho se budou prejmenovavat vechny ostatni
-- @param [[KAStav]] seznam vsech stavu
-- @param Int cislo od ktereho se stavy cisluji = dava se jim nove jmeno
-- @return [RenameStav] seznam struktur ve ktechych se uchovava inforamce o novem pojmenovani stavu
renameStates :: [[KAStav]] -> Int -> [RenameStav]
renameStates [] _ = []
renameStates (x:xs) cislo = RenameStav x (show cislo) : renameStates xs (cislo + 1)

-- Prejmenuje pocatecni stavy
-- @param [[KAStav]] seznam pocatecnich makro stavu.. ve skutecnosti by tam mel byt ale jenom jeden.... 
-- @param  [RenameStav] seznam struktur ve ktechych se uchovava inforamce o novem pojmenovani stavu
-- @return KAStav pocatecni stv s novym pojemnovanim
renameSt:: [[KAStav]] -> [RenameStav] -> KAStav
renameSt [] _ = []
renameSt (x:xs) pojmenovani = rename' x pojmenovani

-- Prejmenuje pocatecni a koncove stavy (spolecna funkce, vola se jednou pro pocatecni a jednou pro koncove)
-- @param [[KAStav]] seznam pocatecnich/koncovych makro stavu
-- @param  [RenameStav] seznam struktur ve ktechych se uchovava inforamce o novem pojmenovani stavu
-- @return [KAStav] seznam pocatecnich/koncovych stavu - s novym pojmenovanim
renameFin:: [[KAStav]] -> [RenameStav] -> [KAStav]
renameFin [] _ = []
renameFin (x:xs) pojmenovani = rename' x pojmenovani :  renameFin xs pojmenovani

-- pomocna funkce, makrostav prejmenuje podle prekladatelske struktury
-- @param [[KAStav]] makro stav - e-uzaver
-- @param  [RenameStav] seznam struktur ve ktechych se uchovava inforamce o novem pojmenovani stavu
-- @return KAStav nove pojmenovani stavu
rename' :: [KAStav] -> [RenameStav] -> KAStav
rename' _ [] = [] --k tomuhle by nikdy dojit nemelo....ani nesmi!!!!
rename' stav (x:xs) = 
	if (stav == (puvodni x))
		then nove x
		else rename' stav xs

-- Prejmenuje prechody
-- @param [NewPrechod] seznam prechodu co se musi prejmenovat
-- @param  [RenameStav] seznam struktur ve ktechych se uchovava inforamce o novem pojmenovani stavu
-- @return [KAPrechod] seznam prejmenovanych prechodu
renamePrechody :: [NewPrechod] -> [RenameStav] -> [KAPrechod]
renamePrechody [] _ = []
renamePrechody (x:xs) pojmenovani = renamePrechody' x pojmenovani : renamePrechody xs pojmenovani

-- pomocna funcke, ktera prevezme prechod a vola funkce na prejmenovani
-- @param NewPrechod prechod s makro stavy
-- @param  [RenameStav] seznam struktur ve ktechych se uchovava inforamce o novem pojmenovani stavu
-- @return KAPrechod prejmenovany preechod
renamePrechody' :: NewPrechod -> [RenameStav] -> KAPrechod
renamePrechody' prechod pojmenovani = 
	KAPrechod  -- vytvoreni prechodu s prejmenovanymi stavy KAPrechod start symbol end
	(rename' (startStateN prechod) pojmenovani) 
	(symbolPrN prechod)
	(rename' (endStateN prechod) pojmenovani)
	
-- Z prekladelske struktury dostat jmena novych stavu
-- @param  [RenameStav] seznam struktur ve ktechych se uchovava inforamce o novem pojmenovani stavu
-- @return [KAStav] nove pojmenovani
getNewStates :: [RenameStav] -> [KAStav]
getNewStates [] = []
getNewStates (x:xs) = nove x : getNewStates xs

----------------------------------------------------------------------------------------
-- Vytvori nove koncove stavy
-- @param [[KAStav]] seznam stavu
-- @param KAutomat seznam puvodnich koncovych stavu
-- @return [[KAStav]] seznam novych koncovych stavu
finalStates :: [[KAStav]] -> [KAStav] -> [[KAStav]]
finalStates _ [] = []
finalStates	stavy (x:xs) = finalStates' stavy x ++ finalStates stavy xs

-- pomocna funkce pro nalezeni koncovych stavu
finalStates' :: [[KAStav]] -> KAStav -> [[KAStav]]
finalStates' [] _ = []
finalStates' (x:xs) stav = 
	if (elem stav x)
		then x : finalStates' xs stav
		else finalStates' xs stav
	
-- Vytvori novy startovaci stav - startovaci stavy
-- @param [[KAStav]] seznam stavu
-- @param KAutomat puvodni automat
-- @return [[KAStav]] seznam startovacich stavu
startStates :: [[KAStav]] -> KAutomat -> [[KAStav]]
startStates [] _ = []
startStates	(x:xs) automat = 
	if (elem (start automat) x)
		then x : startStates xs automat
		else startStates xs automat

	
-- Kontrola hotovych a rozpracovanych stavu, pokud se v hotovych nachazi stav z rozpracovanych, tak se ten v rozpracovanych odstani
-- @param [[KAStav]] seznam zpracovanych makrostavu
-- @param [[KAStav]] seznam rozpracovanych makrostavu
-- @return [[KAStav]] seznam rozpracovanych makrostavu bez stavu co nechazeji ve zpracovanych
removeDuplicates :: [[KAStav]] -> [[KAStav]] -> [[KAStav]]
removeDuplicates [] rozpracovane = rozpracovane
removeDuplicates (x:xs) rozpracovane = nub $ removeDuplicates xs (removeDuplicates' x rozpracovane)

-- pomocna funkce pro odstraneni duplikatu, vytvori novy seznam rozpracovano ktery se pak pouziva dale
removeDuplicates' :: [KAStav] -> [[KAStav]] -> [[KAStav]]
removeDuplicates' stav rozpracovane = delete stav rozpracovane
	
-- Z PreStavu vrati seznam novych makrostavu
-- @param [PreStav]Seznam novych stavu ve typu PresTav
-- @return [[KAStav]] Seznam novych makrostavu
preStav2KAStav :: [PreStav] -> [[KAStav]]
preStav2KAStav [] = []
preStav2KAStav (x:xs) = filter (not . null) $ preStavy x : preStav2KAStav xs


-- Vytvori nove pravidla
-- @param [PreStav] cilove makro stavy + pres jake znaky abecedy se k nim lze dostat
-- @param [KAStav] pocatecni makro stav
-- @return [NewPrechod] nove prechody
createNewRules :: [PreStav] -> [KAStav] -> [NewPrechod]
createNewRules [] _ = []
createNewRules (x:xs) sStavy = 
	if null (preStavy x)
		then createNewRules [] sStavy -- diky tomuhle by se nemely vytvaret prechody pro prazdne stavy
		else [NewPrechod sStavy (symbolAbc x) (preStavy x)] ++ createNewRules xs sStavy


--------------------------STAVY + ABECEDA------------------------------
-- pro vechny symboly z abecedy nalezne prechody pro dany seznam stavu N:M, tady ale vysledky stavu potrebuju oddelene + potrebuju informaci k jakému symbolu to je
-- skoro muzeme tvorit novy prechod
allStAllSymb :: [KAStav] -> [KASymbol] -> [KAPrechod] -> [PreStav]
allStAllSymb _ [] _  = []
allStAllSymb stavy (x:xs) pravidla = [PreStav (allPrechodySymbol stavy x pravidla) x] ++ allStAllSymb stavy xs pravidla
	
-- dostane vsechny stavy najde vechny prechody pro ten jeden symbol, N:1 (z danych stavu)
allPrechodySymbol :: [KAStav] -> KASymbol -> [KAPrechod] -> [KAStav]
allPrechodySymbol [] _ _  = []
allPrechodySymbol (x:xs) symbol pravidla = prechodSymbol x symbol pravidla ++ allPrechodySymbol xs symbol pravidla 

-- pro dany stav nalezne vsechny prechody pro dany symbol abecedy, 1:1
prechodSymbol :: KAStav ->  KASymbol -> [KAPrechod] -> [KAStav]
prechodSymbol _ _ [] = []
prechodSymbol stav symbol (x:xs)
	| stav == startState x && symbolPr x == symbol = [endState x] ++ prechodSymbol stav symbol xs --neco nasel, pokracuj v hledani
	| otherwise = prechodSymbol stav symbol xs
-----------------------------------------------------------------------------
------------------------------------------------EPSILON UZAVERY------------------------------
--prochazi PreStavy a vola funkce ktere pro ne tvori epsilon uzavery, vysledkem budou noce PreStavy - prakticky uz nove stavy
pre2Eps :: [PreStav] -> [KAPrechod] -> [PreStav]
pre2Eps [] _ = []
pre2Eps (x:xs) pravidla = [epsAbc (preStavy x) (symbolAbc x) pravidla] ++ pre2Eps xs pravidla

-- vytvori pre stav aby se vedelo jakym prechodem se doslo do daneho epsilon uzaveru
epsAbc :: [KAStav] -> KASymbol -> [KAPrechod] -> PreStav
epsAbc stavy symbol pravidla = PreStav (allEpsilonUzaver stavy pravidla) symbol 

-- pro seznam stavu nalezne jejich epsilon uzaver, dostane vice stavu
allEpsilonUzaver :: [KAStav] -> [KAPrechod] -> [KAStav]
allEpsilonUzaver [] _ = []
allEpsilonUzaver (x:xs)  pravidla = sort $ nub $ eplisolUzaver x pravidla pravidla ++ allEpsilonUzaver xs pravidla

-- Prochazet jednotliva pravidla a hledat, ktere pravidlo obsahuje jako pocatecni stav dany stav
-- Pri nalezeni otestovat zda je to epsilon pravidlo, pokud ano tak vyhledat epsilon pravidla cile danoho pravidla a znovu prohledavat vechny pravidla pro tento stav atd....
-- epsilon uzaver v sobe obsahuje prvek pro ktery se tvori - pro nezapomenuti, rvej to vsude
-- bude tam hodne duplikatu!!!!! pouzij nub!!!!!!
eplisolUzaver :: KAStav -> [KAPrechod] -> [KAPrechod] -> [KAStav]
eplisolUzaver stav [] _= [stav]
eplisolUzaver stav (x:xs) pravidla
	| stav == startState x && symbolPr x == "" = [stav] ++ [endState x] ++ eplisolUzaver (endState x) pravidla pravidla ++ eplisolUzaver stav xs pravidla
	| otherwise = [stav] ++ eplisolUzaver stav xs pravidla
-- eplisolUzaver stav xs pravidla, to v tom prvnim guardu musi byt, musim totiz prohledat i zbytek pravidel, bez toho by se to odchytlo jenom na prvnim pravidle a zbytek by se ignorovalo
-- eplisolUzaver (endState x) pravidla pravidla, proc v teto funkci jsou potreba 2x pravidla? myselela jsem, ze kdyz se udela (x:xs), tak to znovu slozi dohromady, zkousela jsem i ++,
--												ale nepostavilo to znovu vsechny pravidla, tak proto tam jsou 2x
--------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------