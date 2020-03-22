module ArtifactProjection where

{-
 Définition des tags pour les types de noeuds (sequentiel ou parallèle)
 Dans un artefact donné, un noeud A est taggé par "Seq" (resp. "Par") lorsque ses sous-artefacts s'exécutent en séquence (resp. potentiellement en parallèle)
 Un noeud ayant au plus un sous-artefact est toujours taggé par "Seq"
-}
data ProductionTag x = Seq x | Par x deriving (Eq, Show)

-- La fonction untagProduction ci-dessous débarrasse un symbole donné de son tag ("Seq" ou "Par")
untagProduction:: ProductionTag x -> x
untagProduction (Seq x) = x
untagProduction (Par x) = x

{-
 Définition des tags pour les symboles (clos, verrouillé, deverrouillé ou en dessus)
 Dans un artefact: 
 					- un noeud clos est taggé par "Closed"
 					- un bourgeon déverouillé est taggé par "Unlocked"
 					- un bourgeon vérouillé est taggé par "Locked"
 					- un bourgeon en dessus est taggé par "Upstair" (on n'en trouve qu'après l'expansion)
-}
data NodeTag x = Closed x | Locked x | Unlocked x | Upstair x deriving (Eq, Show)

-- La fonction untagNode ci-dessous débarrasse un symbole donné de son tag ("Closed", "Locked", "Unlocked" ou "Upstair")
untagNode:: NodeTag x -> x
untagNode (Closed x) = x
untagNode (Locked x) = x
untagNode (Unlocked x) = x
untagNode (Upstair x) = x

{-
 Définition des tags pour les types de symboles (structuration ou standard)
 Les symboles d'un artefact t donné sont soit ceux du modèle grammatical G dénotant t, soit des symboles de (re)structuration introduits
 Pour préserver certaines propriété importante de notre modèle (principalement, la forme des productions utilisées dans les GMWf):
 Dans ce cas, les symboles de G sont dits standard et sont taggés par "Standard" et les symboles de (re)structuration sont taggés par "Structural"
-}
data SymbolTag x = Structural x | Standard x deriving (Eq, Show)

-- La fonction untagSymbol ci-dessous débarrasse un symbole donné de son tag ("Structural" ou "Standard")
untagSymbol:: SymbolTag x -> x
untagSymbol (Structural x) = x
untagSymbol (Standard x) = x

{-
 Définition d'un artefact: récursivement, nous considérons qu'un artefact est donné par son noeud racine (nodeLabel) et 
 La liste de ses sous-artefacts (sonsList) taggée soit par "Seq" (pour indiquer que ces derniers s'exécutent en séquence), soit par "Par" (pour indiquer que ces derniers s'exécutent potentiellement en parallèle)
 Nous ne considérons pas d'artefact vide
-}
data Artifact a = Node {nodeLabel:: a, sonsList:: ProductionTag [Artifact a]} deriving Eq

-- Pour des besoins de lisibilité dans les tests, nous surchargeons la fonction show qui s'applique aux artefacts
instance (Show a) => Show (Artifact a) where 
	show (Node a (Seq sons)) = (show a) ++ " [" ++ (printWithSep " ; " (map show sons)) ++ "] "
	show (Node a (Par sons)) = (show a) ++ " [" ++ (printWithSep " || " (map show sons)) ++ "] "
	
printWithSep sep [] = ""
printWithSep sep [xs] = xs
printWithSep sep (xs:xss) = xs ++ sep ++ (printWithSep sep xss)

{-
 Exemples d'artefacts pour des besoins de tests. Ceux ci correspondent aux artefacts cibles de notre exemple d'application.

							Ag													Ag
							|													|
							|													|
	art1	-------->		A						art2	-------->       	A
						   / \												   / \
						  /   \												  /   \
						 B  ;  D											 C  ;  D
																			/ \
																		   /   \
																		  E  ;  F
																		 / \
																		/   \
																	  G1 ||  G2
																	 / \     / \
																	/   \   /   \
																  H1 ;  I1 H2 ;  I2
-}
art1 = Node (Closed "Ag") (
		   Seq [
			Node (Closed "A") (
			   Seq [
				Node (Closed "B") (Seq []), 
				Node (Closed "D") (Seq [])
			   ])
		   ])
art2 = Node (Closed "Ag") (
		   Seq [
			Node (Closed "A") (
			   Seq [
				Node (Closed "C") (
				   Seq [
					Node (Closed "E") (
					   Par [
						Node (Closed "G1") (
						   Seq [
							Node (Closed "H1") (Seq []), 
							Node (Closed "I1") (Seq [])
						   ]), 
						Node (Closed "G2") (
						   Seq [
							Node (Closed "H2") (Seq []), 
							Node (Closed "I2") (Seq [])
						   ])
					   ]), 
					Node (Closed "F") (Seq [])
				   ]), 
				Node (Closed "D") (Seq [])
			   ])
		   ])

-- Définition d'un synonyme de type pour représenter une vue donnée
type View a = [a]

{-
 Exemples de vues: ici nous avons les vues de notre exemple d'application; celles de l'éditeur en chef (viewEC), 
 De l'éditeur associé (viewAE), du premier referee (viewR1) et du second referee (viewR2)
-}
viewEC = ["Ag", "A", "B", "C", "D", "H1", "H2", "I1", "I2", "F"]
viewAE = ["Ag", "A", "C", "E", "F", "H1", "H2", "I1", "I2"]
viewR1 = ["Ag", "C", "G1", "H1", "I1"]
viewR2 = ["Ag", "C", "G2", "H2", "I2"]


{-
 Algorithme de projection d'un artefact suivant une vue donnée. Les noeuds des artefacts resultants sont taggés (Standard ou Structural)
 Principe de l'algorithme :
 On réalise un parcours en profondeur préfixé de l'artefact à projeter. Le traitement appliqué au niveau de chaque noeud est le suivant :
	- Si le symbole associé au noeud visité est visible (appartient à la vue) alors :
		1- Le noeud visité n'est pas éffacé le symbole qui lui est associé est taggé "Standard"
		2- Chacun des fils (sous-artefacts) du noeud visité est remplacé par sa projection ayant subi un traitement préalable
		   En fait, pour chaque sous-artefact projeté, on retient le symbole associé à son noeud racine initial, le type de production initial et sa projection
		   * Si son type de production initial est identique à celui du noeud actuellement visité, on remplace le sous-artefact par l'ensemble
		     des artefacts de sa projection
		   * Sinon
		     ** Si sa projection n'est pas vide, on crée un nouvel artefact dont le noeud racine est associé à un symbole de (re)structuration (le symbole associé à son noeud racine initial taggé par "Structural")
			    et les sous-artefacts sont ceux de sa projection; le sous-artefact est alors remplacé par le nouvel artefact
			 ** Sinon le sous-artefact est juste éffacé de la liste
	- Dans le cas contraire, le noeud visité est effacé et le résultat de la projection est l'union des projections de chacun de ses fils
 Notons que nous utilisons quelques routines commentées plus bas pour parvenir à nos fins.
-}
projection:: (Eq a) => View a -> Artifact a -> [Artifact (SymbolTag a)]
projection view t@(Node a (Seq sons)) = case elem a view of
											True -> map removeUnconsistentStructSymb [Node (Standard a) (Seq tr)]
											False -> map removeUnconsistentStructSymb tr
										where
											ctss = map' (projection view) sons
											tr = flattern [f (s, taggedts) | (s, taggedts) <- ctss]
											f (s, (Seq ts)) = ts
											f (s, (Par ts)) | ts /= [] = [Node (Structural s) (Par ts)]
															| otherwise = []
projection view t@(Node a (Par sons)) = case elem a view of
											True -> map removeUnconsistentStructSymb [Node (Standard a) (Par tr)]
											False -> map removeUnconsistentStructSymb tr
										where
											ctss = map' (projection view) sons
											tr = flattern [f (s, taggedts) | (s, taggedts) <- ctss]
											f (s, (Par ts)) = ts
											f (s, (Seq ts)) | ts /= [] = [Node (Structural s) (Seq ts)]
															| otherwise = []

{-
 La routine map' suivante applique une fonction f donnée à chaque artefact d'une liste donnée et fournit comme résultat pour chacun, un couple formé :
	- Du symbole associé à la racine de l'artefact initial
	- Du type de production du noeud racine de l'artefact initial, utilisé comme "tag" du résultat de l'application de la fonction f à l'artefact initial considéré
 map' se comporte donc comme map mais elle conserve en plus certaines informations initiales importantes pour l'algorithme de projection
-}
map':: (Artifact a -> b) -> [Artifact a] -> [(a, ProductionTag b)]
map' _ [] = []
map' f (l@(Node a (Seq sons)):ls) = (a, Seq (f l)): map' f ls
map' f (l@(Node a (Par sons)):ls) = (a, Par (f l)): map' f ls


-- La fonction flattern applati une liste de liste: flattern [[1, 2], [5]] = [1, 2, 5]
flattern:: [[a]] -> [a]
flattern lss = [l | ls <- lss, l <- ls]


{-
 La routine "removeUnconsistentStructSymb" suivante est utilisée durant la projection pour éliminer les symboles de (re)structuration dits inconsistents.
 La projection d'un artefact telle qu'elle a été codée ici peut conduire à des artefacts dans lequels on rencontre les cas de figure suivants:

					S						S							X
	cas 1  ---->   	|		cas 2   ---->   |			cas 3   ----> 	|		où S est un symbole de (re)structuration et X un symbole standard.
					|						|							|
					£						X							S

 Dans chacun de ces cas, le symbole de (re)structuration peut être éliminé sans grande conséquence: on dit qu'il est inconsistent.
 La routine suivante ne traite que les cas 2 et 3 pour des raisons de simplicité dans le code.
 Pour le cas 2:
	Le symbole de (re)structuration n'a qu'un seul fils. On l'éfface et on fait remonter ce fils d'un niveau.
 Pour le cas 3:
	Le symbole de (re)structuration est le seul fils d'un noeud donné. On l'éfface et on fait remonter ses potentiels fils d'un niveau.
-}
removeUnconsistentStructSymb:: Artifact (SymbolTag a) -> Artifact (SymbolTag a)
removeUnconsistentStructSymb t@(Node (Structural a) taggedsons) = case length (untagProduction taggedsons) of
																	1 -> removeUnconsistentStructSymb (head (untagProduction taggedsons))
																	otherwise -> case taggedsons of
																					Seq sons -> Node (Structural a) (Seq (map removeUnconsistentStructSymb sons))
																					Par sons -> Node (Structural a) (Par (map removeUnconsistentStructSymb sons))
removeUnconsistentStructSymb t@(Node (Standard a) taggedsons) = case length (untagProduction taggedsons) of
																	1 -> case head (untagProduction taggedsons) of
																			(Node (Structural a2) taggedsons2) -> removeUnconsistentStructSymb (Node (Standard a) taggedsons2)
																			otherwise -> Node (Standard a) (Seq [removeUnconsistentStructSymb (head (untagProduction taggedsons))])
																	otherwise -> case taggedsons of
																					Seq sons -> Node (Standard a) (Seq (map removeUnconsistentStructSymb sons))
																					Par sons -> Node (Standard a) (Par (map removeUnconsistentStructSymb sons))

-- Pour tester la projection, saisir par exemple > projection viewEC (mapArtifact untagNode art2)

-- Fonction utilitaire qui applique une fonction donnée à tous les noeuds d'un artefact donné
mapArtifact :: (a -> b) -> Artifact a -> Artifact b
mapArtifact f l@(Node a (Seq sons)) = (Node (f a) (Seq (map (mapArtifact f) sons)))
mapArtifact f l@(Node a (Par sons)) = (Node (f a) (Par (map (mapArtifact f) sons)))


{-
 Type des productions: une production est donnée par sa partie gauche (lhs) constituée d'un symbole et par sa partie droite (rhs)
 constitué de plusieurs symboles.
-}
data Production a = Prod {lhs:: a, rhs:: [a]} deriving Eq
instance (Show a) => Show (Production a) where 
	show (Prod ls rss) = (show ls) ++ " -> " ++ show rss


{-
 Type des GMWf: un GMWf est donné par l'ensemble des symboles et l'ensemble des productions le constituant.
 Les productions sont taggées soit par "Seq" soit par "Par"
-}
data GMWf a = GMWf {symbols:: [a], productions:: [ProductionTag (Production a)]} deriving (Eq, Show)


{-
 Fonction qui collecte les productions ayant permis de construire un artefact donné: les doublons sont supprimés durant la collecte
 On réalise un parcours en profondeur préfixé de l'artefact. A chaque niveau on récolte la production qui s'y trouve et on l'ajoute
 à l'ensemble des productions (sans doublons) ayant servi à construire les sous-artefacts.
-}
collectProductions:: (Eq a) => Artifact a -> [ProductionTag (Production a)]
collectProductions t@(Node a (Seq sons)) = case elem p prods of
												True -> prods
												False -> p:prods
										   where
												p = Seq (Prod a (map nodeLabel sons))
												prods = flatternRemDup (map collectProductions sons)
collectProductions t@(Node a (Par sons)) = case elem p prods of
												True -> prods
												False -> p:prods
										   where
												p = Par (Prod a (map nodeLabel sons))
												prods = flatternRemDup (map collectProductions sons)


-- La fonction flatternRemDup applati une liste de liste en supprimant les doublons: flatternRemDup [[1, 2, 5], [5, 3]] = [1, 2, 5, 3]												
flatternRemDup lss = flatternRemDup' lss []
flatternRemDup' [] acc = acc
flatternRemDup' (ls:lss) acc = flatternRemDup' lss (acc ++ [l | l <- ls, not (elem l acc)])

{-
 Fonction qui collecte les symboles ayant permis de construire un artefact donné: les doublons sont supprimés durant la collecte
 On réalise un parcours en profondeur préfixé de l'artefact. A chaque niveau on récolte le symbole qui s'y trouve et on l'ajoute
 à l'ensemble des symboles (sans doublons) ayant servi à construire les sous-artefacts.
-}
collectSymbols:: (Eq a) => Artifact a -> [a]
collectSymbols t@(Node a taggedsons) = case elem a symbs of
											True -> symbs
											False -> a:symbs
									   where
											symbs = flatternRemDup (map collectSymbols (untagProduction taggedsons))


{-
 Fonction qui génère un GMWf à partir d'une liste d'artefacts donnée.
 On collecte juste les symboles et les productions ayant permis de construire chacun des artefacts et on se sert de l'union de chacun de ces
 groupes d'ensembles pour construire un GMWf
-}
generateGMWf:: (Eq a) => [Artifact a] -> GMWf a
generateGMWf ls = GMWf symbs prods
				  where
					symbs = flatternRemDup (map collectSymbols ls)
					prods = flatternRemDup (map collectProductions ls)


{-
 Fonction qui génère la liste d'artefacts reconnus par un GMWf donné.
 La fonction prend en entrée un GMWf et un symbole X considéré comme axiome puis:
	- A partir de l'ensemble des productions du GMWf, on génère le sous-ensemble des X-productions
	- Ensuite, chaque X-productions est utilisée pour générer (grâce à la fonction genArtifacts) un ensemble d'artefacts donné
		Chaque artefact t de l'ensemble des artefacts obtenue à partir d'une production p donnée est tel que: 
		* le noeud racine de t est associé au symbole en partie gauche de p (lhs p)
		* et l'ensemble des sous-artefacts du noeud racine de t appartient au produit cartésien des ensembles d'artefacts générés 
		  (grâce à la fonction generateArtifacts - récursivité croisée -) en utilisant chaque symbole en partie droite de p (rhs p)
		  comme axiome
	- Le résultat est l'union des ensembles d'artefacts obtenus à partir des X-productions
-}
generateArtifacts:: (Eq a) => GMWf a -> a -> [Artifact a]
generateArtifacts mgwf axiom = flatternRemDup arts
								where
									taggedxprods = [taggedprod | taggedprod <- productions mgwf, lhs (untagProduction taggedprod) == axiom]
									arts = [genArtifacts taggedxprod | taggedxprod <- taggedxprods]
									genArtifacts (Seq xprod) = map (Node (lhs xprod)) [taggedsons | sons <- potentialSons, taggedsons <- [Seq sons]]
																where
																	potentialSons = dist (map (generateArtifacts mgwf) (rhs xprod))
									genArtifacts (Par xprod) = map (Node (lhs xprod)) [taggedsons | sons <- potentialSons, taggedsons <- [Par sons]]
																where
																	potentialSons = dist (map (generateArtifacts mgwf) (rhs xprod))
								

-- Fonction réalisant le produit cartésien d'un ensemble de listes							
dist :: [[a]] -> [[a]]
dist [ ] = [[ ]]
dist (xs : xss) = [y : ys | y <- xs, ys <- dist xss]


{-
 Fonction qui calcule la projection d'un GMWf suivant une vue donnée.
 Le principe utilisé ici est le suivant:
 - A partir du GMWf de départ, on génère l'ensemble des artefacts cibles (grâce à la fonction generateArtifacts écrite précedemment)
 - On projète chacun des artefacts cibles suivant la vue prise en paramètre (on obtient l'ensemble projArts)
 - A partir de l'ensemble des projections des artefacts cibles, on génère (grâce à la fonction generateGMWf écrite précedemment) le GMWf recherché
-}
projectGMWf:: (Eq a) => GMWf a -> View a -> a -> GMWf (SymbolTag a)
projectGMWf mgwf view axiom = generateGMWf projArts
						where
							arts = generateArtifacts mgwf axiom
							projArts = flatternRemDup (map (projection view) arts)


-- Test de projection des GMWf. Saisir > localGMWf viewEC
explGMWf = generateGMWf (map (mapArtifact untagNode) [art1, art2])
localGMWf view = projectGMWf explGMWf view "Ag"


{-
 Fonction qui permet de vérifier qu'un artefact donné (dont les noeuds sont considérés non taggés) est un préfixe d'un autre
 Principe:
	On réalise un parcours en profondeur préfixé des deux artefacts (_art1, _art2). Lorsqu'on visite le noeud n1 de _art1 et n2 de _art2 (situés à la même adresse) on procède comme suit:
	- Si les deux noeuds n1 et n2 sont associés au même symbole et s'ils engendrent le même type (sequentiel ou parallèle) et le même nombre de sous-artefacts,
		alors _art1 est un préfixe de _art2 ssi chaque sous-artefact de n1 est un préfixe du sous-artefact de n2 situé à la même position (adresse)
	- Sinon _art1 n'est pas un préfixe de _art2
-}
isPrefixOf:: (Eq a) => Artifact a -> Artifact a -> Bool
isPrefixOf _art1@(Node a1 taggedsons1) _art2@(Node a2 taggedsons2) = case (taggedsons1, taggedsons2) of
																		(Seq sons1, Seq sons2) -> matchPrefix sons1 sons2
																		(Par sons1, Par sons2) -> matchPrefix sons1 sons2
																		otherwise -> False
																	 where
																		matchPrefix sons1 sons2 = case sons1 of
																									[] -> a1 == a2
																									otherwise -> and ((a1 == a2):((length sons1 == length sons2):(zipWith isPrefixOf sons1 sons2)))

{-
 Fonction qui permet de chercher la liste des artefacts cibles pouvant être utilisés pour guider la fusion three-way (expansion) d'une réplique partielle mise à jour donnée
 Il s'agit des artefacts cibles conformes au GMWf (global) (globalmgwf) tels que: un état précédent de l'artefact en cours d'execution (currentArt) est un préfixe de chacun d'entre eux et,
 la réplique partielle mise à jour (partialRepUptd) est un préfixe de chacune de leurs projection respectives suivant une vue (view) donnée.
 L'algorithme procède naïvement en générant la liste des artefacts dénotés par le GMWf (global) considéré, puis en filtrant cette liste pour ne retenir que tout artefact art tel que (nos deux critères) :
	1- currentArt débarrassé de ses tags (notons que les noeuds d'un artefact en cours d'exécution sont taggés avec NodeTag car ils peuvent être des bourgeons) est un préfixe de art
	2- partialRepUptd débarrassé de ses tags (notons que les noeuds de la réplique partielle d'un artefact en cours d'exécution sont taggés avec NodeTag et SymbolTag car il peuvent être 
	   des bourgeons et les symboles qui leur sont associés sont soit "Standard" - issus du GMWf global - soit "Structural") est un préfixe de la projection (nous considérons ici qu'elle ne produit qu'un 
	   seul artefact) de art suivant la vue considérée
-}
getExpansionGuides:: (Eq a) => GMWf a -> View a -> a -> Artifact (NodeTag a) -> Artifact (NodeTag (SymbolTag a)) -> [Artifact a]
getExpansionGuides globalmgwf view axiom currentArt partialRepUptd = filter f arts
																	 where
																		arts = generateArtifacts globalmgwf axiom
																		f art = (isPrefixOf untaggedCurrentArt art) && (isPrefixOf untaggedPartialRepUptd projArt)
																				where
																					untaggedCurrentArt = mapArtifact untagNode currentArt
																					untaggedPartialRepUptd = mapArtifact untagNode partialRepUptd
																					projs = projection view art
																					projArt = case length projs of
																								1 -> head projs
																								otherwise -> error ""


{-
 Test de l'obtention de la liste d'artefacts devant guider la fusion three-way (site de l'éditeur en chef) > getExpansionGuides explGMWf viewEC "Ag" curArt parRepUptd1.

																				Ag													Ag
																				|													|
																				|													|
	curArt	-------->		Agwu					parRepUptd1		-------->   A					parRepUptd2		-------->		A
						    												   / \												   / \
						     												  /   \												  /   \
																		   Cwu  ;  Dwl										     B  ;  Dwu
-}
curArt = Node (Unlocked "Ag") (Seq [])
parRepUptd1 = Node (Closed (Standard "Ag")) (
				   Seq [
					Node (Closed (Standard "A")) (
					   Seq [
						Node (Unlocked (Standard "C")) (Seq []), 
						Node (Locked (Standard "D")) (Seq [])
					   ])
				   ])
parRepUptd2 = Node (Closed (Standard "Ag")) (
				   Seq [
					Node (Closed (Standard "A")) (
					   Seq [
						Node (Closed (Standard "B")) (Seq []), 
						Node (Unlocked (Standard "D")) (Seq [])
					   ])
				   ])


{-
 Fonction qui retourne une liste correspondant à un artefact donné parcouru en profondeur et en préordre.
 Cette fonction est écrite pour des besoins techniques uniquement. La liste obtenue contient des couples formés par les noeuds (taggés par le type de production utilisée pour construire ses sous-artefacts) de l'artefact parcouru et
 l'adresse (suivant la méthode DLN - Dynamic Level Numbering -) de ces noeuds dans l'artefact.
 Exemple:
	toPrefixList parRepUptd1 = [(Seq tagged-Ag, ""), (Seq tagged-A, "1"), (Seq tagged-C, "1.1"), (Seq tagged-D, "1.2")]
-}
toPrefixList:: Artifact a -> [(ProductionTag a, [Char])]
toPrefixList art = toPrefixList_ "" art
toPrefixList_ addr (Node val (Seq sons)) = case addr of
											  "" -> (Seq val, addr) : (flattern [toPrefixList_ (show i) (sons !! (i - 1)) | i <- [1..length sons]])
											  otherwise -> (Seq val, addr) : (flattern [toPrefixList_ (addr ++ "." ++ (show i)) (sons !! (i - 1)) | i <- [1..length sons]])
toPrefixList_ addr (Node val (Par sons)) = case addr of
											  "" -> (Par val, addr) : (flattern [toPrefixList_ (show i) (sons !! (i - 1)) | i <- [1..length sons]])
											  otherwise -> (Par val, addr) : (flattern [toPrefixList_ (addr ++ "." ++ (show i)) (sons !! (i - 1)) | i <- [1..length sons]])


{-
 Fonction utilitaire qui retourne l'indice de la dernière occurrence d'un élément donné dans une liste
-}
lastIndexOf:: Eq a => a -> [a] -> Int
lastIndexOf l ls = lastIndexOf_ l ls 0
				   where
						lastIndexOf_ _ [] _ = -1
						lastIndexOf_ a (x:xs) index | a == x = index + (lastIndexOf a xs) + 1
													| otherwise = lastIndexOf_ a xs (index + 1)


{-
 Fonction qui construit un artefact à partir d'une liste de noeuds adressés et taggés. Cette fonction réalise l'inverse de la fonction toPrefixList.
-}
fromPrefixList:: [(ProductionTag a, [Char])] -> Artifact a
fromPrefixList ls = fromPrefixList_ "" ls
				   where
						fromPrefixList_ addr [] = error ""
						fromPrefixList_ addr [couple] = case addr == snd couple of
															True -> Node val (Seq [])
															False -> error ""
													   where
															val = untagProduction (fst couple)
						fromPrefixList_ addr couples = case couple of
														(Seq val, addr) -> Node val (Seq sons)
														(Par val, addr) -> Node val (Par sons)
													  where
														couple = head (filter (\(v, ad) -> ad == addr) couples)
														remCouples = filter (\(v, ad) -> ad /= addr) couples
														directSons = filter (\(v, ad) -> (addr == (take (lastIndexOf '.' ad) ad) && not (elem '.' (drop ((lastIndexOf '.' ad) + 1) ad)))) remCouples
														sons = [fromPrefixList_ ad (removeNonSubtreeNodes ad couples) | ad <- map snd directSons]

{-
 Supression des noeuds d'un sous-artefact donné par son adresse dans la liste des noeuds de l'artefact.
 Cette fonction supprime dans une liste donnée tous les couples représentant des noeuds du sous-artefact dont l'adresse de la racine est fournie en paramètre.
-}
removeSubtreeNodes:: [Char] -> [(ProductionTag a, [Char])] -> [(ProductionTag a, [Char])]
removeSubtreeNodes addr = filter (\(taggedNode, nodeAddr) -> addr /= (take (length addr) nodeAddr))


{-
 Supression des noeuds n'appartenant pas à un sous-artefact donné par son adresse dans la liste des noeuds de l'artefact.
 Cette fonction supprime dans une liste donnée tous les couples représentant des noeuds n'appartenant pas au sous-artefact dont l'adresse de la racine est fournie en paramètre.
-}
removeNonSubtreeNodes:: [Char] -> [(ProductionTag a, [Char])] -> [(ProductionTag a, [Char])]
removeNonSubtreeNodes addr = filter (\(taggedNode, nodeAddr) -> addr == (take (length addr) nodeAddr))


{-
 Fonction qui réalise la fusion three-way (un artefact cible - le guide -, un artefact en cours d'exécution - l'état précédent - et un réplica partiel mis à jour)
 Pour réaliser la fusion des trois artefacts pris en paramètre, on procède comme suit:
	- On réalise un parcours en profondeur préfixé de chacun de ces artefacts (fonction toPrefixList) pour obtenir trois listes (l1 - le guide -, l2 - l'état précédent - et l3 - le réplica partiel mis à jour) contenant leurs noeuds respectifs (la liste nous semble plus simple à manipuler)
	- On réalise le parcours de ces liste simultanément (par appels récursifs de la fonction threeWayMerge_) jusqu'à ce qu'elles soient toutes vides; au fur et à mesure on construit l'artefact resultant (la liste préfixée l4 de ce dernier) en respectant le principe suivant :
	  Si les noeuds n1 du couple c1=(n1, ad1) de l1 actuellement visité, n2 du couple c2=(n2, ad2) de l2 actuellement visité et n3 du couple c3=(n3, ad3) de l3 actuellement visité sont tel que:
		* n3 est associé à un symbole de (re)structuration alors, on ignore le couple c3 et on reprend le traitement avec l1, l2 et la liste résiduelle de l3
		
		* n1, n2 et n3 existent et sont tous associés au même symbole alors, on insère le couple (n3, ad1) (n3 est le noeud le plus à jour) dans la liste l4 en cours de construction.
		  si n3 est un bourgeon alors, on supprime de l1 tous les noeuds du sous-artefact dont la racine a pour adresse ad1 (la fonction removeSubtreeNodes est utilisée à cet effet)
		  on reprend le même traitement avec les listes résiduelles de l1, l2 et l3
		
		* n1, n2 et n3 existent et le symbole associé à n1 ne correspond à aucun des symboles associés à n2 et n3 alors, on insère le couple (Upstair n1, ad1) (il s'agit d'un bourgeon en dessus) dans la liste l4 en cours de construction.
		  on reprend le même traitement avec la liste résiduelle de l1, la liste l2 et la liste l3
		
		* n1, n2 existent et sont associés au même symbole alors, on insère le couple (n2, ad1) dans la liste l4 en cours de construction.
		  si n2 est un bourgeon alors, on supprime de l1 tous les noeuds du sous-artefact dont la racine a pour adresse ad1 (la fonction removeSubtreeNodes est utilisée à cet effet)
		  on reprend le même traitement avec les listes résiduelles de l1 et l2 et la liste l3
		
		* n1, n3 existent et sont associés au même symbole alors, on insère le couple (n3, ad1) dans la liste l4 en cours de construction.
		  si n3 est un bourgeon alors, on supprime de l1 tous les noeuds du sous-artefact dont la racine a pour adresse ad1 (la fonction removeSubtreeNodes est utilisée à cet effet)
		  on reprend le même traitement avec les listes résiduelles de l1 et l3 et la liste l2 (dans le bon ordre bien sûr)
	- On construit l'artefact recherché à partir de la liste l4 obtenue (la fonction fromPrefixList est utilisée à cet effet)
-}
threeWayMerge:: (Eq a) => Artifact a -> Artifact (NodeTag a) -> Artifact (NodeTag (SymbolTag a)) -> Artifact (NodeTag a)
threeWayMerge guide currentArt partialRepUptd = fromPrefixList taggedNodeList
												where
													taggedNodeList = threeWayMerge_ itGuide itCurrentArt itPartialRepUptd []
													itGuide = toPrefixList guide
													itCurrentArt = toPrefixList currentArt
													itPartialRepUptd = toPrefixList partialRepUptd
													threeWayMerge_ itGuide itCurrentArt itPartialRepUptd acc = case (itGuide, itCurrentArt, itPartialRepUptd) of
																													(((taggedGuideNode, guideNodeAddr):guideNodes), ((taggedCurArtNode, curArtNodeAddr):curArtNodes), []) -> case taggedGuideNode of
																																																								(Seq val) -> case untagProduction taggedCurArtNode of
																																																												Closed val2 -> threeWayMerge_ guideNodes curArtNodes [] (acc ++ [(Seq (Closed val), guideNodeAddr)])
																																																												Locked val2 -> threeWayMerge_ prunedGuideNodes curArtNodes [] (acc ++ [(Seq (Locked val), guideNodeAddr)])
																																																												Unlocked val2 -> threeWayMerge_ prunedGuideNodes curArtNodes [] (acc ++ [(Seq (Unlocked val), guideNodeAddr)])
																																																								(Par val) -> case untagProduction taggedCurArtNode of
																																																												Closed val2 -> threeWayMerge_ guideNodes curArtNodes [] (acc ++ [(Par (Closed val), guideNodeAddr)])
																																																												Locked val2 -> threeWayMerge_ prunedGuideNodes curArtNodes [] (acc ++ [(Par (Locked val), guideNodeAddr)])
																																																												Unlocked val2 -> threeWayMerge_ prunedGuideNodes curArtNodes [] (acc ++ [(Par (Unlocked val), guideNodeAddr)])
																																																							 where
																																																								prunedGuideNodes = removeSubtreeNodes guideNodeAddr guideNodes
																													
																													(((taggedGuideNode, guideNodeAddr):guideNodes), [], ((taggedPRepUptdNode, pRepUptdNodeAddr):pRepUptdNodes)) -> case untagNode (untagProduction taggedPRepUptdNode) of
																																																										(Standard val2) -> case taggedGuideNode of
																																																																(Seq val) -> case (val == val2, untagProduction taggedPRepUptdNode) of
																																																																				(True, Closed taggedVal2) -> threeWayMerge_ guideNodes [] pRepUptdNodes (acc ++ [(Seq (Closed val), guideNodeAddr)])
																																																																				(True, Locked taggedVal2) -> threeWayMerge_ prunedGuideNodes [] pRepUptdNodes (acc ++ [(Seq (Locked val), guideNodeAddr)])
																																																																				(True, Unlocked taggedVal2) -> threeWayMerge_ prunedGuideNodes [] pRepUptdNodes (acc ++ [(Seq (Unlocked val), guideNodeAddr)])
																																																																				otherwise -> threeWayMerge_ guideNodes [] ((taggedPRepUptdNode, pRepUptdNodeAddr):pRepUptdNodes) (acc ++ [(Seq (Upstair val), guideNodeAddr)])
																																																																(Par val) -> case (val == val2, untagProduction taggedPRepUptdNode) of
																																																																				(True, Closed taggedVal2) -> threeWayMerge_ guideNodes [] pRepUptdNodes (acc ++ [(Par (Closed val), guideNodeAddr)])
																																																																				(True, Locked taggedVal2) -> threeWayMerge_ prunedGuideNodes [] pRepUptdNodes (acc ++ [(Par (Locked val), guideNodeAddr)])
																																																																				(True, Unlocked taggedVal2) -> threeWayMerge_ prunedGuideNodes [] pRepUptdNodes (acc ++ [(Par (Unlocked val), guideNodeAddr)])
																																																																				otherwise -> threeWayMerge_ guideNodes [] ((taggedPRepUptdNode, pRepUptdNodeAddr):pRepUptdNodes) (acc ++ [(Par (Upstair val), guideNodeAddr)])
																																																										(Structural val2) -> threeWayMerge_ ((taggedGuideNode, guideNodeAddr):guideNodes) [] pRepUptdNodes acc
																																																								   where
																																																										prunedGuideNodes = removeSubtreeNodes guideNodeAddr guideNodes
																													
																													(((taggedGuideNode, guideNodeAddr):guideNodes), ((taggedCurArtNode, curArtNodeAddr):curArtNodes), ((taggedPRepUptdNode, pRepUptdNodeAddr):pRepUptdNodes)) -> case untagNode (untagProduction taggedPRepUptdNode) of
																																																																					(Standard val2) -> case taggedGuideNode of
																																																																											(Seq val) -> case (val == (untagNode (untagProduction taggedCurArtNode)), val == (untagSymbol (untagNode (untagProduction taggedPRepUptdNode)))) of
																																																																															(True, True) -> case untagProduction taggedPRepUptdNode of
																																																																																				Closed taggedVal2 -> threeWayMerge_ guideNodes curArtNodes pRepUptdNodes (acc ++ [(Seq (Closed val), guideNodeAddr)])
																																																																																				Locked taggedVal2 -> threeWayMerge_ prunedGuideNodes curArtNodes pRepUptdNodes (acc ++ [(Seq (Locked val), guideNodeAddr)])
																																																																																				Unlocked taggedVal2 -> threeWayMerge_ prunedGuideNodes curArtNodes pRepUptdNodes (acc ++ [(Seq (Unlocked val), guideNodeAddr)])
																																																																															(True, False) -> case untagProduction taggedCurArtNode of
																																																																																				Closed val2 -> threeWayMerge_ guideNodes curArtNodes ((taggedPRepUptdNode, pRepUptdNodeAddr):pRepUptdNodes) (acc ++ [(Seq (Closed val), guideNodeAddr)])
																																																																																				Locked val2 -> threeWayMerge_ prunedGuideNodes curArtNodes ((taggedPRepUptdNode, pRepUptdNodeAddr):pRepUptdNodes) (acc ++ [(Seq (Locked val), guideNodeAddr)])
																																																																																				Unlocked val2 -> threeWayMerge_ prunedGuideNodes curArtNodes ((taggedPRepUptdNode, pRepUptdNodeAddr):pRepUptdNodes) (acc ++ [(Seq (Unlocked val), guideNodeAddr)])
																																																																															(False, True) -> case untagProduction taggedPRepUptdNode of
																																																																																				Closed taggedVal2 -> threeWayMerge_ guideNodes ((taggedCurArtNode, curArtNodeAddr):curArtNodes) pRepUptdNodes (acc ++ [(Seq (Closed val), guideNodeAddr)])
																																																																																				Locked taggedVal2 -> threeWayMerge_ prunedGuideNodes ((taggedCurArtNode, curArtNodeAddr):curArtNodes) pRepUptdNodes (acc ++ [(Seq (Locked val), guideNodeAddr)])
																																																																																				Unlocked taggedVal2 -> threeWayMerge_ prunedGuideNodes ((taggedCurArtNode, curArtNodeAddr):curArtNodes) pRepUptdNodes (acc ++ [(Seq (Unlocked val), guideNodeAddr)])
																																																																															otherwise -> threeWayMerge_ guideNodes ((taggedCurArtNode, curArtNodeAddr):curArtNodes) ((taggedPRepUptdNode, pRepUptdNodeAddr):pRepUptdNodes) (acc ++ [(Seq (Upstair val), guideNodeAddr)])
																																																																											(Par val) -> case (val == (untagNode (untagProduction taggedCurArtNode)), val == (untagSymbol (untagNode (untagProduction taggedPRepUptdNode)))) of
																																																																															(True, True) -> case untagProduction taggedPRepUptdNode of
																																																																																				Closed taggedVal2 -> threeWayMerge_ guideNodes curArtNodes pRepUptdNodes (acc ++ [(Par (Closed val), guideNodeAddr)])
																																																																																				Locked taggedVal2 -> threeWayMerge_ prunedGuideNodes curArtNodes pRepUptdNodes (acc ++ [(Par (Locked val), guideNodeAddr)])
																																																																																				Unlocked taggedVal2 -> threeWayMerge_ prunedGuideNodes curArtNodes pRepUptdNodes (acc ++ [(Par (Unlocked val), guideNodeAddr)])
																																																																															(True, False) -> case untagProduction taggedCurArtNode of
																																																																																				Closed val2 -> threeWayMerge_ guideNodes curArtNodes ((taggedPRepUptdNode, pRepUptdNodeAddr):pRepUptdNodes) (acc ++ [(Par (Closed val), guideNodeAddr)])
																																																																																				Locked val2 -> threeWayMerge_ prunedGuideNodes curArtNodes ((taggedPRepUptdNode, pRepUptdNodeAddr):pRepUptdNodes) (acc ++ [(Par (Locked val), guideNodeAddr)])
																																																																																				Unlocked val2 -> threeWayMerge_ prunedGuideNodes curArtNodes ((taggedPRepUptdNode, pRepUptdNodeAddr):pRepUptdNodes) (acc ++ [(Par (Unlocked val), guideNodeAddr)])
																																																																															(False, True) -> case untagProduction taggedPRepUptdNode of
																																																																																				Closed taggedVal2 -> threeWayMerge_ guideNodes ((taggedCurArtNode, curArtNodeAddr):curArtNodes) pRepUptdNodes (acc ++ [(Par (Closed val), guideNodeAddr)])
																																																																																				Locked taggedVal2 -> threeWayMerge_ prunedGuideNodes ((taggedCurArtNode, curArtNodeAddr):curArtNodes) pRepUptdNodes (acc ++ [(Par (Locked val), guideNodeAddr)])
																																																																																				Unlocked taggedVal2 -> threeWayMerge_ prunedGuideNodes ((taggedCurArtNode, curArtNodeAddr):curArtNodes) pRepUptdNodes (acc ++ [(Par (Unlocked val), guideNodeAddr)])
																																																																															otherwise -> threeWayMerge_ guideNodes ((taggedCurArtNode, curArtNodeAddr):curArtNodes) ((taggedPRepUptdNode, pRepUptdNodeAddr):pRepUptdNodes) (acc ++ [(Par (Upstair val), guideNodeAddr)])
																																																																					(Structural val2) -> threeWayMerge_ ((taggedGuideNode, guideNodeAddr):guideNodes) ((taggedCurArtNode, curArtNodeAddr):curArtNodes) pRepUptdNodes acc
																																																																				 where
																																																																					prunedGuideNodes = removeSubtreeNodes guideNodeAddr guideNodes
																													
																													otherwise -> acc


{-
 Fonction qui réalise l'expansion d'une réplique partielle donnée
 Pour le principe ici, on va juste se servir des fonction précédentes pour obtenir un artefact devant guider la fusion three-way, ensuite on juste réaliser la fusion three-way de nos trois artefacts
-}
expand:: (Eq a) => GMWf a -> View a -> a -> Artifact (NodeTag a) -> Artifact (NodeTag (SymbolTag a)) -> Artifact (NodeTag a)
expand globalmgwf view axiom currentArt partialRepUptd = expansion
														 where
															expansion = threeWayMerge guide currentArt partialRepUptd
															guide = head (getExpansionGuides globalmgwf view axiom currentArt partialRepUptd)
															

{-
 Fonction qui réalise l'élagage d'un artefact contenant potentiellement des bourgeons en dessus
 Après l'expansion, tous les bourgeons en dessus sont taggés par "Upstair". La fonction suivant va parcourir l'artefact et élaguer à chaque fois (dans chaque branche) au niveau du premier bourgeon en dessus rencontré.
 Les bourgeons obtenus après élagage apparaissent vérouillés
-}
prune:: Artifact (NodeTag a) -> Artifact (NodeTag a)
prune (Node taggedNode (Seq sons)) = case taggedNode of
										Upstair val -> Node (Locked val) (Seq [])
										otherwise -> Node taggedNode (Seq (map prune sons))
prune (Node taggedNode (Par sons)) = case taggedNode of
										Upstair val -> Node (Locked val) (Par [])
										otherwise -> Node taggedNode (Par (map prune sons))

{-
 Fonction qui réalise l'expansion-élagage d'une réplique partielle donnée
 Ici on élague immédiatement après l'expansion et on retourne le résultat. Les bourgeons obtenus après élagage apparaissent vérouillés
-}
expandPrune:: (Eq a) => GMWf a -> View a -> a -> Artifact (NodeTag a) -> Artifact (NodeTag (SymbolTag a)) -> Artifact (NodeTag a)
expandPrune globalmgwf view axiom currentArt partialRepUptd = prune expansion
															  where
																expansion = expand globalmgwf view axiom currentArt partialRepUptd


{-
 Fonction qui déverrouille les bourgeons prêts à être complétés
 Après l'expansion élagage, tous les bourgeons de l'artefact sont vérouillés. La fonction ci-après va parcourir l'artefact dans le but 
 de dévérouiller tous les bourgeons associés à des tâches prêtes à être exécutées.
-}
unlockReadyBuds:: Artifact (NodeTag a) -> Artifact (NodeTag a)
unlockReadyBuds t@(Node taggedNode (Seq sons)) = case sons of
													[] -> case taggedNode of
															Closed val -> t
															otherwise -> Node (Unlocked (untagNode taggedNode)) (Seq [])
													otherwise -> Node taggedNode (Seq ((unlockReadyBuds (head sons)) : tail sons))
unlockReadyBuds t@(Node taggedNode (Par sons)) = case sons of
													[] -> case taggedNode of
															Closed val -> t
															otherwise -> Node (Unlocked (untagNode taggedNode)) (Seq [])
													otherwise -> Node taggedNode (Par (map unlockReadyBuds sons))


{-
 Test de l'expansion-élagage d'une réplique partielle donnée (site de l'éditeur associé) > expandPrune explGMWf viewAE "Ag" curArt2 parRepUptd3

							Ag													Ag
							|													|
							|													|
	curArt2	-------->		A					parRepUptd3		-------->   	A
						   / \													|
						  /   \ 												|
					   Cwu  ;  Dwl												C
																			   / \
																			  /   \
																			 E  ;  Fwl
																			/ \
																		   /   \
																		 S1 ||  S2
																		/ \	    / \
																	   /   \   /   \
																	  /    |   |    \
																  H1wl ; I1wl H2wl ; I2wl
-}
curArt2 = Node (Closed "Ag") (
			   Seq [
				Node (Closed "A") (
				   Seq [
					Node (Unlocked "C") (Seq []), 
					Node (Locked "D") (Seq [])
				   ])
			   ])
parRepUptd3 = Node (Closed (Standard "Ag")) (
				   Seq [
					Node (Closed (Standard "A")) (
					   Seq [
						Node (Closed (Standard "C")) (
						   Seq [
							Node (Closed (Standard "E")) (
							   Par [
								Node (Closed (Structural "G1")) (
								   Seq [
									Node (Locked (Standard "H1")) (Seq []), 
									Node (Locked (Standard "I1")) (Seq [])
								   ]), 
								Node (Closed (Structural "G2")) (
								   Seq [
									Node (Locked (Standard "H2")) (Seq []), 
									Node (Locked (Standard "I2")) (Seq [])
								   ])
							   ]), 
							Node (Locked (Standard "F")) (Seq [])
						   ])
					   ])
				   ])

