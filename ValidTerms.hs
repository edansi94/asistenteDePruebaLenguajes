-- TO DO:
--      + Impresión Equation.
--      + Revisar impresiones.
--      + Agregar el operador infijo ===.
--      + Definir si es infijo o no.
--      + Revisar que falta.
-- *****************************************************************************

-- Universidad Simón Bolívar.
-- Departamento de Computación y Tecnología de la Información.
-- Laboratorio de Lenguajes de Programación.
-- Abril - Julio 2016.
-- Alumnos:
-- 			Cinthya Ramos.    09-11237.
-- 			Edward Fernández. 10-11121.

-- Descripción:
-- 		Archivo en el que se definirán los diferentes términos válidos que
--  	forman parte del lenguaje de proposiciones lógicas.

-- *****************************************************************************

-- Se define el módulo que permitirá importar los términos definidos en el archivo
-- "Functions.hs".
module ValidTerms
(
	Term,
	showTerm,
	a, b, c, d, e, f, g, h, i,
	j, k, l, m, n, o, p, q, r,
	s, t, u, v, w, x, y, z,
	neg, (\/), (/\), (==>), 
	(<==>), (!<==>),
) where

-- .............................................................................

import Data.Char (toLower) -- Se importa la librería que transforma todas las 
						   -- letras a minúsculas.

-- Definición de los términos válidos del lenguaje de proposiciones lógicas.
data Term = Cons Bool                 -- Términos booleanos.  
		  | Var Char          		  -- Termino unitario.
		  | Neg	Term    	  		  -- Negacion de términos.
		  | Or  Term Term             -- Disjunción de términos.
		  | And Term Term     		  -- Conjunción de términos.
		  | Imp Term Term     		  -- Implicación de términos.
		  | SiYSi Term Term   		  -- Si y sólo sí de términos.
		  | NoSiYSi Term Term 		  -- Negación de si y sólo sí de términos.
		  | Equ Term Term  	  		  -- Equivalencía de términos

-- Definición del tipo Equation
data Equation = Ecu Term Term

-- .............................................................................

-- Definición de las variables que se utilizán para representar la lógica 
-- proposicional.
-- 		Las variables van desde la letra 'a' hasta la 'z'
a :: Term 		
a = Var 'a'

b :: Term
b = Var 'b'

c :: Term
c = Var 'c'

d :: Term
d = Var 'd'

e :: Term
e = Var 'e'

f :: Term
f = Var 'f'

g :: Term
g = Var 'g'

h :: Term
h = Var 'h'

i :: Term
i = Var 'i'

j :: Term
j = Var 'j'

k :: Term
k = Var 'k'

l :: Term
l = Var 'l'

m :: Term
m = Var 'm'

n :: Term
n = Var 'n'

o :: Term
o = Var 'o'

p :: Term
p = Var 'p'

q :: Term
q = Var 'q'

r :: Term
r = Var 'r'

s :: Term
s = Var 's'

t :: Term
t = Var 't'

u :: Term
u = Var 'u'

v :: Term
v = Var 'v'

w :: Term
w = Var 'w'

x :: Term
x = Var 'x'

y :: Term
y = Var 'y'

z :: Term
z = Var 'z'

true :: Term
true = Cons True

false :: Term
false = Cons False

-- .............................................................................

-- Definición del conjunto de operadores infijos a utilizar en los enunciados de
-- la lógica proposicional.

-- Negación.
neg	:: Term -> Term
neg (Var  i) = Neg (Var  i)
neg (Cons i) = Neg (Cons i)
neg (Or  term1 term2) = Neg (Or  term1 term2)
neg (And term1 term2) = Neg (And term1 term2)
neg (Imp term1 term2) = Neg (Imp term1 term2)
neg (SiYSi   term1 term2) = Neg (SiYSi   term1 term2)
neg (NoSiYSi term1 term2) = Neg (NoSiYSi term1 term2)
infixl 4 `neg`

-- Disjunción
(\/)  ::  Term -> Term -> Term
(\/)  term1 term2 = Or term1 term2
infixl 3 \/


-- Conjunción.
(/\)  ::  Term -> Term -> Term
(/\)  term1 term2 = And term1 term2
infixl 3 /\

-- Implicación.
(==>) :: Term -> Term -> Term
(==>) term1 term2 = Imp term1 term2
infixr 2 ==>

-- Si y sólo sí.
(<==>) :: Term -> Term -> Term
(<==>) term1 term2 = SiYSi term1 term2
infixl 1 <==>

-- Negación de si y sólo sí.
(!<==>) :: Term -> Term -> Term
(!<==>) term1 term2 = NoSiYSi term1 term2
infixl 1 !<==>

-- Equivalencia.
(===)  :: Term -> Term -> Equation
(===)  term1 term2 = Ecu term1 term2
infixl 0 ===

-- .............................................................................

-- Descripción:
--		Permite darle estilo a la impresión de los diferentes términos del lenguaje.
showTerm :: Term -> String
																								          -- Caso a: término unitario.
showTerm (Var  i) =  i:[]																		          --     a.1: Caracter.
showTerm (Cons i) =  map toLower (show i)          									    	              --	   a.2: Booleano  

showTerm (Neg (Var  i)) = "¬" ++ i:[]
showTerm (Neg (Cons i)) = "¬" ++ map toLower (show i)
showTerm (Neg term) = "¬(" ++ showTerm (term) ++ ")" 
																								          -- Caso b: Disjunción.
showTerm (Or (Var  i) (Var  j)) = showTerm (Var i) ++ " \\/ " ++ showTerm (Var j)                         --      b.1: Un término unitario o un término unitario.       
showTerm (Or (Var  i)   term2 ) = showTerm (Var i) ++ " \\/ (" ++ showTerm (term2) ++ ")" 		          --      b.2: Término unitario o termino compuesto.
showTerm (Or   term1  (Var  j)) = "(" ++ showTerm (term1) ++ ") \\/ " ++ showTerm (Var j)                 --      b.3: Término compuesto o término unitario.
showTerm (Or (Cons x) (Cons y)) = showTerm (Cons x) ++ " \\/ " ++ showTerm (Cons y)				          --      b.4: Un término booleano o un término booleano.
showTerm (Or   term1  (Cons y)) = "(" ++ showTerm (term1) ++ ") \\/ " ++ showTerm (Cons y)                --      b.5: Un término compuesto o un booleano.
showTerm (Or (Cons x)   term2 ) = showTerm (Cons x) ++ " \\/ (" ++ showTerm (term2) ++ ")"                --      b.6: Un booleano o un termino compuesto.
showTerm (Or   term1    term2 ) = "(" ++ showTerm (term1) ++ ") \\/ (" ++ showTerm (term2) ++ ")"         --      b.7: Un término compuesto o un término compuesto.
																								          -- Caso c: Conjunción.
showTerm (And (Var  i) (Var  j)) = showTerm (Var i) ++ " /\\ " ++ showTerm (Var j)                        --      c.1: Un término unitario y un término unitario.         
showTerm (And (Var  i)   term2 ) = showTerm (Var i) ++ " /\\ (" ++ showTerm (term2) ++ ")"                --      c.2: Un término unitario y un término compuesto.
showTerm (And   term1  (Var  j)) = "(" ++ showTerm (term1) ++ ") /\\  " ++ showTerm (Var j)               --      c.3: Un término compuesto y un término unitario.
showTerm (And (Cons x) (Cons y)) = showTerm (Cons x) ++ " /\\ " ++ showTerm (Cons y)			          --      c.4: Un término booleano y un término booleano.
showTerm (And   term1  (Cons y)) = "(" ++ showTerm (term1) ++ ") /\\ " ++ showTerm (Cons y)               --      c.5: Un término compuesto y un booleano.
showTerm (And (Cons x)   term2 ) = showTerm (Cons x) ++ " /\\ (" ++ showTerm (term2) ++ ")"               --      c.6: Un booleano y un termino compuesto.
showTerm (And   term1    term2 ) = "(" ++ showTerm (term1) ++ ") /\\  (" ++ showTerm (term2) ++ ")"       --      c.7: Un término compuesto y un término compuesto. 
																									      -- Caso d: Implicación.
showTerm (Imp (Var  i) (Var  j)) = showTerm (Var i) ++ " ==> " ++ showTerm (Var j)                        --      d.1: Un término unitario implica un término unitario.
showTerm (Imp (Var  i)   term2 ) = showTerm (Var i) ++ " ==> (" ++ showTerm (term2) ++ ")"                --      d.2: Un término unitario implica un término compuesto.
showTerm (Imp   term1  (Var  j)) = "( " ++ showTerm (term1) ++ " ) ==> " ++ showTerm (Var j)              --      d.3: Un término compuesto implica un término unitario.
showTerm (Imp (Cons x) (Cons y)) = showTerm (Cons x) ++ " ==> " ++ showTerm (Cons y)			          --      d.4: Un término booleano implica un término booleano.
showTerm (Imp   term1  (Cons y)) = "(" ++ showTerm (term1) ++ ") ==> " ++ showTerm (Cons y)               --      d.5: Un término compuesto implica un booleano.
showTerm (Imp (Cons x)   term2 ) = showTerm (Cons x) ++ " ==> (" ++ showTerm (term2) ++ ")"               --      d.6: Un booleano implica un termino compuesto.
showTerm (Imp   term1    term2 ) = "(" ++ showTerm (term1) ++ ") ==>  (" ++ showTerm (term2) ++ ")"       --      d.7: Un término compuesto implica un término compuesto. 
																									      -- Caso e: Si y sólo sí.
showTerm (SiYSi (Var  i) (Var  j)) = showTerm (Var i) ++ " <==> " ++ showTerm (Var j)                     --      e.1: Un término unitario si y sólo sí un término unitario.
showTerm (SiYSi (Var  i)   term2 ) = showTerm (Var i) ++ " <==> (" ++ showTerm (term2) ++ ")"             --      e.2: Un término unitario si y sólo sí un término compuesto.
showTerm (SiYSi   term1  (Var  j)) = "( " ++ showTerm (term1) ++ " ) <==> " ++ showTerm (Var j)           --      e.3: Un término compuesto si y sólo sí un término unitario.
showTerm (SiYSi (Cons x) (Cons y)) = showTerm (Cons x) ++ " <==> " ++ showTerm (Cons y)			          --      e.4: Un término booleano si y sólo sí un término booleano.
showTerm (SiYSi   term1  (Cons y)) = "(" ++ showTerm (term1) ++ ") <==> " ++ showTerm (Cons y)            --      e.5: Un término compuesto si y sólo sí un booleano.
showTerm (SiYSi (Cons x)   term2 ) = showTerm (Cons x) ++ " <==> (" ++ showTerm (term2) ++ ")"            --      e.6: Un booleano si y sólo sí un termino compuesto.
showTerm (SiYSi   term1    term2 ) = "(" ++ showTerm (term1) ++ ") <==>  (" ++ showTerm (term2) ++ ")"    --      e.7: Un término compuesto si y sólo sí término compuesto. 
																									      -- Caso f: No si y sólo sí.
showTerm (NoSiYSi (Var  i) (Var  j)) = showTerm (Var i) ++ " !<==> " ++ showTerm (Var j)                  --      f.1: Un término unitario no si y sólo sí un término unitario.
showTerm (NoSiYSi (Var  i)   term2 ) = showTerm (Var i) ++ " !<==> (" ++ showTerm (term2) ++ ")"          --      f.2: Un término unitario no si y sólo sí un término compuesto.
showTerm (NoSiYSi   term1  (Var  j)) = "( " ++ showTerm (term1) ++ " ) !<==> " ++ showTerm (Var j)        --      f.3: Un término compuesto no si y sólo sí un término unitario.
showTerm (NoSiYSi (Cons x) (Cons y)) = showTerm (Cons x) ++ " !<==> " ++ showTerm (Cons y)			      --      f.4: Un término booleano no si y sólo sí un término booleano.
showTerm (NoSiYSi   term1  (Cons y)) = "(" ++ showTerm (term1) ++ ") !<==> " ++ showTerm (Cons y)         --      f.5: Un término compuesto no si y sólo sí un booleano.
showTerm (NoSiYSi (Cons x)   term2 ) = showTerm (Cons x) ++ " !<==> (" ++ showTerm (term2) ++ ")"         --      f.6: Un booleano no si y sólo sí un termino compuesto.
showTerm (NoSiYSi   term1    term2 ) = "(" ++ showTerm (term1) ++ ") !<==>  (" ++ showTerm (term2) ++ ")" --      f.7: Un término compuesto no si y sólo sí un término compuesto. 
																									      -- Caso g: Equivalencia.
showTerm (Equ (Var  i) (Var  j)) = showTerm (Var i) ++ " === " ++ showTerm (Var j)                        --      g.1: Un término unitario equivale un término unitario.
showTerm (Equ (Var  i)   term2 ) = showTerm (Var i) ++ " === (" ++ showTerm (term2) ++ ")"                --      g.2: Un término unitario equivale un término compuesto.
showTerm (Equ   term1  (Var  j)) = "( " ++ showTerm (term1) ++ " ) === " ++ showTerm (Var j)              --      g.3: Un término compuesto equivale un término unitario.
showTerm (Equ (Cons x) (Cons y)) = showTerm (Cons x) ++ " === " ++ showTerm (Cons y)			          --      g.4: Un término booleano equivale un término booleano.
showTerm (Equ   term1  (Cons y)) = "(" ++ showTerm (term1) ++ ") === " ++ showTerm (Cons y)               --      g.5: Un término compuesto equivale un booleano.
showTerm (Equ (Cons x)   term2 ) = showTerm (Cons x) ++ " === (" ++ showTerm (term2) ++ ")"               --      g.6: Un booleano equivale un termino compues      to.
showTerm (Equ   term1    term2 ) = "(" ++ showTerm (term1) ++ ") ===  (" ++ showTerm (term2) ++ ")"       --      g.7: Un término compuesto equivale un término compuesto. 

instance Show Term where show = showTerm

showEquation :: Equation -> String
showEquation (Ecu (Var  i) (Var  j)) = showEquation (Var i) ++ " <==> " ++ showEquation (Var j)  
showEquation (Ecu (Var  i) term2) = showEquation (Var i) ++ " === " ++ showEquation (Var j)
showEquation (Ecu   term1  (Var  j)) = "( " ++ showEquation (term1) ++ " ) <==> " ++ showEquation (Var j)

instance Show Equation where show = showEquation

