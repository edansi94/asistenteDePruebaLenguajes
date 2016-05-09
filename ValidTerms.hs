-- TO DO:
-- 		+ Agregar casos de impresión de la implicación.
-- 		+ Agregar casos de impresión de la equivalencia.
-- 		+ Agregar casos de impresión de la inequivalencia.
--      + Agregar el operador infijo ===.
-- 		+ Agregar precedencia.
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
	true, false,
	(\/), (/\), (==>), (<==>), 
	(!<==>), neg
) where

-- .............................................................................

import Data.Char (toLower) -- Se importa la librería que transforma todas las 
						   -- letras a minúsculas.

-- Definición de los términos válidos del lenguaje de proposiciones lógicas.
data Term = Var Char         -- Termino unitario.er.
		  | Or  Term Term    -- Disjunción de términos.
		  | And Term Term    -- Conjunción de términos.
		  | Imp Term Term    -- Implicación de términos.
		  | Equ Term Term    -- Equivalencia de términos.
		  | InEqu Term Term  -- Inequivalencia de términos.
		  | Neg	Term    	 -- Negacion de términos.
		  | Bool Bool		 -- Términos booleanos.


-- Descripción:
--		Permite darle estilo a la impresión de los diferentes términos del lenguaje.
showTerm :: Term -> String
																								    -- Caso a: término unitario.
showTerm (Var  i) =  i:[]																		    --      a.1: Caracter.
showTerm (Bool i) =  map toLower (show i)          									    	        --	   a.2: Booleano  
																								    -- Caso b: Disjunción.
showTerm (Or (Var  i) (Var  j)) = showTerm (Var i) ++ " \\/ " ++ showTerm (Var j)                   --      b.1: Un término unitario o un término unitario.       
showTerm (Or (Var  i)   term2 ) = showTerm (Var i) ++ " \\/ (" ++ showTerm (term2) ++ ")" 		    --      b.2: Término unitario o termino compuesto.
showTerm (Or   term1  (Var  j)) = "(" ++ showTerm (term1) ++ ") \\/ " ++ showTerm (Var j)           --      b.3: Término compuesto o término unitario.
showTerm (Or (Bool x) (Bool y)) = showTerm (Bool x) ++ " \\/ " ++ showTerm (Bool y)				    --      b.4: Un término booleano o un término booleano.
showTerm (Or   term1  (Bool y)) = "(" ++ showTerm (term1) ++ ") \\/ " ++ showTerm (Bool y)          --      b.5: Un término compuesto o un booleano.
showTerm (Or (Bool x)   term2 ) = showTerm (Bool x) ++ " \\/ (" ++ showTerm (term2) ++ ")"          --      b.6: Un booleano o un termino compuesto.
showTerm (Or   term1    term2 ) = "(" ++ showTerm (term1) ++ ") \\/ (" ++ showTerm (term2) ++ ")"   --      b.7: Un término compuesto o un término compuesto.
																								    -- Caso c: Conjunción.
showTerm (And (Var  i) (Var  j)) = showTerm (Var i) ++ " /\\ " ++ showTerm (Var j)                  --      c.1: Un término unitario y un término unitario.         
showTerm (And (Var  i)   term2 ) = showTerm (Var i) ++ " /\\ (" ++ showTerm (term2) ++ ")"          --      c.2: Un término unitario y un término compuesto.
showTerm (And   term1  (Var  j)) = "(" ++ showTerm (term1) ++ ") /\\  " ++ showTerm (Var j)         --      c.3: Un término compuesto y un término unitario.
showTerm (And (Bool x) (Bool y)) = showTerm (Bool x) ++ " /\\ " ++ showTerm (Bool y)			    --      c.4: Un término booleano y un término booleano.
showTerm (And   term1  (Bool y)) = "(" ++ showTerm (term1) ++ ") /\\ " ++ showTerm (Bool y)         --      c.5: Un término compuesto y un booleano.
showTerm (And (Bool x)   term2 ) = showTerm (Bool x) ++ " /\\ (" ++ showTerm (term2) ++ ")"         --      c.6: Un booleano y un termino compuesto.
showTerm (And   term1    term2 ) = "(" ++ showTerm (term1) ++ ") /\\  (" ++ showTerm (term2) ++ ")" --      c.7: Un término compuesto y un término compuesto. 
																									-- Caso d: Implicación.
showTerm (Imp (Var  i) (Var  j)) = showTerm (Var i) ++ " ==> " ++ showTerm (Var j)                  --      d.1: Un término unitario implica un término unitario.
showTerm (Imp (Var  i)   term2 ) = showTerm (Var i) ++ " ==> (" ++ showTerm (term2) ++ ")"          --      d.2: Un término unitario implica un término compuesto.
showTerm (Imp   term1  (Var  j)) = "( " ++ showTerm (term1) ++ " ) ==> " ++ showTerm (Var j)        --      d.3: Un término compuesto implica un término unitario.
showTerm (Imp (Bool x) (Bool y)) = showTerm (Bool x) ++ " ==> " ++ showTerm (Bool y)			    --      d.4: Un término booleano implica un término booleano.
showTerm (Imp   term1  (Bool y)) = "(" ++ showTerm (term1) ++ ") ==> " ++ showTerm (Bool y)         --      d.5: Un término compuesto implica un booleano.
showTerm (Imp (Bool x)   term2 ) = showTerm (Bool x) ++ " ==> (" ++ showTerm (term2) ++ ")"         --      d.6: Un booleano implica un termino compuesto.
showTerm (Imp   term1    term2 ) = "(" ++ showTerm (term1) ++ ") ==>  (" ++ showTerm (term2) ++ ")" --      d.7: Un término compuesto implica un término compuesto. 
																									-- Caso e: Equivalencia.
showTerm (Equ (Var  i) (Var  j)) = showTerm (Var i) ++ " <==> " ++ showTerm (Var j)                 --      e.1: Un término unitario equivale un término unitario.
showTerm (Equ (Var  i)   term2 ) = showTerm (Var i) ++ " <==> (" ++ showTerm (term2) ++ ")"         --      e.2: Un término unitario equivale un término compuesto.
showTerm (Equ   term1  (Var  j)) = "( " ++ showTerm (term1) ++ " ) <==> " ++ showTerm (Var j)       --      e.3: Un término compuesto equivale un término unitario.
showTerm (Equ (Bool x) (Bool y)) = showTerm (Bool x) ++ " <==> " ++ showTerm (Bool y)			    --      e.4: Un término booleano equivale un término booleano.
showTerm (Equ   term1  (Bool y)) = "(" ++ showTerm (term1) ++ ") <==> " ++ showTerm (Bool y)        --      e.5: Un término compuesto equivale un booleano.
showTerm (Equ (Bool x)   term2 ) = showTerm (Bool x) ++ " <==> (" ++ showTerm (term2) ++ ")"        --      e.6: Un booleano equivale un termino compuesto.
showTerm (Equ   term1    term2 ) = "(" ++ showTerm (term1) ++ ") <==>  (" ++ showTerm (term2) ++ ")"--      e.7: Un término compuesto equivale un término compuesto. 
																									-- Caso f: Inequivalencia.
showTerm (InEqu (Var  i) (Var  j)) = showTerm (Var i) ++ " !<==> " ++ showTerm (Var j)                  --  f.1: Un término unitario no equivale a un término unitario.
showTerm (InEqu (Var  i)   term2 ) = showTerm (Var i) ++ " !<==> (" ++ showTerm (term2) ++ ")"          --  f.2: Un término unitario no equivale a un término compuesto.
showTerm (InEqu   term1  (Var  j)) = "( " ++ showTerm (term1) ++ " ) !<==> " ++ showTerm (Var j)        --  f.3: Un término compuesto no equivale a un término unitario.
showTerm (InEqu (Bool x) (Bool y)) = showTerm (Bool x) ++ " !<==> " ++ showTerm (Bool y)			    --  f.4: Un término booleano no equivale a un término booleano.
showTerm (InEqu   term1  (Bool y)) = "(" ++ showTerm (term1) ++ ") !<==> " ++ showTerm (Bool y)         --  f.5: Un término compuesto no equivale a un booleano.
showTerm (InEqu (Bool x)   term2 ) = showTerm (Bool x) ++ " !<==> (" ++ showTerm (term2) ++ ")"         --  f.6: Un booleano no equivale a un termino compuesto.
showTerm (InEqu   term1    term2 ) = "(" ++ showTerm (term1) ++ ") !<==>  (" ++ showTerm (term2) ++ ")" --  f.7: Un término compuesto no equivale a un término compuesto. 

instance Show Term where show = showTerm

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
true = Bool True

false :: Term
false = Bool False

-- .............................................................................

-- Definición del conjunto de operadores infijos a utilizar en los enunciados de
-- la lógica proposicional.
-- Disjunción.
(\/) 	::  Term -> Term -> Term
(\/) term1 term2 = Or term1 term2

-- Conjunción.
(/\) 	::  Term -> Term -> Term
(/\) term1 term2 = And term1 term2

-- Implicación.
(==>)  :: Term -> Term -> Term
(==>) term1 term2 = Imp term1 term2

-- Equivalencia.
(<==>) :: Term -> Term -> Term
(<==>) term1 term2 = Equ term1 term2

-- Inequivalencia.
(!<==>) :: Term -> Term -> Term
(!<==>) term1 term2 = InEqu term1 term2

-- Negación.
neg	 :: Term -> Term
neg  term1 = Neg term1

-- .............................................................................