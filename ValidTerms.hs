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
	(\/), (/\), (\==>), (\<==>), (\!<==>)
) where

-- .............................................................................

-- Definición de los términos válidos del lenguaje de proposiciones lógicas.
data Term = Var Char         -- Termino unitario.
		  | Or  Term Term    -- Conjunción de términos.
		  | And Term Term    -- Disjunción de términos.
		  | Imp Term Term    -- Implicación de términos.
		  | Equ Term Term    -- Equivalencia de términos.
		  | InEqu Term Term  -- Inequivalencia de términos.


-- Descripción:
--		Permite darle estilo a la impresión de los diferentes términos del lenguaje.
showTerm :: Term -> String
showTerm (Var i) =  i:[]                                                                        -- Caso: termino unitario.  
showTerm (Or (Var i) (Var j)) = showTerm (Var i) ++ " \\/" ++ showTerm (Var j)                  -- Caso: Conjunción.   
showTerm (Or (Var i)  term2 ) = showTerm (Var i) ++ " \\/  (" ++ showTerm (term2) ++ ")"
showTerm (Or  term1  (Var j)) = "(" ++ showTerm (term1) ++  ") " ++ "\\/" ++ showTerm (Var j)
showTerm (Or  term1   term2)  = "(" ++ showTerm (term1) ++ ") \\/ (" ++ showTerm (term2) ++ ")"

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

-- .............................................................................

-- Definición del conjunto de operadores infijos a utilizar en los enunciados de
-- la lógica proposicional.
-- Conjunción.
(\/) ::  Term -> Term -> Term
(\/) term1 term2 = Or term1 term2

-- Disjunción.
(/\) ::  Term -> Term -> Term
(/\) term1 term2 = And term1 term2

-- Implicación.
(\==>) :: Term -> Term -> Term
(\==>) term1 term2 = Imp term1 term2

-- Equivalencia.
(\<==>) :: Term -> Term -> Term
(\<==>) term1 term2 = Equ term1 term2

-- Inequivalencia.
(\!<==>) :: Term -> Term -> Term
(\!<==>) term1 term2 = InEqu term1 term2

-- .............................................................................