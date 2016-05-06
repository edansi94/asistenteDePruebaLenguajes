-- TODO:
-- * Ver forma de como quitar las comillas simples.


-- Definición de los términos válidos.
data Term = Var Char         -- Termino unitario.
		  | Or  Term Term    -- Conjunción de términos.
		  | And Term Term    -- Disjunción de términos.
		  | Imp Term Term    -- Implicación de términos.
		  | Equ Term Term    -- Equivalencia de términos.
		  | InEqu Term Term  -- Inequivalencia de términos.

-- Descripción:
--		Permite ayudar a la impresión de los terminos definidos.
showTerm :: Term -> String
showTerm (Var i) = show i

instance Show Term where show = showTerm

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
