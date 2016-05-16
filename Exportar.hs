-- Laboratorio de Lenguajes de Programacion I
-- Proyecto I
-- Archivo: Exportar.hs 
-- Descripcion: Contiene las definiciones de tipos de datos, operadores y
-- variables
-- Ultima modificacion: 15/05/16
-- Autores: - Benjamin Amos    #12-10240
--			- Douglas Torres   #11-11027
--			- Roberto Camara   #11-10235

module Exportar where
    
data Term = Var Char 
            | Neg Term 
            | And Term Term 
            | Or Term Term 
            | Impl Term Term 
            | Equ Term Term 
            | Inequ Term Term 
            | Constant String
            
data Equation = Equivalent Term Term 

data Sust = Simple Term Term 


-- Declaracion de las variables de la 'a' a la 'z' como Vars
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
true = Constant "true"

false :: Term
false = Constant "false"

-- OPERADORES

-- Operador negacion con la mayor precedencia
infixl 7 `neg`
neg :: Term -> Term
neg term = Neg term

-- Operador conjuncion con precedencia menor a la negacion
infixl 6 /\
(/\) :: Term -> Term -> Term
term1 /\ term2 = And term1 term2

-- Operador disyuncion con precedencia menor a la negacion
infixl 6 \/
(\/) :: Term -> Term -> Term
term1 \/ term2 = Or term1 term2

-- Operador implicacion con asociacion hacia la derecha
infixr 5 ==>
(==>) :: Term -> Term -> Term
term1 ==> term2 = Impl term1 term2

-- Operador equivalencia con la menor precedencia
infixl 4 <==>
(<==>) :: Term -> Term -> Term
term1 <==> term2 = Equ term1 term2

-- Operador inequivalencia con la menor precedencia junto con la equivalencia
infixl 4 !<==>
(!<==>) :: Term -> Term -> Term
term1 !<==> term2 = Inequ term1 term2

-- Operador Equivalencia de terminos, que representa la equivalencia logica
-- No usa precedencia ya que no devuelve un termino operable sino un ecuacion
infixl 2 ===
(===) :: Term -> Term -> Equation
term1 === term2 = Equivalent term1 term2

-- Operador sustitucion textual
infixl 1 =:
(=:) :: Term -> Term -> Sust
term1 =: term2 = Simple term2 term1
