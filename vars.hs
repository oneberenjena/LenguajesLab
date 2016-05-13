-- Laboratorio de Lenguajes de Programacion I
-- Proyecto I
-- Archivo: vars.hs 
-- Descripcion: Contiene las definiciones de las variables, tipos de datos
-- y definiciones de funciones que seran usadas
-- Ultima modificacion: 11/05/16
-- Autores: - Benjamin Amos    #12-10240
--			- Douglas Torres   #11-11027
--			- Roberto Camara   #11-10235

data Term = Var Char | Neg Term | And Term Term | Or Term Term | Impl Term Term | Equ Term Term | Inequ Term Term | Constant String
data Equation = Equivalent Term Term 
data Sust = Simple Term Term | Tup2 (Term, Sust, Term) | Tup3 (Term, Term, Sust, Term, Term)


-- Esta funcion lambda es la identidad, dado un elemento devuelve el mismo elemento
ident = \x -> x

-- Esta funcion lambda recibe dos elementos y devuelve solo el primero que se paso
killSecond = \x -> \y -> x

-- Esta funcion recibe tres elementos y devuelve disjuntamente el primero con el tercero
-- Y el segundo con el tercero. Es como una especie de distributiva de derecha a izquierda
splash = \x -> \y -> \z -> (x z) (y z)

-- ESTA ES LA FUNCION ABSTRAER 
-- Basicamente toma una variable que es la que va a ser modificada,
-- un termino que puede ser cualquier conjunto de variables en una operacion
-- booleana, y la variable por la que se quiere sustituir.
-- Si no hay instancias de la variable a sustituir en el termino retorna el mismo term
abstraer :: Term -> Term -> (Term -> Term)
abstraer (Var x) (Var y) = if x == y then ident else killSecond (Var y)
abstraer (Var x) (Or t1 t2) = splash (splash (killSecond Or) (abstraer (Var x) t1)) (abstraer (Var x) t2) 
abstraer (Var x) (And t1 t2) = splash (splash (killSecond And) (abstraer (Var x) t1)) (abstraer (Var x) t2) 
abstraer (Var x) (Impl t1 t2) = splash (splash (killSecond Impl) (abstraer (Var x) t1)) (abstraer (Var x) t2) 
abstraer (Var x) (Equ t1 t2) = splash (splash (killSecond Equ) (abstraer (Var x) t1)) (abstraer (Var x) t2) 
abstraer (Var x) (Inequ t1 t2) = splash (splash (killSecond Inequ) (abstraer (Var x) t1)) (abstraer (Var x) t2) 

abstraer (Or t1 t2) _ = error "solo se puede abstraer una variable"
abstraer (And t1 t2) _ = error "solo se puede abstraer una variable"
abstraer (Impl t1 t2) _ = error "solo se puede abstraer una variable"
abstraer (Equ t1 t2) _ = error "solo se puede abstraer una variable"
abstraer (Inequ t1 t2) _ = error "solo se puede abstraer una variable"

-- Clase polimorfica para los objetos que son substituibles
-- De momento nada mas contiene a la funcion polimorfica sust
-- Recibe un Termino y un objeto de tipo Sust o (Term, Sust, Term) o
-- (Term, Term, Sust, Term, Term) y devuelve un termino con la sustitucion
-- aplicada
class Sustituible s where
	sust :: Term -> s -> Term

-- Se asocia el comportamiento de un objeto Sust en la clase sustituible para
-- la funcion sust.
instance Sustituible Sust where
	sust t1 (Simple sustme t2) = abstraer sustme t1 t2
	
	-- REVISAR
	sust tsust (Tup2 (t1, Simple sustme1 t2, sustme2)) = sust (sust tsust (Simple sustme1 t1)) (Simple sustme2 t2)
	sust tsust (Tup3 (t1, t2, Simple sustme1 t3, sustme2, sustme3)) = sust (sust (sust tsust (Simple sustme1 t1)) (Simple sustme2 t2)) (Simple sustme3 t3)

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
(===) :: Term -> Term -> Equation
term1 === term2 = Equivalent term1 term2

-- Operador sustitucion textual
infixl 6 =:
(=:) :: Term -> Term -> Sust
term1 =: term2 = Simple term2 term1


-- FORMA DE IMPRESION
showTerm :: Term -> String
showTerm (Var x) = x:[]
showTerm (Constant bool) = bool 
showTerm (Neg (Var x)) = "¬" ++ showTerm(Var x)
showTerm (Neg t) = "¬(" ++ showTerm(t) ++ ")"

-- Conjuncion
showTerm (And (Var x) (Var y)) = showTerm(Var x) ++ " /\\ " ++ showTerm(Var y)
showTerm (And (Var x) t) = showTerm(Var x) ++ " /\\ (" ++ showTerm(t) ++ ")"
showTerm (And t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " /\\ " ++ showTerm(Var x)
showTerm (And t1 t2) = "(" ++ showTerm t1 ++ ") /\\ (" ++ showTerm t2 ++ ")"

-- Disyuncion
showTerm (Or (Var x) (Var y)) = showTerm(Var x) ++ " \\/ " ++ showTerm(Var y)
showTerm (Or (Var x) t) = showTerm(Var x) ++ " \\/ (" ++ showTerm(t) ++ ")"
showTerm (Or t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " \\/ " ++ showTerm(Var x)
showTerm (Or t1 t2) = "(" ++ showTerm t1 ++ ") \\/ (" ++ showTerm t2 ++ ")"

-- Implicacion
showTerm (Impl (Var x) (Var y)) = showTerm(Var x) ++ " ==> " ++ showTerm(Var y)
showTerm (Impl (Var x) t) = showTerm(Var x) ++ " ==> (" ++ showTerm(t) ++ ")"
showTerm (Impl t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " ==> " ++ showTerm(Var x)
showTerm (Impl t1 t2) = "(" ++ showTerm t1 ++ ") ==> (" ++ showTerm t2 ++ ")"

-- Equivalencia
showTerm (Equ (Var x) (Var y)) = showTerm(Var x) ++ " <==>" ++ showTerm(Var y)
showTerm (Equ (Var x) t) = showTerm(Var x) ++ " <==> (" ++ showTerm(t) ++ ")"
showTerm (Equ t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " <==> " ++ showTerm(Var x)
showTerm (Equ t1 t2) = "(" ++ showTerm t1 ++ ") <==> (" ++ showTerm t2 ++ ")"

-- Inequivalencia
showTerm (Inequ (Var x) (Var y)) = showTerm(Var x) ++ " !<==> " ++ showTerm(Var y)
showTerm (Inequ (Var x) t) = showTerm(Var x) ++ " !<==> (" ++ showTerm(t) ++ ")"
showTerm (Inequ t (Var x)) = "(" ++ showTerm(t) ++ ")" ++ " !<==> " ++ showTerm(Var x)
showTerm (Inequ t1 t2) = "(" ++ showTerm t1 ++ ") !<==> (" ++ showTerm t2 ++ ")"

instance Show Term where show t = showTerm t

-- Funciones dummy
statement :: ()
statement = ()

with :: ()
with = ()

using :: ()
using = ()

lambda :: ()
lambda = ()

-- Funciones del sistema
