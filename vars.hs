-- Laboratorio de Lenguajes de Programacion I
-- Proyecto I
-- Archivo: vars.hs 
-- Descripcion: Contiene las definiciones de las variables, tipos de datos
-- y definiciones de funciones que seran usadas
-- Ultima modificacion: 11/05/16
-- Autores: - Benjamin Amos    #12-10240
--			- Douglas Torres   #11-11027
--			- Roberto Camara   #11-10235

--import Theorems

data Term = Var Char | Neg Term | And Term Term | Or Term Term | Impl Term Term | Equ Term Term | Inequ Term Term | Constant String
data Equation = Equivalent Term Term 
data Sust = Simple Term Term | Tup2 (Term, Sust, Term) | Tup3 (Term, Term, Sust, Term, Term)

instance Eq Term where
	(Var x) == (Var y) = x == y 

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
	-- Sustitucion simple a una variable
	sust (Var x) (Simple (Var ss) t) = abstraer (Var ss) (Var x) t
	sust (Neg t1) (Simple (Var ss) t) = (Neg (sust t1 (Simple (Var ss) t)))
	sust (Or t1 t2) (Simple (Var ss) t) = (Or (sust t1 (Simple (Var ss) t)) (sust t2 (Simple (Var ss) t)))
	sust (And t1 t2) (Simple (Var ss) t) = (And (sust t1 (Simple (Var ss) t)) (sust t2 (Simple (Var ss) t)))
	sust (Impl t1 t2) (Simple (Var ss) t) = (Impl (sust t1 (Simple (Var ss) t)) (sust t2 (Simple (Var ss) t)))
	sust (Equ t1 t2) (Simple (Var ss) t) = (Equ (sust t1 (Simple (Var ss) t)) (sust t2 (Simple (Var ss) t)))
	sust (Inequ t1 t2) (Simple (Var ss) t) = (Inequ (sust t1 (Simple (Var ss) t)) (sust t2 (Simple (Var ss) t)))
	
	-- Sustitucion paralela a dos variables
	sust (Var x) (Tup2 (t1, Simple (Var ss1) t2, (Var ss2))) = if ss1 == x then sust (Var x) (Simple (Var ss1) t1) else sust (Var x) (Simple (Var ss2) t2)	
	sust (Neg x) (Tup2 (t1, Simple (Var ss1) t2, (Var ss2))) = (Neg (sust x (Tup2 (t1, Simple (Var ss1) t2, (Var ss2)))))	
	sust (Or x1 x2) (Tup2 (t1, Simple (Var ss1) t2, (Var ss2))) = (Or (sust x1 (Tup2 (t1, Simple (Var ss1) t2, (Var ss2)))) (sust x2 (Tup2 (t1, Simple (Var ss1) t2, (Var ss2))))) 
	sust (And x1 x2) (Tup2 (t1, Simple (Var ss1) t2, (Var ss2))) = (And (sust x1 (Tup2 (t1, Simple (Var ss1) t2, (Var ss2)))) (sust x2 (Tup2 (t1, Simple (Var ss1) t2, (Var ss2))))) 
	sust (Impl x1 x2) (Tup2 (t1, Simple (Var ss1) t2, (Var ss2))) = (Impl (sust x1 (Tup2 (t1, Simple (Var ss1) t2, (Var ss2)))) (sust x2 (Tup2 (t1, Simple (Var ss1) t2, (Var ss2))))) 
	sust (Equ x1 x2) (Tup2 (t1, Simple (Var ss1) t2, (Var ss2))) = (Equ (sust x1 (Tup2 (t1, Simple (Var ss1) t2, (Var ss2)))) (sust x2 (Tup2 (t1, Simple (Var ss1) t2, (Var ss2))))) 
	sust  (Inequ x1 x2) (Tup2 (t1, Simple (Var ss1) t2, (Var ss2))) = (Inequ (sust x1 (Tup2 (t1, Simple (Var ss1) t2, (Var ss2)))) (sust x2 (Tup2 (t1, Simple (Var ss1) t2, (Var ss2))))) 

	-- Sustitucion paralela a tres variables
	sust (Var x) (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3))) = if x == ss1 then sust (Var x) (Simple (Var ss1) t1) else if x == ss2 then sust (Var x) (Simple (Var ss2) t2) else sust (Var x) (Simple (Var ss3) t3) 
	sust (Neg x) (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3))) =  (Neg (sust x (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))))
	sust (Or x1 x2) (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3))) = (Or (sust x1 (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))) (sust x2 (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))))
	sust (And x1 x2) (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3))) = (And (sust x1 (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))) (sust x2 (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))))
	sust (Impl x1 x2) (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3))) = (Impl (sust x1 (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))) (sust x2 (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))))
	sust (Equ x1 x2) (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3))) = (Equ (sust x1 (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))) (sust x2 (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))))
	sust (Inequ x1 x2) (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3))) = (Inequ (sust x1 (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))) (sust x2 (Tup3 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))))

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
infixl 6 =:
(=:) :: Term -> Term -> Sust
term1 =: term2 = Simple term2 term1


-- FORMA DE IMPRESION
showTerm :: Term -> String
showTerm (Var x) = x:[]
showTerm (Constant bool) = bool 
showTerm (Neg (Var x)) = "neg(" ++ showTerm(Var x) ++ ")"
showTerm (Neg t) = "neg(" ++ showTerm(t) ++ ")"

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

-- Equivalencia de terminos ===
showEquation :: Equation -> String
showEquation (Equivalent t1 t2) = showTerm(t1) ++ " === " ++ showTerm(t2)

-- Instanciamos los tipos de datos en la clase Show
instance Show Term where show t = showTerm t
instance Show Equation where show e = showEquation e

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

-- instantiate 
-- Recibe una ecuacion y una sustitucion (simple o a dos o tres terminos)
-- Devuelve una ecuacion de tipo Equivalent e1 e2
-- Donde e1 y e2 fueron sustituidos con la sutitucion dada
instantiate :: Equation -> Sust -> Equation
instantiate (Equivalent t1 t2) (Simple (Var x) tsust) = Equivalent (sust t1 (Simple (Var x) tsust)) (sust t2 (Simple (Var x) tsust))
instantiate (Equivalent t1 t2) (Tup2 (tsust1, Simple (Var x) tsust2, (Var y))) = Equivalent (sust t1 (Tup2 (tsust1, Simple (Var x) tsust2, (Var y)))) (sust t2 (Tup2 (tsust1, Simple (Var x) tsust2, (Var y))))
instantiate (Equivalent t1 t2) (Tup3 (tsust1, tsust2, Simple (Var x) tsust3, (Var y), (Var z))) = Equivalent (sust t1 (Tup3 (tsust1, tsust2, Simple (Var x) tsust3, (Var y), (Var z)))) (sust t2 (Tup3 (tsust1, tsust2, Simple (Var x) tsust3, (Var y), (Var z))))

-- leibniz
-- Recibe una ecuacion de tipo Equivalent eX eY, un termino E y una variable z
-- Devuelve una ecuacion de tipo Equivalent e1 e2 
-- Donde e1 y e2 resultan de aplicarle al termino E una sustitucion donde
-- la variable que indique Var z sera sustituida por eX y eY respectivamente 
leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equivalent e1 e2) e (Var z) = (Equivalent (sust e (Simple (Var z) e1)) (sust e (Simple (Var z) e2)))


infer :: Float -> Equation -> Sust -> Term -> Term -> Equation
infer n (Equivalent e1 e2) sus (Var z) e = inference (instantiate (prop n) sus) (Equivalent e1 e2) (Var z) e
	where
		inference (Equivalent eX eY) (Equivalent e1 e2) (Var z) e =  leibniz (Equivalent eX eY) e (Var z)


---------------------------------------------------------
-- ESTO IRA AQUI DE MOMENTO PORQUE NO SE HACER MODULOS --
---------------------------------------------------------
prop :: Float -> Equation 
prop num
  | num == 3.1  = (p <==> q) <==> r === p <==> (q <==> r)
  | num == 3.2  = (p <==> q) === (q <==> p)
  | num == 3.3  = p <==> p === true
  | num == 3.4  = p === p <==> true
  | otherwise = error "The statement doesn't exists"
---------------------------------------------------------
---------------------------------------------------------
---------------------------------------------------------