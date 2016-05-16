{-# LANGUAGE FlexibleInstances #-}
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
data Sust = Simple Term Term 

instance Eq Term where
	(Var x) == (Var y) = x == y
	(And t1 t2) == (And t3 t4) = t1 == t3 &&  t2 == t4
	(And t1 t2) == t = False
	t == (And t1 t2) = False
	(Or t1 t2) == (Or t3 t4) = t1 == t3 &&  t2 == t4
	(Or t1 t2) == t = False
	t == (Or t1 t2) = False
	(Impl t1 t2) == (Impl t3 t4) = t1 == t3 &&  t2 == t4
	(Impl t1 t2) == t = False
	t == (Impl t1 t2) = False
	(Equ t1 t2) == (Equ t3 t4) = t1 == t3 &&  t2 == t4
	(Equ t1 t2) == t = False
	t == (Equ t1 t2) = False
	(Inequ t1 t2) == (Inequ t3 t4) = t1 == t3 &&  t2 == t4
	(Inequ t1 t2) == t = False
	t == (Inequ t1 t2) = False
	(Neg t1) == (Neg t2) = t1 == t2
	t == (Neg t2) = False
	(Neg t1) == t = False
	(Constant tf1) == (Constant tf2) = tf1 == tf2
	(Constant tf) == t = False
	t == (Constant tf) = False

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

-- Sustitucion simple a una variable
instance Sustituible Sust where
	sust (Var x) (Simple (Var ss) t) = abstraer (Var ss) (Var x) t
	sust (Constant tf) (Simple (Var ss) t) = (Constant tf)
	sust (Neg t1) (Simple (Var ss) t) = (Neg (sust t1 (Simple (Var ss) t)))
	sust (Or t1 t2) (Simple (Var ss) t) = (Or (sust t1 (Simple (Var ss) t)) (sust t2 (Simple (Var ss) t)))
	sust (And t1 t2) (Simple (Var ss) t) = (And (sust t1 (Simple (Var ss) t)) (sust t2 (Simple (Var ss) t)))
	sust (Impl t1 t2) (Simple (Var ss) t) = (Impl (sust t1 (Simple (Var ss) t)) (sust t2 (Simple (Var ss) t)))
	sust (Equ t1 t2) (Simple (Var ss) t) = (Equ (sust t1 (Simple (Var ss) t)) (sust t2 (Simple (Var ss) t)))
	sust (Inequ t1 t2) (Simple (Var ss) t) = (Inequ (sust t1 (Simple (Var ss) t)) (sust t2 (Simple (Var ss) t)))

-- Sustitucion paralela a dos variables
instance Sustituible (Term, Sust, Term) where
	sust (Var x) (t1, Simple (Var ss1) t2, (Var ss2)) = if ss1 == x then sust (Var x) (Simple (Var ss1) t1) else sust (Var x) (Simple (Var ss2) t2)	
	sust (Constant tf) (t1, Simple (Var ss1) t2, (Var ss2)) = Constant tf
	sust (Neg x) (t1, Simple (Var ss1) t2, (Var ss2)) = Neg (sust x (t1, Simple (Var ss1) t2, (Var ss2)))	
	sust (Or x1 x2) (t1, Simple (Var ss1) t2, (Var ss2)) = Or (sust x1 (t1, Simple (Var ss1) t2, (Var ss2))) (sust x2 (t1, Simple (Var ss1) t2, (Var ss2))) 
	sust (And x1 x2) (t1, Simple (Var ss1) t2, (Var ss2)) = And (sust x1 (t1, Simple (Var ss1) t2, (Var ss2))) (sust x2 (t1, Simple (Var ss1) t2, (Var ss2))) 
	sust (Impl x1 x2) (t1, Simple (Var ss1) t2, (Var ss2)) = Impl (sust x1 (t1, Simple (Var ss1) t2, (Var ss2))) (sust x2 (t1, Simple (Var ss1) t2, (Var ss2))) 
	sust (Equ x1 x2) (t1, Simple (Var ss1) t2, (Var ss2)) = Equ (sust x1 (t1, Simple (Var ss1) t2, (Var ss2))) (sust x2 (t1, Simple (Var ss1) t2, (Var ss2))) 
	sust  (Inequ x1 x2) (t1, Simple (Var ss1) t2, (Var ss2)) = Inequ (sust x1 (t1, Simple (Var ss1) t2, (Var ss2))) (sust x2 (t1, Simple (Var ss1) t2, (Var ss2))) 

-- Sustitucion paralela a tres variables
instance Sustituible (Term, Term, Sust, Term, Term) where
	sust (Var x) (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)) = if x == ss1 then sust (Var x) (Simple (Var ss1) t1) else if x == ss2 then sust (Var x) (Simple (Var ss2) t2) else sust (Var x) (Simple (Var ss3) t3) 
	sust (Constant tf) (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)) = Constant tf
	sust (Neg x) (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)) =  Neg (sust x (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))
	sust (Or x1 x2) (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)) = Or (sust x1 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3))) (sust x2 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))
	sust (And x1 x2) (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)) = And (sust x1 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3))) (sust x2 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))
	sust (Impl x1 x2) (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)) = Impl (sust x1 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3))) (sust x2 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))
	sust (Equ x1 x2) (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)) = Equ (sust x1 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3))) (sust x2 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))
	sust (Inequ x1 x2) (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)) = Inequ (sust x1 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3))) (sust x2 (t1, t2, Simple (Var ss1) t3, (Var ss2), (Var ss3)))

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
showTerm (Equ (Var x) (Var y)) = showTerm(Var x) ++ " <==> " ++ showTerm(Var y)
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

-- Sustitucion textual
showSust :: Sust -> String
showSust (Simple (Var x) t) = showTerm(t) ++ " =: " ++ showTerm(Var x)

-- Instanciamos los tipos de datos en la clase Show
instance Show Term where show t = showTerm t
instance Show Equation where show e = showEquation e
instance Show Sust where show s = showSust s


-- Funciones dummy
with :: String
with = "with"

using :: String
using = "using"

lambda :: String
lambda = "lambda"

-- Funciones del sistema

-- instantiate 
-- Recibe una ecuacion y una sustitucion (simple o a dos o tres terminos)
-- Devuelve una ecuacion de tipo Equivalent e1 e2
-- Donde e1 y e2 fueron sustituidos con la sutitucion dada
instantiate :: (Sustituible s) => Equation -> s -> Equation
instantiate (Equivalent t1 t2) sus = Equivalent sus1 sus2
	where
		sus1 = sust t1 sus
		sus2 = sust t2 sus

-- leibniz
-- Recibe una ecuacion de tipo Equivalent eX eY, un termino E y una variable z
-- Devuelve una ecuacion de tipo Equivalent e1 e2 
-- Donde e1 y e2 resultan de aplicarle al termino E una sustitucion donde
-- la variable que indique Var z sera sustituida por eX y eY respectivamente 
leibniz :: Equation -> Term -> Term -> Equation
leibniz (Equivalent e1 e2) exp (Var z) = (Equivalent sus1 sus2)
	where
		sus1 = sust exp (Simple (Var z) e1)
		sus2 = sust exp (Simple (Var z) e2)

-- Inferencia
-- Recibe un float representando a un numero de un teorema
-- Una sustitucion para ese teorema
-- Una variable z
-- Un termino exp
-- Retorna la ecuacion que representa aplicar la regla de leibniz a la 
-- expresion con el teorema teorema como X == Y
infer :: Sustituible s => Float -> s -> Term -> Term -> Equation
infer n sus (Var z) exp = leibToTh
	where
		leib = (\eq -> leibniz eq exp (Var z))
		th = prop n
		leibToTh = leib $ instantiate th sus 
-- Step
-- Representa la deduccion de un paso, recibe
-- un termino1 que es el lado izquierdo de una ecuacion antes de realizar
-- una inferencia, y los mismos argumentos de infer
-- Retorna el lado de la ecuacion resultante a aplicar el teorema dado a
-- la expresion exp
step :: Sustituible s => Term -> Float -> s -> Term -> Term -> Term
step termino1 n sus (Var z) exp = check termino1 $ checkinf
	where
		checkinf =
			if (sust exp (Simple (Var z) termino1)) == termino1 then
				infer n sus (Var z) exp
			else
				error "invalid inference rule"
		check t1 (Equivalent t2izq t2der) = 
			if t1 == t2izq then 
				t2der 
			else if t1 == t2der then 
				t2izq else error "Proof failed"

statement :: (Sustituible s, Show s) => Float -> String -> s -> String -> String -> Term -> Term -> Term -> IO Term
statement = \n with sus using lambda varz exp t1 -> 
	do 
		putStrLn $ "=== <statement " ++ show n ++ " " ++ with ++ " " ++ show sus ++ " " ++ using ++ " " ++ lambda ++" " ++ show varz ++ "." ++ show exp ++">"
		return $ step t1 n sus varz exp

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