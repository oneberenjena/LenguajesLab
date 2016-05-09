data Prop = Var String | Sum Prop Prop

toString :: Prop -> String
toString (Var s1) = s1
toString (Sum prop1 prop2) = (toString prop1) ++ " + " ++ (toString prop2)

instance Show Prop where show t = toString t

a :: Prop
a = Var "a"

b :: Prop
b = Var "b"

c :: Prop
c = Var "c"

d :: Prop
d = Var "d"

e :: Prop
e = Var "e"

f :: Prop
f = Var "f"

g :: Prop
g = Var "g"

h :: Prop
h = Var "h"

i :: Prop
i = Var "i"

j :: Prop
j = Var "j"

k :: Prop
k = Var "k"

l :: Prop
l = Var "l"

m :: Prop
m = Var "m"

n :: Prop
n = Var "n"

o :: Prop
o = Var "o"

p :: Prop
p = Var "p"

q :: Prop
q = Var "q"

r :: Prop
r = Var "r"

s :: Prop
s = Var "s"

t :: Prop
t = Var "t"

u :: Prop
u = Var "u"

v :: Prop
v = Var "v"

w :: Prop
w = Var "w"

x :: Prop
x = Var "x"

y :: Prop
y = Var "y"

z :: Prop
z = Var "z"