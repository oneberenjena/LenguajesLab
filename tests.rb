$LOAD_PATH << '.'
require "bfs"

#PRUEBAS
na = Trees::Node.new(2, Trees::Node.new(4, Trees::Node.new(8)))
nb = Trees::Node.new(3, Trees::Node.new(5, Trees::Node.new(7)), Trees::Node.new(6))
nr = Trees::Node.new(1, na, nb)	

# puts "PRUEBA NODOS"
# nr.each { |node| puts node.value }

n1 = Trees::PinkNode.new(9, [Trees::PinkNode.new(15, [Trees::PinkNode.new(20)]), Trees::PinkNode.new(16)])
n2 = Trees::PinkNode.new(10)
n3 = Trees::PinkNode.new(11, [Trees::PinkNode.new(17),Trees::PinkNode.new(18),Trees::PinkNode.new(19)])
n4 = Trees::PinkNode.new(12)
n5 = Trees::PinkNode.new(13, [Trees::PinkNode.new(14)])

rootsa = Trees::PinkNode.new(4, [n1,n2,n3,n4,n5])
# puts "\nPRUEBA NODOS ROSA"
# rootsa.each {|node| puts node.value}

class Fuego
end

nc = Trees::Node.new("hola Nivel 1", Trees::Node.new(2, Trees::Node.new(["Nivel 3",89,9]), Trees::Node.new("Nivel 3 jajajja")))
nr1 = Trees::Node.new("soy root Nivel 0", nc, Trees::Node.new("soy right root Nivel 1", Trees::Node.new(Fuego.new)))

puts "\nPRUEBA BFS CON NODOS"
Bfs::bfs(nr1) {|node| puts node.value}
# puts "\n"
# Bfs::bfs(nr) {|node| puts node.value*2}
# puts "\nPRUEBA RECOGER CON NODOS"
# print_even = lambda { |node| puts node.value if (node.value % 2 == 0) }
# Bfs::recoger(nr, &print_even)


# puts "\nPRUEBA BFS CON NODOS ROSA"
# Bfs::bfs(rootsa) {|node| puts node.value}
# puts "\nPRUEBA RECOGER CON NODOS ROSA"
# Bfs::recoger(rootsa, &print_even)

