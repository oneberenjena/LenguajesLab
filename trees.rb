class Node
	#include Enumerable
	# Necesario para la implementacion del metodo each

	attr_accessor :value
	attr_reader :left, :right # Configurados para ser de solo lectura
	def initialize(value, left=nil, right=nil)
		# Constructor del arbol binario
		@value = value
		@left  = left if left.class == Node
		@right = right if right.class == Node
	end

	def each
		# Iterador del arbol
		@left.each  {|node| yield node} unless @left.nil?
		yield self 
		@right.each {|node| yield node} unless @right.nil? 
  	end
end

class PinkNode

	attr_accessor :value
	attr_reader :sons
	def initialize(value, sons=[])
		@value = value
		@sons  = sons if sons.each{|son| son.class == PinkNode}
	end
	
	def each(&block)
		yield self
		@sons.each do |son| 
			yield son
			son.sons.each {|son2| yield son2} unless son.nil?
		end
	end
end

#PRUEBAS
na = Node.new(2, Node.new(4))
nb = Node.new(3, Node.new(5), Node.new(6))
nr = Node.new(1, na, nb)

puts "PRUEBA NODOS"
nr.each { |node| puts node.value }

n1 = PinkNode.new(9, PinkNode.new(12))
n2 = PinkNode.new(10)
n3 = PinkNode.new(11)

puts "\nPRUEBA NODOS ROSA"
rootsa = PinkNode.new(4, [n1,n2,n3])
rootsa.each {|node| puts node.value}