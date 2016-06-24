module Trees
	class Node
		#include Enumerable
		# Necesario para la implementacion del metodo each

		attr_accessor :value
		attr_reader :left, :right # Configurados para ser de solo lectura
		def initialize(value=nil, left=nil, right=nil)
			# Constructor del arbol binario
			@value = value if value.class == Fixnum || value.class == Array || value.class == String
			@left  = left if left.class == Node
			@right = right if right.class == Node
		end

		def each
			# Iterador del arbol
			yield left if !left.nil?
			yield right if !right.nil?
	  	end
	end

	class PinkNode

		attr_accessor :value
		attr_reader :sons
		def initialize(value=nil, sons=[])
			@value = value if value.class == Fixnum || value.class == Array || value.class == String
			@sons  = sons if sons.each{|son| son.class == PinkNode}
		end
		
		def each
			sons.each do |son| 
				yield son if !son.nil?
			end
		end
	end
end

