$LOAD_PATH << '.'
require "trees"

module Bfs
	include Trees

	def Bfs.bfs(node)
		queue = []
		queue.push(node)

		while (queue.size != 0)
			act_node = queue.shift
			yield act_node if !act_node.value.nil?		# Pasaje implicito del bloque
			act_node.each do |child|
				queue.push(child)
			end
		end
	end

	def Bfs.recoger(node, &block)
		queue = []
		queue.push(node)

		while (queue.size != 0)
			act_node = queue.shift
			block.call(act_node) if !act_node.value.nil? 	# Pasaje explicito del bloque
			act_node.each do |child|
				queue.push(child)
			end
		end
	end
end

