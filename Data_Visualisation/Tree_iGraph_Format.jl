using JuDGE, DelimitedFiles, DataFrames, Glob, CSV

treeFileName = "7node-tree2.csv"
# treeFileName = "341node-tree.csv"

# Load the n-node tree and ASCII-print the tree to the REPL
mytree, data = tree_from_file(joinpath(@__DIR__,treeFileName))

# Define a function that gets the parent for a given node
parentfn = JuDGE.parent_builder(mytree)

# Get all nodes, and the corresponding parent of each node
rawNodes = JuDGE.collect(mytree)
rawParents = parentfn.(rawNodes)

# Get all nodenames
node = []
for n in rawNodes; push!(node, n.name) end

# Tree hierarchy
hierarchy = vcat(hcat("", reshape(node, (1,length(node)))), hcat(node, zeros(Int64, length(node),length(node))))

for i in 1:length(rawNodes)
    # Get the name, and the name of the parent of the 'i'th rawNode, accounting
    # for the root
    nodename = rawNodes[i].name
    if rawParents[i] != nothing
        parname = rawParents[i].name
        idx = findall(x->x==rawParents[i], rawNodes)[1]
        hierarchy[idx + 1, i + 1] = 1
    end
end

# Write to CSV. This can be imported to igraph in R as an adjacency matrix
CSV.write(joinpath(@__DIR__, "node_adj_matrix.csv"), DataFrame(hierarchy), header=false)
