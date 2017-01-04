A = {
    "A": ["B", "C", "H"],
    "B": ["A", "C", "D", "F", "H"],
    "C": ["A", "B", "E"],
    "D": ["B"],
    "E": ["C"],
    "F": ["B", "H"],
    "G": ["J"],
    "H": ["A", "B", "F"],
    "I": ["G"],
    "J": ["I"]
}
G = DiGraph(A, format='dict_of_lists')
H = G.plot(edge_labels=False, graph_border=False, vertex_size=200, max_dist=0)
H.show()
H.save('graph.pdf')