import random

print_again = True

def generate_graph(num_of_nodes, num_of_edges, file_name):
    existing_edges = set()
    if num_of_edges > num_of_nodes * (num_of_nodes - 1) / 2:
        raise Exception('Too many edges for the given number of nodes')
    with open(file_name, 'w') as f:
        # Generate nodes
        for i in range(0, num_of_nodes):
            f.write(f'{i}:{random.randint(1, num_of_nodes*4)}\n')

        f.write('\n')

        # Generate edges
        current_edges_added = 0
        while current_edges_added != num_of_edges:
            start_node = random.randint(0, num_of_nodes-1)
            end_node = random.randint(0, num_of_nodes-1)
            if start_node == end_node:
                continue
            if (start_node, end_node) in existing_edges or (end_node, start_node) in existing_edges:
                continue
            existing_edges.add((start_node, end_node))
            current_edges_added += 1
            f.write(f'{start_node}-{end_node}\n')

if print_again:
    generate_graph(num_of_nodes=7, num_of_edges=10, file_name='undirected_small_graph.txt')