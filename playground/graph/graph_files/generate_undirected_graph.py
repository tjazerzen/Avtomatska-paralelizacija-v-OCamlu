import random

print_again = True

def generate_graph(num_of_nodes, max_num_of_edges, file_name):
    existing_edges = set()
    with open(file_name, 'w') as f:
        # Generate nodes
        for i in range(1, num_of_nodes + 1):
            f.write(f'{i}:{random.randint(1, num_of_nodes*4)}\n')

        f.write('\n')

        # Generate edges
        for _ in range(max_num_of_edges):
            start_node = random.randint(1, num_of_nodes)
            end_node = random.randint(1, num_of_nodes)
            if start_node == end_node:
                continue
            if (start_node, end_node) in existing_edges or (end_node, start_node) in existing_edges:
                continue
            f.write(f'{start_node}-{end_node}\n')

if print_again:
    generate_graph(num_of_nodes=100, max_num_of_edges=2000, file_name='undirected_graph_large.txt')