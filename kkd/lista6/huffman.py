from collections import Counter

class Node:
    def __init__(self, value=None, probability=None):
        self.value = value
        self.probability = probability
        self.left = None
        self.right = None

def calculate_probabilities(numbers):
    total_numbers = len(numbers)
    counts = Counter(numbers)
    probabilities = {number: count / total_numbers for number, count in counts.items()}
    
    return probabilities

def build_huffman_tree(probabilities):
    nodes = [Node(value=v, probability=p) for v, p in probabilities.items()]

    while len(nodes) > 1:
        nodes.sort(key=lambda x: x.probability)
        left = nodes.pop(0)
        right = nodes.pop(0)

        new_node = Node(probability=left.probability + right.probability)
        new_node.left = left
        new_node.right = right
        nodes.append(new_node)

    return nodes[0]


def generate_huffman_codes(node, code="", mapping=None):
    if mapping is None:
        mapping = {}

    if node.value is not None:
        mapping[node.value] = code
    else:
        generate_huffman_codes(node.left, code + "0", mapping)
        generate_huffman_codes(node.right, code + "1", mapping)

    return mapping


def build_tree_from_mapping(huffman_mapping):
    root = Node()
    for value, code in huffman_mapping.items():
        current_node = root
        for bit in code:
            if bit == "0":
                if current_node.left is None:
                    current_node.left = Node()
                current_node = current_node.left
            elif bit == "1":
                if current_node.right is None:
                    current_node.right = Node()
                current_node = current_node.right
        current_node.value = value

    return root


def huffman_encode(numbers, huffman_mapping):
    encoded_text = ""
    for number in numbers:
        encoded_text += huffman_mapping[number]
    return encoded_text


def huffman_decode(encoded_text, huffman_tree):
    decoded_numbers = []
    current_node = huffman_tree

    for bit in encoded_text:
        if bit == "0":
            current_node = current_node.left
        else:
            current_node = current_node.right

        if current_node.value is not None:
            decoded_numbers.append(current_node.value)
            current_node = huffman_tree

    return decoded_numbers