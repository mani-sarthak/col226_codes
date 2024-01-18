import random

def generate_random_relations(S):
    """ Generate a random list of relations over the set S """
    R = set()
    for _ in range(len(S)*2):
        a = random.choice(S)
        b = random.choice(S)
        R.add((a, b))
    return list(R)

def reflexive_transitive_closure(R, S):
    """ Compute the reflexive-transitive closure of a relation R over a set S """
    closure = set()
    
    # Add reflexive relations
    for s in S:
        closure.add((s, s))

    # Add given relations and compute transitive relations
    changes = True
    while changes:
        changes = False
        new_relations = set()
        for (a, b) in closure:
            for (c, d) in R:
                if b == c and (a, d) not in closure:
                    new_relations.add((a, d))
                    changes = True
        closure.update(new_relations)
    
    return closure

def main():
    S = [i + 1 for i in range(30)]  # Example set S
    R = generate_random_relations(S)  # Random relations R

    closure = reflexive_transitive_closure(R, S)

    # Output for testing
    print("S = ", S)
    print("R =", R)
    print("test_results = ", closure)

    # Check each pair in S*S for membership in the reflexive-transitive closure
    test_results = dict()
    for x in S:
        for y in S:
            test_results[(x, y)] = (x, y) in closure

    return test_results

# Execute the main function and get the test results
test_results = main()
test_results
