with open("day12_sample_output.txt") as infile:
    data = infile.read()

solns = data.split("\n\n")
largest = max(solns, key=lambda x: len(x))

print(largest.split("\n")[0])
