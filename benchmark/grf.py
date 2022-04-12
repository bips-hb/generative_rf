from os import path
basepath = path.dirname(__file__)
filepath = path.abspath(path.join(basepath, "..", "..", "genrf.py"))
exec(open(filepath, "r").read())
exec(open("benchmark_individual.py").read())

my_genrf = genrf(x = adult_train)
my_genrf.sample(n = 200)