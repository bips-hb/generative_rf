try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())

for i in range(5):
    print(adult_test[i]['label'].iloc[0])

for i in range(5):
    print(census_test[i]['label'].iloc[0])

for i in range(5):
    print(credit_test[i]['label'].iloc[0])