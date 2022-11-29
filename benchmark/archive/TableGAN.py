try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())
from sdv.tabular import CopulaGAN

# adult
adult_res_2 = run_benchmark(training_data=adult_train, test_data=adult_test, classifiers=adult_classifiers,
metrics= adult_metrics, data_synthesizer=  {"CopulaGAN": CopulaGAN()})
pd.concat(adult_res).to_csv("CopulaGAN_adult.csv")

