exec(open("benchmark_individual.py").read())
from sdv.tabular import CTGAN


# adult
    # hier noch ein seed
adult_res = scores(data_train = adult_train, data_test = adult_test, list_of_classifiers = adult_classifiers, 
metric = adult_metrics, synthesizer = {"CTGAN": CTGAN()})
adult_res.to_csv("CTGAN_adult.csv")

# census
    # hier noch ein seed?
census_res = scores(data_train = census_train, data_test = census_test, list_of_classifiers = census_classifiers,
metric = adult_metrics, synthesizer = {"CTGAN": CTGAN()})