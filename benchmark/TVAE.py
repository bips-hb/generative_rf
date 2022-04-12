exec(open("benchmark_individual.py").read())
from sdv.tabular import TVAE
import numpy as np

# adult
np.random.seed(2022)
adult_res = scores(data_train = adult_train, data_test = adult_test, list_of_classifiers = adult_classifiers, 
metric = adult_metrics, synthesizer = {"TVAE": TVAE})
adult_res.to_csv("TVAE_adult.csv")