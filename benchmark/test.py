try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())
from sdv.tabular import CTGAN
import numpy as np
import torch


# adult
np.random.seed(2022)
torch.manual_seed(2022)
adult_res = scores(data_train = adult_train, data_test = adult_test, list_of_classifiers = adult_classifiers, 
metric = adult_metrics, synthesizer = {"CTGAN": CTGAN(cuda=False)})
adult_res.to_csv("CTGAN_adult.csv")
