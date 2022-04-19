try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())

import numpy as np 
import torch 

# census
np.random.seed(2022)
torch.manual_seed(2022)

census_res = [scores(data_train = census_train_sub[i], data_test = census_test_sub[i], list_of_classifiers = census_classifiers,
metric = census_metrics, synthesizer = {"Identity": Identity()}) for i in range(len(subs))]

pd.concat(census_res).to_csv("Identity_sub.csv")