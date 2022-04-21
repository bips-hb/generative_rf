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
import rpy2
import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages
from rpy2.robjects import pandas2ri

base = rpackages.importr('base')

r = robjects.r
r.source('../generative_ranger.R')
r.source('/home/blesch/generative_RF/generative_rf/generative_ranger.R')
generative_ranger = robjects.globalenv['generative_ranger']
pandas2ri.activate()
doPar = rpackages.importr('doParallel')
doPar.registerDoParallel(20)

# define generative_ranger version in python 
def gen_rf(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = False )
    return grf_syn_dat.astype(real_data.dtypes)

# code data synthesizer 
def synth_data(data_train, synthesizer):
    """
    Arguments:
    @data_train: data to learn synthesizer from
    @synthesizer: model for generating synthetic data
    Return: synthesized data of size data_train
    """     
    if synthesizer == Identity():
       return data_train.copy()
    elif synthesizer == gen_rf:
        return gen_rf(real_data = data_train)
    elif synthesizer == gen_rf_oob:
        return gen_rf_oob(real_data = data_train)
    else: 
        synthesizer.fit(data = data_train)
        return synthesizer.sample(data_train.shape[0])  

# doParallel
base.set_seed(2022,kind = "L'Ecuyer-CMRG")
census_res = [scores(data_train = census_train_sub[i], data_test = census_test_sub[i], list_of_classifiers = census_classifiers,
metric = census_metrics, synthesizer = {"gen_rf": gen_rf}) for i in range(len(subs))]

pd.concat(census_res).to_csv("grf_sub.csv")
