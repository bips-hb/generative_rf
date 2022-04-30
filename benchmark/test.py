<<<<<<< HEAD
try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())

from cgi import test
from random import triangular
import numpy as np
import torch
from os import path
import rpy2
import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages
from rpy2.robjects import pandas2ri
from rpy2.robjects import IntVector, Formula
base = rpackages.importr('base')
r = robjects.r
r.source('../generative_ranger.R')
#r.source('/home/blesch/generative_RF/generative_rf/generative_ranger.R')
generative_ranger = robjects.globalenv['generative_ranger']
pandas2ri.activate()
doPar = rpackages.importr('doParallel')
doPar.registerDoParallel(5)

# define generative_ranger version in python 
def gen_rf(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = False )
    return grf_syn_dat.astype(real_data.dtypes)

generative_ranger(x_real = adult_train[1].iloc[1:30,], n_new = 3, oob = False )


rr = rpackages.importr('ranger')

dat = adult_train[1]
pred = rr.ranger(formula = Formula('age ~ education'), data = dat, keep_inbag = True, classification = True, num_trees = 3, min_node_size = 11)
=======
import pandas as pd
pd.read_csv("TVAE_adult.csv")
>>>>>>> 1b75399ab03381c8abe8d47e1b97533cb9f7e6d9
