import numpy as np
import pandas as pd

###
import rpy2
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri

from sdgym.datasets import load_dataset,load_tables

r = robjects.r
r.source('../generative_ranger.R')
r.source('/home/blesch/generative_RF/generative_rf/generative_ranger.R')
generative_ranger = robjects.globalenv['generative_ranger']
pandas2ri.activate()

def gen_rf(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = False )
    return grf_syn_dat.astype(real_data.dtypes)

adult = load_tables(load_dataset('adult'))['adult']

gen_rf(real_data=adult)