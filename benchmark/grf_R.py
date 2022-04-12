exec(open("benchmark_individual.py").read())
import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages
from rpy2.robjects import pandas2ri

r = robjects.r
r.source('../generative_ranger.R')
generative_ranger = robjects.globalenv['generative_ranger']
doPar = rpackages.importr('doParallel')
doPar.registerDoParallel(6)
pandas2ri.activate()

def gen_rf(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = False, num_trees = 20 )
    return grf_syn_dat.astype(real_data.dtypes)

def gen_rf_oob(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = True, num_trees = 20 )
    return grf_syn_dat.astype(real_data.dtypes)

    # data set name im .csv


adult_res = scores(data_train = adult_train, data_test = adult_test, list_of_classifiers = adult_classifiers, metric = adult_metrics, synthesizer = {"GRF_R_version": gen_rf})
adult_res.to_csv("grf_adult.csv")