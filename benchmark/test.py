try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())

####################
# grf credit failure reprex
#################

# load relevant stuff 
import numpy as np
import torch
from os import path
import rpy2
import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages
from rpy2.robjects import pandas2ri

# import grf
base = rpackages.importr('base')
r = robjects.r
r.source('../generative_ranger.R') 
generative_ranger = robjects.globalenv['generative_ranger']
pandas2ri.activate()
doPar = rpackages.importr('doParallel')
doPar.registerDoParallel(40)

# define generative_ranger version in python 
def gen_rf(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = False, num_trees = 10, min_node_size = 5 )
    return grf_syn_dat.astype(real_data.dtypes)

# code data synthesizer -> adujst for gen_rf
def synth_data(data_train, synthesizer):
    """
    Arguments:
    @data_train: data to learn synthesizer from
    @synthesizer: model for generating synthetic data
    Return: synthesized data of size data_train
    """     
    if synthesizer == gen_rf:
        return gen_rf(real_data = data_train)
    else: 
        synthesizer.fit(data = data_train)
        return synthesizer.sample(data_train.shape[0])  


# this takes ~ 2min
np.random.seed(2022)
torch.manual_seed(2022)
base.set_seed(2022,kind = "L'Ecuyer-CMRG")
res = pd.concat((scores(data_train = credit_train[i], data_test = credit_test[i], list_of_classifiers = credit_classifiers, 
metric = credit_metrics, synthesizer = {"gen_rf": gen_rf}) for i in range(2)))

# low accuracy problem:
res.iloc[4:5,:]

# some notes:
# - problem occurs in credit_train[1]/credit_test[1] data set 
# - need to run it in loop to ensure same initialization of hyperparameters as in our full benchmark
# - rerunning grf + AdaBoost on this particular data set with different
#  initialization still leads to a very bad performance of acc ~0.15, see: 

np.random.seed(2022)
torch.manual_seed(2022)
base.set_seed(2022,kind = "L'Ecuyer-CMRG")
scores(data_train = credit_train[1], data_test = credit_test[1], list_of_classifiers = [BinaryAdaBoostClassifier], 
metric = credit_metrics, synthesizer = {"gen_rf": gen_rf})