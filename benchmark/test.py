try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())


import numpy as np
import torch
from os import path
import rpy2
import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages
from rpy2.robjects import pandas2ri


#basepath = path.dirname("__file__")
#filepath = path.abspath(path.join(basepath, "..", "genrf.py"))
#exec(open(filepath, "r").read())
#exec(open("benchmark_individual.py").read())
base = rpackages.importr('base')
r = robjects.r
#r.source('../generative_ranger.R')
r.source('/home/blesch/generative_RF/generative_rf/generative_ranger.R')
generative_ranger = robjects.globalenv['generative_ranger']
pandas2ri.activate()
doPar = rpackages.importr('doParallel')
doPar.registerDoParallel(40)

# define generative_ranger version in python 
def gen_rf(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = False, num_trees = 10, min_node_size = 5 )
    return grf_syn_dat.astype(real_data.dtypes)


# define generative_ranger version in python 
def gen_rf(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = False, num_trees = 10, min_node_size = 5 )
    return grf_syn_dat.astype(real_data.dtypes)

# code data synthesizer 
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


def run_grf_benchmark(training_data, test_data, classifiers, metrics, data_synthesizer, seed = 2022):
    np.random.seed(seed)
    torch.manual_seed(seed)
    base.set_seed(2022,kind = "L'Ecuyer-CMRG")
    comp_exp = (scores(data_train = training_data[i], data_test = test_data[i], list_of_classifiers = classifiers, 
    metric = metrics, synthesizer = data_synthesizer) for i in rep)
    return list(comp_exp)


def run_grf_benchmark(training_data, test_data, classifiers, metrics, data_synthesizer, seed = 2022):
    np.random.seed(seed)
    torch.manual_seed(seed)
    base.set_seed(2022,kind = "L'Ecuyer-CMRG")
    res = pd.DataFrame()
    for i in rep:
        item = scores(data_train = training_data[i], data_test = test_data[i], list_of_classifiers = classifiers, 
        metric = metrics, synthesizer = data_synthesizer)
        res.append(item)
    return res



adult_res2 = run_grf_benchmark(training_data= adult_train, test_data = adult_test, classifiers= adult_classifiers, 
metrics= adult_metrics, data_synthesizer= {"gen_rf": gen_rf})
gen_rf(real_data = adult_train[1])