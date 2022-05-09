## from benchmark_individual
## now without stratification of intrusion

import time
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sdgym.datasets import load_dataset
from sdgym.datasets import load_tables
from sklearn.metrics import f1_score, accuracy_score, r2_score
from sdv.metrics.tabular import BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryLogisticRegression,BinaryMLPClassifier, LinearRegression, MLPRegressor
import torch

import rpy2
import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages
from rpy2.robjects import pandas2ri

base = rpackages.importr('base')
r = robjects.r
#r.source('../generative_ranger.R')
r.source('/home/blesch/generative_RF/generative_rf/generative_ranger.R')
generative_ranger = robjects.globalenv['generative_ranger']
pandas2ri.activate()
doPar = rpackages.importr('doParallel')
doPar.registerDoParallel(40)


def oracle(): 
    pass 

def f1_none(*args):
  return f1_score(average=None, *args)

def f1_macro(*args):
  return f1_score(average = 'macro', *args)

def f1_micro(*args):
  return f1_score(average = 'micro', *args)


# define generative_ranger version in python 
def gen_rf(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = False, num_trees = 10, min_node_size = 5 )
    return grf_syn_dat.astype(real_data.dtypes)
    
def synth_data(data_train, synthesizer):
    """
    Arguments:
    @data_train: data to learn synthesizer from
    @synthesizer: model for generating synthetic data
    Return: synthesized data of size data_train
    """     
    if synthesizer == gen_rf:
        return gen_rf(real_data = data_train)
    if synthesizer == oracle():
        return data_train.copy()
    else: 
        synthesizer.fit(data = data_train)
        return synthesizer.sample(data_train.shape[0])  


def scores(data_train, data_test, list_of_classifiers, metric, synthesizer):
    """
    Args: 
    - list_of_classifiers: list of classifiers for the prediction task, subset of [BinaryDecisionTreeClassifier, BinaryAdaBoostClassifier,BinaryLogisticRegression, BinaryMLPClassifier, LinearRegression, MLPRegressor]
    - metric: metric to use for score subset of [f1_none, accuracy_score, r2_score]
    - synthesizer: dict of synthesizer to generate synthetic data
    Returns: scores
    """ 
    syn_dat_res = pd.DataFrame()
    wall_time_start, process_time_start = time.perf_counter(), time.process_time()
    syn_data = synth_data(data_train = data_train, synthesizer = synthesizer[list(synthesizer.keys())[0]])
    wall_time, process_time = time.perf_counter()-wall_time_start, time.process_time()-process_time_start
    res = pd.DataFrame()
    for item in list_of_classifiers:
        scores = item.compute(real_data = data_test, synthetic_data = syn_data, target = "label", scorer = metric)
        new_metric = pd.DataFrame()
        for i in range(len(metric)): new_metric = pd.concat([new_metric, pd.DataFrame({metric[i].__name__ : [scores[i]] })], axis=1)
        res = res.append(pd.concat([pd.DataFrame({'dataset': data_train.name, 'model': list(synthesizer.keys())[0],'classifier': [item.__name__] , 
        'wall_time':wall_time, 'process_time':process_time}), new_metric], axis = 1))
    syn_dat_res = syn_dat_res.append(res)
    return syn_dat_res

rep = range(5)

def run_benchmark(training_data, test_data, classifiers, metrics, data_synthesizer, seed = 2022):
    np.random.seed(seed)
    torch.manual_seed(seed)
    comp = (scores(data_train = training_data[i], data_test = test_data[i], list_of_classifiers = classifiers, 
    metric = metrics, synthesizer = data_synthesizer) for i in rep)
    return list(comp)


def run_grf_benchmark(training_data, test_data, classifiers, metrics, data_synthesizer, seed = 2022):
    np.random.seed(seed)
    torch.manual_seed(seed)
    base.set_seed(2022,kind = "L'Ecuyer-CMRG")
    comp = (scores(data_train = training_data[i], data_test = test_data[i], list_of_classifiers = classifiers, 
    metric = metrics, synthesizer = data_synthesizer) for i in rep)
    return list(comp) 

################################
# get indices of train/test data sets for each data set
################################

np.random.seed(2022)
subsample = 0.05 # use subsample of intrusion only

# intrusion
intrusion = load_tables(load_dataset('intrusion'))['intrusion']
intrusion_train, intrusion_test = zip(*[train_test_split(intrusion, train_size= (394/(394+100))*subsample, test_size=(100/(394+100))*subsample) for i in rep])

for i in rep:
  intrusion_train[i].name = 'intrusion'
intrusion_classifiers = [BinaryDecisionTreeClassifier,BinaryMLPClassifier]
intrusion_metrics = [accuracy_score, f1_micro, f1_macro] 

# intrusion oracle
intrusion_res = run_benchmark(training_data= intrusion_train, test_data= intrusion_test, classifiers= intrusion_classifiers,
metrics = intrusion_metrics, data_synthesizer= {"oracle": oracle()})
pd.concat(intrusion_res).to_csv("oracle_intrusion_nonstratified.csv")

# intrusion grf
intrusion_res = run_grf_benchmark(training_data = intrusion_train, test_data= intrusion_test, classifiers= intrusion_classifiers,
metrics= intrusion_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(intrusion_res).to_csv("grf_intrusion_nonstratified.csv")

# intrusion CTGAN
from sdv.tabular import CTGAN
intrusion_res = run_benchmark(training_data= intrusion_train, test_data= intrusion_test, classifiers= intrusion_classifiers,
metrics= intrusion_metrics, data_synthesizer= {"CTGAN": CTGAN()})
pd.concat(intrusion_res).to_csv("CTGAN_nonstratified.csv")

# intrusion TVAE 
from sdv.tabular import TVAE
intrusion_res = run_benchmark(training_data= intrusion_train, test_data= intrusion_test, classifiers = intrusion_classifiers,
metrics = intrusion_metrics, data_synthesizer= {"TVAE": TVAE()})
pd.concat(intrusion_res).to_csv("TVAE_intrusion_nonstratified.csv")