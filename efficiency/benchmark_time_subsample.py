import time

start = time.perf_counter()
def func():
   return([i for i in range(100000)])
func()


wall_time_start, process_time_start = time.perf_counter(), time.process_time()
func()
wall_time_end, process_time_end = time.perf_counter(), time.process_time()
print(f"Walltime delta {wall_time_end - wall_time_start} seconds \n Process time delta {process_time_end - process_time_start} seconds")



import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sdgym.datasets import load_dataset
from sdgym.datasets import load_tables
from sdv.tabular import CTGAN, CopulaGAN, GaussianCopula, TVAE
from sdv.metrics.tabular import BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryLogisticRegression,BinaryMLPClassifier
from sklearn.metrics import f1_score, accuracy_score
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri


######################################
# Define relevant functions 
######################################

r = robjects.r
r.source('generative_ranger.R')
generative_ranger = robjects.globalenv['generative_ranger']
import rpy2.robjects.packages as rpackages
doPar = rpackages.importr('doParallel')
doPar.registerDoParallel(20)
pandas2ri.activate()

def gen_rf(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = False, num_trees = 20 )
    return grf_syn_dat.astype(real_data.dtypes)

def gen_rf_oob(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = True, num_trees = 20 )
    return grf_syn_dat.astype(real_data.dtypes)
    
def Identity(): 
    pass 

def f1_none(*args):
  return f1_score(average=None, *args)

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

def scores(data_train, data_test, list_of_classifiers, metric, dict_of_synthesizers):
        """
        Args: 
        - list_of_classifiers: list of classifiers for the prediction task, subset of [BinaryDecisionTreeClassifier, BinaryAdaBoostClassifier,BinaryLogisticRegression, BinaryMLPClassifier]
        - metric: metric to use for score subset of [f1_none, accuracy_score]
        - dict_of_synthesizers: dict of synthesizers to generate synthetic data
        Returns: scores
        """
        syn_dat_res = pd.DataFrame()
        keys = list(dict_of_synthesizers)
        for mod in range(len(dict_of_synthesizers)):
            wall_time_start, process_time_start = time.perf_counter(), time.process_time()
            syn_data = synth_data(data_train = data_train, synthesizer = dict_of_synthesizers[keys[mod]])
            wall_time, process_time = time.perf_counter()-wall_time_start, time.process_time()-process_time_start
            res = pd.DataFrame()
            for item in list_of_classifiers:
                scores = item.compute(data_test, syn_data, target = "label", scorer = metric)
                new_metric = pd.DataFrame()
                for i in range(len(metric)): new_metric = pd.concat([new_metric, pd.DataFrame({metric[i].__name__ : [scores[i]] })], axis=1)
                res = res.append(pd.concat([pd.DataFrame({'model': keys[mod],'classifier': [item.__name__] , 'wall_time':wall_time, 'process_time':process_time}), new_metric], axis = 1))
            syn_dat_res = syn_dat_res.append(res)
        return syn_dat_res

#######################################
# Efficiency benchmark: census data set 
# - subsample performance
# - time
#######################################


census = load_tables(load_dataset('census'))['census']
census = census.dropna(axis=0, how='any', thresh=None, subset=None, inplace=False) 

sub_res = pd.DataFrame()
sub = [1000,2000, 5000,10000,20000,30000,40000,50000,75000]*1

for i in sub:
    train, test = train_test_split(census, train_size= round(i*(2/3)), test_size=round(i*(1/3)), stratify=census['label'])
    res = scores(data_train= train, data_test= test, list_of_classifiers=[BinaryLogisticRegression], 
    dict_of_synthesizers={'Identity': Identity(), 'gen_rf': gen_rf, 'gen_rf_oob': gen_rf_oob, "CTGAN":CTGAN(), "TVAE": TVAE()},metric=[f1_none])
    res['subsample'] = i
    sub_res = sub_res.append(res)
sub_res


# to do: check whether registerDoParallel() is working