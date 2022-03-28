import time

start = time.perf_counter()
def func():
   return([i for i in range(100000)])
func()


wall_time_start, process_time_start = time.perf_counter(), time.process_time()
func()
wall_time_end, process_time_end = time.perf_counter(), time.process_time()
print(f"Walltime delta {wall_time_end - wall_time_start} seconds \n Process time delta {process_time_end - process_time_start} seconds")


######### copy code from sdv_benchmark_manual


import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sdgym.datasets import load_dataset
from sdgym.datasets import load_tables
from sdv.tabular import CTGAN, CopulaGAN, GaussianCopula, TVAE
from sdv.metrics.tabular import BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryLogisticRegression,BinaryMLPClassifier
from sklearn.metrics import f1_score, accuracy_score, r2_score
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
#from multiprocessing import Pool
#import multiprocessing 
#multiprocessing.set_start_method('fork')
import matplotlib.pyplot as plt
#from sdgym.synthesizers import (CLBN, CopulaGAN, CTGAN, Identity, MedGAN, PrivBN, TableGAN, VEEGAN)


######################################
# Define relevant functions 
######################################

r = robjects.r
r.source('generative_ranger.R')
generative_ranger = robjects.globalenv['generative_ranger']
pandas2ri.activate()

def gen_rf(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = False )
    return grf_syn_dat.astype(real_data.dtypes)

def gen_rf_oob(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = True )
    return grf_syn_dat.astype(real_data.dtypes)
    
def Identity(): 
    pass 

def f1_none(*args):
  """
  should equal their f1_score
  """
  return f1_score(average=None, *args)

def f1_macro(*args):
  return f1_score(average = 'macro', *args)

def f1_micro(*args):
  return f1_score(average = 'micro', *args)


class benchmark:
    def __init__(self, dataset, test_size, subsample = 10000, stratified_census = False):
        """
        Args: 
        dataset: real dataset, spefiy one data set from Xu et al. 2020 Table 4,5
        test_size: train/test split as defined in Xu et al. 2020 Table 4
        
        """
        self.metadata = load_dataset(dataset)
        #self.real_data = load_tables(self.metadata)[dataset]# use this line for full data set
        full_dat = load_tables(self.metadata)[dataset]
        if subsample == None:
            subsample = len(full_dat.index)
        if stratified_census == True:
            # make subsample balanced - labels '- 50000' or '50000+'
            ones = full_dat.index[full_dat['label'] == '50000+']
            zeros = full_dat.index[full_dat['label'] == '- 50000']
            real_dat_raw = full_dat.loc[np.append(np.random.choice(ones, round(subsample* (len(ones)/ (len(ones) + len(zeros)))), replace = False),
            np.random.choice(zeros, round(subsample*(len(zeros)/ (len(ones) + len(zeros)))), replace=False))]
           # self.real_data = real_dat_raw.sample(frac=1).reset_index(drop=True) # shuffle rows macht train test automatisch
            self.real_data_train, self.real_data_test = train_test_split(self.real_data, test_size=test_size, random_state=2022)
            # in train test split jeweils den gleichen Anteil von 1en und 0en 
            # train test kann auch stratify 
        else:    
            self.real_data = full_dat.loc[np.random.choice(full_dat.index, subsample, replace=False)] # subset of subsample obs.
            self.real_data_train, self.real_data_test = train_test_split(self.real_data, test_size=test_size, random_state=2022)

    def synth_data(self, model):
        """ 
        Returns: synthesized data of size real_data_train
        """     
        if model == Identity():
            return self.real_data_train.copy()
        elif model == gen_rf:
            return gen_rf(real_data = self.real_data_train)
        elif model == gen_rf_oob:
            return gen_rf_oob(real_data = self.real_data_train)
        else: 
            model.fit(data = self.real_data_train)
            return model.sample(self.real_data_train.shape[0])      
    def scores(self, list_of_classifiers, metric, dict_of_syn_models):
        """
        TO DO: list_of_syn_models = None as real data prediction, scores
        Args: 
        - list_of_classifiers: list of classifiers for the prediction task, subset of [BinaryDecisionTreeClassifier, BinaryAdaBoostClassifier,BinaryLogisticRegression, BinaryMLPClassifier]
        - metric: metric to use for score calculation,choose from f1_none, f1_micro, f1_macro, r2_score, accuracy_score
        Returns: scores
        """
        res = pd.DataFrame()
        syn_dat_res = pd.DataFrame()
        keys = list(dict_of_syn_models)
        for mod in range(len(dict_of_syn_models)):
            syn_dat = self.synth_data(model = dict_of_syn_models[keys[mod]])
            res = pd.DataFrame()
            for item in list_of_classifiers:
                scores = item.compute(self.real_data_test, syn_dat, target = "label", scorer = metric)
                new_metric = pd.DataFrame()
                for i in range(len(metric)): new_metric = pd.concat([new_metric, pd.DataFrame({metric[i].__name__ : [scores[i]] })], axis=1)
                res = res.append(pd.concat([pd.DataFrame({'model': keys[mod],'classifier': [item.__name__] }), new_metric], axis = 1))
            syn_dat_res = syn_dat_res.append(res)
        return syn_dat_res


my_subs = [100, 250,500,1000,2000,5000,7500, 10000, 20000,30000]*8
# *reps
my_subs = [100,250]
subsample_res = pd.DataFrame()
time_res = pd.DataFrame()
for i in my_subs:
    census = benchmark(dataset='census', test_size=100/(200+100), subsample=i, stratified_census=True)
    census.real_data_test = census.real_data_test.dropna(axis=0, how='any', thresh=None, subset=None, inplace=False)
    census.real_data_train = census.real_data_train.dropna(axis=0, how='any', thresh=None, subset=None, inplace=False)
    result_census = census.scores(list_of_classifiers=[BinaryAdaBoostClassifier],metric=[f1_none], dict_of_syn_models= {"ID":Identity(), "grf": gen_rf, "CTGAN":CTGAN()})
    result_census['subsample'] = i
    subsample_res = subsample_res.append(result_census)
subsample_res


wall_time_start, process_time_start = time.perf_counter(), time.process_time()
func()
wall_time_end, process_time_end = time.perf_counter(), time.process_time()
print(f"Walltime delta {wall_time_end - wall_time_start} seconds \n Process time delta {process_time_end - process_time_start} seconds")

