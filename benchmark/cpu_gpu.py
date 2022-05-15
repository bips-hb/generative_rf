
import time
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sdgym.datasets import load_dataset
from sdgym.datasets import load_tables
from sklearn.metrics import f1_score, accuracy_score, r2_score
from sdv.metrics.tabular import BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryLogisticRegression,BinaryMLPClassifier, LinearRegression, MLPRegressor
import torch 
from sklearn.utils import resample

def oracle(): 
    pass 

def f1_none(*args):
  return f1_score(average=None, *args)

def f1_macro(*args):
  return f1_score(average = 'macro', *args)

def f1_micro(*args):
  return f1_score(average = 'micro', *args)

def synth_data(data_train, synthesizer):
    """
    Arguments:
    @data_train: data to learn synthesizer from
    @synthesizer: model for generating synthetic data
    Return: synthesized data of size data_train
    """     
    if synthesizer == oracle():
       return data_train.copy()
    else: 
        synthesizer.fit(data = data_train)
        return synthesizer.sample(data_train.shape[0])  

def run_sub(synthesizer_name, R_seed = False):
    np.random.seed(2022)
    torch.manual_seed(2022)
    if R_seed:
        base = rpackages.importr('base')
        base.set_seed(2022,kind = "L'Ecuyer-CMRG")
        print("R seed set")
    my_syn = []
    i = 0
    while i < len(subs):
      if synthesizer_name == "TVAE_gpu":
        my_syn.append({"TVAE": TVAE(cuda=True)})
      elif synthesizer_name == "TVAE_cpu":
        my_syn.append({"TVAE": TVAE(cuda=False)})
      elif synthesizer_name == "CTGAN_gpu":
        my_syn.append({"TVAE": CTGAN(cuda=True)})
      elif synthesizer_name == "CTGAN_cpu":
        my_syn.append({"TVAE": CTGAN(cuda=False)})
      else: 
        print("please specify synthesizer name")
      i=i+1
    res = (syn_time(data = data_sub[i], synthesizer =  my_syn[i]) for i in range(len(subs)))
    return list(res)

  
def run_CTGAN_cpu_sub(range_i):
    np.random.seed(2022)
    torch.manual_seed(2022)
    my_syn = []
    i = 0
    while i < len(subs):
      my_syn.append({"CTGAN": CTGAN(cuda=False)})
      i = i+1
    res = (syn_time(data = data_sub[i], synthesizer =  my_syn[i]) for i in range_i)
    return list(res)


####### subsample setup


# load adult data
adult = load_tables(load_dataset('adult'))['adult']

###
# SELECT WHICH BENCHMARK TO RUN
# COMMENT OUT OTHER BENCHMARK
###

#########
# for sample size benchmark, use
#########
#subs_log = np.exp(np.linspace(np.log(1000), np.log(32561), 8))
#subs = [round(i) for i in subs_log]*5
## sample subsets from data
#np.random.seed(2022)
#data_sub= [resample(adult, n_samples=subs[i], replace=False, stratify=adult['label']) for i in range(len(subs))] 

def syn_time(data, synthesizer):
    """
    Args: 
    - data: real data to train data synthesizer on
    - synthesizer: dict of synthesizer to generate synthetic data
    Returns: time to generate a synthetic data set of same size as data
    """ 
    wall_time_start, process_time_start = time.perf_counter(), time.process_time()
    synth_data(data_train = data, synthesizer = synthesizer[list(synthesizer.keys())[0]])
    wall_time, process_time = time.perf_counter()-wall_time_start, time.process_time()-process_time_start
    res = pd.DataFrame({'dataset': data.shape[0], 'model': list(synthesizer.keys())[0],
    'wall_time':wall_time, 'process_time':process_time}, index=[0])
    return res

##########
# for dimensionality benchmark, use
##########
np.random.seed(2022)
## adult dtypes: 6 times 'int64', 9 times 'object' (including 'label')
## adult continuous features:
adult_cont = adult.select_dtypes(include='int64')
## adult categorical features: without 'label'
adult_cat = adult.select_dtypes(include='object').drop('label', axis=1)
## select one continous, one categorical and the label for subsets
rep = 5
subs = [2,4,6,8,10,12]*rep
data_sub = [pd.concat([adult_cont.sample(int(i/2), axis=1),adult_cat.sample(int(i/2), axis=1), adult['label']], axis=1) for i in subs]

i = 0
while i < rep:
  data_sub.append(adult)  # add full data set
  subs.append(14)
  i = i+1

# redefine output -> return (data.shape[1] -1) for number of features excluding 'label'
def syn_time(data, synthesizer):
    """
    Args: 
    - data: real data to train data synthesizer on
    - synthesizer: dict of synthesizer to generate synthetic data
    Returns: time to generate a synthetic data set of same size as data
    """ 
    wall_time_start, process_time_start = time.perf_counter(), time.process_time()
    synth_data(data_train = data, synthesizer = synthesizer[list(synthesizer.keys())[0]])
    wall_time, process_time = time.perf_counter()-wall_time_start, time.process_time()-process_time_start
    res = pd.DataFrame({'dataset': data.shape[1]-1, 'model': list(synthesizer.keys())[0],
    'wall_time':wall_time, 'process_time':process_time}, index=[0])
    return res



