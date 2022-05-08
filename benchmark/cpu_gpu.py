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

####### subsample setup


# adult 
adult = load_tables(load_dataset('adult'))['adult']
subs_log = np.exp(np.linspace(np.log(1000), np.log(32561), 8))
subs = [round(i) for i in subs_log]*5
# sample subsets from data
np.random.seed(2022)
data_sub= [resample(adult, n_samples=subs[i], replace=False, stratify=adult['label']) for i in range(len(subs))] 

def run_sub(synthesizer_dict, R_seed = False):
    np.random.seed(2022)
    torch.manual_seed(2022)
    if R_seed:
        base = rpackages.importr('base')
        base.set_seed(2022,kind = "L'Ecuyer-CMRG")
        print("R seed set")
    res = (syn_time(data = data_sub[i], synthesizer =  synthesizer_dict)  for i in range(len(subs)))
    return list(res)
  
def run_CTGAN_cpu_sub(range_i, synthesizer_dict):
    np.random.seed(2022)
    torch.manual_seed(2022)
    res = (syn_time(data = data_sub[i], synthesizer =  synthesizer_dict) for i in range_i)
    return list(res)



# worst case runtime adult subsample benchmark
#CTGAN cpu 45 min
#TVAE cpu 5,5 min
#grf cpu 0,5 min
#CTGAN gpu 5 min 
#TVAE gpu 2 min
# -> 1h per run
# -> 10 subsample sizes
# -> 5 rep
