import time
import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from sdgym.datasets import load_dataset
from sdgym.datasets import load_tables
from sklearn.metrics import f1_score, accuracy_score, r2_score
from sdv.metrics.tabular import BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryLogisticRegression,BinaryMLPClassifier, LinearRegression, MLPRegressor
import torch 

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
  #  elif synthesizer == gen_rf:
   #     return gen_rf(real_data = data_train)
  #  elif synthesizer == gen_rf_oob:
   #     return gen_rf_oob(real_data = data_train)
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


####### subsample setup


# adult 
adult = load_tables(load_dataset('adult'))['adult']
sub_classifiers = [BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryLogisticRegression,BinaryMLPClassifier]
sub_metrics = [f1_none, accuracy_score]
subs_log = np.exp(np.linspace(np.log(1000), np.log(32561), 8))
subs = [round(i) for i in subs_log]*5
np.random.seed(2022)
train_sub, test_sub = zip(*[train_test_split(adult,
train_size=round(23/(10+23)*i),test_size=round(10/(10+23)*i), stratify=adult['label']) for i in subs])

for i in range(len(subs)):
  train_sub[i].name = subs[i]

def run_sub(synthesizer_dict, R_seed = False):
    np.random.seed(2022)
    torch.manual_seed(2022)
    if R_seed:
        base = rpackages.importr('base')
        base.set_seed(2022,kind = "L'Ecuyer-CMRG")
        print("R seed set")
    res = [scores(data_train = train_sub[i], data_test = test_sub[i], list_of_classifiers = sub_classifiers, 
    metric = sub_metrics, synthesizer =  synthesizer_dict)  for i in range(len(subs))]
    return res
  
def run_CTGAN_cpu_sub(range_i, synthesizer_dict):
    np.random.seed(2022)
    torch.manual_seed(2022)
    res = (scores(data_train = train_sub[i], data_test = test_sub[i], list_of_classifiers = sub_classifiers, 
    metric = sub_metrics, synthesizer =  synthesizer_dict) for i in range_i)
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
