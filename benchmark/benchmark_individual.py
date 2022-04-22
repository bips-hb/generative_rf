import time
import pandas as pd
from sklearn.model_selection import train_test_split
from sdgym.datasets import load_dataset
from sdgym.datasets import load_tables
from sklearn.metrics import f1_score, accuracy_score, r2_score
from sdv.metrics.tabular import BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryLogisticRegression,BinaryMLPClassifier, LinearRegression, MLPRegressor


def Identity(): 
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
    if synthesizer == Identity():
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


################################
# get indices of train/test data sets for each data set
################################

# adult 
adult = load_tables(load_dataset('adult'))['adult']
adult_train, adult_test = train_test_split(adult, test_size=10/(23+10), stratify=adult['label'], random_state=2022)
adult_train.name = 'adult' # keep information on which dataset is used
adult_classifiers = [BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryLogisticRegression,BinaryMLPClassifier]
adult_metrics = [f1_none, accuracy_score]

# census 
census = load_tables(load_dataset('census'))['census']
census = census.dropna(axis=0, how='any', thresh=None, subset=None, inplace=False) 
census_train, census_test = train_test_split(census, test_size=100/(100+200), stratify=census['label'], random_state=2022)
census_train.name = 'census'
census_classifiers = [BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryMLPClassifier]
census_metrics = [f1_none, accuracy_score]

# census -- subsamples for time benchmark

subs = [250,500,1000,2000,5000,7500, 10000, 25000, 50000,100000,200000,298006]*10
census_train_sub, census_test_sub = zip(*[train_test_split(census,
 train_size=round(200/(100+200)*i),test_size=round(100/(100+200)*i), stratify=census['label'], random_state=2022) for i in subs])

for i in range(len(subs)):
  census_train_sub[i].name = subs[i]

# covtype
covtype = load_tables(load_dataset('covtype'))['covtype']
covtype_train, covtype_test = train_test_split(covtype, test_size=100/(481+100), stratify=covtype['label'], random_state=2022)
covtype_train.name = 'covtype'
covtype_classifiers = [BinaryDecisionTreeClassifier,BinaryMLPClassifier]
covtype_metrics = [accuracy_score, f1_micro, f1_macro]

# credit
credit = load_tables(load_dataset('credit'))['credit']
credit_train, credit_test = train_test_split(credit, test_size=20/(264+20), stratify=credit['label'], random_state=2022)
credit_train.name = 'credit'
credit_classifiers = [BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryMLPClassifier]
credit_metrics = [f1_none, accuracy_score]

# intrusion
intrusion = load_tables(load_dataset('intrusion'))['intrusion']
intrusion_train, intrusion_test = train_test_split(intrusion, test_size=100/(394+100), stratify=intrusion['label'], random_state=2022)
intrusion_train.name = 'intrusion'
intrusion_classifiers = [BinaryDecisionTreeClassifier,BinaryMLPClassifier]
intrusion_metrics = [accuracy_score, f1_micro, f1_macro] 

# mnist12
mnist12 = load_tables(load_dataset('mnist12'))['mnist12']
mnist12_train, mnist12_test = train_test_split(mnist12, test_size=10/(60+10), stratify=mnist12['label'], random_state=2022)
mnist12_train.name = 'mnist12'
mnist12_classifiers = [BinaryDecisionTreeClassifier,BinaryLogisticRegression,BinaryMLPClassifier]
mnist12_metrics = [accuracy_score, f1_micro, f1_macro]

# mnist28
mnist28 = load_tables(load_dataset('mnist28'))['mnist28']
mnist28_train, mnist28_test = train_test_split(mnist28, test_size=10/(60+10), stratify=mnist28['label'], random_state=2022)
mnist28_train.name = 'mnist28'
mnist28_classifiers = [BinaryDecisionTreeClassifier,BinaryLogisticRegression,BinaryMLPClassifier]
mnist28_metrics = [accuracy_score, f1_micro, f1_macro]

# news
news = load_tables(load_dataset('news'))['news']
news_train, news_test = train_test_split(news, test_size=8/(31+8), random_state=2022)
news_train.name = 'news'
news_classifiers = [MLPRegressor, LinearRegression]
news_metrics = [r2_score]