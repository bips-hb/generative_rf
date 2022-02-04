import pandas as pd
import numpy as np
from sklearn.model_selection import train_test_split
from random import sample
from sdgym.datasets import load_dataset
from sdgym.datasets import load_tables
from sdv.tabular import CTGAN, CopulaGAN, GaussianCopula, TVAE
from sdv.metrics.tabular import BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryLogisticRegression,BinaryMLPClassifier
from sklearn.metrics import f1_score, accuracy_score, r2_score
import rpy2
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
#import multiprocessing
#from multiprocessing import Pool
#multiprocessing.set_start_method('spawn')

######################################
# Define relevant functions 
######################################

# call generative_ranger.R in python
r = robjects.r
r.source('generative_ranger.R')
generative_ranger = robjects.globalenv['generative_ranger']
pandas2ri.activate()

def gen_rf(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0])
    return grf_syn_dat.astype(real_data.dtypes)
    



#from sdgym.synthesizers import (CLBN, CopulaGAN, CTGAN, Identity, MedGAN, PrivBN, TableGAN, VEEGAN)

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
    def __init__(self, dataset, test_size):
        """
        Args: 
        dataset: real dataset, spefiy one data set from Xu et al. 2020 Table 4,5
        test_size: train/test split as defined in Xu et al. 2020 Table 4
        
        """
        self.metadata = load_dataset(dataset)
        #self.real_data = load_tables(self.metadata)[dataset]# use this line for full data set
        full_dat = load_tables(self.metadata)[dataset]
        self.real_data = full_dat.loc[np.random.choice(full_dat.index, 10000, replace=False)] # subset of 200 obs.
        self.real_data_train, self.real_data_test = train_test_split(self.real_data, test_size=test_size, random_state=2022)
    def synth_data(self, model):
        """ 
        Returns: synthesized data of size real_data_train
        """     
        if model == Identity():
            return self.real_data_train.copy()
        elif model == gen_rf:
            return gen_rf(real_data = self.real_data_train)
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


#ex = benchmark(dataset= 'census', test_size=10/33)
#ex.synth_data(model=gen_rf)
#ex.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,
#BinaryLogisticRegression,BinaryMLPClassifier],metric=[accuracy_score, f1_none], dict_of_syn_models={"ID":Identity(), "grf": gen_rf, "CTGAN":CTGAN(), "TVAE": TVAE()})

#adult = benchmark(dataset= 'adult', test_size=10/33)
#result_adult = ex.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,
#BinaryLogisticRegression,BinaryMLPClassifier],metric=[accuracy_score, f1_none], dict_of_syn_models={"Identity":Identity(), "CTGAN":CTGAN(), "TVAE": TVAE()})

full_dict_of_syn_models ={"ID":Identity(), "grf": gen_rf, "CTGAN":CTGAN(), "TVAE": TVAE()}

adult = benchmark(dataset= 'adult', test_size=10/(23+10))
result_adult = adult.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,
BinaryLogisticRegression,BinaryMLPClassifier],metric=[accuracy_score, f1_none], dict_of_syn_models={"ID":Identity(), "grf": gen_rf} )

census = benchmark(dataset='census', test_size=100/(200+100))
# census contains NA values -> drop these rows
census.real_data_test = census.real_data_test.dropna(axis=0, how='any', thresh=None, subset=None, inplace=False)
census.real_data_train = census.real_data_train.dropna(axis=0, how='any', thresh=None, subset=None, inplace=False)
result_census = census.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryMLPClassifier],metric=[accuracy_score, f1_none], dict_of_syn_models= 
{"ID":Identity(), "grf": gen_rf, "CTGAN":CTGAN(), "TVAE": TVAE()})

credit = benchmark(dataset='credit', test_size=20/(264+20))
result_credit = credit.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryMLPClassifier],metric=[accuracy_score, f1_none], dict_of_syn_models= )

covtype = benchmark(dataset='covtype', test_size=100/(481+100))
result_covtype = covtype.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryMLPClassifier],metric=[accuracy_score, f1_micro, f1_macro], dict_of_syn_models= 
{"ID":Identity(), "grf": gen_rf, "CTGAN":CTGAN(), "TVAE": TVAE()})

mnist12 = benchmark(dataset='mnist12', test_size=10/(60+10))
result_mnist12 = mnist12.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryLogisticRegression,BinaryMLPClassifier],metric=[accuracy_score, f1_micro, f1_macro], dict_of_syn_models= 
{"ID":Identity(), "grf": gen_rf, "CTGAN":CTGAN(), "TVAE": TVAE()})

mnist28 = benchmark(dataset='mnist28', test_size=10/(60+10))
result_mnist28 = mnist28.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryLogisticRegression,BinaryMLPClassifier],metric=[accuracy_score, f1_micro, f1_macro], dict_of_syn_models=
{"ID":Identity(), "grf": gen_rf, "CTGAN":CTGAN(), "TVAE": TVAE()} )

intrusion = benchmark(dataset='intrusion', test_size=100/(394+100))
result_intrusion = intrusion.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryMLPClassifier],metric=[accuracy_score, f1_micro, f1_macro], dict_of_syn_models= )


result_mnist28.to_csv("/Users/kristinblesch/Desktop/BIPS/BIPS_inhalt/generative_RF/result_mnist28_10000.csv")

#############
list_of_classif = [BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,
BinaryLogisticRegression,BinaryMLPClassifier]

############ some notes 
def wrapper_classif(arg):
    return adult.scores(list_of_classifiers=[arg],metric=[accuracy_score, f1_none], dict_of_syn_models={"Identity":Identity(), "grf": gen_rf})

import multiprocess
from multiprocess import Pool
#multiprocess.set_start_method('spawn')
if __name__ == '__main__':
    with Pool(5) as p:
        print(p.map(wrapper_classif,list_of_classif))
pool_obj = Pool(3)
#res = pool_obj.map(wrapper_classif,list_of_classif)
#print(res)

