

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
    def __init__(self, dataset, test_size, subsample = 10000, balanced_census = False):
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
        if balanced_census == True:
            # make subsample balanced - labels '- 50000' or '50000+'
            ones = full_dat.index[full_dat['label'] == '- 50000']
            zeros = full_dat.index[full_dat['label'] == '50000+']
            real_dat_raw = full_dat.loc[np.append(np.random.choice(ones, int(subsample*0.5), replace = False),
            np.random.choice(zeros, int(subsample*0.5), replace=False))]
            self.real_data = real_dat_raw.sample(frac=1).reset_index(drop=True) # shuffle rows
            self.real_data_train, self.real_data_test = train_test_split(self.real_data, test_size=test_size, random_state=2022)
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



#############

adult = benchmark(dataset= 'adult', test_size=10/(23+10))
result_adult = adult.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,
BinaryLogisticRegression,BinaryMLPClassifier],metric=[accuracy_score, f1_none], dict_of_syn_models={"ID":Identity(), "grf": gen_rf, "grf_oob": gen_rf_oob,  "CTGAN":CTGAN(), "TVAE": TVAE()} )
result_adult = result_adult.groupby('model').apply(np.mean)
result_adult.rename(columns=lambda x: 'adult.' + x, inplace=True)
result_adult

census = benchmark(dataset='census', test_size=100/(200+100))
# census contains NA values -> drop these rows
census.real_data_test = census.real_data_test.dropna(axis=0, how='any', thresh=None, subset=None, inplace=False)
census.real_data_train = census.real_data_train.dropna(axis=0, how='any', thresh=None, subset=None, inplace=False)
result_census = census.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryMLPClassifier],metric=[accuracy_score, f1_none], dict_of_syn_models= 
{"ID":Identity(), "grf": gen_rf,  "grf_oob": gen_rf_oob, "CTGAN":CTGAN(), "TVAE": TVAE()})
result_census = result_census.groupby('model').apply(np.mean)
result_census.rename(columns=lambda x: 'census.' + x, inplace=True)
result_census

credit = benchmark(dataset='credit', test_size=20/(264+20))
result_credit = credit.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryAdaBoostClassifier,BinaryMLPClassifier],metric=[accuracy_score, f1_none], dict_of_syn_models= 
{"ID":Identity(), "grf": gen_rf,  "grf_oob": gen_rf_oob, "CTGAN":CTGAN(), "TVAE": TVAE()})
result_credit = result_credit.groupby('model').apply(np.mean)
result_credit.rename(columns=lambda x: 'credit.' + x, inplace=True)
result_credit

covtype = benchmark(dataset='covtype', test_size=100/(481+100))
result_covtype = covtype.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryMLPClassifier],metric=[accuracy_score, f1_micro, f1_macro], dict_of_syn_models= 
{"ID":Identity(), "grf": gen_rf,  "grf_oob": gen_rf_oob, "CTGAN":CTGAN(), "TVAE": TVAE()})
result_covtype = result_covtype.groupby('model').apply(np.mean)
result_covtype.rename(columns=lambda x: 'covtype.' + x, inplace=True)
result_covtype

intrusion = benchmark(dataset='intrusion', test_size=100/(394+100))
result_intrusion = intrusion.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryMLPClassifier],metric=[accuracy_score, f1_micro, f1_macro], dict_of_syn_models={"ID":Identity(), "grf": gen_rf,  "grf_oob": gen_rf_oob, "CTGAN":CTGAN(), "TVAE": TVAE()} )
result_intrusion = result_intrusion.groupby('model').apply(np.mean)
result_intrusion.rename(columns=lambda x: 'instrusion.' + x, inplace=True)
result_intrusion

mnist12 = benchmark(dataset='mnist12', test_size=10/(60+10))
result_mnist12 = mnist12.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryLogisticRegression,BinaryMLPClassifier],metric=[accuracy_score, f1_micro, f1_macro], dict_of_syn_models= 
{"ID":Identity(), "grf": gen_rf, "grf_oob": gen_rf_oob,  "CTGAN":CTGAN(), "TVAE": TVAE()})
result_mnist12 = result_mnist12.groupby('model').apply(np.mean)
result_mnist12.rename(columns=lambda x: 'mnist12.' + x, inplace=True)
result_mnist12

mnist28 = benchmark(dataset='mnist28', test_size=10/(60+10))
result_mnist28 = mnist28.scores(list_of_classifiers=[BinaryDecisionTreeClassifier,BinaryLogisticRegression,BinaryMLPClassifier],metric=[accuracy_score, f1_micro, f1_macro], dict_of_syn_models=
{"ID":Identity(), "grf": gen_rf, "grf_oob": gen_rf_oob,  "CTGAN":CTGAN(), "TVAE": TVAE()} )
result_mnist28 = result_mnist28.groupby('model').apply(np.mean)
result_mnist28.rename(columns=lambda x: 'mnist28.' + x, inplace=True)
result_mnist28



# final result 
#pd.concat([result_adult, result_census, result_credit, result_covtype, result_mnist12, result_intrusion], axis=1).to_csv("/Users/kristinblesch/Desktop/BIPS/BIPS_inhalt/generative_RF/result_full_10k.csv")

####################################
# # efficiency plot: vary subsample - in a for loop -> make parallel -> fit multiprocessing working later 
####################################

my_subs = [100, 250,500,1000,2000,5000,7500, 10000]*10
subsample_res = pd.DataFrame()
for i in my_subs:
    adult = benchmark(dataset= 'adult', test_size=10/(23+10), subsample=i)
    result_adult = adult.scores(list_of_classifiers=[BinaryLogisticRegression],metric=[accuracy_score], dict_of_syn_models={"ID":Identity(), "grf": gen_rf, "CTGAN": CTGAN()} )
    result_adult = result_adult.groupby('model').apply(np.mean)
    result_adult['subsample'] = i
    subsample_res = subsample_res.append(result_adult)
subsample_res

# plot result 
subsample_res['model'] = subsample_res.index
subsample_res.set_index('subsample', inplace=True)
data = subsample_res.groupby('model')['accuracy_score']
data.plot(legend=True, ylabel = "accuracy")

###################################
# efficiency plot: stratified census data set, maximum possible balanced subset ~35.000
###################################
my_subs = [100, 250,500,1000,2000,5000,7500, 10000, 20000,30000]*8
# *reps
#my_subs = [100,250]
subsample_res = pd.DataFrame()
for i in my_subs:
    census = benchmark(dataset='census', test_size=100/(200+100), subsample=i, balanced_census=True)
    census.real_data_test = census.real_data_test.dropna(axis=0, how='any', thresh=None, subset=None, inplace=False)
    census.real_data_train = census.real_data_train.dropna(axis=0, how='any', thresh=None, subset=None, inplace=False)
    result_census = census.scores(list_of_classifiers=[BinaryAdaBoostClassifier],metric=[f1_none], dict_of_syn_models= {"ID":Identity(), "grf": gen_rf, "CTGAN":CTGAN()})
  #  result_census = result_census.groupby('model').apply(np.mean)
    result_census['subsample'] = i
    subsample_res = subsample_res.append(result_census)
subsample_res


# plot result 
#subsample_res['model'] = subsample_res.index
subsample_res.set_index('subsample', inplace=True)
data = subsample_res.groupby('model')['f1_none']
data.plot(legend=True, ylabel = "accuracy")




########  TO DO: fix multiprocessing 
def par_helper_subsample(subsamples):
    adult = benchmark(dataset= 'adult', test_size=10/(23+10), subsample=subsamples)
    result_adult = adult.scores(list_of_classifiers=[BinaryLogisticRegression],metric=[accuracy_score], dict_of_syn_models={"ID":Identity(), "grf": gen_rf, "CTGAN":CTGAN(), "TVAE": TVAE()} )
    result_adult = result_adult.groupby('model').apply(np.mean)
    result_adult['subsample'] = subsamples
    return(result_adult)

if __name__ == '__main__':
    with Pool(processes = 6, maxtasksperchild = 1) as p:
        subsample_res = p.map(par_helper_subsample,my_subs)
subsample_res = pd.concat(subsample_res)


#subsample_res.to_csv("/Users/kristinblesch/Desktop/BIPS/BIPS_inhalt/generative_RF/adult_subsample_var.csv")

