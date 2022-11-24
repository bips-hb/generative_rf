import os
print(os.getcwd())
os.chdir("/home/blesch/generative_RF/CTAB-GAN-Plus")
print(os.getcwd())
from model.ctabgan import CTABGAN
from model.eval.evaluation import get_utility_metrics,stat_sim,privacy_metrics
import numpy as np
import pandas as pd
import glob

os.chdir("../generative_rf/benchmark")
try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())



# TO DO - automize detection of categorical variables and pass list in arguments
def ctabgan_fun(real_data): 
    return CTABGAN(df = real_data,
                 test_ratio = 0.20,
                 categorical_columns = ['workclass', 'education', 'marital-status', 'occupation', 'relationship', 'race', 'sex', 'native-country', 'label'], 
                 log_columns = [],
                 mixed_columns= {'capital-loss':[0.0],'capital-gain':[0.0]},
                 general_columns = ["age"],
                 non_categorical_columns = [],
                 integer_columns = ['age', 'fnlwgt','capital-gain', 'capital-loss','hours-per-week'],
                 problem_type= {"Classification": 'label'})


def synth_data(data_train, synthesizer):
    """
    Arguments: 
    @data_train: data to learn synthesizer from
    @synthesizer: model for generating synthetic data
    Return: synthesized data of size data_train
    """
    if synthesizer == ctabgan_fun:
        syn = ctabgan_fun(real_data = data_train)
        syn.fit()
        return syn.generate_samples()
    else: 
        print("please use CTABGAN synthesizer")

adult_res = run_benchmark(training_data= adult_train, test_data = adult_test, classifiers= adult_classifiers, 
metrics= adult_metrics, data_synthesizer= {"CTABGAN+": ctabgan_fun})
pd.concat(adult_res).to_csv("CTABGAN_adult.csv")