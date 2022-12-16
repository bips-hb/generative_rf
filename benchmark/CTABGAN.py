
import os
print(os.getcwd())
os.chdir("./CTABGAN")
print(os.getcwd())
print(os.listdir())
import sys
print(sys.path)
sys.path.append("")
print(sys.path)
from model.ctabgan import CTABGAN
from model.eval.evaluation import get_utility_metrics,stat_sim,privacy_metrics
import numpy as np
import pandas as pd
import glob

os.chdir("..")
try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())


# prepare CTABGAN+ model
def ctabgan_fun(real_data):
    # find categorical columns
    cat_col = real_data.select_dtypes('object').columns.to_list()
    # find integer columns
    int_col = real_data.select_dtypes('int').columns.to_list()
    return CTABGAN( df = real_data,
    test_ratio = 0.2,
    categorical_columns = cat_col,
    integer_columns = int_col,
    problem_type = { None: None}
    )

# define synthetic data generation
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



#####################
# !!!! only one rep, choose first data instance of adult, census, etc.

rep = range(1)

print(f"CTABGAN+ sucessfully initialized, number of reps is {rep}")


# adult
#adult_res = run_benchmark(training_data= adult_train, test_data = adult_test, classifiers= adult_classifiers, 
#metrics= adult_metrics, data_synthesizer= {"CTABGAN+": ctabgan_fun})
#pd.concat(adult_res).to_csv("CTABGAN_adult.csv")

# census
#census_res = run_benchmark(training_data = census_train, test_data = census_test, classifiers = census_classifiers,
#metrics = census_metrics, data_synthesizer = {"CTABGAN+": ctabgan_fun})
#pd.concat(census_res).to_csv("CTABGAN_census.csv")

# credit
#credit_res = run_benchmark(training_data= credit_train, test_data= credit_test, classifiers= credit_classifiers,
#metrics= credit_metrics, data_synthesizer= {"CTABGAN+": ctabgan_fun})
#pd.concat(credit_res).to_csv("CTABGAN_credit.csv")


# covtype
covtype_res = run_benchmark(training_data = covtype_train, test_data= covtype_test, classifiers= covtype_classifiers,
metrics= covtype_metrics, data_synthesizer= {"CTABGAN+": ctabgan_fun})
pd.concat(covtype_res).to_csv("CTABGAN_covtype.csv")


# intrusion
intrusion_res = run_benchmark(training_data= intrusion_train, test_data= intrusion_test, classifiers= intrusion_classifiers,
metrics= intrusion_metrics, data_synthesizer= {"CTABGAN+": ctabgan_fun})
pd.concat(intrusion_res).to_csv("CTABGAN_intrusion.csv")

#-----------------
## some notes
# --------------
# IMPORTANT note: to sample data points, CTABGAN needs at least 2 different levels, 
# otherwise sampling never finishes -- take this into account when subsampling!
#syn = ctabgan_fun(real_data=dd)
#syn.fit()
#syn.generate_samples()

#dd =intrusion_train[1].iloc[1:500,1:7]
#dd.select_dtypes('object').columns.to_list()
#dd.select_dtypes('int').columns.to_list()

# another IMPORTANT note 
# CTABGAN leaves dead processes lying around 
# one needs to clean them afterwards!
