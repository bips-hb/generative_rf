import os
import pandas as pd

os.chdir("/home/blesch/generative_RF/ITGAN_adjusted2")
import train_itgan as ITGAN
from util.data import load_dataset,get_metadata
os.chdir("/home/blesch/generative_RF/generative_rf/benchmark")
try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())


# prepare ITGAN model

def itgan_fun(real_data):
    # find categorical columns
    cat_cols = real_data.select_dtypes('object').columns.to_list()
    ord_cols = []
    arg = ITGAN.getArgs(data = real_data, cat_col = cat_cols, ord_col = ord_cols, GPU_NUM=1)
    return ITGAN.AEGANSynthesizer(**arg)


# define synthetic data generation
def synth_data(data_train, synthesizer):
    """
    Arguments: 
    @data_train: data to learn synthesizer from
    @synthesizer: model for generating synthetic data
    Return: synthesized data of size data_train
    """
    if synthesizer == itgan_fun:
        syn = itgan_fun(real_data = data_train)
        syn.fit()
        return syn.sample(data_train.shape[0])
    else: 
        print("please use ITGAN synthesizer")


# example
#url = 'https://raw.githubusercontent.com/sdv-dev/CTGAN/master/examples/csv/adult.csv'
#data = pd.read_csv(url)
#ex = itgan_fun(data)
#synth_data(data_train=data, synthesizer=itgan_fun)

#####################
# !!!! only one rep, choose first data instance of adult, census, etc.

rep = range(1)

# adult
adult_res = run_benchmark(training_data= adult_train, test_data = adult_test, classifiers= adult_classifiers, 
metrics= adult_metrics, data_synthesizer= {"ITGAN": itgan_fun})
pd.concat(adult_res).to_csv("ITGAN_adult.csv")

# census
census_res = run_benchmark(training_data = census_train, test_data = census_test, classifiers = census_classifiers,
metrics = census_metrics, data_synthesizer = {"ITGAN": itgan_fun})
pd.concat(census_res).to_csv("ITGAN_census.csv")

# credit
credit_res = run_benchmark(training_data= credit_train, test_data= credit_test, classifiers= credit_classifiers,
metrics= credit_metrics, data_synthesizer= {"ITGAN": itgan_fun})
pd.concat(credit_res).to_csv("ITGAN_credit.csv")

