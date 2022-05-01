try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())


import numpy as np
import torch
from os import path
import rpy2
import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages
from rpy2.robjects import pandas2ri


#basepath = path.dirname("__file__")
#filepath = path.abspath(path.join(basepath, "..", "genrf.py"))
#exec(open(filepath, "r").read())
#exec(open("benchmark_individual.py").read())
base = rpackages.importr('base')
r = robjects.r
#r.source('../generative_ranger.R')
r.source('/home/blesch/generative_RF/generative_rf/generative_ranger.R')
generative_ranger = robjects.globalenv['generative_ranger']
pandas2ri.activate()
doPar = rpackages.importr('doParallel')
doPar.registerDoParallel(40)



# define generative_ranger version in python 
def gen_rf(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = False, num_trees = 10, min_node_size = 5 )
    return grf_syn_dat.astype(real_data.dtypes)

# code data synthesizer 
def synth_data(data_train, synthesizer):
    """
    Arguments:
    @data_train: data to learn synthesizer from
    @synthesizer: model for generating synthetic data
    Return: synthesized data of size data_train
    """     
    if synthesizer == gen_rf:
        return gen_rf(real_data = data_train)
    else: 
        synthesizer.fit(data = data_train)
        return synthesizer.sample(data_train.shape[0])  


def run_grf_benchmark(i, training_data, test_data, classifiers, metrics, data_synthesizer, seed = 2022):
    np.random.seed(seed)
    torch.manual_seed(seed)
    base.set_seed(2022,kind = "L'Ecuyer-CMRG")
    return scores(data_train = training_data[i], data_test = test_data[i], list_of_classifiers = classifiers, 
    metric = metrics, synthesizer = data_synthesizer)

# adult
#adult_res = run_grf_benchmark(i = 0, training_data= adult_train, test_data = adult_test, classifiers= adult_classifiers, 
#metrics= adult_metrics, data_synthesizer= {"gen_rf": gen_rf})
#adult_res.to_csv("grf_adult-0.csv")

# census
#census_res = run_grf_benchmark(training_data= census_train, test_data= census_test, classifiers= census_classifiers,
#metrics = census_metrics, data_synthesizer= {"gen_rf": gen_rf})
#pd.concat(census_res).to_csv("grf_census.csv")

# covtype
covtype_res = run_grf_benchmark(i = 0, training_data= covtype_train, test_data= covtype_test, classifiers= covtype_classifiers,
metrics = covtype_metrics, data_synthesizer= {"gen_rf": gen_rf})
covtype_res.to_csv("grf_covtype-0.csv")
# covtype
covtype_res = run_grf_benchmark(i =1, training_data= covtype_train, test_data= covtype_test, classifiers= covtype_classifiers,
metrics = covtype_metrics, data_synthesizer= {"gen_rf": gen_rf})
covtype_res.to_csv("grf_covtype-1.csv")
# covtype
covtype_res = run_grf_benchmark(i = 2, training_data= covtype_train, test_data= covtype_test, classifiers= covtype_classifiers,
metrics = covtype_metrics, data_synthesizer= {"gen_rf": gen_rf})
covtype_res.to_csv("grf_covtype-2.csv")
# covtype
covtype_res = run_grf_benchmark(i = 3, training_data= covtype_train, test_data= covtype_test, classifiers= covtype_classifiers,
metrics = covtype_metrics, data_synthesizer= {"gen_rf": gen_rf})
covtype_res.to_csv("grf_covtype-3.csv")
# covtype
covtype_res = run_grf_benchmark(i = 4, training_data= covtype_train, test_data= covtype_test, classifiers= covtype_classifiers,
metrics = covtype_metrics, data_synthesizer= {"gen_rf": gen_rf})
covtype_res.to_csv("grf_covtype-4.csv")

# credit
credit_res = run_grf_benchmark(i = 0, training_data= credit_train, test_data= credit_test, classifiers= credit_classifiers,
metrics= credit_metrics, data_synthesizer= {"gen_rf": gen_rf})
credit_res.to_csv("grf_credit-0.csv")
# credit
credit_res = run_grf_benchmark(i = 1, training_data= credit_train, test_data= credit_test, classifiers= credit_classifiers,
metrics= credit_metrics, data_synthesizer= {"gen_rf": gen_rf})
credit_res.to_csv("grf_credit-1.csv")
# credit
credit_res = run_grf_benchmark(i = 2, training_data= credit_train, test_data= credit_test, classifiers= credit_classifiers,
metrics= credit_metrics, data_synthesizer= {"gen_rf": gen_rf})
credit_res.to_csv("grf_credit-2.csv")
# credit
credit_res = run_grf_benchmark(i = 3, training_data= credit_train, test_data= credit_test, classifiers= credit_classifiers,
metrics= credit_metrics, data_synthesizer= {"gen_rf": gen_rf})
credit_res.to_csv("grf_credit-3.csv")
# credit
credit_res = run_grf_benchmark(i = 4, training_data= credit_train, test_data= credit_test, classifiers= credit_classifiers,
metrics= credit_metrics, data_synthesizer= {"gen_rf": gen_rf})
credit_res.to_csv("grf_credit-4.csv")


# intrusion
intrusion_res = run_grf_benchmark(i = 0, training_data = intrusion_train, test_data= intrusion_test, classifiers= intrusion_classifiers,
metrics= intrusion_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(intrusion_res).to_csv("grf_intrusion-0.csv")
# intrusion
intrusion_res = run_grf_benchmark(i = 1, training_data = intrusion_train, test_data= intrusion_test, classifiers= intrusion_classifiers,
metrics= intrusion_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(intrusion_res).to_csv("grf_intrusion-1.csv")
# intrusion
intrusion_res = run_grf_benchmark(i = 2, training_data = intrusion_train, test_data= intrusion_test, classifiers= intrusion_classifiers,
metrics= intrusion_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(intrusion_res).to_csv("grf_intrusion-2.csv")
# intrusion
intrusion_res = run_grf_benchmark(i = 3, training_data = intrusion_train, test_data= intrusion_test, classifiers= intrusion_classifiers,
metrics= intrusion_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(intrusion_res).to_csv("grf_intrusion-3.csv")
# intrusion
intrusion_res = run_grf_benchmark(i = 4, training_data = intrusion_train, test_data= intrusion_test, classifiers= intrusion_classifiers,
metrics= intrusion_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(intrusion_res).to_csv("grf_intrusion-4.csv")


# mnist12
mnist12_res = run_grf_benchmark(i = 0, training_data= mnist12_train, test_data= mnist12_test, classifiers= mnist12_classifiers,
metrics= mnist12_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(mnist12_res).to_csv("grf_mnist12-0.csv")

# mnist12
mnist12_res = run_grf_benchmark(i = 1, training_data= mnist12_train, test_data= mnist12_test, classifiers= mnist12_classifiers,
metrics= mnist12_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(mnist12_res).to_csv("grf_mnist12-1.csv")

# mnist12
mnist12_res = run_grf_benchmark(i = 2, training_data= mnist12_train, test_data= mnist12_test, classifiers= mnist12_classifiers,
metrics= mnist12_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(mnist12_res).to_csv("grf_mnist12-2.csv")

# mnist12
mnist12_res = run_grf_benchmark(i = 3, training_data= mnist12_train, test_data= mnist12_test, classifiers= mnist12_classifiers,
metrics= mnist12_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(mnist12_res).to_csv("grf_mnist12-3.csv")

# mnist12
mnist12_res = run_grf_benchmark(i = 4, training_data= mnist12_train, test_data= mnist12_test, classifiers= mnist12_classifiers,
metrics= mnist12_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(mnist12_res).to_csv("grf_mnist12-4.csv")


# mnist28
mnist28_res = run_grf_benchmark(i = 0, training_data= mnist28_train, test_data= mnist28_test, classifiers= mnist28_classifiers,
metrics= mnist28_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(mnist28_res).to_csv("grf_mnist28-0.csv")
# mnist28
mnist28_res = run_grf_benchmark(i = 1, training_data= mnist28_train, test_data= mnist28_test, classifiers= mnist28_classifiers,
metrics= mnist28_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(mnist28_res).to_csv("grf_mnist28-1.csv")
# mnist28
mnist28_res = run_grf_benchmark(i = 2, training_data= mnist28_train, test_data= mnist28_test, classifiers= mnist28_classifiers,
metrics= mnist28_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(mnist28_res).to_csv("grf_mnist28-2.csv")
# mnist28
mnist28_res = run_grf_benchmark(i = 3, training_data= mnist28_train, test_data= mnist28_test, classifiers= mnist28_classifiers,
metrics= mnist28_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(mnist28_res).to_csv("grf_mnist28-3.csv")
# mnist28
mnist28_res = run_grf_benchmark(i = 4, training_data= mnist28_train, test_data= mnist28_test, classifiers= mnist28_classifiers,
metrics= mnist28_metrics, data_synthesizer= {"gen_rf": gen_rf})
pd.concat(mnist28_res).to_csv("grf_mnist28-4.csv")

