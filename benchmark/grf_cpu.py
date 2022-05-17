try:
    exec(open("cpu_gpu.py").read())
except:
    pass

exec(open("cpu_gpu.py").read())

import rpy2
import rpy2.robjects as robjects
import rpy2.robjects.packages as rpackages
from rpy2.robjects import pandas2ri

base = rpackages.importr('base')
r = robjects.r
r.source('../generative_ranger.R')
generative_ranger = robjects.globalenv['generative_ranger']
pandas2ri.activate()
doPar = rpackages.importr('doParallel')
doPar.registerDoParallel(10)
# define generative_ranger version in python 
def gen_rf(real_data):
    grf_syn_dat = generative_ranger(x_real = real_data, n_new = real_data.shape[0], oob = False, num_trees = 10, min_node_size = 5 )
    return grf_syn_dat.astype(real_data.dtypes)
# redefine relevant functions such that they work with gen_rf()
def synth_data(data_train, synthesizer):
    """
    Arguments:
    @data_train: data to learn synthesizer from
    @synthesizer: model for generating synthetic data
    Return: synthesized data of size data_train
    """     
    if synthesizer == oracle():
       return data_train.copy()
    elif synthesizer == gen_rf:
        return gen_rf(real_data = data_train)
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
      elif synthesizer_name == "gen_rf":
        my_syn.append({"gen_rf": gen_rf})
      else: 
        print("please specify synthesizer name")
      i=i+1
    res = (syn_time(data = data_sub[i], synthesizer =  my_syn[i]) for i in range(len(subs)))
    return list(res)

pd.concat(run_sub(synthesizer_name= "gen_rf", R_seed = True)).to_csv("grf_cpu.csv")

