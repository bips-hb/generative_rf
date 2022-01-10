
import numpy as np
import pandas as pd
from itertools import repeat
import rpy2.robjects as robjects
from rpy2.robjects import pandas2ri
from sdv import Metadata
from sdv.evaluation import evaluate
import sdgym

######################################
# Define relevant functions 
######################################

# call generative_ranger.R in python
r = robjects.r
r.source('generative_ranger.R')
generative_ranger = robjects.globalenv['generative_ranger']
pandas2ri.activate()

# define generative_ranger data synthesizer that works with the SDV framework
def grf_synthesizer(real_data : {"data_name": pd.DataFrame}, metadata: "sdv.Metadata object"):
    """
    generative ranger data synthesizer
    """
    table_name = metadata.get_tables()[0]
    synthetic_data = generative_ranger(x_real =real_data[table_name], n_new = real_data[table_name].shape[0])
    return {table_name: synthetic_data}

######################################
# Example
######################################

# generate "real" data
my_dat = pd.DataFrame(np.random.normal(loc = np.zeros(5), scale=1, size=[50,5]))
my_dat.columns = ['A', 'B', 'C', 'D', 'E']

# add a column with categorical data -> does not work with grf_synthesizer yet! -> double check .dtype conversion in rpy2
# my_dat['F'] = np.repeat(['a', 'b'],25)

# generate a metadata file for my_dat
metadata = Metadata()
metadata.add_table(name='example', data=my_dat)

# apply generative ranger data synthesizer 
grf_synthesizer(real_data={"example": my_dat}, metadata=metadata)

#######################################
# Integration into SDV evaluation framework
# gives useful metrics, e.g. KL divergence for given real and synthetics data
#######################################

my_real_data = my_dat
my_synthetic_data  = grf_synthesizer(real_data={"example": my_dat}, metadata=metadata)

evaluate(real_data= my_real_data, synthetic_data= my_synthetic_data['example'], aggregate=False)

######################################
# Integration into SDV benchmark framework
# CAUTION: so far, issues with name conversion -> not working properly
# CAUTION: so far, generative_ranger works with continuous data only! -> probably some .dtype issue in rpy2 
######################################

# import other synthesizers for comparison
# full list of available synthesizers: https://github.com/sdv-dev/SDGym/blob/master/SYNTHESIZERS.md
from sdgym.synthesizers import Identity, CTGAN

# get list of available datasets
from sdgym.datasets import get_available_datasets
get_available_datasets()

# this should work after solving the names passing issue python->R
sdgym.run(synthesizers=[grf_synthesizer, Identity, CTGAN], datasets=['ring'])

##### DEBUGGING 
# try to solve columns names issue

from sdgym.datasets import load_dataset
metadata = load_dataset('adult')
from sdgym.datasets import load_tables
tables = load_tables(metadata)
table_name = metadata.get_tables()[0]

ex = tables[table_name]
ex_r = pandas2ri.py2rpy(ex)

## some useful notes 
    # conversion Python to R: dat_pd_rpy = pandas2ri.py2rpy(pd.DataFrame(real_data)) 
        # get data from dict[str, pd.DataFrame]: dat_pd = pd.DataFrame(list(real_data.items())[0][1]) 
