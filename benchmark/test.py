try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())

import numpy as np
import torch

# adult
np.random.seed(2022)
torch.manual_seed(2022)
print(adult_train.index[1:5])

print(census_train.index[1:5])