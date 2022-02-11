
import numpy as np
import pandas as pd
from sklearn import datasets
import genrf as grf

# Reload package to update
import importlib
importlib.reload(grf)

# Prepare iris data
iris = datasets.load_iris()
dat = pd.DataFrame(data=iris['data'], columns=iris['feature_names'])
dat["species"] = iris.target_names[iris.target]
dat["species"] = dat["species"].astype("category")

# Run generative RF
mod = grf.genrf(dat,  oob = False, dist = "normal")
mod.sample(10)
