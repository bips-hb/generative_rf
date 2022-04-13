try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())
from sdv.tabular import TVAE
import numpy as np
import torch

# adult
np.random.seed(2022)
torch.manual_seed(2022)
adult_res = scores(data_train = adult_train, data_test = adult_test, list_of_classifiers = adult_classifiers, 
metric = adult_metrics, synthesizer = {"TVAE": TVAE()})
adult_res.to_csv("TVAE_adult.csv")

# census
np.random.seed(2022)
torch.manual_seed(2022)
census_res = scores(data_train = census_train, data_test = census_test, list_of_classifiers = census_classifiers,
metric = adult_metrics, synthesizer = {"TVAE": TVAE()})
census_res.to_csv("TVAE_census.csv")
# covtype
np.random.seed(2022)
torch.manual_seed(2022)
covtype_res = scores(data_train = covtype_train, data_test = covtype_test, list_of_classifiers = covtype_classifiers,
metric = covtype_metrics, synthesizer = {"TVAE": TVAE()})
covtype_res.to_csv("TVAE_covtype.csv")
# credit
np.random.seed(2022)
torch.manual_seed(2022)
credit_res = scores(data_train = credit_train, data_test = credit_test, list_of_classifiers = credit_classifiers,
metric = credit_metrics, synthesizer = {"TVAE": TVAE()})
credit_res.to_csv("TVAE_credit.csv")
# intrusion
np.random.seed(2022)
torch.manual_seed(2022)
intrusion_res = scores(data_train = intrusion_train, data_test = intrusion_test, list_of_classifiers = intrusion_classifiers,
metric = intrusion_metrics, synthesizer = {"TVAE": TVAE()})
intrusion_res.to_csv("TVAE_intrusion.csv")
# mnist12
np.random.seed(2022)
torch.manual_seed(2022)
mnist12_res = scores(data_train = mnist12_train, data_test = mnist12_test, list_of_classifiers = mnist12_classifiers,
metric = mnist12_metrics, synthesizer = {"TVAE": TVAE()})
mnist12_res.to_csv("TVAE_mnist12.csv")
# mnist28
np.random.seed(2022)
torch.manual_seed(2022)
mnist12_res = scores(data_train = mnist28_train, data_test = mnist28_test, list_of_classifiers = mnist28_classifiers,
metric = mnist28_metrics, synthesizer = {"TVAE": TVAE()})
mnist28_res.to_csv("TVAE_mnist28.csv")
# news 
np.random.seed(2022)
torch.manual_seed(2022)
news_res = scores(data_train = news_train, data_test = news_test, list_of_classifiers = news_classifiers,
metric = news_metrics, synthesizer = {"TVAE": TVAE()})
news_res.to_csv("TVAE_news.csv")