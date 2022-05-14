try:
    exec(open("cpu_gpu.py").read())
except:
    pass

exec(open("cpu_gpu.py").read())

from sdv.tabular import TVAE

pd.concat(run_sub(synthesizer_dict = {"TVAE": TVAE(cuda=True)})).to_csv("TVAE_gpu.csv")
