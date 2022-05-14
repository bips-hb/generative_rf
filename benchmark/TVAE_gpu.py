try:
    exec(open("cpu_gpu.py").read())
except:
    pass

exec(open("cpu_gpu.py").read())

from sdv.tabular import TVAE

pd.concat(run_sub(synthesizer_name = "TVAE_gpu")).to_csv("TVAE_gpu.csv")
