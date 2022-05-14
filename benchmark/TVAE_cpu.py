try:
    exec(open("cpu_gpu.py").read())
except:
    pass

exec(open("cpu_gpu.py").read())

from sdv.tabular import TVAE

pd.concat(run_sub(synthesizer_dict = {"TVAE": TVAE(cuda=False)})).to_csv("TVAE_cpu.csv")
