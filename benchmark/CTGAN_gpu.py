try:
    exec(open("cpu_gpu.py").read())
except:
    pass

exec(open("cpu_gpu.py").read())

from sdv.tabular import CTGAN

pd.concat(run_sub(synthesizer_name = "CTGAN_gpu")).to_csv("CTGAN_gpu.csv")
