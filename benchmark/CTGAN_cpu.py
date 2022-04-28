try:
    exec(open("cpu_gpu.py").read())
except:
    pass

exec(open("cpu_gpu.py").read())

from sdv.tabular import CTGAN

pd.concat(run_sub(synthesizer_dict = {"CTGAN": CTGAN(cuda=False)})).to_csv("CTGAN_cpu.csv")
