try:
    exec(open("cpu_gpu.py").read())
except:
    pass

exec(open("cpu_gpu.py").read())


pd.concat(run_sub(synthesizer_dict = {"oracle": oracle()})).to_csv("oracle_cpu.csv")

