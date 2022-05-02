try:
    exec(open("cpu_gpu.py").read())
except:
    pass

exec(open("cpu_gpu.py").read())

from sdv.tabular import CTGAN

sub_0 = run_CTGAN_cpu_sub(i = 0,  synthesizer_dict =  {"CTGAN": CTGAN(cuda=False)} )
sub_0.to_csv("CTGAN_cpu-0.csv")

sub_1 = run_CTGAN_cpu_sub(i = 1, synthesizer_dict =  {"CTGAN": CTGAN(cuda=False)} )
sub_1.to_csv("CTGAN_cpu-1.csv")

sub_2 = run_CTGAN_cpu_sub(i = 2, synthesizer_dict =  {"CTGAN": CTGAN(cuda=False)} )
sub_2.to_csv("CTGAN_cpu-2.csv")

sub_3 = run_CTGAN_cpu_sub(i = 3, synthesizer_dict =  {"CTGAN": CTGAN(cuda=False)} )
sub_3.to_csv("CTGAN_cpu-3.csv")

sub_4 = run_CTGAN_cpu_sub(i = 4, synthesizer_dict =  {"CTGAN": CTGAN(cuda=False)} )
sub_4.to_csv("CTGAN_cpu-4.csv")


