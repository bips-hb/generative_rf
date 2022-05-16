try:
    exec(open("benchmark_individual.py").read())
except:
    pass

exec(open("benchmark_individual.py").read())
from sdv.tabular import TVAE

def f1_none(*args):
  return f1_score(average = 'binary', *args)