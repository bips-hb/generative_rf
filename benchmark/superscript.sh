#python grf_cpu.py
#python TVAE_gpu.py
#python TVAE_cpu.py
#python CTGAN_gpu.py
#python CTGAN_cpu.py
source /home/blesch/miniconda3/etc/profile.d/conda.sh
conda activate ctabgan
conda info
python CTABGAN.py 
conda activate gan
conda info
python RCCGAN.py
conda activate itgan
conda info
python ITGAN_adult.py
timeout 24h python ITGAN_census.py
timeout 24h python ITGAN_credit.py