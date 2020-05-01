
# coding: utf-8

# In[ ]:

# %load "/nfs/science/users/xiaojiez/Python/Step1-Model-template.py"


# In[6]:

### Save to a python script
# !jupyter nbconvert  Step1-Model-template.ipynb --to script


# In[1]:

#%run "/Users/x644435/Documents/Good2Know/PythonLearn/Useful/UsefulFunc.py"
get_ipython().magic(u'run /nfs/science/users/xiaojiez/Python/UsefulPyFunc.py')


# #### Setting up the environment

# In[ ]:

#Import popular libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt


get_ipython().magic(u'matplotlib inline')

### Set up display options
pd.set_option('display.max_rows', 1000)
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 300) # maximum display width 
pd.set_option('display.max_colwidth',120)

import os

print '### set up working path'
currpath = "/nfs/science/users/xiaojiez"
os.chdir(currpath)
print 'Current Working Directory:', os.getcwd(), '\n', 'List of Files:', os.listdir(currpath)
# os.remove(path)  # remove a file


# In[ ]:

get_ipython().magic(u'run "/nfs/science/moo/value/churn/hshd_churn/xjz/Python/Step0-Data.py"')

