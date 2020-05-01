
# coding: utf-8

# ### Combined python script

# In[3]:

import sys
sys.path.append('/Users/x644435/anaconda2/lib/python2.7/site-packages/')
import  tensorflow as tf


# In[9]:

### Save to a python script
get_ipython().system(u'jupyter nbconvert --to script PyHeader.ipynb')


# In[10]:

print '### set up working path'
import os
from os import chdir, listdir

mypath = "/Users/x644435/Documents/Good2Know/PythonLearn/Pandas"
chdir(mypath)
print 'Current Working Directory:', os.getcwd(), '\n', 'List of Files:', listdir(mypath)
# os.remove(path)  # remove a file

#Import popular libraries
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt

### Set up display options
pd.set_option('display.max_rows', 1000)
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 300) # maximum display width 
pd.set_option('display.max_colwidth',120)


# In[ ]:




# In[5]:

sys.path


# In[6]:

import pandas as pd


# In[ ]:




# In[ ]:




# #### Load R data in Python

# In[ ]:

import rpy2.robjects as robjects
import pandas as pd

from rpy2.robjects import pandas2ri
from rpy2.robjects import r

robjects.r['load']("Q20152DM.RData")

df = pandas2ri.ri2py(r['Q20152DM'])
df =  pd.DataFrame(df)


# #### Variable transformation

# In[ ]:

import numpy as np
from sklearn.preprocessing import MinMaxScaler, StandardScaler


to_recode = ['TSpend20142','TSpend20143','TSpend20144','TSpend20151']

# Select numeric columns only
ndf = df.select_dtypes([np.number])
ndf.drop('HOUSEHOLD_ID', axis=1)  #drop column

# Apply log(x+1) element-wise to a subset of columns
X[to_recode] = X[to_recode].applymap(lambda x: np.log(x+1))

# Recode X_train columns to 0-1, apply the formular to X_test
X_train.is_copy = False
scaler = MinMaxScaler(feature_range=(0, 1))
X_train[to_recode] = scaler.fit_transform(X_train[to_recode])
X_test[to_recode] = scaler.transform(X_test[to_recode])


# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:




# In[ ]:



