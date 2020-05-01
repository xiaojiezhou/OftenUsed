
# coding: utf-8

# #### Python code that (recursively) zips the content of the Jupiter folder you're working on.
# 
# It can be used to create a zip file of coursera course materials
# 
# * Create a notebook and run code below
# 
# * Hit "File/Open" menu item to find a .zip file having the same name of the folder (e.g.: "Logistic Regression with a Neural Network mindset.zip", "Planar data classification with one hidden layer.zip", and so on) 
# 
# * Download the zip file
# 
# https://www.coursera.org/learn/neural-networks-deep-learning/discussions/all/threads/-dxINIy9EeeLNRKDXdRmQA

# In[ ]:

import os
import zipfile

def zipdir(path, ziph, zipfilename, DEBUG = 0):
    for root, dirs, files in os.walk(path):
        for file in files:
            if DEBUG > 0:
                print(root + '/' + file)
                
            if zipfilename == file:
                if DEBUG > 0:
                    print("skip")
                continue 
                
            ziph.write(os.path.join(root, file))

def packit():
    rootdirname = '.'
    zipfilename = os.getcwd().split(os.sep)[-1]+'.zip'
    if os.path.isfile(zipfilename):
        os.remove(zipfilename)
    zipf = zipfile.ZipFile(zipfilename, 'w', zipfile.ZIP_DEFLATED)
    zipdir(rootdirname, zipf, zipfilename)
    zipf.close()
    print("Paking done.")
    
packit()


# In[4]:

#!jupyter nbconvert RecursaveZip.ipynb --to script 
get_ipython().system(u'jupyter nbconvert RecursaveZip.ipynb --to script ')

