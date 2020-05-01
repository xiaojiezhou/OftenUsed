
# coding: utf-8

# In[37]:

### Save to a python script
#!jupyter nbconvert UsefulPyFunc.ipynb --to script 
# %run "/nfs/science/users/xiaojiez/Python/UsefulFunc.py"

import pandas as pd

print "Useful functions:"
print "1. convert2Categ(df, threshold=5): to convert all category variables to dummy columns"

def convert2Categ(df, threshold=5):
    if df.empty:
        return df

    cat_names = [col for col in df if len(df[col].unique()) <= threshold]
    for n in cat_names:
        df[n] = df[n].astype('category')

    return pd.get_dummies(df, columns=cat_names, drop_first=True)


print '2. flat2String(arg): to flattening arbitrary levels.'
from itertools import chain
def flat2String(arg):
    if isinstance(arg, str):
        yield arg
    else:
        for one in arg:
            for x in flat2String(one):
                 yield x
                    


# #### Function that prints out performance of binary classification

# In[ ]:

print '3. binary_perform(actual, pred): to print out binary classification performance.'
def binary_perform(actual, pred):
    from sklearn import metrics
    import pandas as pd
    print 'Total Accuracy:', round(metrics.accuracy_score(actual, pred) ,3)
    C=metrics.confusion_matrix(actual, pred) 
    C = C / C.astype(np.float).sum(axis=1)
    print 'Confusion Matrix:\n',  numpy.round_(C,3)
    print 'AUC ROC:', round(metrics.roc_auc_score(actual, pred),3)
    print 'F1:', round(metrics.f1_score(actual, pred),3)
    print 'Precision: ', round(metrics.precision_score(actual, pred),3)

    print 'Recall (True Negative):' ,round(metrics.recall_score(actual, pred),3)

    print metrics.classification_report(actual, pred)

