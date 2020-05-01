How do I look inside a Python object?  
    
    type()
    dir()
    id()
    getattr()
    hasattr()
    globals()
    locals()
    callable()
    
    myobject.__dict__
    
    help(myobject)
    
    .shape.eval()
    
How to run shell command within Python?

    import subprocess
    subprocess.call('ls')
    subprocess.call('python mypython.py')

How to create .pyc file?  

    python -m myfile.py
    or "python -m compileall ." to compile .py files recursively in all sub directories
    
How to run separate python files?

    Step 1:  generate myfile.pyc (See above)
    Step 2:  from myfile import my_function
    
Theano shared variable:

    Theano variable shape.eval()
    .get_value()
    .set_value()
    
Theano shared datasets:

    shared_dataset(test_set)
    
Remove indentation: "<shift> + <tab>"