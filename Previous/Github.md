<a id="table-of-contents"></a>
## Github.md - Table of Contents
* [Git Commands](#git)
* [Markdown](#markdown)
    - [Useful Markdown links] (#markdown)
 
  



<div id='git'/>
## Git Commands

### -- To clone an Existing Repository --
     git clone https://github.com/xiaojiezhou/My-Often-Used-Files.git


### -- To start --
     git remote add origin https://github.com/xiaojiezhou/My-Often-Used-Files.git
     git pull origin master
     
### -- To commit new adds --
        git init
        git add README.md
        git add .     ---------- add all new files; 
        git add -u    ---------- update tracking for files that changed names or deleted; 
        git add -A    ---------- does both

        git commit -m "Message here"
        git push -u origin master
        
        git config --global user.name  "xxxx@gmail.com"
        git config --global user.email "xxxx@gmail.com"
        
### -- git push origin master error

    $  git push -u origin master  
    ERROR: Repository not found.

    Solution:  Modify .git/config file as follow:  
        ...
        [remote "origin"]
        url = https://github.com/xiaojiezhou/My-Often-Used-Files.git
        fetch = +refs/heads/*:refs/remotes/origin/*
        ...


[(back to top)](#table-of-contents)




<div id='markdown'/>
## Markdowns
### --  Useful links --
[GitHub Help: Markdown Basics](https://help.github.com/articles/markdown-basics/)  
[GitHub Flavored Markdown] (https://help.github.com/articles/github-flavored-markdown/)  
[GitHub Markdown Examples] (https://guides.github.com/features/mastering-markdown/#GitHub-flavored-markdown)  
[Creating Table of contents] (https://github.com/rasbt/markdown-toclify)
[(back to top)](#table-of-contents)

### -- Need to remember --
*line break*:  double space at the end of the line