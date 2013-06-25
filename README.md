n## Prerequisites

1. install git, svn, hg

    sudo apt-get install git mercurial subversion
    sudo apt-get install markdown               # for markdown-preview
    sudo apt-get install texinfo                # for magit
    sudo apt-get install python-virtualenv      # for jedi
    sudo apt-get install clang                  # for flymake C++
    
2. Make sure that you're connected on the internet

## Clone and be ready

    cd # move to home
    git clone git://github.com/inlinechan/EmacsDot2.git
    mv .emacs.d .emacs.d.old
    mkdir .emacs.d -p
    cd .emacs.d
    ln -s ../EmacsDot2/init.el .

## Run

    emacs

if there's an error, run emacs until no error found.
