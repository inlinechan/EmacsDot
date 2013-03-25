## Prerequisites

1. install git, svn, hg

    sudo apt-get install git mercurial subversion

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
