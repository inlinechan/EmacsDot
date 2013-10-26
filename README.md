## Prerequisites

1. install git, svn, hg

        $ sudo apt-get install git mercurial subversion
        $ sudo apt-get install markdown               # for markdown-preview
        $ sudo apt-get install texinfo                # for magit
        $ sudo apt-get install python-virtualenv      # for jedi
        $ sudo apt-get install clang                  # for flymake C++

1. Make sure that you're connected on the internet

## Clone and be ready

        $ cd $HOME
        $ mv .emacs.d .emacs.d.old
        $ git clone git://github.com/inlinechan/EmacsDot2.git
        $ mkdir .emacs.d -p
        $ cd .emacs.d
        $ ln -s ../EmacsDot2/init.el .
        $ ln -s ../EmacsDot2/el-get-user .
        $ ln -s ../EmacsDot2/el-get-init-files .
        $ ln -s ../EmacsDot2/hc .

## Run

        $ emacs
