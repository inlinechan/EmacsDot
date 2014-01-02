## Prerequisites

1. Install packages

        $ sudo apt-get install git mercurial subversion
        $ sudo apt-get install markdown               # for markdown-preview
        $ sudo apt-get install texinfo                # for magit
        $ sudo apt-get install python-virtualenv      # for jedi
        $ sudo apt-get install clang                  # for flymake C++
        $ sudo apt-get install automake1.10           # for gnuplot-mode
        $ sudo apt-get install w3m cvs                # for w3m newsticker

1. Make sure that you're connected on the internet
1. Sign-in github.com in your web-browser to avoid getting html error
   page instead of .el files 

## Clone and be ready

        $ cd $HOME
        $ mv .emacs.d .emacs.d.old
        $ git clone git://github.com/inlinechan/EmacsDot2.git
        $ ln -s EmacsDot2 .emacs.d

## Run

        $ emacs                 # Run again with --debug-init if any error
