#! /usr/bin/env bash
if [ $# == 0 ]; then
    echo "usage: $1 <workdir>"
    exit 1
fi

pip --version &> /dev/null
if  [ $? != 0 ]; then
    echo "Python not found."
    exit 1
fi

rime --version &> /dev/null
if [ $? != 0 ]; then
    echo "Installing rime..."
    pip install -U git+https://github.com/icpc-jag/rime || exit 1
fi

emacs --script install-packages.el || exit 1
mkdir -p ~/.emacs.d
echo "(require 'org)" >> ~/.emacs.d/init.el
echo "(require 'multi-term)" >> ~/.emacs.d/init.el
echo "(load \"$(pwd)/eidea.el\")" >> ~/.emacs.d/init.el

ls $1/.git/ &> /dev/null
if [ $? != 0 ]; then
    CUR_DIR=$(pwd)
    mkdir -p $1
    cd $1
    rime_init --git || exit 1
    cd $CUR_DIR
fi

echo "(setq eidea/workdir \"$1\")" >> ~/.emacs.d/init.el
echo "Configure eidea successfully."
