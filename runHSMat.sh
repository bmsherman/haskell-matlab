#!/usr/bin/env bash

export PATH=$HOME/.local/bin:$MATLAB_PATH/bin:$PATH

LD_LIBRARY_PATH=$MATLAB_PATH/bin/glnxa64:$MATLAB_PATH/sys/os/glnxa64:$LD_LIBRARY_PATH "$@"
