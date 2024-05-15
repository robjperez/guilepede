#!/bin/bash

GUILE_LOAD_PATH=`pwd`/src LD_LIBRARY_PATH=$LD_LIBRARY_PATH:`pwd`/lib guile src/main.scm
