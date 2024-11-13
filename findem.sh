#!/bin/sh
find .. -name '*.csv' | grep -v 'pythcomb' | sort > paths.txt
