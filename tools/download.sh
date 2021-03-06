#!/bin/bash

# create data folder if not exists
[ -d data ] || mkdir data

# https://archive.ics.uci.edu/ml/datasets/Covertype 581012
wget "https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.data.gz" -O $PWD"/data/covtype.data.gz"
gzip -d $PWD"/data/covtype.data.gz"
rm -rf $PWD"/data/covtype.data.gz"
wget "https://archive.ics.uci.edu/ml/machine-learning-databases/covtype/covtype.info" -O $PWD"/data/covtype.info"

# https://archive.ics.uci.edu/ml/datasets/Letter+Recognition 20000
wget "https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.data.Z" -O $PWD"/data/letter-recognition.data.Z"
gzip -d $PWD"/data/letter-recognition.data.Z"
rm -rf $PWD"/data/letter-recognition.data.Z"
wget "https://archive.ics.uci.edu/ml/machine-learning-databases/letter-recognition/letter-recognition.names" -O $PWD"/data/letter-recognition.names"

# https://archive.ics.uci.edu/ml/datasets/default+of+credit+card+clients# 30000
wget "https://archive.ics.uci.edu/ml/machine-learning-databases/00350/default%20of%20credit%20card%20clients.xls" -O $PWD"/data/default-of-credit-card-clients.xls"
ssconvert $PWD"/data/default-of-credit-card-clients.xls" $PWD"/data/default-of-credit-card-clients.csv"
tail -n +2 "data/default-of-credit-card-clients.csv" > "data/default-of-credit-card-clients.csv.tmp" && mv "data/default-of-credit-card-clients.csv.tmp" "data/default-of-credit-card-clients.csv" # remove first line
rm $PWD"/data/default-of-credit-card-clients.xls"