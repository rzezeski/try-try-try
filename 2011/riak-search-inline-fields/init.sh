#!/bin/bash
#
#> Usage:
#>    init RIAK_PATH SCHEMA [PBC_PORT]

set -e

if [ ! -e earthquake ]
then
    echo "The tweet corpus must first be downloaded and extracted."
    echo "See http://www.infochimps.com/datasets/twitter-haiti-earthquake-data"
    exit 1
fi

if [ $# -lt 2 ]
then
    grep '#>' $0 | tr -d '#>' | sed '$d'
    exit 1
fi

RIAK=$1
SCHEMA=$2
PORT=${3:-8087}
i=0

echo "Install schema $SCHEMA..."
$RIAK/bin/search-cmd set-schema tweets $SCHEMA

echo "Install Search precommit hook..."
$RIAK/bin/search-cmd install tweets

printf "Upload data"
python upload.py $PORT < earthquake
