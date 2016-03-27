#!/bin/bash

cp index.html tree.html

sed -i -e "s/<!--TEXT-->/$1/" tree.html
google-chrome --app="file:///$(pwd)/tree.html"

