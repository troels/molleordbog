#!/usr/bin/env bash

dir=$(dirname $0)
find -name '*.doc' -print0 | xargs -0 -n1 abiword -t html
find -name '*.html' -print0 | xargs -0 "${dir}/extract_data.py"
