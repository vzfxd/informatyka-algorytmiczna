#!/bin/bash

find $1 -type f | xargs cat | tr -s '[:space:]' '\n' | sort | uniq -c | sort -nr
