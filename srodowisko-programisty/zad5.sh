#/bin/bash

find $1 -type f | xargs sed -i 's/a/A/g'
echo $(find $1 -type f | xargs cat | tr -s '[:space:]' '\n')