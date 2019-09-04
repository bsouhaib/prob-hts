#!/bin/bash

full_path=$(realpath $0)
dir_path=$(dirname $full_path)
mypath=$(dirname $dir_path )

rscript="aggregation.R"

njobs=12
nperjobs=368

allijobs=$(seq 1 $njobs )

for ijob in ${allijobs[@]}
do
  start=$(( 0 + ($ijob - 1) * $nperjobs + 1 ))
  end=$(( 0 + ($ijob - 1) * $nperjobs + $nperjobs ))
  allidtest=( $(seq $start $end ) )

  echo "${allidtest[@]}"
  Rscript --vanilla $rscript $ijob ${allidtest[@]} > "$mypath/work/rout/aggregation-$ijob.Rout" 2> "$mypath/work/rout/aggregation-$ijob.err" &
done


