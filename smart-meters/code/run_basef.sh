#!/bin/bash

full_path=$(realpath $0)
dir_path=$(dirname $full_path)
mypath=$(dirname $dir_path )

rscript="basef.R"

doagg=FALSE &&  tag="bottom" && algo="KD-IC-NML" && njobs=4 && nperjobs=395 # (bottom-level forecasts)
# doagg=TRUE &&  tag="agg" && algo="DETS" && njobs=28 && nperjobs=2         # (aggregate-level forecasts)

allijobs=$(seq 1 $njobs )

for ijob in ${allijobs[@]}
do
  start=$(( 0 + ($ijob - 1)* ($nperjobs) + 1 ))
  end=$(( 0 + ($ijob - 1)* ($nperjobs) + ($nperjobs) ))
  alliseries=( $(seq $start $end ) )
  
  echo "${alliseries[@]}"
  
  Rscript --vanilla $rscript $algo $doagg ${alliseries[@]} > "$mypath/work/rout/basef-$tag-$algo-$ijob.Rout" 2> "$mypath/work/rout/basef-$tag-$algo-$ijob.err" &

done



