#!/usr/bin/env bash
# copy files as if they are coming in from the scanner
# 20241007WF - init

src_dir=$(realpath ${DCM_SRC:-sim/src})
sim_dir=${SIM_DIR:-sim/out}

[ -d "$sim_dir" ] && echo "$sim_dir exists. rm to continue" && exit 1

# TODO: trap on ctrl-c too
trap 'test -d "$sim_dir" && rm -r "$sim_dir"' EXIT 

mkdir -p $sim_dir

find "$src_dir" -type f,l  | while read f; do
  tr=$(bc -l <<< $(dicom_hinfo -no_name -tag 0018,0080 "$f")/1000)
  echo "# sleeping $tr after adding $f to $sim_dir"
  ln -s "$f" "$sim_dir"
  sleep $tr
done
