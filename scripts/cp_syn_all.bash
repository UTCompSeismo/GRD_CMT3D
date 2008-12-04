#!/bin/bash

# this program copies over the synthetics from wagholi and untar them and set them up for cmt3d and grid3d inversions

if [ $# != 1 ]; then
  echo "Usage: cp_syn.bash evid-file"; exit
fi

cluster="lqy@$wag:/net/wagholi/scratch1/lqy/CMT3D_Basin_Adjoint/cmt_mass/"

extra_dir="_rerun"

for evid in `cat $1 `; do 
  mkdir -p ${evid}${extra_dir}; 
  cd ${evid}${extra_dir}; mkdir -p syn; mkdir -p backup
  scp $cluster/${evid}${extra_dir}/* syn/
  cp syn/CMTSOLUTION .; cp ../event_info/STATIONS .
  cp ../event_info/MEASUREMENT_WINDOWS_${evid}_T00?_T030_m12 .
## here check if MEASURE file has the right number of data/syn pairsa
  mafile="MEASUREMENT_WINDOWS_${evid}_ALL"
  nma=0
  rm -f $mafile
  for freq in 2 3 6; do
    mfile="MEASUREMENT_WINDOWS_${evid}_T00${freq}_T030_m12"
    n0=`grep "^[DS]" $mfile | wc | awk '{print $1}'`
    n1=`echo "$n0 / 2" | bc`
    n2=`head -1 $mfile`
    if [ $n1 != $n2 ]; then
      echo "Number of files inconsistent in $mfile: $n1 and $n2"; exit
    fi
## here to modify DATA -> data_T006_T030, SYN -> syn_T006_T030
    perl -pi -e "s/DATA/data_T00${freq}_T030/g" $mfile
    perl -pi -e "s/SYN/syn_T00${freq}_T030/g" $mfile
    perl -pi -e "s/\.T00${freq}_T030//g" $mfile
    perl -pi -e "s/\.semd\.sac\.m12//g" $mfile
    awk 'NR > 1 {print $0}' $mfile >> $mafile
    nm0=`head -n 1 $mfile`
    nma=`echo $nm0 + $nma | bc ` 
    rm -f $mfile
  done
## here to link cmt3d/grid3d_flexwin
  ln -s ../../grd_cmt3d/cmt3d/cmt3d_flexwin
  ln -s ../../grd_cmt3d/grid3d/grid3d_flexwin
## here to modify INVERSION.PAR GRID3D.PAR for MEASURE file
  cp ../../grd_cmt3d/cmt3d/INVERSION.PAR .
  cp ../../grd_cmt3d/grid3d/GRID3D.PAR .
  perl -pi -e "s/flexwin\.out/$mafile/g" INVERSION.PAR
  cp INVERSION.PAR INVERSION.PAR.SAVE
##  make sure you have the right dmoment, ddepth, and dlocation
  perl -pi -e "s/flexwin\.out/$mafile/g" GRID3D.PAR
  echo $nma | cat - $mafile > out.tmp; mv out.tmp $mafile 
## Unzip synthetics
  cd syn
  for ext in Mrr Mtt Mpp Mrt Mrp Mtp dep lat lon; do
    tar xjvf ${evid}_${ext}.tar.bz >/dev/null
    if [ $? != 0 ]; then
      echo "Error untaring ... $ext ..."; exit
    fi
    mv -f ${evid}_${ext}.tar.bz ../backup
    rename "s/semd.sac.$ext/$ext/" *.semd.sac.$ext
  done
  ls -1 *.Mrr | perl -pi -e 's/\.Mrr//g' -- > file_list
  xadd_frechet_derivatives  s file_list ../CMTSOLUTION 1.0e22
  cd ..
####
  cd ..; 
done
