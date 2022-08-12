#!/usr/bin/perl

use Getopt::Std;

sub usage{
print STDERR <<END;
Usage: launch_cmt3d_frechet.pl -d run_dir_on_cluster -o out_syn_dir -s station_file
                     -m initial_cmtsolution 
      (station and cmt file names have to be local ./)

END
exit(1);
}

if (@ARGV == 0) {usage();}
if (!getopts('d:o:s:m:')) {die("Check input arguments -dosm \n");}
$cluster="pangu"; $user="lqy";

# check options
if ($opt_m) {$cmt=$opt_m;} else {die("input -m cmtsolution\n");}
if (not -f $cmt) {die("Check if $cmt exists or not\n");}

if ($opt_s) {$sta=$opt_s;} else {die("input -s station[_adjoint]\n");}
if (not -f $sta) {die("Check $sta\n");}

if ($opt_d) {$run_dir=$opt_d;} else {die("input -d cluster_run_dir\n");}
@tmp=grep("No such file or directory",`ssh $cluster ls $opt_d`);
if (@tmp == 1) {die("Check if $opt_d is a directory on $cluster\n");}

if ($opt_o) {$out_dir=$opt_o;} else {die("input -o out_syn-dir\n");}
if (not -d $out_dir) {mkdir $out_dir;}

$ddelta=0.01; $ddepth=0.02; $dmoment=1.0e22;
system("xmake_frechet_cmtsolutions $cmt $ddelta $ddepth $dmoment");

# copy cmt to directory and start cluster run
print "\nUploading simulation control files and launching job ...\n";
print "scp ${cmt}* $user\@$cluster:$run_dir \n";
print "scp $sta $user\@$cluster:$run_dir/STATIONS \n";
print "ssh $cluster \"source ~/.bash_profile; cd $run_dir; run_cmt3d_frechet.bash Master_Dir $cmt $sta \"\n";
`scp ${cmt}* $user\@$cluster:$run_dir`;
`scp $sta $user\@$cluster:$run_dir/STATIONS`;
#`ssh $cluster \"source ~/.bash_profile; cd $run_dir; run_cmt3d_frechet.bash Master_Dir $cmt $sta \"`;

# check cluster run
print "\nCheck job status ... \n";
for $dir ("syn","Mrr", "Mtt", "Mpp", "Mrp", "Mtp", "Mrt", "dep", "lat", "lon") {
  $sem_dir="$run_dir/$dir";
#  check_cluster_job($cluster,"$sem_dir");
  # collect seismograms and info files
  print "\nCollect seismograms $dir ...\n";
  print "scp $user\@$cluster:$sem_dir/SEM/*.sac $out_dir\n";
  print "scp $user\@$cluster:$sem_dir/DATA/CMTSOLUTION $out_dir\n";
  print "scp $user\@$cluster:$sem_dir/DATA/STATIONS_FILTERED $out_dir\n";
  print "scp $user\@$cluster:$sem_dir/DATA/Par_file $out_dir\n";
  `scp $user\@$cluster:$sem_dir/SEM/*.sac $out_dir`;
  `scp $user\@$cluster:$sem_dir/DATA/CMTSOLUTION $out_dir`;
  `scp $user\@$cluster:$sem_dir/DATA/STATIONS_FILTERED $out_dir`;
  `scp $user\@$cluster:$sem_dir/DATA/Par_file $out_dir`;
  system("cd $out_dir; rename semd.sac $dir *.semd.sac");
} 
print "\nDone !!\n";

#-------------------------------------------
sub check_cluster_job {
  local ($cluster,$cluster_dir) = @_;

  $time_int1 = 15; # check every 15 seconds
  $time_int2 = 60;
  $jobid=0; $jobid_min = 40000;
  # obtain job id
  while ($jobid < $jobid_min) {
     ($jobid) = split(" ", `ssh $cluster "cat $cluster_dir/OUTPUT_FILES/jobid"`);
     sleep $time_int1;
  }
  print "Job id assigned: $jobid\n";

  $status = "NONE";
  # check job status
  while ($status ne "DONE") {
     (undef,undef,$status) = split(" ",`ssh $cluster "bstat | grep $jobid"`);
     if ($status eq "") {die("Something is wrong: no job found on cluster\n");}   
     if ($status eq "PEND" or $status eq "RUN") {print "$status ";}
     if ($status eq "EXIT") {die("Error in SEM simulation, exit ...\n");}
     sleep $time_int2;
  }  
  # return when job is done
  print "\n\nJob $jobid is done !!\n";
  return;
}

