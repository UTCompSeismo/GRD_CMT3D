#!/usr/bin/perl

# this perl script reads the syn files and output the files 
# of the half duration derivatives.

use Getopt::Std;

@ARGV > 1 or die("Usage: frechet_hdur.pl -g -h Hdur -m CMT_FILE SYN_FILES\n");
if (!getopts('gh:m:')) {die("Check options\n");}
if ($opt_h and $opt_m) {die("Specify only one of -h and -m\n");}
if (not $opt_h and not $opt_m) {die("Specify one of -h and -m\n");}
if ($opt_g) {$triangle="false";} else {$triangle="true";}
print " Triangle source time function: $triangle\n";

if ($opt_h) {$hdur=$opt_h;}
else {
   $cmt_file=$opt_m;
   open(CMT, "$cmt_file") or die("Error opening $cmt_file\n");
   @cmt = <CMT>;	
   (undef,undef,$hdur)=split(" ",$cmt[3]);
   close(CMT);
}
print " Half duration: $hdur\n";
if (abs($hdur) < 1.0e-3) {die("Too small half duration\n");}
$infile = "input_frechet_hdur.txt";
print "Input file: $infile\n";

$frechet_hdur = "/home/lqy/tbin/source/frechet_hdur/frechet_hdur";
if (! -e $frechet_hdur) {die("NO such file exist: $frechet_hdur\n");}

foreach $file (@ARGV) {
   if (! -f $file) {die("No such file: $file\n");}
   open(IN,">$infile");
   print IN "$hdur\n";
   print IN "$file\n";
   print IN "$triangle\n";
   close(IN);
   system("$frechet_hdur < $infile; \\rm -f $infile");
}

print "DONE\n";


