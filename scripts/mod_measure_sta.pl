#!/usr/bin/perl 

# this program removes the associated measurement files from MEASURE_FILE 
# according to a list of stations

if (@ARGV != 2) {die("mod_measure_sta.pl MEASURE station-list\n");}

open(STA,"$ARGV[1]");
@sta=<STA>;
for $sta (@sta) {chomp($sta);}
print "Missing stations: @sta\n";

open(FIN,"$ARGV[0]") || die("Check if $ARGV[0] exists or not\n");

$tmp_file="measure.tmp";
open(FOUT,">$tmp_file");

($nfile) = split(" ",<FIN>);

$nn=0;
for ($i=0;$i<$nfile;$i++) {
  $data[$i] = <FIN>; chomp($data[$i]);
  $syn[$i] = <FIN>; chomp($syn[$i]);
  $nwin[$i] = <FIN>; chomp($nwin[$i]);
   for ($j=0;$j<$nwin[$i];$j++) {
     $tt[$j] = <FIN>;}
   for ($k=0;$k<@sta;$k++) {
     if ($data[$i] =~/$sta[$k]/) {last;}}
#  print "processing $data[$i] -- $k file\n";
   if ($k==@sta) { # no matching
     $nn++;
     print FOUT "$data[$i]\n$syn[$i]\n$nwin[$i]\n";
     for ($j=0;$j<$nwin[$i];$j++) {print FOUT "$tt[$j]";}
   } else{
     print "Matching: $data[$i] and $syn[$i]\n";}
 }
close(FIN);
close(FOUT);
print "Orignal number of files $nfile; New number of files $nn\n";

system("echo $nn > $ARGV[0]; cat $tmp_file >> $ARGV[0]");




