#!/usr/bin/perl
#
# Udage: perl check_spaces.pl file1 file2 .. fileN
#

my $line = 0;
while(<>)
{
	$line++;
	if (/^\+\+\+/) {
		next;
  } if (/^\+/) {
		if (/\t/) {
			print "Tabs in the file\n";
			chomp;
			$_ = substr $_,1;
			print "line $line: $_.\n";
			exit 1
		} elsif (/[ \t]+$/) {
			print "Spaces at the end of line\n";
			chomp;
			$_ = substr $_,1;
			print "line $line: $_.\n";
			exit 1;
	  }
  }
}

exit 0
