#!/usr/bin/perl
#
# A script to check tabs and trailing whitespace from diff files.
#
# Usage: perl check_spaces.pl file1.diff file2.diff .. fileN.diff
#
# Use as a precommit hook in .hg/hgrc:
#   pretxncommit.whitespace = hg export tip|perl tools/check_spaces.pl
#
# or (if using color extension also)
#   pretxncommit.whitespace = hg --color never export tip|perl tools/check_spaces.pl
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
