#!/usr/bin/perl

use TAP::Parser;
use TAP::Parser::Aggregator;

my $tester_exe = shift or die "usage: $0 \<tap_test.exe\>\n";

if (! -x $tester_exe ) {
	print "No test executable $tester_exe\n";
	exit(255);
}

my $parser = TAP::Parser->new( { exec => [ $tester_exe ] } );

$parser->run;

if ($parser->tests_planned == 0) {
	print "No planned tests\n";
	exit(255);
}

# while ( my $result = $parser->next ) {
#    print $result->as_string . "\n";
# }

my $aggregate = TAP::Parser::Aggregator->new;
$aggregate->add( 'testcases', $parser );
printf "\tPassed: %s\n\tFailed: %s\n\tSkipped: %s\n", scalar $aggregate->passed, scalar $aggregate->failed, scalar $aggregate->skipped;
printf "\tTests took %d seconds\n", ($parser->end_time - $parser->start_time);

exit($parser->exit);
