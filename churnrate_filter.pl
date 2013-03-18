#!/usr/bin/perl

use strict;
use warnings;

$, = "\t";

my @required_names = qw( user problem result );

my $user = -1;
my $date = "1970-01-01";
my $problem = -1;
my $solved = 0;
my $last_failed = -1;
my %idx;
my ($curr_user, $curr_date, $curr_solved);

chomp(my @names = split("\t", scalar(<>)));
foreach my $i (0..$#names) {
    foreach my $name (@required_names) {
        if ($names[$i] eq $name) {
            $idx{$name} = $i;
        }
    }
}
$names[$idx{problem}] = "last.failed";
print @names, "\n";

while (<>) {
    chomp;
    my @curr = split "\t";
    my $curr_user = $curr[$idx{user}];
    my $curr_problem = $curr[$idx{problem}];
    my $curr_result = $curr[$idx{result}];

    if ($curr_user != $user) {
        $last_failed = -1;
    }
    if ($curr_user != $user || $curr_problem != $problem)
    {
        $user = $curr_user;
        $problem = $curr_problem;
        $solved = 0;
    }

    if ($solved) {
        $curr_result = 1;
        if ($curr_problem eq $last_failed) {
            $last_failed = -1;
        }
    }
    $solved ||= $curr_result;
    if ($curr_result == 0) {
        $last_failed = $curr_problem;
    }

    $curr_problem = $last_failed;

    $curr[$idx{problem}] = $curr_problem;
    $curr[$idx{result}] = $curr_result;
    print @curr, "\n";
}
