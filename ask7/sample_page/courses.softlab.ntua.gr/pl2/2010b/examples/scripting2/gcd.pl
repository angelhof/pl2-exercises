#!/usr/bin/perl -wT
use CGI qw(:standard);
use CGI::Carp qw(warningsToBrowser fatalsToBrowser);
use strict;

sub gcd {
    my ($x, $y) = ($_[0], $_[1]);
    while ($x > 0 && $y > 0) {
        if ($x > $y) {
            $x = $x % $y;
        }
        else {
            $y = $y % $x;
        }
    }
    return $x + $y;
}

print header;
print start_html("CGI script for the GCD");
print h1("Result");

my $x = param("x");
my $y = param("y");
my $result = gcd($x, $y);

print p("The GCD of $x and $y is $result.</p>\n");
print end_html;
