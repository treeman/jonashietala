#!/usr/bin/env perl

use Modern::Perl;

$^I = '.bak'; # create a backup copy

while (<>) {
    s/^title: ([^"].*)$/title: "$1"/;
    print;
}

