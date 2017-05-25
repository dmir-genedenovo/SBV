#!/usr/bin/perl
#------------------------------------------------+
#    [APM] This script is generated by amp.pl    |
#    [APM] Creat time: 2014-05-21 10:51:27       |
#------------------------------------------------+
# name: table2list.pl
# func: turn table file to list which can be used to draw plot directly

use strict;
use warnings;
use Getopt::Std;
use List::Util qw/sum/;

my %opts = ();
getopts('1',\%opts);

my $file = shift @ARGV;
my $sep = "\t";

open FH,$file or die $!;
my @data = <FH>;
chomp @data;
close FH;

my $head = shift @data;
my @type = split /$sep/ , $head;
shift @type;

foreach (@data)
{
	my @items = split /$sep/ , $_;
	my $name = shift @items;

	if ($opts{1})
	{
		my $sum = sum(@items);
		@items = map { $_ / $sum } @items;
	}
	
	foreach my$i( 0 .. $#items)
	{
		print "$name\t$items[$i]\t$type[$i]\n";
	}
}