package SBV::DATA::Align;
#------------------------------------------------+
#    [APM] This moudle is generated by amp.pl    |
#    [APM] Created time: 2013-08-29 17:57:17     |
#------------------------------------------------+
=pod

=head1 Name

SBV::DATA::Align

=head1 Synopsis

This module is not meant to be used directly

=head1 Feedback

Author: Peng Ai
Email:  aipeng0520@163.com

=head1 Version

Version history

=head2 v1.0

Date: 2013-08-29 17:57:17

=cut

use strict;
use warnings;
require Exporter;

use FindBin;
use lib "$FindBin::RealBin";
use lib "$FindBin::RealBin/..";
use lib "$FindBin::RealBin/lib";
use lib "$FindBin::RealBin/../lib";

use SBV::DEBUG;

sub new 
{
	my ($class,%par) = @_;
	ERROR('no_file') if (! exists $par{-file});
	
	my $align = {};
	doInitAlign($align,\%par);

	bless $align , $class;
}

sub doInitAlign
{
	my ($align,$par) = @_;
	my $file = $par->{-file};
	my $key = $par->{-key} || "query";

	open FH,$file or die "can't open file: $file $!";
	<FH> if ($par->{header});
	while(<FH>)
	{
		chomp;
		chop if (/\r$/);
		next if (/^#/);
		my ($qid,$qlen,$qsta,$qend,$sid,$slen,$ssta,$send,$iden) = split /\t/;
		
		#query id overlaps subject id
		#$qid = 'Q' . $qid;
		#$sid = 'S' . $sid;
		
		$align->{length}->{$qid} = $qlen;
		$align->{length}->{$sid} = $slen;

		if ($key eq "query")
		{
			push @{ $align->{align}->{$qid}->{$sid} } , [$qsta,$qend,$ssta,$send,$iden];
		}
		elsif ($key eq "subject")
		{
			push @{ $align->{align}->{$sid}->{$qid} } , [$ssta,$send,$qsta,$qend,$iden];
		}
	}
	close FH;

	return 1;
}
