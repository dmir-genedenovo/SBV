package SBV::IMAGE::TEND;
#-------------------------------------------------+
#    [APM] This moudle was generated by amp.pl    |
#    [APM] Created time: 2014-09-05 15:36:48      |
#-------------------------------------------------+
=pod

=head1 Name

SBV::IMAGE::TEND

=head1 Synopsis

This module is not meant to be used directly

=head1 Feedback

Author: Peng Ai
Email:  aipeng0520@163.com

=head1 Version

Version history

=head2 v1.0

Date: 2014-09-05 15:36:48

=cut


use strict;
use warnings;
require Exporter;

use FindBin;
use lib "$FindBin::RealBin";
use lib "$FindBin::RealBin/lib";
use lib "$FindBin::RealBin/../";
use lib "$FindBin::RealBin/../lib";

sub new 
{
	my ($class,$file,$conf) = @_;
	my $obj = {};

	$obj->{conf} = $conf;

	_load_data_file($file,$conf);

	bless $obj , $class;
	$obj->load_conf($conf);
	return $obj;
}