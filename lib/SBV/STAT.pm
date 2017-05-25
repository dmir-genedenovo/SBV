#
#===============================================================================
#
#         FILE: STAT.pm
#
#  DESCRIPTION: STAT functions for SBV 
#
#        FILES: ---
#         BUGS: ---
#        NOTES: ---
#       AUTHOR: Peng Ai (), apanhui@gmail.com
# ORGANIZATION: 
#      VERSION: 1.0
#      CREATED: 06/03/2013 11:01:53 AM
#     REVISION: ---
#===============================================================================

package SBV::STAT;

use strict;
use warnings;

require Exporter;
our @ISA = qw(Exporter);
our @EXPORT = qw /loop_arr dividing sum max min mean var sd sem 
uniq_arr iset uset cset log2 log10 true_size aindex levels zscore/;

use Math::Round;
use Algorithm::Cluster::Record;
use Algorithm::Cluster qw/kcluster/;

use FindBin;
use lib "$FindBin::RealBin";
use lib "$FindBin::RealBin/..";
use lib "$FindBin::RealBin/lib";
use lib "$FindBin::RealBin../lib";

use SBV::DEBUG;
use SBV::Constants;

# get the loop value of array width specified item
sub loop_arr
{
	my $array = shift;
	my $item = shift;
	
	return $array if ("" eq ref $array);

	my $len = scalar @$array;
	$item = $item % $len;
	
	return $array->[$item];
}


# get the uniq array
sub uniq_arr
{
	my $array = shift;
	
	my %hash;
	my @uniq;
	for (@$array)
	{
		push @uniq , $_ if (! exists $hash{$_});
		$hash{$_} = 1;
	}

	return @uniq;
}

#===  FUNCTION  ================================================================
#         NAME: dividing
#      PURPOSE: get the dividing marker number for axis
#   PARAMETERS: min,max or array
#      RETURNS: axis lim: start end units(window)
#  DESCRIPTION: the main is to get the units by the min and max value
#               the start is the min - 0.2*units
#               the end is the max + 0.2*units
#       THROWS: no exceptions
#     COMMENTS: none
#     SEE ALSO: n/a
#===============================================================================
sub dividing
{
	my ($min,$max);

	my $first = shift;
	if (ref $first eq "ARRAY")
	{
		$min = min($first);
		$max = max($first);
	}
	else
	{
		$min = $first;
		$max = shift;

		($min,$max) = ($max,$min) if ($min > $max);
	}
	
	if ($min * $max > 0)
	{
		$min = 0 if ($min > 0);
		$max = 0 if ($max < 0);
	}
	
	my %par = @_;

	my $size = $max - $min;
	$size = sprintf ("%e",$size);
	my ($num,$unit) = split /e/ , $size;
	$num = round($num);
	
	my $window;
	if (1 == $num)
	{
		$window = 0.2;	
	}
	elsif (2 == $num || 3 == $num)
	{
		$window = 0.5;
	}
	elsif ($num >=4 && $num <= 7)
	{
		$window = 1;
	}
	elsif (8 == $num|| 9 == $num || 10 == $num)
	{
		$window = 2;
	}
	else
	{
		ERROR('dividing_err',"$num");
	}
	
	$window = sprintf ("%f" , $window . "e" . $unit);
	
	if (not exists $par{'-ntrue'})
	{
		my $minlv = $min >= 0 ? int ($min / $window) : int ($min / $window - 1);
		my $temp = $min / $window;
		$min = $minlv * $window;
	}
	
	if (not exists  $par{'-xtrue'})
	{
		my $maxlv = $max > 0 ? int ($max / $window) : int ($max / $window) - 1;
		my $temp = $max / $window;
		$max = int($temp) != $temp ? ($maxlv + 1) * $window : $max + $window * 0.2;
	}
	
	return "$min $max $window";
}



## get all sub group of ARRAY with specified sub elements number
sub groupARRAY
{
	my ($array,$num) = @_;

	my $len = scalar @$array;
	
	## init temporary array
	my $tempArr;
	$tempArr->[$_] = 1 for (0 .. $num-1);
	$tempArr->[$_] = 0 for ($num .. $#$array);


	my @group; # save the group result
	my $bound; # save the position of first 10
	my $num1; # save the 1 number of 01 left
	my $has10; # cantains 10 or not
	
	# save the init array
	push @group , &get1pos($array,$tempArr);

	OUTER:while(1)
	{
		$has10 = 0;
		$num1 = 0;

		INNER:for my$i(0 .. $#$array-1)
		{
			## find the first 01
			if ($tempArr->[$i] == 1 && $tempArr->[$i+1] == 0)
			{
				$bound = $i;

				# change 10 to 01
				$tempArr->[$i] = 0;
				$tempArr->[$i+1] = 1;

				# move the 10 left 1 to the left edge
				$tempArr->[$_] = 1 for (0 .. $num1-1);
				$tempArr->[$_] = 0 for ($num1 .. $bound);

				$has10 = 1;
				last INNER;
			}
			elsif ($tempArr->[$i] == 1)
			{
				$num1 ++;
			}
		}

		last OUTER if ($has10 == 0);

		push @group , &get1pos($array,$tempArr);
	}

	return \@group;
}

sub get1pos
{
	my $array = shift;
	my $tempArr = shift;
	my @res;

	for ( 0 .. $#$tempArr )
	{
		push @res , $array->[$_] if ($tempArr->[$_] == 1);
	}

	return \@res;
}

# return the max element of the array
sub max
{
	my $array = shift;

	my $max = $array->[0];
	for (@$array)
	{
		$max = $_ if ($_ > $max);
	}
	
	return $max;
}

# return the min element of the array
sub min
{
	my $array = shift;

	my $min = $array->[0];

	for (@$array)
	{
		$min = $_ if ($_ < $min);
	}

	return $min;
}

# return sum value of the array
sub sum
{
	my $array = shift;

	my $sum = 0;

	$sum += $_ for @$array;

	return $sum;
}

# return the average value of the array
sub mean
{
	my $array = shift;

	return nearest 0.0001 , &sum($array)/($#$array+1);
}

# variance
sub var
{
	my $array = shift;
	my $mean = mean($array);
	
	my $var = 0;
	map { $var += ($_ - $mean)**2 } @$array;

	return $var;
}

# standard deviation
sub sd
{
	my $array = shift;
	my $var = var($array);

	return sqrt($var);
}

# standard error of the mean
sub sem
{
	my $array = shift;
	my $sd = sd($array);
	my $n = scalar @$array;
	
	return $sd/(sqrt $n);
}

# Quartile
sub quartile
{
	my $array = shift;

	my $n = scalar @$array;
	@$array = sort {$a<=>$b} @$array;
	
	my $q1pos = 1 + ($n-1) * 0.25;
	my $i = int $q1pos;
	my $f = $q1pos - $i;
	my $q1 = $array->[$i-1] + ($array->[$i] - $array->[$i])*$f;

	my $q2pos = 1 + ($n-1) * 0.5;
	$i = int $q2pos;
	$f = $q1pos - $i;
	my $q2 = $array->[$i-1] + ($array->[$i] - $array->[$i])*$f;

	my $q3pos = 1 + ($n-1) * 0.75;
	$i = int $q3pos;
	$f = $q1pos - $i;
	my $q3 = $array->[$i-1] + ($array->[$i] - $array->[$i])*$f;

	return ($q1,$q2,$q3);
}

# intersection set 
sub iset
{
	my @arrs = @_;
	return [] if (!@arrs);
	return $arrs[0] if (1 == @arrs);
	
	my ($head,@left) = @_;
	_iset($head,@left);
}

sub _iset
{
	my ($head,@left) = @_;
	
	my %h = map { $_ => undef } @$head;
	for my $arr(@left)
	{
		%h = map { $_ => undef } grep {exists $h{$_}} @$arr;
	}

	my @set = keys %h;
	return \@set;
}

# union set 
sub uset
{
	my @arrs = @_;
	return [] if (!@arrs);
	return $arrs[0] if (1 == @arrs);

	_uset(@arrs);
}

sub _uset
{
	my (@arrs) = @_;

	my %h;
	for my$arr(@arrs)
	{
		map { $h{$_} = undef } @$arr;
	}
	
	my @set = keys %h;
	return \@set;
}

# complementary set 
sub cset
{
	my @arrs = @_;
	return [] if (!@arrs);
	return $arrs[0] if (1 == @arrs);
	
	my ($main,$threw) = @_;
	_cset($main,$threw);
}

sub _cset
{
	my ($main,$threw) = @_;

	my %h = map {$_ => undef} @$threw;
	%h = map {$_ => undef} grep {! exists $h{$_}} @$main;
	
	my @set = keys %h;
	return \@set;
}

sub log2
{
	my $val = shift;
	return logx($val,2);
}

sub log10
{
	my $val = shift;
	return logx($val,10);
}

sub logx
{
	my ($val,$x) = @_;
	return log($val)/log($x);
}

# get the true size of element
sub true_size
{
	my ($w,$h,$angle) = @_;
	$angle = 0 if (! defined $angle);	
	$angle = $TWOPI*$angle/360;
	$angle = abs($angle);

	my $th = nearest 0.001 , (cos($angle) * $h + sin($angle) * $w); 
	my $tw = nearest 0.001 , (sin($angle) * $h + cos($angle) * $w);
	return ($tw,$th);
}

# get the level of elemets in array
sub levels
{
	my $arr = shift;
	my @uniq_arr = uniq_arr($arr);
	my @level = map { aindex(\@uniq_arr,$_) } @$arr;
	return \@level;
}

# array index
sub aindex
{
	my $arr = shift;
	my $item = shift;

	for my$i(0 .. $#$arr)
	{
		return $i if ($arr->[$i] eq $item);
	}
	
	return -1;
}

# z score normalization
sub zscore
{
	my $array = shift;

	my $mean = &mean($array);
	my $sd = &sd($array);
	
	return $array if (0 == $sd);

	@$array = map { ($_-$mean)/$sd } @$array;
	return $array;
}
