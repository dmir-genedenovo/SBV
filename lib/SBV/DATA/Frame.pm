package SBV::DATA::Frame;
#------------------------------------------------+
#    [APM] This moudle is generated by amp.pl    |
#    [APM] Created time: 2013-06-07 09:34:44     |
#------------------------------------------------+
=pod

=head1 Name

SBV::DATA::Frame

=head1 Synopsis

This module is not meant to be used directly

=head1 Feedback

Author: Peng Ai
Email:  aipeng0520@163.com

=head1 Version

Version history

=head2 v1.0

Date: 2013-06-07 09:34:44

=cut

use strict;
use warnings;
require Exporter;

use FindBin;
use lib "$FindBin::RealBin";
use lib "$FindBin::RealBin/lib";
use lib "$FindBin::RealBin/..";
use lib "$FindBin::RealBin/../lib";

use SBV::DEBUG;
use SBV::STAT;

sub new
{
	my $class = shift;
	my $data  = shift;
	my %param = @_;
	my $frame = {};
	
	$frame = doInitFrame($data,%param);

	bless $frame , $class;
	return $frame;
}

sub doInitFrame
{
	my ($data,%param) = @_;
	my $frame = {};
	my $format = $param{'-format'} || "frame";

	# @names: col names (must be unique)
	# @rownames: row names (must be unique)
	# $rnum : row number
	# $cnum : col number
	my (@names,@rownames,$rnum,$cnum);

	if (ref $data eq "HASH" && $format eq "frame") # input is hash
	{
		@names = keys %$data;
		$rnum = scalar @{$data->{$names[0]}};
		$cnum = scalar @names;
		
		foreach my$name(@names)
		{
			my $subdata = $data->{$name};
			my $len = scalar @$subdata;
			ERROR('frame_row_len_err') if ($len != $rnum);
			
			$frame->{col}->{$name} = $subdata;
			map { push @{$frame->{row}->{$_}} , $subdata->[$_-1] } 1 .. $len;
		}
	}
	elsif (ref $data eq "SBV::DATA" && $format eq "list2")
	{
		@names = ("V1","V2");
		$cnum = 2;
		my @val1 = $data->names;
		
		my $rowid = 0;
		foreach my$key (@val1)
		{
			my $yval = $data->{$key};
			map { 
				push @{$frame->{col}->{'V1'}} , $key;
				push @{$frame->{col}->{'V2'}} , $_;
				$frame->{row}->{$rowid} = [$key,$_];
				push @rownames , $rowid;
				$rowid ++;
			} @$yval;
		}
	}
	elsif (ref $data eq "ARRAY") # input is matrix 
	{
		my $frow = $data->[0];
		my $rnum = scalar @$data;
		my $cnum = scalar @$frow;

		@names = map { "V" . $_ } 1 .. $cnum;
		
		my $rowid = 1;
		my $sta = $param{header} ? 1 : 0;
		foreach my$i ($sta .. $rnum-1)
		{
			my $subdata = $data->[$i];
			my $len = scalar @$subdata;
			ERROR('frame_col_len_err') if ($len != $cnum);
			
			push @rownames , $rowid;
			$rowid ++;

			$frame->{row}->{$rowid} = $subdata;
			
			map { push @{$frame->{col}->{$names[$_-1]}} , $subdata->[$_-1] } 1 .. $cnum;
		}
	}
	elsif (-e $data) # input is file
	{
		my $sep = $param{"sep"} || "\t";
		
		open FH,$data or die "can't open file: $data $!";
		
		# deal the header
		my $head = <FH>;
		chomp $head;
		chop $head if ($head =~ /\r$/);
		
		my @temp = split /$sep/ , $head;
		$cnum = scalar @temp;
		
		if ($param{rownames})
		{
			$cnum --;
		}

		if ($param{header})
		{
			if ($param{rownames})
			{
				shift @temp ;
			}
			@names = @temp;
		}
		else
		{
			@names = map { "V" . $_ } 1 .. $cnum;
			seek(FH,0,0);
		}
		
		my $rowid = 1;
		while(<FH>)
		{
			chomp;
			chop if (/\r$/);
			next if (/^#/);
			next if ($_ eq "");

			my @arr = split /$sep/;
			my $rowname = $param{rownames} ? shift @arr : $rowid;
			push @rownames , $rowname;
			$rowid ++;
			
			my $len = scalar @arr;
			ERROR('frame_col_len_err') if ($len != $cnum);
			
			$frame->{row}->{$rowname} = \@arr;
			
			map { push @{$frame->{col}->{$names[$_-1]}} , $arr[$_-1] } 1 .. $cnum;
		}
			
		close FH;
	}
	else
	{
		ERROR('frame_input_err');
	}

	$frame->{names} = \@names;
	$frame->{rownames} = \@rownames;
	
	return $frame;
}

sub col
{
	my $self = shift;

}

sub row
{
	my $self = shift;
	
}

sub val
{
	my $self = shift;
	
}

sub rownames
{
	my $self = shift;
	
	if (@_ == 0)
	{
		return @{$self->{rownames}};	
	}
	else
	{
		my $newRowNames = shift;
		my $newRowNum = scalar @$newRowNames;

		my $rownames = $self->{rownames};
		my $rownum = scalar @$rownames;

		ERROR('frame_rowname_length_err') if ($rownum != $newRowNum);

		foreach my$i(1 .. $rownum)
		{
			$self->{row}->{$newRowNames->[$i-1]} = $self->{row}->{$rownames->[$i-1]};
		}

		return 1;
	}
}

sub names
{
	my $self = shift;
	
	if (@_ == 0)
	{
		return @{$self->{names}};
	}
	else
	{
		my $newnames = shift;
		my $newnum = scalar @$newnames;

		my $names = $self->{names};
		my $num = scalar @$names;

		ERROR('frame_name_length_err') if ($num != $newnum);
		
		$self->{names} = \@$newnames;
		foreach my$i(1 .. $num)
		{
			$self->{col}->{$newnames->[$i-1]} = $self->{col}->{$names->[$i-1]};
			delete $self->{col}->{$names->[$i-1]};
		}

		return 1;
	}
}

sub toRA
{
	my $self = shift;
	my %param = @_;
	
	my @names = $self->names;
	my $xname = $param{x};
	my $yname = $param{y};
	
	if (! $xname || !$yname)
	{
		$xname = $names[0];
		$yname = $names[1];
	}

	my $xval = $self->{col}->{$xname};
	my $yval = $self->{col}->{$yname};
	
	my (@mval,@aval);
	foreach my$i ( 0 .. $#$xval )
	{
		my ($m,$a);
		if ($$xval[$i] == 0)
		{
			$$xval[$i] = $param{init} || 0.001;
		}
		
		if ($$yval[$i] == 0)
		{
			$$yval[$i] = $param{init} || 0.001;
		}
		
		$m = log2($$xval[$i]) - log2($$yval[$i]);
		$a = 0.5 * (log2($$xval[$i]) + log2($$yval[$i]));
		push @aval , $a;
		push @mval , $m;
	}

	$self->{col}->{$xname} = \@aval;
	$self->{col}->{$yname} = \@mval;

	return $self;
}

sub toMA
{
	my $self = shift;
	my %param = @_;
	
	my @names = $self->names;
	my $xname = $param{x};
	my $yname = $param{y};
	
	if (! $xname || !$yname)
	{
		$xname = $names[0];
		$yname = $names[1];
	}

	my $xval = $self->{col}->{$xname};
	my $yval = $self->{col}->{$yname};
	
	my (@mval,@aval);
	foreach my$i ( 0 .. $#$xval )
	{
		my ($m,$a);
		if ($$xval[$i] == 0)
		{
			$$xval[$i] = $param{init} || 0.001;
		}
		
		if ($$yval[$i] == 0)
		{
			$$yval[$i] = $param{init} || 0.001;
		}
		
		$m = log2($$xval[$i]) - log2($$yval[$i]);
		$a = 0.5 * (log2($$xval[$i]) + log2($$yval[$i]));
		push @aval , $a;
		push @mval , $m;
	}

	$self->{col}->{$xname} = \@aval;
	$self->{col}->{$yname} = \@mval;

	return $self;
}

# just for GWAS format frame
sub toManhattan
{
	my $self = shift;
	my %param = @_;
	
	my $coord = $param{-coord};
	my @names = $self->names;
	
	my @chr = @{$self->{col}->{$names[1]}};
	my @pos = @{$self->{col}->{$names[2]}};
	my @pval = @{$self->{col}->{$names[3]}};

	my @xval;
	my @yval;
	for my$i (0 .. $#chr)
	{
		push @xval , $coord->{$chr[$i]}->{start} + $pos[$i];
		push @yval , 0 - log10($pval[$i]);
	}
	
	$self->{col}->{'V1'} = \@xval;
	$self->{col}->{'V2'} = \@yval;
	$self->{col}->{'V3'} = \@chr;

	return $self;
}

sub toarray
{
	my $self = shift;
}

sub scale
{
	my $self = shift;
	my %opts = @_;

	my @names = $self->names;
	my @rownames = $self->rownames;
	
	return unless (defined $opts{scale});

	if ($opts{scale} eq "row")
	{
		foreach my$i (0 .. $#rownames)
		{
			$self->{row}->{$rownames[$i]} = zscore($self->{row}->{$rownames[$i]});
			map { $self->{col}->{$names[$_]}->[$i] = $self->{row}->{$rownames[$i]}->[$_] } 0 .. $#names;
		}
	}
	elsif ($opts{scale} eq "column")
	{
		foreach my$i(0 .. $#names)
		{
			$self->{data}->{$names[$i]} = zscore($self->{col}->{$names[$i]});
			map { $self->{row}->{$rownames[$_]}->[$i] = $self->{col}->{$names[$i]}->[$_] } 0 ..$#rownames;
		}
	}
}

sub save
{
	my $self = shift;
	my %opts = @_;
	my $file = $opts{file} or die "file";
	my @names = $opts{names} ? @{$opts{names}} : $self->names;
	my @rownames = $opts{rownames} ? @{$opts{rownames}} : $self->rownames;

	open OUT,">$file" or die "$!";
	print OUT "ID\t" , @{ [ join "\t",@names ] } , "\n" if ($opts{header});
	foreach my$i( 0 .. $#rownames)
	{
		my $line = $self->{row}->{$rownames[$i]};
		@$line = map { $self->{col}->{$_}->[$i] } @names;
		print OUT $rownames[$i] , "\t" , @{[ join "\t",@$line ]} , "\n";
	}
	close OUT;
}