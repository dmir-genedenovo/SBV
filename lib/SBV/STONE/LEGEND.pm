package SBV::STONE::LEGEND;
#------------------------------------------------+
#    [APM] This moudle is generated by amp.pl    |
#    [APM] Created time: 2013-07-29 16:25:55     |
#------------------------------------------------+
=pod

=head1 Name

SBV::STONE::LEGEND

=head1 Synopsis

This module is not meant to be used directly

=head1 Description

used to add the guide legend for graph

=head1 Feedback

Author: Peng Ai
Email:  aipeng0520@163.com

=head1 Version

Version history

=head2 v1.0

Date: 2013-07-29 16:25:55

=cut

use strict;
use warnings;
require Exporter;

use FindBin;
use lib "$FindBin::RealBin";
use lib "$FindBin::RealBin/lib";
use lib "$FindBin::RealBin/..";
use lib "$FindBin::RealBin/../lib";

use SBV;
use SBV::STAT;
use SBV::DEBUG;
use SBV::Constants;
use SBV::STONE::SYMBOL;

sub new 
{
	my $class = shift;
	my %par = @_;
	my $legend = {};
	
	do_init_par(\%par);
	do_cal_size($legend,%par);
	
	$legend->{par} = \%par;
	bless $legend , $class;
	return $legend;
}

sub do_init_par
{
	my $par = shift;
	my $conf = $par->{conf};
	
	if (! exists $par->{'label'} && exists $conf->{'label_file'})
	{
		my (@labels,@color,@fill,@size,@shape);
		my $file = check_path($conf->{'label_file'});
	
		open IN,$file or die $!;
		while(<IN>)
		{
			chomp;
			my ($id,$attrs) = split /\t/;
			push @labels , $id;
			if ($attrs)
			{
				my @temp = split /;/ , $attrs;
				my %hash = map {
					my ($key,$val) = split /=/ , $_;
					$key => $val;
				} @temp;

				if ($hash{fill})
				{
					push @fill , SBV::Colors::fetch_color($hash{fill});
				}

				if ($hash{color})
				{
					push @color , SBV::Colors::fetch_color($hash{color});
				}

				if ($hash{size})
				{
					push @size , $hash{size};
				}

				if ($hash{shape})
				{
					push @shape , $hash{shape};
				}
			}
		}
		close IN;
		
		$par->{'label'} = \@labels;
		$par->{'color'} = \@color if ($#labels == $#color);
		$par->{'fill'} = \@fill if ($#labels == $#fill);
		$par->{'size'} = \@size if ($#labels == $#size);
		$par->{'shape'} = \@shape if ($#labels == $#shape);
	}

	if (! exists $par->{'label'})
	{
		ERROR('legend_text_err') if (! exists $conf->{'label'});
		my @temp = SBV::CONF::fetch_val($conf,'label');
		@temp = map { $_ =~ s/\\s/ /g; $_ } @temp;
		$par->{'label'} = \@temp;
	}

	foreach my$name(keys %$conf)
	{
		next if ($name eq "label");
		$par->{$name} = $conf->{$name} if (! exists $par->{$name});
	}
	
	my %defaultPar = (
		'pos'         => 'outright',
		
		'title_pos'   => 'top',
		'title_theme' => 'face:bold',
		'title_hjust' => 0,
		'title_vjust' => 0,
		
		'label_show'  => 1,
		'label_pos'   => 'right',
		'label_theme' => '',
		'label_hjust' => 0,
		'label_vjust' => 0,

		'ncol'        => 1,
		'byrow'       => 0,
		'reverse'     => 0,
		
		'shape'       => 1,
		'color'       => 'black',
		'fill'        => 'none',
		'width'       => 20,
		'height'      => 20,
		'size'        => 1,
		'opacity'     => 1,
		'stroke_width'=> 1,
		
		# for text legend
		'fsize'       => 16,
		'ffamily'     => 'arail',
		'fstyle'      => 'normal',
		'fweight'     => 'normal',

		'hspace'      => $SBV::conf->{hspace},
		'vspace'      => $SBV::conf->{vspace},
		'margin'      => '10'
	);

	foreach my$name (keys %defaultPar)
	{
		$par->{$name} = $defaultPar{$name} if (! exists $par->{$name});
	}
	
	if ("" eq ref $par->{color})
	{
		my @color = SBV::CONF::fetch_val($par,'color');
		@color = map { SBV::Colors::fetch_color($_) } @color;
		$par->{color} = \@color;
	}
	
	if ("" eq ref $par->{fill})
	{
		my @fill = SBV::CONF::fetch_val($par,'fill');
		@fill = map { SBV::Colors::fetch_color($_) } @fill;
		$par->{fill} = \@fill;
	}

	if ("" eq ref $par->{shape})
	{
		my @shape = SBV::CONF::fetch_val($par,'shape');
		$par->{shape} = \@shape;
	}

	if ("" eq ref $par->{size})
	{
		my @size = SBV::CONF::fetch_val($par,'size');
		$par->{size} = \@size;
	}

	return $par;
}

# calculate the size of legend
sub do_cal_size
{
	my ($legend,%par) = @_;
	
	my $num = scalar @{$par{'label'}}; 
	my $hi = $par{'hspace'};
	my $vi = $par{'vspace'};
	
	my ($nrow,$ncol);

	if (exists $par{'nrow'})
	{
		$nrow = $par{'nrow'};
		$ncol = int ($num/$nrow);
		$ncol ++ if (0 != $num % $nrow);

		if ($par{byrow})
		{
			$nrow = int ($num/$ncol);
			$nrow ++ if (0 != $num % $ncol);
		}
	}
	else
	{
		$ncol = $par{'ncol'};
		$nrow = int ($num/$ncol);
		$nrow ++ if (0 != $num % $ncol);

		if (! $par{byrow})
		{
			$ncol = int ($num/$nrow);
			$ncol ++ if (0 != $num % $nrow);
		}
	}
	
	$legend->{num}  = $num;
	$legend->{nrow} = $nrow;
	$legend->{ncol} = $ncol;
	
	# label 
	my $label_font = SBV::Font->new($par{'label_theme'});
	my $label_width = 0;
	my $label_height = 0;
	
	my $symbol_width = $par{width};
	my $symbol_height = $par{height};
	
	if ($par{'label_show'})
	{
		my @size;
		map {
			my ($w,$h) = 
			_do_cal_size($symbol_width,$symbol_height,$label_font,$par{'label'}->[$_],$par{'label_pos'},$vi); 
			push @size , [$w,$h];
		} 0 .. $num-1;

		my %width;
		my %height;
		if ($par{byrow})
		{
			foreach my$i(0 .. $num-1)
			{
				my $rowid = int ($i / $ncol);
				my $colid = $i % $ncol;
				push @{$width{$colid}} , $size[$i][0];
				push @{$height{$rowid}} , $size[$i][1];
			}
		}
		elsif (! $par{byrow})
		{
			foreach my$i(0 .. $num-1)
			{
				my $rowid = $i % $nrow;
				my $colid = int($i / $nrow);
				
				push @{$width{$colid}} , $size[$i][0];
				push @{$height{$rowid}} , $size[$i][1];
			}
		}
		
		$legend->{guide_width} = \%width;
		$legend->{guide_height} = \%height;

		foreach my$i (0 .. $ncol-1)
		{
			$label_width += max($width{$i});
		}

		foreach my$i (0 .. $nrow-1)
		{
			$label_height += max($height{$i});
		}
		
		$label_width += ($ncol-1) * $hi;
		$label_height += ($nrow-1) * $vi;
	}
	else
	{
		$label_width = $ncol * ($symbol_width + $hi) - $hi;
		$label_height = $nrow * ($symbol_height + $vi) - $vi;
	}
	
	$legend->{'label_width'} = $label_width;
	$legend->{'label_height'} = $label_height;
		
	# title
	if (exists $par{'title'})
	{
		my $title_font = SBV::Font->new($par{'title_theme'});
		my ($w,$h) = 
			_do_cal_size($label_width,$label_height,$title_font,$par{title},$par{'title_pos'},$vi);
		$legend->{width} = $w;
		$legend->{height} = $h;
	}
	else
	{
		$legend->{width} = $label_width;
		$legend->{height} = $label_height;
	}
	
	my $margin = SBV::CONF::fetch_margin(\%par);
	
	$legend->{width} += ($margin->{left} + $margin->{right});
	$legend->{height} += ($margin->{top} + $margin->{bottom});
	
	return 1;
}

sub _do_cal_size
{
	my ($w,$h,$font,$label,$pos,$space) = @_;
	my ($width,$height);

	my $label_width = $font->fetch_text_width($label);
	my $label_height = $font->fetch_text_height();
	($label_width,$label_height) = true_size($label_width,$label_height,$font->{'font-angle'});
	
	if ($pos =~ /^top/i || $pos =~ /^bottom/i)
	{
		$width = $w > $label_width ? $w : $label_width;
		$height = $label_height + $space + $h;
	}
	elsif ($pos =~ /^right/i || $pos =~ /^left/i)
	{
		$width = $label_width + $space + $w;
		$height = $h > $label_height ? $h : $label_height;
	}
	else
	{
		ERROR('label_position_err');	
	}
	
	return ($width,$height);
}

# location the legend
sub location
{
	my $self = shift;
	my $conf = shift; # the father conf 
	
	my $par = $self->{par};
	
	my $width = $self->width;
	my $height = $self->height;
	my $margin = SBV::CONF::fetch_margin($par);
	
	my ($x,$y) = SBV::CONF::fetch_xy($par->{pos},$width,$height,$conf);

	$par->{x} = $x;
	$par->{y} = $y;
	#$par->{width} = $width;
	#$par->{height} = $height;
	
	$par->{ox} = $x + $margin->{left}; 
	$par->{oty} = $y + $margin->{top};
	$par->{oy} = $y + $height - $margin->{bottom};
	$par->{tw} = $width - $margin->{left} - $margin->{right};
	$par->{th} = $height - $margin->{top} - $margin->{bottom};

	return 1;
}

# add the legend to SVG graph
sub draw
{
	my $self = shift;
	my $parent = shift || $SBV::svg;
	my $par = $self->{'par'};

	# init legend group
	my $legend = $parent->group(id=>"legend$SBV::idnum",class=>"legend");
	$SBV::idnum ++;

	# draw legend background and border
	SBV::DRAW::background($par,$legend);
	
	# init the guide start coord 
	my $ox = $par->{ox};
	my $oy = $par->{oty};
	
	# draw title 
	if (exists $par->{title})
	{
		my $title_font = SBV::Font->new($par->{'title_theme'});
		my $title_width = $title_font->fetch_text_width($par->{title});
		my $title_height = $title_font->fetch_text_height();
		($title_width,$title_height) = true_size($title_width,$title_height,$title_font->{'font-angle'});
		
		my ($tx,$ty) = SBV::CONF::fetch_xy($par->{'title_pos'},$title_width,$title_height,$par);
		$ty += $title_height;
		my $title = SBV::DRAW::theme_text($legend,$tx,$ty,$par->{'title_theme'},$par->{'title'});
		
		my $pos = $par->{'title_pos'};
		if ($pos =~ /^top/i)
		{
			$oy += $title_height + $par->{vspace};
		}
		elsif ($pos =~ /^left/i)
		{
			$ox += $title_width + $par->{hspace} + 6;
		}
	}

	# draw guide
	my $guide = $legend->group();

	my @labels = @{$par->{'label'}};
	my $num = $self->{num};
	my $ncol = $self->{ncol};
	my $nrow = $self->{nrow};

	my $cid;
	my $rid;
	my $guide_width = $par->{width};
	my $guide_height = $par->{height};
	
	my $fill = $par->{fill};
	my $color = $par->{color};
	my $shape = $par->{shape};
	my $size = $par->{size};
	my $opacity = $par->{opacity};
	my $stroke_width = $par->{stroke_width};

	my $fsize = $par->{fsize};
	my $ffamily = $par->{ffamily};
	my $fweight = $par->{fweight};
	my $fstyle = $par->{fstyle};

	# guide topleft point coord 
	my $gx = $ox;
	my $gy = $oy;
	for my$i ( 0 .. $#labels)
	{
		if ($par->{byrow})
		{
			$rid = int ($i / $ncol);
			$cid = $i % $ncol;
		}
		else
		{
			$cid = int ($i / $nrow);
			$rid = $i % $nrow;
		}
		
		if ($par->{"label_show"})
		{
			$guide_width = max($self->{guide_width}{$cid});
			$guide_height = max($self->{guide_height}{$rid});
		}
		
		my $color_sub = loop_arr($color,$i);
		my $fill_sub = loop_arr($fill,$i);
		my $shape_sub = loop_arr($shape,$i);
		my $size_sub = loop_arr($size,$i);
		my $opacity_sub = loop_arr($opacity,$i);

		my $fsize_sub = loop_arr($fsize,$i);
		my $ffamily_sub = loop_arr($ffamily,$i);
		my $fstyle_sub = loop_arr($fstyle,$i);
		my $fweight_sub = loop_arr($fweight,$i);
	
		my $symid = SBV::STONE::SYMBOL::new($shape_sub,
			width=>$par->{width},height=>$par->{height},opacity=>$opacity_sub,
			fill=>$fill_sub,color=>$color_sub,size=>$size_sub,stroke_width=>$stroke_width,
			font_size=>$fsize_sub,font_family=>$ffamily_sub,font_style=>$fstyle_sub,font_weight=>$fweight_sub
			);
		
		if ($par->{label_show})
		{
			my $pos = $par->{'label_pos'};
			my $label_font = SBV::Font->new($par->{'label_theme'});
			my $label_width = $label_font->fetch_text_width($labels[$i]);
			my $label_height = $label_font->fetch_text_height();
			($label_width,$label_height) = true_size($label_width,$label_height,$label_font->{'font-angle'});
			
			my $ltx = $gx;
			my $lty = $gy;
			if ($pos eq "top")	
			{
				$lty += $label_height;
				SBV::DRAW::theme_text($guide,$ltx,$lty,$par->{'label_theme'},$labels[$i]);
				$guide->group()->use(x=>$ltx,y=>$lty+$par->{vspace},width=>$par->{width},height=>$par->{height},'-href'=>"#$symid");
			}elsif ($pos eq "right")
			{
				$guide->group()->use(x=>$gx,y=>$gy,width=>$par->{width},height=>$par->{height},'-href'=>"#$symid");
				$ltx += $par->{width} + $par->{hspace};
				#$lty += $par->{height};
				$lty += ($label_height + $par->{height}) / 2;
				SBV::DRAW::theme_text($guide,$ltx,$lty,$par->{'label_theme'},$labels[$i]);
			}elsif ($pos eq "bottom")
			{
				$guide->group()->use(x=>$gx,y=>$gy,width=>$par->{width},height=>$par->{height},'-href'=>"#$symid");
				$lty += $par->{height} + $label_height + $par->{vspace};
				SBV::DRAW::theme_text($guide,$ltx,$lty,$par->{'label_theme'},$labels[$i]);
			}elsif ($pos eq "left")
			{
				#$lty += $par->{height};
				$lty += ($label_height + $par->{height}) / 2;
				SBV::DRAW::theme_text($guide,$ltx,$lty,$par->{'label_theme'},$labels[$i]);
				$guide->group()->use(x=>$gx+$label_width+$par->{hspace},y=>$gy,
				width=>$par->{width},height=>$par->{height},'-href'=>"#$symid");
			}
		}
		else
		{
			$guide->group()->use(x=>$ox,y=>$oy,width=>$par->{width},height=>$par->{height},'-href'=>"#$symid");
		}
		
		# cal the next symbol coord
		if ($par->{byrow})
		{
			$gx = $gx + $guide_width + $par->{hspace};
			$gy = $gy + $guide_height + $par->{vspace} if ($cid == $ncol - 1);
			$gx = $ox if ($cid == $ncol -1 );
		}
		else
		{
			$gx = $gx + $guide_width + $par->{hspace} if ($rid == $nrow - 1);
			$gy = $gy + $guide_height + $par->{vspace};
			$gy = $oy if ($rid == $nrow - 1);
		}
	}

	return $legend;
}

# return the width of legend
sub width
{
	my $self = shift;
	return $self->{width};
}

# return the height of legend
sub height
{
	my $self = shift;
	return $self->{height};
}
