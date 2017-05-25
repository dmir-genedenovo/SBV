package SBV::Coordinate::POLAR;
#------------------------------------------------+
#    [APM] This moudle is generated by amp.pl    |
#    [APM] Created time: 2013-06-21 08:47:21     |
#------------------------------------------------+
=pod

=head1 Name

SBV::Coordinate::POLAR

=head1 Synopsis

This module is not meant to be used directly

=head1 Feedback

Author: Peng Ai
Email:  aipeng0520@163.com

=head1 Version

Version history

=head2 v1.0

Date: 2013-06-21 08:47:21

=cut

use strict;
use warnings;
require Exporter;
our @ISA = qw(Exporter);
our @EXPORT    = qw(theta2arc);

#use Math::Cephes qw(:trigs :constants);
use Math::Round;

use FindBin;
use lib "$FindBin::RealBin";
use lib "$FindBin::RealBin/lib";
use lib "$FindBin::RealBin/..";
use lib "$FindBin::RealBin/../lib";

use SBV::Constants;

# create new polar system with original point (cx,cy)
sub new 
{
	my $class = shift;

	my ($cx,$cy,%par) = @_;
	my $polar = {};
	$polar->{cx} = $cx;
	$polar->{cy} = $cy;
	$polar->{parent} = $par{parent} || $SBV::svg;

	bless $polar , $class; 

	return $polar;
}

# set the parent 
sub parent
{
	my $self = shift;
	$self->{parent} = shift;

	return 1;
}

# turn svg position coordinate to polar coordinate
# under build 
sub pos2polar
{
	my $self = shift;
	my $x = shift;
	my $y = shift;

	my $newx = $x - $self->{cx};
	my $newy = $self->{cy} - $y;

	my $r = sqrt($newx**2+$newy**2);
	my $theta = 0;
	
	if ($newx >= 0)
	{
		$theta = acos($newy/$r);
	}
	else
	{
		$theta = $PI + acos(-$newy/$r);
	}

	return ($r,$theta);
}

# turn polar coordinate to svg position coordinate 
sub polar2pos
{
	my $self = shift;
	my $r = shift;
	my $theta = shift;
	my $theta_type = shift || "arc";
	
	if ($theta_type eq "ratio")
	{
		$theta *= $TWOPI;
	}
	elsif ($theta_type eq "angle")
	{
		$theta = theta2arc($theta);
	}

	my $x = nearest 0.001 , sin($theta) * $r;
	my $y = nearest 0.001 , cos($theta) * $r;

	return ($self->{cx} + $x, $self->{cy} - $y);
}

# polar line (arc)
# default the angle is arc
# if angle please divide 360 and times TWOPI
# if ratio please times TWOPI
sub pline
{
	my $self = shift;
	my ($cx,$cy) = ($self->{cx} , $self->{cy});
	my $parent = $self->{parent};
	
	my ($r,$theta1,$theta2,%param) = @_;
	($r,$theta1,$theta2) = simple_float($r,$theta1,$theta2);
	
	$param{theta_type} = "angle" unless defined $param{theta_type}; 
	if ($param{theta_type} eq "angle")
	{
		$theta1 = theta2arc($theta1);
		$theta2 = theta2arc($theta2);
	}

	my ($x1,$y1) = $self->polar2pos($r,$theta1);
	my ($x2,$y2) = $self->polar2pos($r,$theta2);
	my $flag = abs($theta2 - $theta1) > $PI ? 1 : 0;
	my $flag1 = $theta2 > $theta1 ? 1 : 0;
	my $path = "M$x1 $y1 A$r $r $theta1 $flag $flag1 $x2 $y2";

	my $obj = $parent->path(d=>$path,%param);
	return $obj;
}

*arc = \&pline;

# polar rect (fan)
sub prect
{
	my $self = shift;
	my ($cx,$cy,$parent) = ($self->{cx} , $self->{cy}, $self->{parent});

	my ($r1,$theta1,$r2,$theta2,%param) = @_;
	$param{theta_type} = "angle" unless defined $param{theta_type}; 

	if ($param{theta_type} eq "angle")
	{
		$theta1 = theta2arc($theta1);
		$theta2 = theta2arc($theta2);
	}

	($r1,$theta1,$r2,$theta2) = simple_float($r1,$theta1,$r2,$theta2);
	
	my ($x1,$y1) = $self->polar2pos($r1,$theta1);
	my ($x2,$y2) = $self->polar2pos($r2,$theta1);
	my ($x3,$y3) = $self->polar2pos($r2,$theta2);
	my ($x4,$y4) = $self->polar2pos($r1,$theta2);

	my $flag = abs($theta2 - $theta1) > $PI ? 1 : 0;
	my $flag1 = $theta2 > $theta1 ? 1 : 0;
	my $flag2 = $theta2 > $theta1 ? 0 : 1;
	my $path = "M$x2 $y2 A$r2 $r2 $theta1 $flag $flag1 $x3 $y3 L$x4 $y4 A$r1 $r1 $theta1 $flag $flag2 $x1 $y1 Z";

	my $obj = $parent->path(d=>$path,%param);
	return $obj;
}

*fan = \&prect;

# line 
sub line
{
	my $self = shift;
	my ($cx,$cy,$parent) = ($self->{cx} , $self->{cy}, $self->{parent});
	
	my ($r1,$theta1,$r2,$theta2,%param) = @_;

	$param{theta_type} = "angle" unless defined $param{theta_type}; 
	if ($param{theta_type} eq "angle")
	{
		$theta1 = theta2arc($theta1);
		$theta2 = theta2arc($theta2);
	}

	($r1,$theta1,$r2,$theta2) = simple_float($r1,$theta1,$r2,$theta2);
	my ($x1,$y1) = $self->polar2pos($r1,$theta1);
	my ($x2,$y2) = $self->polar2pos($r2,$theta2);
	
	my $line = $parent->line(x1=>$x1,y1=>$y1,x2=>$x2,y2=>$y2,%param);
	return $line;
}

# rectangle
sub rect 
{
	my $self = shift;
	my ($cx,$cy,$parent) = ($self->{cx} , $self->{cy}, $self->{parent});

	my ($r,$theta,$w,$h,%param) = @_;
	($r,$theta,$w,$h) = simple_float($r,$theta,$w,$h);

	my ($x,$y) = $self->polar2pos($r,$theta);
	my $rect = $parent->rect(x=>$x,y=>$y,width=>$w,height=>$h,%param);
	return $rect;
}

# BCurve
sub BCurve
{
	my $self = shift;
	my ($r0,$Qangle1,$Qangle2,$r1,$Rangle1,$Rangle2,%attrs) = @_;
	my ($cx,$cy,$parent) = ($self->{cx} , $self->{cy}, $self->{parent});
	
	my ($Qx1,$Qy1) = $self->polar2pos($r0,$Qangle1,"angle");
	my ($Qx2,$Qy2) = $self->polar2pos($r0,$Qangle2,"angle");
	my ($Rx1,$Ry1) = $self->polar2pos($r1,$Rangle1,"angle");
	my ($Rx2,$Ry2) = $self->polar2pos($r1,$Rangle2,"angle");

	my $color = $attrs{color} || "#000";
	my $fill = $attrs{fill} || "#000";
	my $thickness = 1;
	
	my $flag1 = $Qangle1 > $Qangle2 ? 0 : 1;
	my $flag2 = $Rangle1 > $Rangle2 ? 0 : 1;
	$parent->path(
		d=>"M$Qx1 $Qy1 A$r0 $r0 $Qangle1 0 $flag1 $Qx2 $Qy2 C$Qx2 $Qy2 $cx $cy $Rx1 $Ry1 A$r1 $r1 $Rangle1 0 $flag2 $Rx2 $Ry2 C$Rx2 $Ry2 $cx $cy $Qx1 $Qy1",
		style=>"stroke-width:$thickness;stroke:$color;fill:$fill"
	);
}

# text
sub text 
{
	my $self = shift;
	my ($r,$theta,$trans,$label,%param) = @_;
	
	my ($cx,$cy,$parent) = ($self->{cx} , $self->{cy}, $self->{parent});
	($r,$theta,$trans) = simple_float($r,$theta,$trans);
	
	my $font = SBV::Font->new($param{theme});
	my $style = $font->toStyle();

	$param{theta_type} = "angle" unless (defined $param{theta_type});
	if ($param{theta_type} eq "arc")
	{
		$theta = arc2theta($theta);
	}
	
	my $text;
	if ($param{parallel})
	{
		if ($theta <= 90 || $theta >180)
		{
			$text = $parent->text(x=>$cx+$trans,y=>$cy-$r,
				transform=>"rotate($theta,$cx,$cy)",style=>$style)->cdata($label);
		}
		else 
		{
			$theta -= 180;
			my $text_height = $font->fetch_text_height;
			$text = $parent->text(x=>$cx+$trans,y=>$cy+$r+$text_height,
				transform=>"rotate($theta,$cx,$cy)",style=>$style)->cdata($label);
		}
	}
	else
	{
		if ($theta <= 180)
		{
			$theta -= 90;
			$text = $parent->text(x=>$cx+$r,y=>$cy+$trans,
				transform=>"rotate($theta,$cx,$cy)",style=>$style)->cdata($label);
		}
		else 
		{
			$theta -= 270;
			my $text_width = $font->fetch_text_width($label);
			$text = $parent->text(x=>$cx-$r-$text_width,y=>$cy+$trans,
				transform=>"rotate($theta,$cx,$cy)",style=>$style)->cdata($label);
		}
	}
	return $text;
}

# turn angle to arc
sub theta2arc
{
	my $angle = shift;
	return $angle * $TWOPI / 360;
}

sub arc2theta
{
	my $angle = shift;
	return $angle * 360 / $TWOPI;
}

sub simple_float
{
	return map { nearest 0.001 , $_ } @_;
}