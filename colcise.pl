#!/usr/bin/env perl
#
use strict;
use warnings;
use List::Util qw(max);
use List::MoreUtils qw(pairwise);
use Getopt::Long 2.49 qw(:config auto_help);

# options

my $file;
my $delimeter = ' ';
my $separator;
my $ignore = 1;
my @align;

GetOptions (
  'file=s' => \$file,
  'delimeter=s' => \$delimeter,
  'separator=s' => \$separator,
  'align=s{1,}' => \@align,
  'ignore=s' => \$ignore
)
  or die "Could not parse options\n";

$separator = $separator || $delimeter;

# colcise

my @columns = ();

open(my $FF, '<', $file)
  or die $!;

while (my $line = <$FF>) {
  chomp($line);
  my @words = split($delimeter, $line);

  for my $i (0 .. $#words) {
    @columns[$i] = max((length($words[$i]), $columns[$i] || 0))
  }
}

# create sub routine for reading this file
open($FF, '<', $file)
  or die $!;

# consider creating a subroutine for itterating over the passed in file
while (my $line = <$FF>) {
  # This is duplicated in the previous itteration
  chomp($line);
  my @words = split($delimeter, $line);
  my @result = ();

  for my $i (0 .. $#words) {
    my $word = $words[$i];
  ; my $size = length($word);
    my $column = $columns[$i];
    my $alignment = $align[$i] || 'l';
    my $rest = $column - $size;

    if ($alignment eq 'r') {
      push @result, (' ' x $rest) . $word;
    } else {
      push @result, $word . (' ' x $rest);
    }
  }

  print join($separator, @result) . "\n"
}
