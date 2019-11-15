#!/usr/bin/env perl

=head1 NAME

colcise - outline text into columns

=cut

use strict;
use warnings;
use List::Util qw(max);
use List::MoreUtils qw(pairwise);
use Getopt::Long 2.49 qw(:config auto_help);

my @files = ();
my $delimeter = ' ';
my $separator;
my $ignore = 1;
my @align;

=head1 SYNOPSIS

colcise
[--delimeter=<regex>]
[--separator=<string>]
[--align=<direction>...]
[--ignore][--no-ignore]
<file>

=cut

=head1 DESCRIPTION

B<colcise> is a CLI tool for formatting text into human readable columns.

I<file> reffers to the file or stdin that should be formatted.

I<direction> reffers to the allignment of each column. Allowed values include:
l L R

=cut

=head1 OPTIONS

=over 4

=item B<-d, --delimeter>=<regex>

Regex to be used to split line into columns.

=item B<-s, --separator>=<string>

Overwrite the delimeter matches with string.

=item B<-a, --align>=<direction>...

Valid values include l, R, and r

=item B<-i, --ignore>

Toggles the ignoring of subsequent delimiters to false, causing every
delimiter to create a new column. TBD

=item B<-i, --no-ignore>

TBD

=cut

GetOptions (
  'delimeter=s' => \$delimeter,
  'separator=s' => \$separator,
  'align=s{1,}' => \@align,
  'ignore' => \$ignore # TO BE IMPLEMENTED
)
  or die "Could not parse options\n";

my @columns = ();

open(my $FF, '<', $ARGV[0])
  or die $!;

while (my $line = <$FF>) {
  chomp($line);
  my @words = split($delimeter, $line);

  for my $i (0 .. $#words) {
    @columns[$i] = max((length($words[$i]), $columns[$i] || 0))
  }
}

seek($FF, 0, 0);

# consider creating a subroutine for itterating over the passed in file
while (my $line = <$FF>) {
  # This is duplicated in the previous itteration
  chomp($line);
  my @words = split($delimeter, $line);
  my @result = ();
  my @matches;

  push (@matches, $&) while ($line =~ /$delimeter/g);

  for my $i (0 .. $#words) {
    my $word = $words[$i];
    my $size = length($word);
    my $column = $columns[$i];
    my $alignment = $align[$i] || 'l';
    my $rest = $column - $size;
    my $match = ($matches[$i] && $separator) || $matches[$i] || '';

    if ($alignment eq 'r') {
      push @result, (' ' x $rest) .  $word . $match;
    } elsif ($alignment eq 'L') {
      push @result, $word .  $match . (' ' x $rest);
    } else {
      push @result, $word . (' ' x $rest) . $match;
    }
  }

  print join('', @result) . "\n"
}
