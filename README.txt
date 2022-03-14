NAME
    colcise - outline text into columns

SYNOPSIS
    colcise [--delimeter=<regex>] [--separator=<string>]
    [--align=<direction>...] [--no-ignore] [<file>]

DESCRIPTION
    colcise is a CLI tool for formatting text into human readable columns.

    *file* reffers to the file or stdin that should be formatted.

    *direction* reffers to the allignment of each column. Allowed values
    include: l L R

OPTIONS
    -d, --delimeter=<regex>
        Regex to be used to split line into columns.

    -s, --separator=<string>
        Overwrite the delimeter matches with string.

    -a, --align=<direction>...
        Valid values include L, l, R, and r

    -n, --no-ignore
        Sets the ignoring of subsequent delimiters to false, causing every
        delimiter to create a new column. TBD

BUGS
    https://github.com/bas080/colcise/issues

AUTHOR
    Written by Bassim Huis and Antonis Kalou.

LICENSE
    GPL-3.0

