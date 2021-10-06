NAME
    colcise - outline text into columns

SYNOPSIS
    colcise [--delimeter=<regex>] [--separator=<string>]
    [--align=<direction>...] [--ignore][--no-ignore] <file>

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

    -i, --ignore
        Toggles the ignoring of subsequent delimiters to false, causing
        every delimiter to create a new column. TBD

    -i, --no-ignore
        TBD

