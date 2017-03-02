#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import optparse


def colcise_rows(rows, widths, options):
    '''prints the rows while colcising'''
    result = []
    line = ''
    for row in rows:
        for index, field in enumerate(row):

            if (options.ignore_subsequent and field == ''):
                continue

            alignment = 'l'  # default alignment

            if (len(options.alignments) > index):
                alignment = options.alignments[index]

            if (options.strip):
                field = field.strip()

            width = widths[str(index)]
            diff = width - len(field)

            if (alignment == "r"):
                line += (' ' * diff)

            if (is_last(row, index)):
                line += field
                break

            if not (alignment == 'r'):
                line += field

            if not (options.append_separator):
                line += options.separator

            line += (' ' * diff)

            if (options.append_separator):
                line += options.separator

        result.append(line)
        line = ''
    return result


def repeat(string, times):
    return str(string) * times


def is_last(array, index):
    return (len(array) == (index + 1))


def within_array(array, index):
    '''check if the index is in the array'''
    return (index > len(array))


def columnWidths(rows):
    '''returns the max widths of the columns'''
    widths = {}
    for row in rows:
        for index, field in enumerate(row):
            prop = str(index)
            if prop in widths:
                widths[prop] = max(widths[prop], len(field))
            else:
                widths[prop] = len(field)
    return widths


def string_to_rows(string):
    return string.split('\n')[:-1]


def rows_to_columns(rows, options):
    result = []
    for row in rows:
        columns = row.split(options.delimiter)
        result.append(columns)
    return result


def main(string, options):
    rows = rows_to_columns(string_to_rows(string), options)
    colcise_rows(rows, columnWidths(rows), options)


if __name__ == '__main__':
    parser = optparse.OptionParser()
    parser.set_defaults(debug=False, xls=False)
    parser.add_option('-d', '--delimiter', dest='delimiter', default=' ')
    parser.add_option('-i', '--ignore', action='store_false',
                      dest='ignore_subsequent', default=True)
    parser.add_option('-s', '--separator', dest='separator', default=' ')
    parser.add_option('-a', '--append-separator', action='store_true',
                      dest='append_separator', default=True)
    parser.add_option('-p', '--prepend-separator', action='store_false',
                      dest='append_separator')
    parser.add_option('-t', '--strip', action='store_false',
                      dest='strip', default=True)
    parser.add_option('-l', '--alignment', dest='alignments', default='')
    (options, _) = parser.parse_args()
    main(sys.stdin.read(), options)
