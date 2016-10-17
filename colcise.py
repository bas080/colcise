#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import optparse

def main(
        string,
        delimiter,
        separator,
        ignore_subsequent,
        alignments,
        append_separator):

    # if no separator is passed as arg, then use the delimiter
    separator = separator or delimiter

    rows = splitIntoColumns(splitIntoRows(string), delimiter, ignore_subsequent)
    widths = columnWidths(rows)
    colciseRows(rows, widths, separator, alignments, append_separator)

def colciseRows(rows, widths, separator, alignments, append_separator):
    '''prints the rows while colcising'''
    result = []
    line = ''
    for row in rows:
        for index, field in enumerate(row):

            alignment='l' #default alignment

            if (len(alignments) > index):
                alignment = alignments[index]

            width = widths[str(index)]
            diff = width - len(field)

            if ( alignment == "r"):
              line += ( ' ' * diff )

            if (isLast(row, index)):
                line += field
                break

            # will left align if not right aligned, future feature might include
            # center
            if not (alignment == 'r'):
                  line += field

            if not (append_separator):
                line += separator

            line += ( ' ' * diff )

            if (append_separator):
                line += separator

        print line
        result.append(line)
        line = ''
    return result

def repeat(string, times):
    return str(string) * times

def isLast(array, index):
    return (len(array) == (index + 1))

def withinArray(array, index):
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

def splitIntoRows(string):
    return string.split('\n')[:-1]

def splitIntoColumns(rows, delimiter, ignore_subsequent):
    result = []
    for row in rows:
        columns = row.split(delimiter)
        if (ignore_subsequent):
            columns = filterEmpty(columns)
        result.append(columns)
    return result

def isEmpty(v):
    return (v == '')

def filterEmpty(items):
    result=[]
    for item in items:
        if not (isEmpty(item)):
            result.append(item.strip())
    return result

parser = optparse.OptionParser()

parser.set_defaults(debug = False,xls = False)

parser.add_option('-d', '--delimiter', dest='delimiter', default=' ')

parser.add_option('-i', '--ignore', action='store_false', dest='ignore_subsequent', default = True)

parser.add_option('-s', '--separator', dest='separator', default = None)

parser.add_option('-a', '--append-separator', action='store_true', dest='append_separator', default = True)

parser.add_option('-p', '--prepend-separator', action='store_false', dest='append_separator')

parser.add_option('-l', '--alignment', dest='alignments', default='')

(options, args) = parser.parse_args()

main(
     sys.stdin.read(),
     options.delimiter,
     options.separator,
     options.ignore_subsequent,
     options.alignments,
     options.append_separator);
