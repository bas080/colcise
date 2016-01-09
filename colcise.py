#!/usr/bin/env python
# -*- coding: utf-8 -*-

import sys
import optparse

def main(
        string,
        delimeter,
        seperator,
        ignore_subsequent,
        alignments,
        append_seperator):

    # if no seperator is passed as arg, then use the delimiter
    seperator = seperator or delimeter

    rows = splitIntoColumns(splitIntoRows(string), delimeter, ignore_subsequent)
    widths = columnWidths(rows)
    colciseRows(rows, widths, seperator, alignments, append_seperator)

def colciseRows(rows, widths, seperator, alignments, append_seperator):
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

            if not (append_seperator):
                line += seperator

            line += ( ' ' * diff )

            if (append_seperator):
                line += seperator

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
    return string.split('\n')

def splitIntoColumns(rows, delimeter, ignore_subsequent):
    result = []
    for row in rows:
        columns = row.split(delimeter)
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
            result.append(item)
    return result

parser = optparse.OptionParser()

parser.set_defaults(debug = False,xls = False)

parser.add_option('-d', '--delimeter', dest='delimeter', default=' ')

parser.add_option('-i', '--ignore', action='store_false', dest='ignore_subsequent', default = True)

parser.add_option('-s', '--seperator', dest='seperator', default = None)

parser.add_option('-a', '--append-seperator', action='store_true', dest='append_seperator', default = True)

parser.add_option('-p', '--prepend-seperator', action='store_false', dest='append_seperator')

parser.add_option('-l', '--allignment', dest='alignments', default='')

(options, args) = parser.parse_args()

main(
     sys.stdin.read(),
     options.delimeter,
     options.seperator,
     options.ignore_subsequent,
     options.alignments,
     options.append_seperator);
