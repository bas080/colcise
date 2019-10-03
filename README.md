# Colcise

[![Build Status](https://travis-ci.org/bas080/colcise.svg?branch=master)](https://travis-ci.org/bas080/colcise)

Python script that takes a string of multiple lines (\n) and an argument as that
is the delimeter to be used to create columns.

Colcise works nicely together with vim's ability to perform external commands on
visual selections. One performs colciseness on a visual selection with a few
steps

- Visually select the lines you would want to colcise
- Enter command mode by pressing :
- Then type !colcise

The default behavior performs colcision using a space as delimiter and
separator.

## Setup

For those that know how to add a script or a folder to their $PATH variable know
how to install get a python script to work in their command line.

Regardless I will share with you how I have set it up on my system.

*If you do not have a personal bin folder setup I advice you to read and follow
the steps mentioned here: http://www.cyberciti.biz/faq/unix-linux-adding-path/*

```bash
# create and change the bin directory
mkdir -p ~/bin
cd ~/bin

# download the python script
wget 'https://raw.githubusercontent.com/bas080/colcise/master/colcise.py' --output-file="colcise"

# make colcise script executable
chmod +x colcise
```

If $PATH is setup correctly you should now be able to use colcise.

## Options

    -d
    --delimiter
        the string to be used to split lines into columns.

    -s
    --separator
        select what string to use for separate the columns. When not defined it
        defaults to the same string as the delimiter.

    -p
    --prepend-separator
        places the separator in front of the spacing.

    -a
    --append_separator
        places the separator after the spacing. This is the default behavior.

    -l
    --alignment
        define the way the text withing columns should be aligned. The first
        letter represents the alignment of the first column, the second letter
        the second column etc.

        l = left
        r = right
        m = middle

    -i
    --ignore
        toggles the ignoring of subsequent delimiters to false, causing every
        delimiter to create a new column.

## Examples

**Define what to use as the delimiter**

```javascript
john = registerPerson("John", "Jiggles");
jesse = registerPerson("Jesse", "James");
jane = registerPerson("Jane", "Joker");
```
```bash
colcise -d '=' | colcise -d ',' -p
```
```javascript
john  = registerPerson("John",  "Jiggles");
jesse = registerPerson("Jesse", "James");
jane  = registerPerson("Jane",  "Joker");

```

**Define what to use as the separator**

Notice how the blank lines are still there.

```html
<script src="lib/d3.js" type="text/javascript" charset="utf-8"></script>

<script src="app/blox/blox.js" type="text/javascript" charset="utf-8"></script>
<script src="app/blox/block.js" type="text/javascript" charset="utf-8"></script>
<script src="app/blox/gui.js" type="text/javascript" charset="utf-8"></script>

<script src="app/puzzles/easy.js" type="text/javascript" charset="utf-8"></script>
<script src="app/puzzles/medium.js" type="text/javascript" charset="utf-8"></script>
```
```bash
colcise -d 'type'
```
```html
<script src="lib/d3.js"             type="text/javascript" charset="utf-8"></script>

<script src="app/blox/blox.js"      type="text/javascript" charset="utf-8"></script>
<script src="app/blox/block.js"     type="text/javascript" charset="utf-8"></script>
<script src="app/blox/gui.js"       type="text/javascript" charset="utf-8"></script>

<script src="app/puzzles/easy.js"   type="text/javascript" charset="utf-8"></script>
<script src="app/puzzles/medium.js" type="text/javascript" charset="utf-8"></script>
```

**Ignore sequential delimiters**

By default colcise ignores subsequent delimiter strings. Assuming that the
delimiter is the default space, the following behavior is to be expected.

*Notice all the spaces between the words, and how they are all treated like one space.*

```txt
hello  world
goodbye regret
happy         times
glorious   day
```
```bash
colcise
```
```txt
hello    world
goodbye  regret
happy    times
glorious day
```

**Aligning text in columns**

```txt
James 123
George 1234
Johny 652
Roy 213
```
```bash
colcise -l 'lr'
```
```txt
James   123
George 1234
Johny   652
Roy     213
```

## Roadmap

- Center alignment with c

## Bugs

- Does not support STDIN or PIPES. Will require temp files.
- An unwanted new line is added at the bottom.

## Running tests

To run tests just run the test script like this

    python3 tests.py

## Contributors

- [Bas Huis](https://github.com/bas080)
- [Antonis Kalou](https://github.com/kalouantonis)
