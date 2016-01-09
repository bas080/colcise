# Colcise

Python script that takes a string of multiple lines (\n) and an argument as that
is the delimeter to be used to create columns.

Colcise works nicely together with vims ability to perform external commands on
visual selections. One performs colciseness on a visual selection with a few
steps

- Visually select the lines you would want to colcise
- Enter command mode by pressing :
- Then type !colcise

The default beheviour performs colcision using a space as delimeter and
seperator.

## Setup

For those that now how to add a script or a folder to their $PATH variable know
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

-d	the delimeter to be used

select what string to use for seperate the columns. A space as default

say if subsequent delimeters should be disregarded of concidered as columns

choose what delimeter to use for creating the columns

## Examples

### Define what to use as the delimeter

```javascript
john = registerPerson("John", "Jiggles");
jesse = registerPerson("Jesse", "James");
jane = registerPerson("Jane", "Joker");
```
```bash
colcise -d '=' | colcise -d ',' -p
```
```javascript
john = registerPerson("John", "Jiggles");
jesse = registerPerson("Jesse", "James");
jane = registerPerson("Jane", "Joker");
```

### Define what to use as the seperator

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

### Ignore sequential delimeters

By default colcise ignores subsequent delimter strings. Assuming that the
delimeter is the default space, the following behaviour is to be expected.

*Notice all the spaces between the words and how they are all treated like one space*

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
