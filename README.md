# kantour

[![Build Status](https://travis-ci.org/Javran/kantour.svg?branch=master)](https://travis-ci.org/Javran/kantour)

A set of tools for KanColle-related developments.

## Overview

(TODO: migration is not yet done)

This repo consists of two packages:

- `kantour-core`: the majority of code. The future plan is to keep core library small
  so that other project could depend on it without pulling in all sorts of other libraries.
- `kantour`: currently just a wrapper around core library. The future plan is to shift mosta
  of the CLI-only code and dependencies to this package.

## Commands

With the help of [stack](https://docs.haskellstack.org/), you don't have to actually install
the package globally. For all commands listed below,
prefixing `stack build && stack exec --` in front
of it to keep binaries sync with their source codes and run command in that isolated enviroment.
(e.g. for `maptool a.xml`, you might actually run `stack build && stack exec -- maptool a.xml`).

### maptool

For loading swf map resources and draw nodes and edges for it.

Usage: `maptool <main xml> [extra xml] [-- <arguments to diagrams>]`

- `main xml` and `extra xml` refers to the `xml` file exported from `ffdec`.
- everything after `--`, if exists, goes to [diagrams](http://projects.haskell.org/diagrams/),
  which in turn renders and outputs the image, use `maptool foo -- --help` to see available
  output options. Here `foo` is just a random string to make the whole argument list looks valid,
  so it can actually be anything.

- a full example: `maptool map.xml extra.xml -- -o test.svg -w 2000`
  (assuming `map.xml` and `extra.xml` (optional) has been prepared properly).

### coded

For decoding `Core.swf`

### quotefetch

For fetching quotes from [kcwiki](https://zh.kcwiki.moe/).
The output file is always `kcwiki.json` under the directory where you execute the program.
It should be of the same format you downloaded from `http://api.kcwiki.moe/subtitles`,
with all quotes extended with seasonal lines.

Usage: `quotefetch <optinal link>`.

- examples:

    - `quotefetch '季节性/2017年女儿节'`
    - `quotefetch '季节性/2017年白色情人节'`

### dropcalc

For computing statistics according to a drop rate.

Usage: `dropcalc <rate> <# of experiments>`

- `rate` can be of the following form: `0.2` or `20%` (both are the same thing)
- `# of experiment` is the number of times that we simulate ship farming with
  the specified drop rate under the assumption that the result complies with uniform
  distribution.

### Notes about map drawing

- Decrypt `Core.swf`

- Save RND in `<Core>.scripts/common/resources/MapResourceLoader` as a text file
  (the final semicolon is optional)

- `stack exec -- kantour decmapurl mapRND.txt` shows areas

- `stack exec -- kantour decmapurl mapRND.txt <area>` prints downloading command

- `ffdec -swf2xml 39-1.swf 39-1.xml` - need to do this for all maps

- Get `SallyMain.swf` or other stuff that contains hidden routes, convert to xml.

- `stack exec -- kantour maptool 39-4.xml bin/sm.xml 394 -- -w 800 -h 480 -o test.png`

- `stack exec -- kantour mapredraw 41_01.mi 41_01.png`
