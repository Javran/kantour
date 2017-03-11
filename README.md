# kantour

[![Build Status](https://travis-ci.org/Javran/kantour.svg?branch=master)](https://travis-ci.org/Javran/kantour)

A set of tools for KanColle-related developments.

Most of them are just re-implementation of existing stuff - another purpose of this repo
is to see how helpful could Haskell be for dealing with real problem beyond parsing,
implementing simple interpreters and online programming contests.

With the help of [stack](https://docs.haskellstack.org/), you don't have to actually install
the package globally. For all commands listed below,
prefixing `stack build && stack exec --` in front
of it to keep binaries sync with their source codes and run command in that isolated enviroment.
(e.g. for `maptool a.xml`, you might actually run `stack build && stack exec -- maptool a.xml`).

## maptool

For loading swf map resources and draw nodes and edges for it.

Usage: `maptool <main xml> [extra xml] [-- <arguments to diagrams>]`

- `main xml` and `extra xml` refers to the `xml` file exported from `ffdec`.
- everything after `--`, if exists, goes to [diagrams](http://projects.haskell.org/diagrams/),
  which in turn renders and outputs the image, use `maptool foo -- --help` to see available
  output options. Here `foo` is just a random string to make the whole argument list looks valid,
  so it can actually be anything.

- a full example: `maptool map.xml extra.xml -- -o test.svg -w 2000`
  (assuming `map.xml` and `extra.xml` (optional) has been prepared properly).

## coded

For decoding `Core.swf`

## quotefetch

For fetching quotes from [kcwiki](https://zh.kcwiki.moe/).
The output file is always `kcwiki.json` under the directory where you execute the program.
It should be of the same format you downloaded from `http://api.kcwiki.moe/subtitles`,
with all quotes extended with seasonal lines.

Usage: `quotefetch <optinal link>`.

- examples:

    - `quotefetch '季节性/2017年女儿节'`
    - `quotefetch '季节性/2017年白色情人节'`


## dropcalc

For computing statistics according to a drop rate.

Usage: `dropcalc <rate> <# of experiments>`

- `rate` can be of the following form: `0.2` or `20%` (both are the same thing)
- `# of experiment` is the number of times that we simulate ship farming with
  the specified drop rate under the assumption that the result complies with uniform
  distribution.
