[![License: AGPL v3](https://img.shields.io/badge/License-AGPL%20v3-blue.svg)](https://www.gnu.org/licenses/agpl-3.0)
[![Build Status](https://www.travis-ci.com/Xalgorithms/lib-rules-int-scala.svg?branch=master)](https://www.travis-ci.com/Xalgorithms/lib-rules-int-scala)

# About

[Rule execution in
Interlibr](https://github.com/Xalgorithms/general-documentation/blob/master/docs/xalgo.md)
is made up of three stages:
[compilation](https://github.com/Xalgorithms/xa-rules), execution and
revision. This library implements the execution stage. Its role is to
load rules from MongoDB, to arrange a logic series of steps that will
form the execution of that rule and to output a set of revisions that
*could be* applied to the original document to yield a new document.

This library is a *naive reference implementation* of the execution
model for Xalgo. It is (and never will be) optimized for improved
production exeuction times. Addition implementations of the Xalgo
execution model will be added for this purpose, leaving this library
to remain clean as a reference for new implementations.

# Status

This library is *currently capable* of executing rules *locally*
within a test harness.

# Getting started

*This library is a work-in-progress, these instructions will change
over time as development progresses.*

### Smoke Test

To get started working with this library, you will need a working [SBT
installation](https://www.scala-sbt.org/). Once you have that
configured, you will be able to run a simple *smoke test* for the
project. Within a terminal / shell run:

```
$ sbt test
```

This command runs the unit tests for the library. They should be **all
green**. If you notice a problem, please log an issue with the output
of the failure.

Next, run:

```
$ sbt "runMain org.xalgorithms.rules.Runner test-runs/map/"
```

This command runs a prewritten, precompiled run. You should see this
output:

```
ALL TABLES

table:t
| e:          2 | a:          1 | b:          2 | c:          3 | d:          4 |
| e:          4 | a:          2 | b:          4 | c:          6 | d:          8 |
| e:          6 | a:          3 | b:          6 | c:          9 | d:         12 |
| e:          8 | a:          4 | b:          8 | c:         12 | d:         16 |
| e:         10 | a:          5 | b:         10 | c:         15 | d:         20 |

table:table0
| a:          1 | b:          2 |
| a:          2 | b:          4 |
| a:          3 | b:          6 |
| a:          4 | b:          8 |
| a:          5 | b:         10 |

[success] Total time: 2 s, completed 22-May-2018 5:37:31 PM
```

Again, if you do not receive the expected output, log an issue.

### Playing with Rules

Having completed the smoke test, the next step is to understand the
[Xalgo
specification](https://github.com/Xalgorithms/general-documentation/blob/master/docs/xalgo.md)
and write some rules of your own. Within the `test-runs/` directory in
the project, there are a number of example Xalgo rules that can serve
as starting points for your own rules.

The [Xalgo compiler](https://github.com/Xalgorithms/xa-rules) is
implemented as a different library. Over time, this will change, but,
for now, you will need to follow the [getting
started](https://github.com/Xalgorithms/xa-rules/blob/master/README.md)
for that library in order to compile your rules.

Once you have been able to compile the rules to the *internal JSON
format*, you can create your own "test run" in the `test-runs/`
directory (the layout is straightforward). To invoke your rule, use:

```
$ sbt "runMain org.xalgorithms.rules.Runner test-runs/<your directory>/"
```
