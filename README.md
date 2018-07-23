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
model for Xalgo. The primary purpose of the library is to have an *initial
implementation* that can be used to evaluate the effectiveness of the
execution model. It is (and never will be) optimized for improved
production execution times. Addition implementations of the Xalgo
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
$ ./test-run.sh validate
```

This command runs a basic validation test to demonstrate that the
application is running properly.

```
> compiling test-runs/validate/validate.rule to test-runs/validate/validate.rule.json
# discovered 1 test runs in test-runs/validate, executing all...
EXECUTE: VALIDATE
# no expectations exist, dumping tables
table:validate
     0 | a:              1 | b:              2 |
     1 | a:              2 | b:              4 |
     2 | a:              3 | b:              6 |

timing
> load: 295ms (295483393ns)
> populate_context: 0ms (193146ns)
> execute: 8ms (8218206ns)
> execute > step0: 6ms (6931532ns)
> load_expected: 0ms (176204ns)

[success] Total time: 2 s, completed 19-Jul-2018 12:17:05 PM
```

If you do not receive the expected output, log an issue.

### Playing with Rules

Having completed the smoke test, the next step is to understand the
[Xalgo
specification](https://github.com/Xalgorithms/general-documentation/blob/master/docs/xalgo.md)
and write some rules of your own. Within the `test-runs/` directory in
the project, there are a number of example Xalgo rules that can serve
as starting points for your own rules.

The [Xalgo compiler](https://github.com/Xalgorithms/xa-rules) is
implemented as a different library, in ruby. This project contains the
`test-run.sh` helper script that will run the compiler for you. If you
want to run validation **without** recompiling the rules, do (using
the previous example):

```
$ sbt "runMain org.xalgorithms.rules.Runner test-runs/validate"
```

### Test Run Structure

In the project, there is a directory called `test-runs/`. Any test of
the interpreter should be created in that directory. The directory
format follows a basic structure:

- <name>.rule: A rule that will be compiled and executed.
- <name>.context.json: An initial context to be loaded before <name> is executed.
- <name>.expected.json: An expected set out outputs for the test runs
  (if this is **not** created, then the test run merely dumps the
  context
- tables/<namespace>/<version>/<table_name>.json: Any tables that are
  referenced in a `REQUIRE` step will be loaded from the `tables/`
  directory. The structure of the directory follows the structure of
  the tables references in Xalgo.
