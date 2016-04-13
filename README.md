# cnf-mutable-tests

Mutable objects inside compact regions in GHC

### Build Status

 * Travis:
   [![Build Status](https://travis-ci.org/iu-parfunc/cnf-mutable-tests.svg?branch=master)](https://travis-ci.org/iu-parfunc/cnf-mutable-tests)
 * Jenkins:
   [![Build Status](http://tester-lin.soic.indiana.edu:8080/buildStatus/icon?job=cnf-mutable-tests_docker)](http://tester-lin.soic.indiana.edu:8080/job/cnf-mutable-tests_docker)
   [![Build Status](http://tester-lin.soic.indiana.edu:8080/buildStatus/icon?job=cnf-mutable-tests_naked)](http://tester-lin.soic.indiana.edu:8080/job/cnf-mutable-tests_naked)
 * Docker:
   [![Docker Stars](https://img.shields.io/docker/stars/vikraman/ghc-mutable-cnf.svg)](https://hub.docker.com/r/vikraman/ghc-mutable-cnf)
   [![Docker Pulls](https://img.shields.io/docker/pulls/vikraman/ghc-mutable-cnf.svg)](https://hub.docker.com/r/vikraman/ghc-mutable-cnf)

### Description

Experiments with various mutable objects inside a compact.

- `compact-fake`: Dummy compact implementation.

- `compact-indexed`: Indexed version of the compact api.

- `Data.CNFRef`: Mutable CNF objects.

- `Data.IntBox`: Mutable vector inside a `CNFRef`

- `Data.CList`: Mutable compact linked-list like data structure.
