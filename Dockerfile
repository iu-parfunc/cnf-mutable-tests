FROM fpco/stack-build:lts-4.2

ADD Makefile Main.hs cnf-mutable-tests/

WORKDIR cnf-mutable-tests

RUN cabal update

# ARG JOBS
RUN make -j  all
