FROM fpco/stack-build:lts-4.2

ADD ./Makefile cnf-mutable-tests/Makefile

RUN cd ./cnf-mutable-tests/ && make submod
RUN cd ./cnf-mutable-tests/ && make ghc
# RUN cd ./cnf-mutable-tests/ &&

# ADD ./ cnf-mutable-tests/
