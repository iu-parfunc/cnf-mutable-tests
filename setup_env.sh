# This file should be sourced with:
#   . setup_env.sh

# Bring the GHC with Data.Compact into path:

DIR=opt/ghc-mutable-cnf-0.2/bin/

if [ -d ~parfunc/$DIR ]; then
    NEWGHC=~parfunc/$DIR
elif [ -d ~crest-team/$DIR ]; then
    NEWGHC=~crest-team/$DIR
elif [ -d $HOME/$DIR ]; then
    NEWGHC=$HOME/$DIR
else
    echo "Couldn't find ${DIR}"
    exit 1
fi

export PATH=$NEWGHC:$PATH

export STACK_YAML=stack-7.11.cnf.yaml
