#!/bin/bash

# test args
if [ ! ${#} -ge 2 ]; then
    echo 1>&2 "Usage: ${0} LOCAL REMOTE [MERGED BASE]"
    echo 1>&2 "       (LOCAL, REMOTE, MERGED, BASE can be provided by \`git mergetool'.)"
    exit 1
fi

# tools
_EMACSCLIENT=emacsclient
#_EMACSCLIENT=/usr/bin/emacs
_BASENAME=basename
_CP=cp
_EGREP=egrep
_MKTEMP=mktemp

# args
_LOCAL=${1}
_REMOTE=${2}
if [ ${3} ] ; then
    _MERGED=${3}
else
    _MERGED=${_REMOTE}
fi
if [ ${4} -a -r ${4} ] ; then
    _BASE=${4}
    _EDIFF=ediff-merge-files-with-ancestor
    _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\" \"${_BASE}\" nil \"${_MERGED}\""
elif [ ${_REMOTE} = ${_MERGED} ] ; then
    _EDIFF=ediff
    _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\""
else
    _EDIFF=ediff-merge-files
    _EVAL="${_EDIFF} \"${_LOCAL}\" \"${_REMOTE}\" nil \"${_MERGED}\""
fi

# console vs. X
if [ "${TERM}" = "linux" ]; then
    unset DISPLAY
    _EMACSCLIENTOPTS=" -t -s ~/.emacs.d/server"
else
    _EMACSCLIENTOPTS=" -n -s ~/.emacs.d/server"
fi

# run emacsclient
echo faraz ${_EMACSCLIENT} ${_EMACSCLIENTOPTS} --eval "(${_EVAL})" 2>&1
${_EMACSCLIENT} ${_EMACSCLIENTOPTS} --eval "(${_EVAL})" 2>&1

# check modified file
if [ ! $(egrep -c '^(<<<<<<<|=======|>>>>>>>|####### Ancestor)' ${_MERGED}) = 0 ]; then
    _MERGEDSAVE=$(${_MKTEMP} --tmpdir `${_BASENAME} ${_MERGED}`.XXXXXXXXXX)
    ${_CP} ${_MERGED} ${_MERGEDSAVE}
    echo 1>&2 "Oops! Conflict markers detected in $_MERGED."
    echo 1>&2 "Saved your changes to ${_MERGEDSAVE}"
    echo 1>&2 "Exiting with code 1."
    exit 1
fi

exit 0
