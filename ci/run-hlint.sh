#!/bin/sh
# This script was originally sourced from
# https://github.com/ndmitchell/neil/blob/b06624fe697c23375222856d538cb974e96da048/misc/run.sh
# and adapted for PureScript to do the following:
#   * specialize for hlint instead of an arbitrary ndmitchell project
#   * use a specified version, instead of the most recent release
#   * install to a native temporary directory instead of a subdirectory of the project
#   * make curl silent
#
# The original script was distributed with the following license, also available at
# https://github.com/ndmitchell/neil/blob/b06624fe697c23375222856d538cb974e96da048/LICENSE
#
#   Copyright (c) Neil Mitchell 2010-2021
#   All rights reserved.
#
#   Redistribution and use in source and binary forms, with or without
#   modification, are permitted provided that the following conditions are
#   met:
#
#       * Redistributions of source code must retain the above copyright
#         notice, this list of conditions and the following disclaimer.
#
#       * Redistributions in binary form must reproduce the above
#         copyright notice, this list of conditions and the following
#         disclaimer in the documentation and/or other materials provided
#         with the distribution.
#
#       * The names of its contributors may not be used to endorse or
#         promote products derived from this software without specific prior
#         written permission.
#
#   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
#   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
#   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
#   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
#   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
#   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
#   LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
#   DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
#   THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
#   (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
#   OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

# This script is invoked from my Travis commands
# It bootstraps to grab the a binary release and run it
set -e # exit on errors

PACKAGE=hlint
if [ -z "$VERSION" ]; then
    echo The environment variable VERSION is required
    exit 1
fi

case "$(uname)" in
    "Darwin")
        OS=osx;;
    MINGW64_NT-*|MSYS_NT-*)
        OS=windows;;
    *)
        OS=linux
esac

if [ "$OS" = "windows" ]; then
    EXT=.zip
else
    EXT=.tar.gz
fi

echo Downloading and running $PACKAGE...
URL=https://github.com/ndmitchell/$PACKAGE/releases/download/v$VERSION/$PACKAGE-$VERSION-x86_64-$OS$EXT
TEMP=$(mktemp -d ${TEMP:-/tmp}/.$PACKAGE-XXXXXX)

cleanup(){
    rm -r $TEMP
}
trap cleanup EXIT

retry(){
    ($@) && return
    sleep 15
    ($@) && return
    sleep 15
    $@
}

retry curl --silent --location -o$TEMP/$PACKAGE$EXT $URL
if [ "$OS" = "windows" ]; then
    7z x -y $TEMP/$PACKAGE$EXT -o$TEMP -r > /dev/null
else
    tar -xzf $TEMP/$PACKAGE$EXT -C$TEMP
fi
$TEMP/$PACKAGE-$VERSION/$PACKAGE $*
