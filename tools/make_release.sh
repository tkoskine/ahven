#!/bin/sh
#
# Copyright (c) 2007 Tero Koskinen <tero.koskinen@iki.fi>
#
# Permission to use, copy, modify, and distribute this software for any
# purpose with or without fee is hereby granted, provided that the above
# copyright notice and this permission notice appear in all copies.
#
# THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
# WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
# MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
# ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
# WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
# ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
# OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
#
#
# ./make_release <version>
#
# For example
# ./make_release 1.0
#

HGROOT=https://bitbucket.org/tkoskine/ahven

failure()
{
    echo "$1"
    exit 1
}

if [ x"$1" = x"" ]; then
    echo "usage: make_release <version>"
    exit 1
fi

if [ x"$2" != x"" ]; then
    HGROOT=$2
    echo "Using $HGROOT as repository"
fi

VERSION=$1

AHVEN_TMP_DIR=`mktemp -d`

cd $AHVEN_TMP_DIR || failure "cd $AHVEN_TMP_DIR failed"
hg clone $HGROOT ahven-$VERSION || failure "checkout failed"
cd ahven-$VERSION || failure "cd failed"
hg archive -p ahven-$VERSION $AHVEN_TMP_DIR/ahven-$VERSION.tar.gz || failure "hg archive .tar.gz failed"
hg archive -p ahven-$VERSION $AHVEN_TMP_DIR/ahven-$VERSION.zip || failure "hg archive .zip failed"
cd ..

echo "Release tarball ready at $AHVEN_TMP_DIR/ahven-$VERSION.tar.gz"
echo "Release zip ready at $AHVEN_TMP_DIR/ahven-$VERSION.zip"
echo "Please remove $AHVEN_TMP_DIR/ahven-$VERSION directory."

echo

