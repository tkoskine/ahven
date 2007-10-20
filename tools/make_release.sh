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
# ./make_release <place> <version>
#
# For example
# ./make_release /tags/ahven_1_0 1.0
#

SVNROOT=svn+ssh://${USER}@svn.gna.org/svn/ahven

failure()
{
    echo "$1"
    exit 1
}

if [ x"$2" = x"" ]; then
    echo "usage: make_release <place> <version>"
    exit 1
fi

PLACE=$1
VERSION=$2

cd /tmp || "cd /tmp failed"
svn export $SVNROOT/$PLACE ahven-$VERSION || failure "checkout failed"
tar zcf ahven-$VERSION.tar.gz ahven-$VERSION || failure "tar zcf failed"

echo "Release tarball ready at /tmp/ahven-$VERSION.tar.gz"
echo "Please remove /tmp/ahven-$VERSION directory."

