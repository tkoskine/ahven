#!/bin/sh
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
svn checkout $SVNROOT/$PLACE ahven-$VERSION || failure "checkout failed"
tar zcf ahven-$VERSION.tar.gz ahven-$VERSION || failure "tar zcf failed"

echo "Release tarball ready at /tmp/ahven-$VERSION.tar.gz"

