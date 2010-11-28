#! /usr/bin/sh

dump=$1
cnt=0
tmp=$TMP/git-archive-`date +%s`
tmptmp=$TMP/git-archive-tmp
tmptar=$tmptmp.tar
base=`cat $dump|sed -e '2,$d'`
cwd=`pwd`
cygpath=echo
if cygpath --help 2>&1 >/dev/null;then
    cygpath=cygpath
fi

echo "base : $base"
for repos in `cat $dump|sed -e '1,2d'`;do
    local_repos=$(echo $repos |sed -e 's#^'$base'##')
    mkdir -p $tmptmp
    cd $tmptmp

    echo git clone `cygpath $repos` .
    git clone `cygpath $repos` .
    git remote rm origin
    fetch=$(cd $repos; git remote show origin|grep "Fetch URL:")
    if test -z $fetch; then
        echo
    else
        git remote add origin $(echo $fetch|sed -e 's/Fetch URL://')
    fi

    rm -f $tmptar
    tar -cvf $tmptar .

    copy=$tmp$local_repos
    if ! test -d $copy ; then
        echo mkdir -p $copy
        mkdir -p $copy
    fi
    cd $copy
    tar -xvf $tmptar

    rm -f $tmptar
    cd $cwd

    echo rm -rf $tmptmp
    rm -rf $tmptmp
done
cd $tmp
tar -cvzf $cwd/gitrepos-`date +%Y-%m-%d.%H.%M.%S`.tar.gz .
cd $cwd
rm -fr $tmp


