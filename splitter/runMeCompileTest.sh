#!/usr/bin/env bash

MB=$((1024*1024))
PATTERNWRITER=`pwd`/patternwriter
SPLITTER=`pwd`"/fileSplitter"
# UNCOMMENT FOLLOWING LINE FOR VERBOSE LOGGING
#SPLITTER=`pwd`"/fileSplitter --verbose "
MD5SUM=md5sum

if [ $SHELL != "/bin/bash" ]
then
    echo "This is meant to be run using /bin/bash shell, your shell is $SHELL"
    echo "Please make a bash shell available at /bin/bash"
    exit
fi

compile() {
## Compile binaries
    make clean
    make

    if [ ! -f $PATTERNWRITER ];
    then
	echo "cannot find $PATTERNWRITER binary"
	exit
    fi

    if [ ! -f $SPLITTER ];
    then
	echo "cannot find filesplitter binary"
	exit
    fi
}


createTestEnv() {
    echo "Create test dir"
    rm -rf ./testDir
    mkdir ./testDir
    cd ./testDir
}

cleanupTestEnv() {
    echo "Cleaning up"
    cd ..
    rm -rf ./testDir
}

#stat tests
statTest() {
    echo "Running Stat Tests"
    for i in 15 20
    do
	FILENAME=$(($i * MB)).origfile
	OUTPUTDIRNAME=$(($i * MB)).sharddir
	$PATTERNWRITER a,$(($i * MB)) $FILENAME

	echo "Created file $FILENAME SIZE $((i)) MB MD5SUM=$FILEMD5"
# Split a file
	$SPLITTER  --filename  $FILENAME --outputdirname $OUTPUTDIRNAME --shardsize=$MB
	echo "Done splitting file $FILENAME"
#Reassemble
	echo "Reassembling ...."
	cd $OUTPUTDIRNAME
	for shardName in `ls ./*.p`
	do
	    echo -ne "Reconciling: $FILENAME->$shardName\r"
	    $SPLITTER --reconcile $shardName
	    $SPLITTER --reconcilestat
	done
	cd ..
    done
}

integrityTest() {
    echo "Running integrity test"
    for i in 1 2 3 5 7 10
    do
# Create a file
	FILENAME=$(($i * MB)).origfile
	OUTPUTDIRNAME=$(($i * MB)).sharddir
	$PATTERNWRITER a,$(($i * MB)) $FILENAME
	FILEMD5=`$MD5SUM $FILENAME  | cut  -f 1 -d " "`
	echo "Created file $FILENAME SIZE $((i)) MB MD5SUM=$FILEMD5"

# Split a file
	$SPLITTER  --filename  $FILENAME --outputdirname $OUTPUTDIRNAME --shardsize=1024
	echo "Done splitting file $FILENAME"

#Reassemble
	echo "Reassembling ...."
	cd $OUTPUTDIRNAME
	for shardName in `ls ./*.p`
	do
	    echo -ne "Reconciling: $FILENAME->$shardName\r"
	    $SPLITTER --reconcile $shardName
	done

	echo ""
	echo "Done reassembling file $FILENAME"
	RECONCILEFILEMD5=`$MD5SUM $FILENAME  | cut  -f 1 -d " "`

	if [ $RECONCILEFILEMD5 == $FILEMD5 ]
	then
	    echo "File Integrity OK: MD5SUM -> $FILEMD5"
	else
	    echo "File Integrity failed EXPECTED MD5 $FILEMD5 got $RECONCILEFILEMD5"
            exit
	fi
	cd ..
    done
}

main() {
    compile
# fail on any error
    set -e
    createTestEnv

    integrityTest
    statTest
    cleanupTestEnv
echo "Splitter Test Success"
}


main