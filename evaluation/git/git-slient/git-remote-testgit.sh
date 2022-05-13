#!/bin/sh
# Copyright (c) 2012 Felipe Contreras

alias=$1
url=$2

dir="$GIT_DIR/testgit/$alias"
prefix="refs/testgit/$alias"

default_refspec="refs/heads/*:${prefix}/heads/*"

refspec="${GIT_REMOTE_TESTGIT_REFSPEC-$default_refspec}"

test -z "$refspec" && prefix="refs"

export GIT_DIR="$url/.git"

mkdir -p "$dir"

if test -z "$GIT_REMOTE_TESTGIT_NO_MARKS"
then
	gitmarks="$dir/git.marks"
	testgitmarks="$dir/testgit.marks"
	test -e "$gitmarks" || >"$gitmarks"
	test -e "$testgitmarks" || >"$testgitmarks"
fi

while read line
do
	case $line in
	capabilities)
		echo 'import'
		echo 'export'
		test -n "$refspec" && echo "refspec $refspec"
		if test -n "$gitmarks"
		then
			echo "*import-marks $gitmarks"
			echo "*export-marks $gitmarks"
		fi
		test -n "$GIT_REMOTE_TESTGIT_SIGNED_TAGS" && echo "signed-tags"
		test -n "$GIT_REMOTE_TESTGIT_NO_PRIVATE_UPDATE" && echo "no-private-update"
		echo
		;;
	list)
		git for-each-ref --format='? %(refname)' 'refs/heads/'
		head=$(git symbolic-ref HEAD)
		echo "@$head HEAD"
		echo
		;;
	import*)
		# read all import lines
		while true
		do
			ref="${line#* }"
			refs="$refs $ref"
			read line
			test "${line%% *}" != "import" && break
		done

		if test -n "$gitmarks"
		then
			echo "feature import-marks=$gitmarks"
			echo "feature export-marks=$gitmarks"
		fi

		if test -n "$GIT_REMOTE_TESTGIT_FAILURE"
		then
			echo "feature done"
			exit 1
		fi

		echo "feature done"
		git fast-export \
			${testgitmarks:+"--import-marks=$testgitmarks"} \
			${testgitmarks:+"--export-marks=$testgitmarks"} \
			$refs |
		sed -e "s#refs/heads/#${prefix}/heads/#g"
		echo "done"
		;;
	export)
		if test -n "$GIT_REMOTE_TESTGIT_FAILURE"
		then
			# consume input so fast-export doesn't get SIGPIPE;
			# git would also notice that case, but we want
			# to make sure we are exercising the later
			# error checks
			while read line; do
				test "done" = "$line" && break
			done
			exit 1
		fi

		before=$(git for-each-ref --format=' %(refname) %(objectname) ')

		git fast-import \
			${testgitmarks:+"--import-marks=$testgitmarks"} \
			${testgitmarks:+"--export-marks=$testgitmarks"} \
			--quiet

		# figure out which refs were updated
		git for-each-ref --format='%(refname) %(objectname)' |
		while read ref a
		do
			case "$before" in
			*" $ref $a "*)
				continue ;;	# unchanged
			esac
			if test -z "$GIT_REMOTE_TESTGIT_PUSH_ERROR"
			then
				echo "ok $ref"
			else
				echo "error $ref $GIT_REMOTE_TESTGIT_PUSH_ERROR"
			fi
		done

		echo
		;;
	'')
		exit
		;;
	esac
done
