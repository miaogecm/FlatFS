# Library of functions shared by all tests scripts, included by
# test-lib.sh.
#
# Copyright (c) 2005 Junio C Hamano
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see http://www.gnu.org/licenses/ .

# The semantics of the editor variables are that of invoking
# sh -c "$EDITOR \"$@\"" files ...
#
# If our trash directory contains shell metacharacters, they will be
# interpreted if we just set $EDITOR directly, so do a little dance with
# environment variables to work around this.
#
# In particular, quoting isn't enough, as the path may contain the same quote
# that we're using.
test_set_editor () {
	FAKE_EDITOR="$1"
	export FAKE_EDITOR
	EDITOR='"$FAKE_EDITOR"'
	export EDITOR
}

test_decode_color () {
	awk '
		function name(n) {
			if (n == 0) return "RESET";
			if (n == 1) return "BOLD";
			if (n == 30) return "BLACK";
			if (n == 31) return "RED";
			if (n == 32) return "GREEN";
			if (n == 33) return "YELLOW";
			if (n == 34) return "BLUE";
			if (n == 35) return "MAGENTA";
			if (n == 36) return "CYAN";
			if (n == 37) return "WHITE";
			if (n == 40) return "BLACK";
			if (n == 41) return "BRED";
			if (n == 42) return "BGREEN";
			if (n == 43) return "BYELLOW";
			if (n == 44) return "BBLUE";
			if (n == 45) return "BMAGENTA";
			if (n == 46) return "BCYAN";
			if (n == 47) return "BWHITE";
		}
		{
			while (match($0, /\033\[[0-9;]*m/) != 0) {
				printf "%s<", substr($0, 1, RSTART-1);
				codes = substr($0, RSTART+2, RLENGTH-3);
				if (length(codes) == 0)
					printf "%s", name(0)
				else {
					n = split(codes, ary, ";");
					sep = "";
					for (i = 1; i <= n; i++) {
						printf "%s%s", sep, name(ary[i]);
						sep = ";"
					}
				}
				printf ">";
				$0 = substr($0, RSTART + RLENGTH, length($0) - RSTART - RLENGTH + 1);
			}
			print
		}
	'
}

nul_to_q () {
	perl -pe 'y/\000/Q/'
}

q_to_nul () {
	perl -pe 'y/Q/\000/'
}

q_to_cr () {
	tr Q '\015'
}

q_to_tab () {
	tr Q '\011'
}

qz_to_tab_space () {
	tr QZ '\011\040'
}

append_cr () {
	sed -e 's/$/Q/' | tr Q '\015'
}

remove_cr () {
	tr '\015' Q | sed -e 's/Q$//'
}

# In some bourne shell implementations, the "unset" builtin returns
# nonzero status when a variable to be unset was not set in the first
# place.
#
# Use sane_unset when that should not be considered an error.

sane_unset () {
	unset "$@"
	return 0
}

test_tick () {
	if test -z "${test_tick+set}"
	then
		test_tick=1112911993
	else
		test_tick=$(($test_tick + 60))
	fi
	GIT_COMMITTER_DATE="$test_tick -0700"
	GIT_AUTHOR_DATE="$test_tick -0700"
	export GIT_COMMITTER_DATE GIT_AUTHOR_DATE
}

# Stop execution and start a shell. This is useful for debugging tests and
# only makes sense together with "-v".
#
# Be sure to remove all invocations of this command before submitting.

test_pause () {
	if test "$verbose" = t; then
		"$SHELL_PATH" <&6 >&3 2>&4
	else
		error >&5 "test_pause requires --verbose"
	fi
}

# Call test_commit with the arguments "<message> [<file> [<contents> [<tag>]]]"
#
# This will commit a file with the given contents and the given commit
# message, and tag the resulting commit with the given tag name.
#
# <file>, <contents>, and <tag> all default to <message>.

test_commit () {
	notick= &&
	signoff= &&
	while test $# != 0
	do
		case "$1" in
		--notick)
			notick=yes
			;;
		--signoff)
			signoff="$1"
			;;
		*)
			break
			;;
		esac
		shift
	done &&
	file=${2:-"$1.t"} &&
	echo "${3-$1}" > "$file" &&
	git add "$file" &&
	if test -z "$notick"
	then
		test_tick
	fi &&
	git commit $signoff -m "$1" &&
	git tag "${4:-$1}"
}

# Call test_merge with the arguments "<message> <commit>", where <commit>
# can be a tag pointing to the commit-to-merge.

test_merge () {
	test_tick &&
	git merge -m "$1" "$2" &&
	git tag "$1"
}

# This function helps systems where core.filemode=false is set.
# Use it instead of plain 'chmod +x' to set or unset the executable bit
# of a file in the working directory and add it to the index.

test_chmod () {
	chmod "$@" &&
	git update-index --add "--chmod=$@"
}

# Unset a configuration variable, but don't fail if it doesn't exist.
test_unconfig () {
	git config --unset-all "$@"
	config_status=$?
	case "$config_status" in
	5) # ok, nothing to unset
		config_status=0
		;;
	esac
	return $config_status
}

# Set git config, automatically unsetting it after the test is over.
test_config () {
	test_when_finished "test_unconfig '$1'" &&
	git config "$@"
}

test_config_global () {
	test_when_finished "test_unconfig --global '$1'" &&
	git config --global "$@"
}

write_script () {
	{
		echo "#!${2-"$SHELL_PATH"}" &&
		cat
	} >"$1" &&
	chmod +x "$1"
}

# Use test_set_prereq to tell that a particular prerequisite is available.
# The prerequisite can later be checked for in two ways:
#
# - Explicitly using test_have_prereq.
#
# - Implicitly by specifying the prerequisite tag in the calls to
#   test_expect_{success,failure,code}.
#
# The single parameter is the prerequisite tag (a simple word, in all
# capital letters by convention).

test_set_prereq () {
	satisfied_prereq="$satisfied_prereq$1 "
}
satisfied_prereq=" "
lazily_testable_prereq= lazily_tested_prereq=

# Usage: test_lazy_prereq PREREQ 'script'
test_lazy_prereq () {
	lazily_testable_prereq="$lazily_testable_prereq$1 "
	eval test_prereq_lazily_$1=\$2
}

test_run_lazy_prereq_ () {
	script='
mkdir -p "$TRASH_DIRECTORY/prereq-test-dir" &&
(
	cd "$TRASH_DIRECTORY/prereq-test-dir" &&'"$2"'
)'
	say >&3 "checking prerequisite: $1"
	say >&3 "$script"
	test_eval_ "$script"
	eval_ret=$?
	rm -rf "$TRASH_DIRECTORY/prereq-test-dir"
	if test "$eval_ret" = 0; then
		say >&3 "prerequisite $1 ok"
	else
		say >&3 "prerequisite $1 not satisfied"
	fi
	return $eval_ret
}

test_have_prereq () {
	# prerequisites can be concatenated with ','
	save_IFS=$IFS
	IFS=,
	set -- $*
	IFS=$save_IFS

	total_prereq=0
	ok_prereq=0
	missing_prereq=

	for prerequisite
	do
		case "$prerequisite" in
		!*)
			negative_prereq=t
			prerequisite=${prerequisite#!}
			;;
		*)
			negative_prereq=
		esac

		case " $lazily_tested_prereq " in
		*" $prerequisite "*)
			;;
		*)
			case " $lazily_testable_prereq " in
			*" $prerequisite "*)
				eval "script=\$test_prereq_lazily_$prerequisite" &&
				if test_run_lazy_prereq_ "$prerequisite" "$script"
				then
					test_set_prereq $prerequisite
				fi
				lazily_tested_prereq="$lazily_tested_prereq$prerequisite "
			esac
			;;
		esac

		total_prereq=$(($total_prereq + 1))
		case "$satisfied_prereq" in
		*" $prerequisite "*)
			satisfied_this_prereq=t
			;;
		*)
			satisfied_this_prereq=
		esac

		case "$satisfied_this_prereq,$negative_prereq" in
		t,|,t)
			ok_prereq=$(($ok_prereq + 1))
			;;
		*)
			# Keep a list of missing prerequisites; restore
			# the negative marker if necessary.
			prerequisite=${negative_prereq:+!}$prerequisite
			if test -z "$missing_prereq"
			then
				missing_prereq=$prerequisite
			else
				missing_prereq="$prerequisite,$missing_prereq"
			fi
		esac
	done

	test $total_prereq = $ok_prereq
}

test_declared_prereq () {
	case ",$test_prereq," in
	*,$1,*)
		return 0
		;;
	esac
	return 1
}

test_expect_failure () {
	test_start_
	test "$#" = 3 && { test_prereq=$1; shift; } || test_prereq=
	test "$#" = 2 ||
	error "bug in the test script: not 2 or 3 parameters to test-expect-failure"
	export test_prereq
	if ! test_skip "$@"
	then
		say >&3 "checking known breakage: $2"
		if test_run_ "$2" expecting_failure
		then
			test_known_broken_ok_ "$1"
		else
			test_known_broken_failure_ "$1"
		fi
	fi
	test_finish_
}

test_expect_success () {
	test_start_
	test "$#" = 3 && { test_prereq=$1; shift; } || test_prereq=
	test "$#" = 2 ||
	error "bug in the test script: not 2 or 3 parameters to test-expect-success"
	export test_prereq
	if ! test_skip "$@"
	then
		say >&3 "expecting success: $2"
		if test_run_ "$2"
		then
			test_ok_ "$1"
		else
			test_failure_ "$@"
		fi
	fi
	test_finish_
}

# test_external runs external test scripts that provide continuous
# test output about their progress, and succeeds/fails on
# zero/non-zero exit code.  It outputs the test output on stdout even
# in non-verbose mode, and announces the external script with "# run
# <n>: ..." before running it.  When providing relative paths, keep in
# mind that all scripts run in "trash directory".
# Usage: test_external description command arguments...
# Example: test_external 'Perl API' perl ../path/to/test.pl
test_external () {
	test "$#" = 4 && { test_prereq=$1; shift; } || test_prereq=
	test "$#" = 3 ||
	error >&5 "bug in the test script: not 3 or 4 parameters to test_external"
	descr="$1"
	shift
	export test_prereq
	if ! test_skip "$descr" "$@"
	then
		# Announce the script to reduce confusion about the
		# test output that follows.
		say_color "" "# run $test_count: $descr ($*)"
		# Export TEST_DIRECTORY, TRASH_DIRECTORY and GIT_TEST_LONG
		# to be able to use them in script
		export TEST_DIRECTORY TRASH_DIRECTORY GIT_TEST_LONG
		# Run command; redirect its stderr to &4 as in
		# test_run_, but keep its stdout on our stdout even in
		# non-verbose mode.
		"$@" 2>&4
		if [ "$?" = 0 ]
		then
			if test $test_external_has_tap -eq 0; then
				test_ok_ "$descr"
			else
				say_color "" "# test_external test $descr was ok"
				test_success=$(($test_success + 1))
			fi
		else
			if test $test_external_has_tap -eq 0; then
				test_failure_ "$descr" "$@"
			else
				say_color error "# test_external test $descr failed: $@"
				test_failure=$(($test_failure + 1))
			fi
		fi
	fi
}

# Like test_external, but in addition tests that the command generated
# no output on stderr.
test_external_without_stderr () {
	# The temporary file has no (and must have no) security
	# implications.
	tmp=${TMPDIR:-/tmp}
	stderr="$tmp/git-external-stderr.$$.tmp"
	test_external "$@" 4> "$stderr"
	[ -f "$stderr" ] || error "Internal error: $stderr disappeared."
	descr="no stderr: $1"
	shift
	say >&3 "# expecting no stderr from previous command"
	if [ ! -s "$stderr" ]; then
		rm "$stderr"

		if test $test_external_has_tap -eq 0; then
			test_ok_ "$descr"
		else
			say_color "" "# test_external_without_stderr test $descr was ok"
			test_success=$(($test_success + 1))
		fi
	else
		if [ "$verbose" = t ]; then
			output=`echo; echo "# Stderr is:"; cat "$stderr"`
		else
			output=
		fi
		# rm first in case test_failure exits.
		rm "$stderr"
		if test $test_external_has_tap -eq 0; then
			test_failure_ "$descr" "$@" "$output"
		else
			say_color error "# test_external_without_stderr test $descr failed: $@: $output"
			test_failure=$(($test_failure + 1))
		fi
	fi
}

# debugging-friendly alternatives to "test [-f|-d|-e]"
# The commands test the existence or non-existence of $1. $2 can be
# given to provide a more precise diagnosis.
test_path_is_file () {
	if ! [ -f "$1" ]
	then
		echo "File $1 doesn't exist. $*"
		false
	fi
}

test_path_is_dir () {
	if ! [ -d "$1" ]
	then
		echo "Directory $1 doesn't exist. $*"
		false
	fi
}

test_path_is_missing () {
	if [ -e "$1" ]
	then
		echo "Path exists:"
		ls -ld "$1"
		if [ $# -ge 1 ]; then
			echo "$*"
		fi
		false
	fi
}

# test_line_count checks that a file has the number of lines it
# ought to. For example:
#
#	test_expect_success 'produce exactly one line of output' '
#		do something >output &&
#		test_line_count = 1 output
#	'
#
# is like "test $(wc -l <output) = 1" except that it passes the
# output through when the number of lines is wrong.

test_line_count () {
	if test $# != 3
	then
		error "bug in the test script: not 3 parameters to test_line_count"
	elif ! test $(wc -l <"$3") "$1" "$2"
	then
		echo "test_line_count: line count for $3 !$1 $2"
		cat "$3"
		return 1
	fi
}

# This is not among top-level (test_expect_success | test_expect_failure)
# but is a prefix that can be used in the test script, like:
#
#	test_expect_success 'complain and die' '
#           do something &&
#           do something else &&
#	    test_must_fail git checkout ../outerspace
#	'
#
# Writing this as "! git checkout ../outerspace" is wrong, because
# the failure could be due to a segv.  We want a controlled failure.

test_must_fail () {
	"$@"
	exit_code=$?
	if test $exit_code = 0; then
		echo >&2 "test_must_fail: command succeeded: $*"
		return 1
	elif test $exit_code -gt 129 -a $exit_code -le 192; then
		echo >&2 "test_must_fail: died by signal: $*"
		return 1
	elif test $exit_code = 127; then
		echo >&2 "test_must_fail: command not found: $*"
		return 1
	elif test $exit_code = 126; then
		echo >&2 "test_must_fail: valgrind error: $*"
		return 1
	fi
	return 0
}

# Similar to test_must_fail, but tolerates success, too.  This is
# meant to be used in contexts like:
#
#	test_expect_success 'some command works without configuration' '
#		test_might_fail git config --unset all.configuration &&
#		do something
#	'
#
# Writing "git config --unset all.configuration || :" would be wrong,
# because we want to notice if it fails due to segv.

test_might_fail () {
	"$@"
	exit_code=$?
	if test $exit_code -gt 129 -a $exit_code -le 192; then
		echo >&2 "test_might_fail: died by signal: $*"
		return 1
	elif test $exit_code = 127; then
		echo >&2 "test_might_fail: command not found: $*"
		return 1
	fi
	return 0
}

# Similar to test_must_fail and test_might_fail, but check that a
# given command exited with a given exit code. Meant to be used as:
#
#	test_expect_success 'Merge with d/f conflicts' '
#		test_expect_code 1 git merge "merge msg" B master
#	'

test_expect_code () {
	want_code=$1
	shift
	"$@"
	exit_code=$?
	if test $exit_code = $want_code
	then
		return 0
	fi

	echo >&2 "test_expect_code: command exited with $exit_code, we wanted $want_code $*"
	return 1
}

# test_cmp is a helper function to compare actual and expected output.
# You can use it like:
#
#	test_expect_success 'foo works' '
#		echo expected >expected &&
#		foo >actual &&
#		test_cmp expected actual
#	'
#
# This could be written as either "cmp" or "diff -u", but:
# - cmp's output is not nearly as easy to read as diff -u
# - not all diff versions understand "-u"

test_cmp() {
	$GIT_TEST_CMP "$@"
}

# Check if the file expected to be empty is indeed empty, and barfs
# otherwise.

test_must_be_empty () {
	if test -s "$1"
	then
		echo "'$1' is not empty, it contains:"
		cat "$1"
		return 1
	fi
}

# Tests that its two parameters refer to the same revision
test_cmp_rev () {
	git rev-parse --verify "$1" >expect.rev &&
	git rev-parse --verify "$2" >actual.rev &&
	test_cmp expect.rev actual.rev
}

# Print a sequence of numbers or letters in increasing order.  This is
# similar to GNU seq(1), but the latter might not be available
# everywhere (and does not do letters).  It may be used like:
#
#	for i in `test_seq 100`; do
#		for j in `test_seq 10 20`; do
#			for k in `test_seq a z`; do
#				echo $i-$j-$k
#			done
#		done
#	done

test_seq () {
	case $# in
	1)	set 1 "$@" ;;
	2)	;;
	*)	error "bug in the test script: not 1 or 2 parameters to test_seq" ;;
	esac
	perl -le 'print for $ARGV[0]..$ARGV[1]' -- "$@"
}

# This function can be used to schedule some commands to be run
# unconditionally at the end of the test to restore sanity:
#
#	test_expect_success 'test core.capslock' '
#		git config core.capslock true &&
#		test_when_finished "git config --unset core.capslock" &&
#		hello world
#	'
#
# That would be roughly equivalent to
#
#	test_expect_success 'test core.capslock' '
#		git config core.capslock true &&
#		hello world
#		git config --unset core.capslock
#	'
#
# except that the greeting and config --unset must both succeed for
# the test to pass.
#
# Note that under --immediate mode, no clean-up is done to help diagnose
# what went wrong.

test_when_finished () {
	test_cleanup="{ $*
		} && (exit \"\$eval_ret\"); eval_ret=\$?; $test_cleanup"
}

# Most tests can use the created repository, but some may need to create more.
# Usage: test_create_repo <directory>
test_create_repo () {
	test "$#" = 1 ||
	error "bug in the test script: not 1 parameter to test-create-repo"
	repo="$1"
	mkdir -p "$repo"
	(
		cd "$repo" || error "Cannot setup test environment"
		"$GIT_EXEC_PATH/git-init" "--template=$GIT_BUILD_DIR/templates/blt/" >&3 2>&4 ||
		error "cannot run git init -- have you built things yet?"
		mv .git/hooks .git/hooks-disabled
	) || exit
}

# This function helps on symlink challenged file systems when it is not
# important that the file system entry is a symbolic link.
# Use test_ln_s_add instead of "ln -s x y && git add y" to add a
# symbolic link entry y to the index.

test_ln_s_add () {
	if test_have_prereq SYMLINKS
	then
		ln -s "$1" "$2" &&
		git update-index --add "$2"
	else
		printf '%s' "$1" >"$2" &&
		ln_s_obj=$(git hash-object -w "$2") &&
		git update-index --add --cacheinfo 120000 $ln_s_obj "$2"
	fi
}

perl () {
	command "$PERL_PATH" "$@"
}

# The following mingw_* functions obey POSIX shell syntax, but are actually
# bash scripts, and are meant to be used only with bash on Windows.

# A test_cmp function that treats LF and CRLF equal and avoids to fork
# diff when possible.
mingw_test_cmp () {
	# Read text into shell variables and compare them. If the results
	# are different, use regular diff to report the difference.
	local test_cmp_a= test_cmp_b=

	# When text came from stdin (one argument is '-') we must feed it
	# to diff.
	local stdin_for_diff=

	# Since it is difficult to detect the difference between an
	# empty input file and a failure to read the files, we go straight
	# to diff if one of the inputs is empty.
	if test -s "$1" && test -s "$2"
	then
		# regular case: both files non-empty
		mingw_read_file_strip_cr_ test_cmp_a <"$1"
		mingw_read_file_strip_cr_ test_cmp_b <"$2"
	elif test -s "$1" && test "$2" = -
	then
		# read 2nd file from stdin
		mingw_read_file_strip_cr_ test_cmp_a <"$1"
		mingw_read_file_strip_cr_ test_cmp_b
		stdin_for_diff='<<<"$test_cmp_b"'
	elif test "$1" = - && test -s "$2"
	then
		# read 1st file from stdin
		mingw_read_file_strip_cr_ test_cmp_a
		mingw_read_file_strip_cr_ test_cmp_b <"$2"
		stdin_for_diff='<<<"$test_cmp_a"'
	fi
	test -n "$test_cmp_a" &&
	test -n "$test_cmp_b" &&
	test "$test_cmp_a" = "$test_cmp_b" ||
	eval "diff -u \"\$@\" $stdin_for_diff"
}

# $1 is the name of the shell variable to fill in
mingw_read_file_strip_cr_ () {
	# Read line-wise using LF as the line separator
	# and use IFS to strip CR.
	local line
	while :
	do
		if IFS=$'\r' read -r -d $'\n' line
		then
			# good
			line=$line$'\n'
		else
			# we get here at EOF, but also if the last line
			# was not terminated by LF; in the latter case,
			# some text was read
			if test -z "$line"
			then
				# EOF, really
				break
			fi
		fi
		eval "$1=\$$1\$line"
	done
}
