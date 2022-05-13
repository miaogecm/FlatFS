#!/bin/sh

test_description='test git rev-parse diagnosis for invalid argument'

exec </dev/null

. ./test-lib.sh

test_did_you_mean ()
{
	sq="'" &&
	cat >expected <<-EOF &&
	fatal: Path '$2$3' $4, but not ${5:-$sq$3$sq}.
	Did you mean '$1:$2$3'${2:+ aka $sq$1:./$3$sq}?
	EOF
	test_cmp expected error
}

HASH_file=

test_expect_success 'set up basic repo' '
	echo one > file.txt &&
	mkdir subdir &&
	echo two > subdir/file.txt &&
	echo three > subdir/file2.txt &&
	git add . &&
	git commit -m init &&
	echo four > index-only.txt &&
	git add index-only.txt &&
	echo five > disk-only.txt
'

test_expect_success 'correct file objects' '
	HASH_file=$(git rev-parse HEAD:file.txt) &&
	git rev-parse HEAD:subdir/file.txt &&
	git rev-parse :index-only.txt &&
	(cd subdir &&
	 git rev-parse HEAD:subdir/file2.txt &&
	 test $HASH_file = $(git rev-parse HEAD:file.txt) &&
	 test $HASH_file = $(git rev-parse :file.txt) &&
	 test $HASH_file = $(git rev-parse :0:file.txt) )
'

test_expect_success 'correct relative file objects (0)' '
	git rev-parse :file.txt >expected &&
	git rev-parse :./file.txt >result &&
	test_cmp expected result &&
	git rev-parse :0:./file.txt >result &&
	test_cmp expected result
'

test_expect_success 'correct relative file objects (1)' '
	git rev-parse HEAD:file.txt >expected &&
	git rev-parse HEAD:./file.txt >result &&
	test_cmp expected result
'

test_expect_success 'correct relative file objects (2)' '
	(
		cd subdir &&
		git rev-parse HEAD:../file.txt >result &&
		test_cmp ../expected result
	)
'

test_expect_success 'correct relative file objects (3)' '
	(
		cd subdir &&
		git rev-parse HEAD:../subdir/../file.txt >result &&
		test_cmp ../expected result
	)
'

test_expect_success 'correct relative file objects (4)' '
	git rev-parse HEAD:subdir/file.txt >expected &&
	(
		cd subdir &&
		git rev-parse HEAD:./file.txt >result &&
		test_cmp ../expected result
	)
'

test_expect_success 'correct relative file objects (5)' '
	git rev-parse :subdir/file.txt >expected &&
	(
		cd subdir &&
		git rev-parse :./file.txt >result &&
		test_cmp ../expected result &&
		git rev-parse :0:./file.txt >result &&
		test_cmp ../expected result
	)
'

test_expect_success 'correct relative file objects (6)' '
	git rev-parse :file.txt >expected &&
	(
		cd subdir &&
		git rev-parse :../file.txt >result &&
		test_cmp ../expected result &&
		git rev-parse :0:../file.txt >result &&
		test_cmp ../expected result
	)
'

test_expect_success 'incorrect revision id' '
	test_must_fail git rev-parse foobar:file.txt 2>error &&
	grep "Invalid object name '"'"'foobar'"'"'." error &&
	test_must_fail git rev-parse foobar 2> error &&
	grep "unknown revision or path not in the working tree." error
'

test_expect_success 'incorrect file in sha1:path' '
	test_must_fail git rev-parse HEAD:nothing.txt 2> error &&
	grep "fatal: Path '"'"'nothing.txt'"'"' does not exist in '"'"'HEAD'"'"'" error &&
	test_must_fail git rev-parse HEAD:index-only.txt 2> error &&
	grep "fatal: Path '"'"'index-only.txt'"'"' exists on disk, but not in '"'"'HEAD'"'"'." error &&
	(cd subdir &&
	 test_must_fail git rev-parse HEAD:file2.txt 2> error &&
	 test_did_you_mean HEAD subdir/ file2.txt exists )
'

test_expect_success 'incorrect file in :path and :N:path' '
	test_must_fail git rev-parse :nothing.txt 2> error &&
	grep "fatal: Path '"'"'nothing.txt'"'"' does not exist (neither on disk nor in the index)." error &&
	test_must_fail git rev-parse :1:nothing.txt 2> error &&
	grep "Path '"'"'nothing.txt'"'"' does not exist (neither on disk nor in the index)." error &&
	test_must_fail git rev-parse :1:file.txt 2> error &&
	test_did_you_mean ":0" "" file.txt "is in the index" "at stage 1" &&
	(cd subdir &&
	 test_must_fail git rev-parse :1:file.txt 2> error &&
	 test_did_you_mean ":0" "" file.txt "is in the index" "at stage 1" &&
	 test_must_fail git rev-parse :file2.txt 2> error &&
	 test_did_you_mean ":0" subdir/ file2.txt "is in the index" &&
	 test_must_fail git rev-parse :2:file2.txt 2> error &&
	 test_did_you_mean :0 subdir/ file2.txt "is in the index") &&
	test_must_fail git rev-parse :disk-only.txt 2> error &&
	grep "fatal: Path '"'"'disk-only.txt'"'"' exists on disk, but not in the index." error
'

test_expect_success 'invalid @{n} reference' '
	test_must_fail git rev-parse master@{99999} >output 2>error &&
	test -z "$(cat output)" &&
	grep "fatal: Log for [^ ]* only has [0-9][0-9]* entries." error  &&
	test_must_fail git rev-parse --verify master@{99999} >output 2>error &&
	test -z "$(cat output)" &&
	grep "fatal: Log for [^ ]* only has [0-9][0-9]* entries." error
'

test_expect_success 'relative path not found' '
	(
		cd subdir &&
		test_must_fail git rev-parse HEAD:./nonexistent.txt 2>error &&
		grep subdir/nonexistent.txt error
	)
'

test_expect_success 'relative path outside worktree' '
	test_must_fail git rev-parse HEAD:../file.txt >output 2>error &&
	test -z "$(cat output)" &&
	grep "outside repository" error
'

test_expect_success 'relative path when cwd is outside worktree' '
	test_must_fail git --git-dir=.git --work-tree=subdir rev-parse HEAD:./file.txt >output 2>error &&
	test -z "$(cat output)" &&
	grep "relative path syntax can.t be used outside working tree." error
'

test_expect_success 'relative path when startup_info is NULL' '
	test_must_fail test-match-trees HEAD:./file.txt HEAD:./file.txt 2>error &&
	grep "BUG: startup_info struct is not initialized." error
'

test_expect_success '<commit>:file correctly diagnosed after a pathname' '
	test_must_fail git rev-parse file.txt HEAD:file.txt 1>actual 2>error &&
	test_i18ngrep ! "exists on disk" error &&
	test_i18ngrep "no such path in the working tree" error &&
	cat >expect <<-\EOF &&
	file.txt
	HEAD:file.txt
	EOF
	test_cmp expect actual
'

test_expect_success 'dotdot is not an empty set' '
	( H=$(git rev-parse HEAD) && echo $H && echo ^$H ) >expect &&

	git rev-parse HEAD.. >actual &&
	test_cmp expect actual &&

	git rev-parse ..HEAD >actual &&
	test_cmp expect actual &&

	echo .. >expect &&
	git rev-parse .. >actual &&
	test_cmp expect actual
'

test_expect_success 'arg before dashdash must be a revision (missing)' '
	test_must_fail git rev-parse foobar -- 2>stderr &&
	test_i18ngrep "bad revision" stderr
'

test_expect_success 'arg before dashdash must be a revision (file)' '
	>foobar &&
	test_must_fail git rev-parse foobar -- 2>stderr &&
	test_i18ngrep "bad revision" stderr
'

test_expect_success 'arg before dashdash must be a revision (ambiguous)' '
	>foobar &&
	git update-ref refs/heads/foobar HEAD &&
	{
		# we do not want to use rev-parse here, because
		# we are testing it
		cat .git/refs/heads/foobar &&
		printf "%s\n" --
	} >expect &&
	git rev-parse foobar -- >actual &&
	test_cmp expect actual
'

test_done
