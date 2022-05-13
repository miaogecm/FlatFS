#!/bin/sh

test_description='git p4 tests'

. ./lib-git-p4.sh

test_expect_success 'start p4d' '
	start_p4d
'

test_expect_success 'add p4 files' '
	(
		cd "$cli" &&
		echo file1 >file1 &&
		p4 add file1 &&
		p4 submit -d "file1" &&
		echo file2 >file2 &&
		p4 add file2 &&
		p4 submit -d "file2"
	)
'

test_expect_success 'basic git p4 clone' '
	git p4 clone --dest="$git" //depot &&
	test_when_finished cleanup_git &&
	(
		cd "$git" &&
		git log --oneline >lines &&
		test_line_count = 1 lines
	)
'

test_expect_success 'depot typo error' '
	test_must_fail git p4 clone --dest="$git" /depot 2>errs &&
	grep "Depot paths must start with" errs
'

test_expect_success 'git p4 clone @all' '
	git p4 clone --dest="$git" //depot@all &&
	test_when_finished cleanup_git &&
	(
		cd "$git" &&
		git log --oneline >lines &&
		test_line_count = 2 lines
	)
'

test_expect_success 'git p4 sync uninitialized repo' '
	test_create_repo "$git" &&
	test_when_finished cleanup_git &&
	(
		cd "$git" &&
		test_must_fail git p4 sync 2>errs &&
		test_i18ngrep "Perhaps you never did" errs
	)
'

#
# Create a git repo by hand.  Add a commit so that HEAD is valid.
# Test imports a new p4 repository into a new git branch.
#
test_expect_success 'git p4 sync new branch' '
	test_create_repo "$git" &&
	test_when_finished cleanup_git &&
	(
		cd "$git" &&
		test_commit head &&
		git p4 sync --branch=refs/remotes/p4/depot //depot@all &&
		git log --oneline p4/depot >lines &&
		test_line_count = 2 lines
	)
'

test_expect_success 'clone two dirs' '
	(
		cd "$cli" &&
		mkdir sub1 sub2 &&
		echo sub1/f1 >sub1/f1 &&
		echo sub2/f2 >sub2/f2 &&
		p4 add sub1/f1 &&
		p4 submit -d "sub1/f1" &&
		p4 add sub2/f2 &&
		p4 submit -d "sub2/f2"
	) &&
	git p4 clone --dest="$git" //depot/sub1 //depot/sub2 &&
	test_when_finished cleanup_git &&
	(
		cd "$git" &&
		git ls-files >lines &&
		test_line_count = 2 lines &&
		git log --oneline p4/master >lines &&
		test_line_count = 1 lines
	)
'

test_expect_success 'clone two dirs, @all' '
	(
		cd "$cli" &&
		echo sub1/f3 >sub1/f3 &&
		p4 add sub1/f3 &&
		p4 submit -d "sub1/f3"
	) &&
	git p4 clone --dest="$git" //depot/sub1@all //depot/sub2@all &&
	test_when_finished cleanup_git &&
	(
		cd "$git" &&
		git ls-files >lines &&
		test_line_count = 3 lines &&
		git log --oneline p4/master >lines &&
		test_line_count = 3 lines
	)
'

test_expect_success 'clone two dirs, @all, conflicting files' '
	(
		cd "$cli" &&
		echo sub2/f3 >sub2/f3 &&
		p4 add sub2/f3 &&
		p4 submit -d "sub2/f3"
	) &&
	git p4 clone --dest="$git" //depot/sub1@all //depot/sub2@all &&
	test_when_finished cleanup_git &&
	(
		cd "$git" &&
		git ls-files >lines &&
		test_line_count = 3 lines &&
		git log --oneline p4/master >lines &&
		test_line_count = 4 lines &&
		echo sub2/f3 >expected &&
		test_cmp expected f3
	)
'

test_expect_success 'exit when p4 fails to produce marshaled output' '
	mkdir badp4dir &&
	test_when_finished "rm badp4dir/p4 && rmdir badp4dir" &&
	cat >badp4dir/p4 <<-EOF &&
	#!$SHELL_PATH
	exit 1
	EOF
	chmod 755 badp4dir/p4 &&
	(
		PATH="$TRASH_DIRECTORY/badp4dir:$PATH" &&
		export PATH &&
		test_expect_code 1 git p4 clone --dest="$git" //depot >errs 2>&1
	) &&
	cat errs &&
	! test_i18ngrep Traceback errs
'

# Hide a file from p4d, make sure we catch its complaint.  This won't fail in
# p4 changes, files, or describe; just in p4 print.  If P4CLIENT is unset, the
# message will include "Librarian checkout".
test_expect_success 'exit gracefully for p4 server errors' '
	test_when_finished "mv \"$db\"/depot/file1,v,hidden \"$db\"/depot/file1,v" &&
	mv "$db"/depot/file1,v "$db"/depot/file1,v,hidden &&
	test_when_finished cleanup_git &&
	test_expect_code 1 git p4 clone --dest="$git" //depot@1 >out 2>err &&
	test_i18ngrep "Error from p4 print" err
'

test_expect_success 'clone --bare should make a bare repository' '
	rm -rf "$git" &&
	git p4 clone --dest="$git" --bare //depot &&
	test_when_finished cleanup_git &&
	(
		cd "$git" &&
		test_path_is_missing .git &&
		git config --get --bool core.bare true &&
		git rev-parse --verify refs/remotes/p4/master &&
		git rev-parse --verify refs/remotes/p4/HEAD &&
		git rev-parse --verify refs/heads/master &&
		git rev-parse --verify HEAD
	)
'

# Sleep a bit so that the top-most p4 change did not happen "now".  Then
# import the repo and make sure that the initial import has the same time
# as the top-most change.
test_expect_success 'initial import time from top change time' '
	p4change=$(p4 -G changes -m 1 //depot/... | marshal_dump change) &&
	p4time=$(p4 -G changes -m 1 //depot/... | marshal_dump time) &&
	sleep 3 &&
	git p4 clone --dest="$git" //depot &&
	test_when_finished cleanup_git &&
	(
		cd "$git" &&
		gittime=$(git show -s --raw --pretty=format:%at HEAD) &&
		echo $p4time $gittime &&
		test $p4time = $gittime
	)
'

test_expect_success 'unresolvable host in P4PORT should display error' '
	test_when_finished cleanup_git &&
	git p4 clone --dest="$git" //depot &&
	(
		cd "$git" &&
		P4PORT=nosuchhost:65537 &&
		export P4PORT &&
		test_expect_code 1 git p4 sync >out 2>err &&
		grep "connect to nosuchhost" err
	)
'

test_expect_success 'kill p4d' '
	kill_p4d
'

test_done
