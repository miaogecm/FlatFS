#!/bin/sh
#
# Copyright (c) 2010 Sverre Rabbelier
#

test_description='Test remote-helper import and export commands'

. ./test-lib.sh
. "$TEST_DIRECTORY"/lib-gpg.sh

compare_refs() {
	git --git-dir="$1/.git" rev-parse --verify $2 >expect &&
	git --git-dir="$3/.git" rev-parse --verify $4 >actual &&
	test_cmp expect actual
}

test_expect_success 'setup repository' '
	git init server &&
	(cd server &&
	 echo content >file &&
	 git add file &&
	 git commit -m one)
'

test_expect_success 'cloning from local repo' '
	git clone "testgit::${PWD}/server" local &&
	test_cmp server/file local/file
'

test_expect_success 'create new commit on remote' '
	(cd server &&
	 echo content >>file &&
	 git commit -a -m two)
'

test_expect_success 'pulling from local repo' '
	(cd local && git pull) &&
	test_cmp server/file local/file
'

test_expect_success 'pushing to local repo' '
	(cd local &&
	echo content >>file &&
	git commit -a -m three &&
	git push) &&
	compare_refs local HEAD server HEAD
'

test_expect_success 'fetch new branch' '
	(cd server &&
	 git reset --hard &&
	 git checkout -b new &&
	 echo content >>file &&
	 git commit -a -m five
	) &&
	(cd local &&
	 git fetch origin new
	) &&
	compare_refs server HEAD local FETCH_HEAD
'

test_expect_success 'fetch multiple branches' '
	(cd local &&
	 git fetch
	) &&
	compare_refs server master local refs/remotes/origin/master &&
	compare_refs server new local refs/remotes/origin/new
'

test_expect_success 'push when remote has extra refs' '
	(cd local &&
	 git reset --hard origin/master &&
	 echo content >>file &&
	 git commit -a -m six &&
	 git push
	) &&
	compare_refs local master server master
'

test_expect_success 'push new branch by name' '
	(cd local &&
	 git checkout -b new-name  &&
	 echo content >>file &&
	 git commit -a -m seven &&
	 git push origin new-name
	) &&
	compare_refs local HEAD server refs/heads/new-name
'

test_expect_failure 'push new branch with old:new refspec' '
	(cd local &&
	 git push origin new-name:new-refspec
	) &&
	compare_refs local HEAD server refs/heads/new-refspec
'

test_expect_success 'cloning without refspec' '
	GIT_REMOTE_TESTGIT_REFSPEC="" \
	git clone "testgit::${PWD}/server" local2 2>error &&
	grep "This remote helper should implement refspec capability" error &&
	compare_refs local2 HEAD server HEAD
'

test_expect_success 'pulling without refspecs' '
	(cd local2 &&
	git reset --hard &&
	GIT_REMOTE_TESTGIT_REFSPEC="" git pull 2>../error) &&
	grep "This remote helper should implement refspec capability" error &&
	compare_refs local2 HEAD server HEAD
'

test_expect_success 'pushing without refspecs' '
	test_when_finished "(cd local2 && git reset --hard origin)" &&
	(cd local2 &&
	echo content >>file &&
	git commit -a -m ten &&
	GIT_REMOTE_TESTGIT_REFSPEC="" &&
	export GIT_REMOTE_TESTGIT_REFSPEC &&
	test_must_fail git push 2>../error) &&
	grep "remote-helper doesn.t support push; refspec needed" error
'

test_expect_success 'pulling without marks' '
	(cd local2 &&
	GIT_REMOTE_TESTGIT_NO_MARKS=1 git pull) &&
	compare_refs local2 HEAD server HEAD
'

test_expect_failure 'pushing without marks' '
	test_when_finished "(cd local2 && git reset --hard origin)" &&
	(cd local2 &&
	echo content >>file &&
	git commit -a -m twelve &&
	GIT_REMOTE_TESTGIT_NO_MARKS=1 git push) &&
	compare_refs local2 HEAD server HEAD
'

test_expect_success 'push all with existing object' '
	(cd local &&
	git branch dup2 master &&
	git push origin --all
	) &&
	compare_refs local dup2 server dup2
'

test_expect_success 'push ref with existing object' '
	(cd local &&
	git branch dup master &&
	git push origin dup
	) &&
	compare_refs local dup server dup
'

test_expect_success GPG 'push signed tag' '
	(cd local &&
	git checkout master &&
	git tag -s -m signed-tag signed-tag &&
	git push origin signed-tag
	) &&
	compare_refs local signed-tag^{} server signed-tag^{} &&
	test_must_fail compare_refs local signed-tag server signed-tag
'

test_expect_success GPG 'push signed tag with signed-tags capability' '
	(cd local &&
	git checkout master &&
	git tag -s -m signed-tag signed-tag-2 &&
	GIT_REMOTE_TESTGIT_SIGNED_TAGS=1 git push origin signed-tag-2
	) &&
	compare_refs local signed-tag-2 server signed-tag-2
'

test_expect_success 'push update refs' '
	(cd local &&
	git checkout -b update master &&
	echo update >>file &&
	git commit -a -m update &&
	git push origin update &&
	git rev-parse --verify remotes/origin/update >expect &&
	git rev-parse --verify testgit/origin/heads/update >actual &&
	test_cmp expect actual
	)
'

test_expect_success 'push update refs disabled by no-private-update' '
	(cd local &&
	echo more-update >>file &&
	git commit -a -m more-update &&
	git rev-parse --verify testgit/origin/heads/update >expect &&
	GIT_REMOTE_TESTGIT_NO_PRIVATE_UPDATE=t git push origin update &&
	git rev-parse --verify testgit/origin/heads/update >actual &&
	test_cmp expect actual
	)
'

test_expect_success 'push update refs failure' '
	(cd local &&
	git checkout update &&
	echo "update fail" >>file &&
	git commit -a -m "update fail" &&
	git rev-parse --verify testgit/origin/heads/update >expect &&
	GIT_REMOTE_TESTGIT_PUSH_ERROR="non-fast forward" &&
	export GIT_REMOTE_TESTGIT_PUSH_ERROR &&
	test_expect_code 1 git push origin update &&
	git rev-parse --verify testgit/origin/heads/update >actual &&
	test_cmp expect actual
	)
'

test_expect_success 'proper failure checks for fetching' '
	(GIT_REMOTE_TESTGIT_FAILURE=1 &&
	export GIT_REMOTE_TESTGIT_FAILURE &&
	cd local &&
	test_must_fail git fetch 2> error &&
	cat error &&
	grep -q "Error while running fast-import" error
	)
'

test_expect_success 'proper failure checks for pushing' '
	(GIT_REMOTE_TESTGIT_FAILURE=1 &&
	export GIT_REMOTE_TESTGIT_FAILURE &&
	cd local &&
	test_must_fail git push --all
	)
'

test_expect_success 'push messages' '
	(cd local &&
	git checkout -b new_branch master &&
	echo new >>file &&
	git commit -a -m new &&
	git push origin new_branch &&
	git fetch origin &&
	echo new >>file &&
	git commit -a -m new &&
	git push origin new_branch 2> msg &&
	! grep "\[new branch\]" msg
	)
'

test_done
