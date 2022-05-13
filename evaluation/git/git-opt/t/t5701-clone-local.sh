#!/bin/sh

test_description='test local clone'
. ./test-lib.sh

repo_is_hardlinked() {
	find "$1/objects" -type f -links 1 >output &&
	test_line_count = 0 output
}

test_expect_success 'preparing origin repository' '
	: >file && git add . && git commit -m1 &&
	git clone --bare . a.git &&
	git clone --bare . x &&
	test "$(GIT_CONFIG=a.git/config git config --bool core.bare)" = true &&
	test "$(GIT_CONFIG=x/config git config --bool core.bare)" = true &&
	git bundle create b1.bundle --all &&
	git bundle create b2.bundle master &&
	mkdir dir &&
	cp b1.bundle dir/b3 &&
	cp b1.bundle b4
'

test_expect_success 'local clone without .git suffix' '
	git clone -l -s a b &&
	(cd b &&
	test "$(GIT_CONFIG=.git/config git config --bool core.bare)" = false &&
	git fetch)
'

test_expect_success 'local clone with .git suffix' '
	git clone -l -s a.git c &&
	(cd c && git fetch)
'

test_expect_success 'local clone from x' '
	git clone -l -s x y &&
	(cd y && git fetch)
'

test_expect_success 'local clone from x.git that does not exist' '
	test_must_fail git clone -l -s x.git z
'

test_expect_success 'With -no-hardlinks, local will make a copy' '
	git clone --bare --no-hardlinks x w &&
	! repo_is_hardlinked w
'

test_expect_success 'Even without -l, local will make a hardlink' '
	rm -fr w &&
	git clone -l --bare x w &&
	repo_is_hardlinked w
'

test_expect_success 'local clone of repo with nonexistent ref in HEAD' '
	echo "ref: refs/heads/nonexistent" > a.git/HEAD &&
	git clone a d &&
	(cd d &&
	git fetch &&
	test ! -e .git/refs/remotes/origin/HEAD)
'

test_expect_success 'bundle clone without .bundle suffix' '
	git clone dir/b3 &&
	(cd b3 && git fetch)
'

test_expect_success 'bundle clone with .bundle suffix' '
	git clone b1.bundle &&
	(cd b1 && git fetch)
'

test_expect_success 'bundle clone from b4' '
	git clone b4 bdl &&
	(cd bdl && git fetch)
'

test_expect_success 'bundle clone from b4.bundle that does not exist' '
	test_must_fail git clone b4.bundle bb
'

test_expect_success 'bundle clone with nonexistent HEAD' '
	git clone b2.bundle b2 &&
	(cd b2 &&
	git fetch &&
	test_must_fail git rev-parse --verify refs/heads/master)
'

test_expect_success 'clone empty repository' '
	mkdir empty &&
	(cd empty &&
	 git init &&
	 git config receive.denyCurrentBranch warn) &&
	git clone empty empty-clone &&
	test_tick &&
	(cd empty-clone
	 echo "content" >> foo &&
	 git add foo &&
	 git commit -m "Initial commit" &&
	 git push origin master &&
	 expected=$(git rev-parse master) &&
	 actual=$(git --git-dir=../empty/.git rev-parse master) &&
	 test $actual = $expected)
'

test_expect_success 'clone empty repository, and then push should not segfault.' '
	rm -fr empty/ empty-clone/ &&
	mkdir empty &&
	(cd empty && git init) &&
	git clone empty empty-clone &&
	(cd empty-clone &&
	test_must_fail git push)
'

test_expect_success 'cloning non-existent directory fails' '
	rm -rf does-not-exist &&
	test_must_fail git clone does-not-exist
'

test_expect_success 'cloning non-git directory fails' '
	rm -rf not-a-git-repo not-a-git-repo-clone &&
	mkdir not-a-git-repo &&
	test_must_fail git clone not-a-git-repo not-a-git-repo-clone
'

test_expect_success 'cloning file:// does not hardlink' '
	git clone --bare file://"$(pwd)"/a non-local &&
	! repo_is_hardlinked non-local
'

test_expect_success 'cloning a local path with --no-local does not hardlink' '
	git clone --bare --no-local a force-nonlocal &&
	! repo_is_hardlinked force-nonlocal
'

test_expect_success 'cloning locally respects "-u" for fetching refs' '
	test_must_fail git clone --bare -u false a should_not_work.git
'

test_done
