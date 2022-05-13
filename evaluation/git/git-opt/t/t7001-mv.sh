#!/bin/sh

test_description='git mv in subdirs'
. ./test-lib.sh

test_expect_success \
    'prepare reference tree' \
    'mkdir path0 path1 &&
     cp "$TEST_DIRECTORY"/../COPYING path0/COPYING &&
     git add path0/COPYING &&
     git commit -m add -a'

test_expect_success \
    'moving the file out of subdirectory' \
    'cd path0 && git mv COPYING ../path1/COPYING'

# in path0 currently
test_expect_success \
    'commiting the change' \
    'cd .. && git commit -m move-out -a'

test_expect_success \
    'checking the commit' \
    'git diff-tree -r -M --name-status  HEAD^ HEAD | \
    grep "^R100..*path0/COPYING..*path1/COPYING"'

test_expect_success \
    'moving the file back into subdirectory' \
    'cd path0 && git mv ../path1/COPYING COPYING'

# in path0 currently
test_expect_success \
    'commiting the change' \
    'cd .. && git commit -m move-in -a'

test_expect_success \
    'checking the commit' \
    'git diff-tree -r -M --name-status  HEAD^ HEAD | \
    grep "^R100..*path1/COPYING..*path0/COPYING"'

test_expect_success \
    'checking -k on non-existing file' \
    'git mv -k idontexist path0'

test_expect_success \
    'checking -k on untracked file' \
    'touch untracked1 &&
     git mv -k untracked1 path0 &&
     test -f untracked1 &&
     test ! -f path0/untracked1'

test_expect_success \
    'checking -k on multiple untracked files' \
    'touch untracked2 &&
     git mv -k untracked1 untracked2 path0 &&
     test -f untracked1 &&
     test -f untracked2 &&
     test ! -f path0/untracked1 &&
     test ! -f path0/untracked2'

test_expect_success \
    'checking -f on untracked file with existing target' \
    'touch path0/untracked1 &&
     test_must_fail git mv -f untracked1 path0 &&
     test ! -f .git/index.lock &&
     test -f untracked1 &&
     test -f path0/untracked1'

# clean up the mess in case bad things happen
rm -f idontexist untracked1 untracked2 \
     path0/idontexist path0/untracked1 path0/untracked2 \
     .git/index.lock
rmdir path1

test_expect_success \
    'moving to absent target with trailing slash' \
    'test_must_fail git mv path0/COPYING no-such-dir/ &&
     test_must_fail git mv path0/COPYING no-such-dir// &&
     git mv path0/ no-such-dir/ &&
     test_path_is_dir no-such-dir'

test_expect_success \
    'clean up' \
    'git reset --hard'

test_expect_success \
    'moving to existing untracked target with trailing slash' \
    'mkdir path1 &&
     git mv path0/ path1/ &&
     test_path_is_dir path1/path0/'

test_expect_success \
    'moving to existing tracked target with trailing slash' \
    'mkdir path2 &&
     >path2/file && git add path2/file &&
     git mv path1/path0/ path2/ &&
     test_path_is_dir path2/path0/'

test_expect_success \
    'clean up' \
    'git reset --hard'

test_expect_success \
    'adding another file' \
    'cp "$TEST_DIRECTORY"/../README path0/README &&
     git add path0/README &&
     git commit -m add2 -a'

test_expect_success \
    'moving whole subdirectory' \
    'git mv path0 path2'

test_expect_success \
    'commiting the change' \
    'git commit -m dir-move -a'

test_expect_success \
    'checking the commit' \
    'git diff-tree -r -M --name-status  HEAD^ HEAD | \
     grep "^R100..*path0/COPYING..*path2/COPYING" &&
     git diff-tree -r -M --name-status  HEAD^ HEAD | \
     grep "^R100..*path0/README..*path2/README"'

test_expect_success \
    'succeed when source is a prefix of destination' \
    'git mv path2/COPYING path2/COPYING-renamed'

test_expect_success \
    'moving whole subdirectory into subdirectory' \
    'git mv path2 path1'

test_expect_success \
    'commiting the change' \
    'git commit -m dir-move -a'

test_expect_success \
    'checking the commit' \
    'git diff-tree -r -M --name-status  HEAD^ HEAD | \
     grep "^R100..*path2/COPYING..*path1/path2/COPYING" &&
     git diff-tree -r -M --name-status  HEAD^ HEAD | \
     grep "^R100..*path2/README..*path1/path2/README"'

test_expect_success \
    'do not move directory over existing directory' \
    'mkdir path0 && mkdir path0/path2 && test_must_fail git mv path2 path0'

test_expect_success \
    'move into "."' \
    'git mv path1/path2/ .'

test_expect_success "Michael Cassar's test case" '
	rm -fr .git papers partA &&
	git init &&
	mkdir -p papers/unsorted papers/all-papers partA &&
	echo a > papers/unsorted/Thesis.pdf &&
	echo b > partA/outline.txt &&
	echo c > papers/unsorted/_another &&
	git add papers partA &&
	T1=`git write-tree` &&

	git mv papers/unsorted/Thesis.pdf papers/all-papers/moo-blah.pdf &&

	T=`git write-tree` &&
	git ls-tree -r $T | grep partA/outline.txt || {
		git ls-tree -r $T
		(exit 1)
	}
'

rm -fr papers partA path?

test_expect_success "Sergey Vlasov's test case" '
	rm -fr .git &&
	git init &&
	mkdir ab &&
	date >ab.c &&
	date >ab/d &&
	git add ab.c ab &&
	git commit -m 'initial' &&
	git mv ab a
'

test_expect_success 'absolute pathname' '(

	rm -fr mine &&
	mkdir mine &&
	cd mine &&
	test_create_repo one &&
	cd one &&
	mkdir sub &&
	>sub/file &&
	git add sub/file &&

	git mv sub "$(pwd)/in" &&
	! test -d sub &&
	test -d in &&
	git ls-files --error-unmatch in/file


)'

test_expect_success 'absolute pathname outside should fail' '(

	rm -fr mine &&
	mkdir mine &&
	cd mine &&
	out=$(pwd) &&
	test_create_repo one &&
	cd one &&
	mkdir sub &&
	>sub/file &&
	git add sub/file &&

	test_must_fail git mv sub "$out/out" &&
	test -d sub &&
	! test -d ../in &&
	git ls-files --error-unmatch sub/file

)'

test_expect_success 'git mv to move multiple sources into a directory' '
	rm -fr .git && git init &&
	mkdir dir other &&
	>dir/a.txt &&
	>dir/b.txt &&
	git add dir/?.txt &&
	git mv dir/a.txt dir/b.txt other &&
	git ls-files >actual &&
	{ echo other/a.txt; echo other/b.txt; } >expect &&
	test_cmp expect actual
'

test_expect_success 'git mv should not change sha1 of moved cache entry' '

	rm -fr .git &&
	git init &&
	echo 1 >dirty &&
	git add dirty &&
	entry="$(git ls-files --stage dirty | cut -f 1)" &&
	git mv dirty dirty2 &&
	[ "$entry" = "$(git ls-files --stage dirty2 | cut -f 1)" ] &&
	echo 2 >dirty2 &&
	git mv dirty2 dirty &&
	[ "$entry" = "$(git ls-files --stage dirty | cut -f 1)" ]

'

rm -f dirty dirty2

test_expect_success 'git mv should overwrite symlink to a file' '

	rm -fr .git &&
	git init &&
	echo 1 >moved &&
	test_ln_s_add moved symlink &&
	git add moved &&
	test_must_fail git mv moved symlink &&
	git mv -f moved symlink &&
	! test -e moved &&
	test -f symlink &&
	test "$(cat symlink)" = 1 &&
	git update-index --refresh &&
	git diff-files --quiet

'

rm -f moved symlink

test_expect_success 'git mv should overwrite file with a symlink' '

	rm -fr .git &&
	git init &&
	echo 1 >moved &&
	test_ln_s_add moved symlink &&
	git add moved &&
	test_must_fail git mv symlink moved &&
	git mv -f symlink moved &&
	! test -e symlink &&
	git update-index --refresh &&
	git diff-files --quiet

'

test_expect_success SYMLINKS 'check moved symlink' '

	test -h moved
'

rm -f moved symlink

test_expect_success 'setup submodule' '
	git commit -m initial &&
	git reset --hard &&
	git submodule add ./. sub &&
	echo content >file &&
	git add file &&
	git commit -m "added sub and file"
'

test_expect_success 'git mv cannot move a submodule in a file' '
	test_must_fail git mv sub file
'

test_expect_success 'git mv moves a submodule with a .git directory and no .gitmodules' '
	entry="$(git ls-files --stage sub | cut -f 1)" &&
	git rm .gitmodules &&
	(
		cd sub &&
		rm -f .git &&
		cp -a ../.git/modules/sub .git &&
		GIT_WORK_TREE=. git config --unset core.worktree
	) &&
	mkdir mod &&
	git mv sub mod/sub &&
	! test -e sub &&
	[ "$entry" = "$(git ls-files --stage mod/sub | cut -f 1)" ] &&
	(
		cd mod/sub &&
		git status
	) &&
	git update-index --refresh &&
	git diff-files --quiet
'

test_expect_success 'git mv moves a submodule with a .git directory and .gitmodules' '
	rm -rf mod &&
	git reset --hard &&
	git submodule update &&
	entry="$(git ls-files --stage sub | cut -f 1)" &&
	(
		cd sub &&
		rm -f .git &&
		cp -a ../.git/modules/sub .git &&
		GIT_WORK_TREE=. git config --unset core.worktree
	) &&
	mkdir mod &&
	git mv sub mod/sub &&
	! test -e sub &&
	[ "$entry" = "$(git ls-files --stage mod/sub | cut -f 1)" ] &&
	(
		cd mod/sub &&
		git status
	) &&
	echo mod/sub >expected &&
	git config -f .gitmodules submodule.sub.path >actual &&
	test_cmp expected actual &&
	git update-index --refresh &&
	git diff-files --quiet
'

test_expect_success 'git mv moves a submodule with gitfile' '
	rm -rf mod/sub &&
	git reset --hard &&
	git submodule update &&
	entry="$(git ls-files --stage sub | cut -f 1)" &&
	(
		cd mod &&
		git mv ../sub/ .
	) &&
	! test -e sub &&
	[ "$entry" = "$(git ls-files --stage mod/sub | cut -f 1)" ] &&
	(
		cd mod/sub &&
		git status
	) &&
	echo mod/sub >expected &&
	git config -f .gitmodules submodule.sub.path >actual &&
	test_cmp expected actual &&
	git update-index --refresh &&
	git diff-files --quiet
'

test_expect_success 'mv does not complain when no .gitmodules file is found' '
	rm -rf mod/sub &&
	git reset --hard &&
	git submodule update &&
	git rm .gitmodules &&
	entry="$(git ls-files --stage sub | cut -f 1)" &&
	git mv sub mod/sub 2>actual.err &&
	! test -s actual.err &&
	! test -e sub &&
	[ "$entry" = "$(git ls-files --stage mod/sub | cut -f 1)" ] &&
	(
		cd mod/sub &&
		git status
	) &&
	git update-index --refresh &&
	git diff-files --quiet
'

test_expect_success 'mv will error out on a modified .gitmodules file unless staged' '
	rm -rf mod/sub &&
	git reset --hard &&
	git submodule update &&
	git config -f .gitmodules foo.bar true &&
	entry="$(git ls-files --stage sub | cut -f 1)" &&
	test_must_fail git mv sub mod/sub 2>actual.err &&
	test -s actual.err &&
	test -e sub &&
	git diff-files --quiet -- sub &&
	git add .gitmodules &&
	git mv sub mod/sub 2>actual.err &&
	! test -s actual.err &&
	! test -e sub &&
	[ "$entry" = "$(git ls-files --stage mod/sub | cut -f 1)" ] &&
	(
		cd mod/sub &&
		git status
	) &&
	git update-index --refresh &&
	git diff-files --quiet
'

test_expect_success 'mv issues a warning when section is not found in .gitmodules' '
	rm -rf mod/sub &&
	git reset --hard &&
	git submodule update &&
	git config -f .gitmodules --remove-section submodule.sub &&
	git add .gitmodules &&
	entry="$(git ls-files --stage sub | cut -f 1)" &&
	echo "warning: Could not find section in .gitmodules where path=sub" >expect.err &&
	git mv sub mod/sub 2>actual.err &&
	test_i18ncmp expect.err actual.err &&
	! test -e sub &&
	[ "$entry" = "$(git ls-files --stage mod/sub | cut -f 1)" ] &&
	(
		cd mod/sub &&
		git status
	) &&
	git update-index --refresh &&
	git diff-files --quiet
'

test_expect_success 'mv --dry-run does not touch the submodule or .gitmodules' '
	rm -rf mod/sub &&
	git reset --hard &&
	git submodule update &&
	git mv -n sub mod/sub 2>actual.err &&
	test -f sub/.git &&
	git diff-index --exit-code HEAD &&
	git update-index --refresh &&
	git diff-files --quiet -- sub .gitmodules
'

test_expect_success 'checking out a commit before submodule moved needs manual updates' '
	git mv sub sub2 &&
	git commit -m "moved sub to sub2" &&
	git checkout -q HEAD^ 2>actual &&
	echo "warning: unable to rmdir sub2: Directory not empty" >expected &&
	test_i18ncmp expected actual &&
	git status -s sub2 >actual &&
	echo "?? sub2/" >expected &&
	test_cmp expected actual &&
	! test -f sub/.git &&
	test -f sub2/.git &&
	git submodule update &&
	test -f sub/.git &&
	rm -rf sub2 &&
	git diff-index --exit-code HEAD &&
	git update-index --refresh &&
	git diff-files --quiet -- sub .gitmodules &&
	git status -s sub2 >actual &&
	! test -s actual
'

test_done
