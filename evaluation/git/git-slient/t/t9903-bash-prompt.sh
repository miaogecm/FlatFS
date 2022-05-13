#!/bin/sh
#
# Copyright (c) 2012 SZEDER Gábor
#

test_description='test git-specific bash prompt functions'

. ./lib-bash.sh

. "$GIT_BUILD_DIR/contrib/completion/git-prompt.sh"

actual="$TRASH_DIRECTORY/actual"
c_red='\\[\\e[31m\\]'
c_green='\\[\\e[32m\\]'
c_lblue='\\[\\e[1;34m\\]'
c_clear='\\[\\e[0m\\]'

test_expect_success 'setup for prompt tests' '
	git init otherrepo &&
	echo 1 >file &&
	git add file &&
	test_tick &&
	git commit -m initial &&
	git tag -a -m msg1 t1 &&
	git checkout -b b1 &&
	echo 2 >file &&
	git commit -m "second b1" file &&
	echo 3 >file &&
	git commit -m "third b1" file &&
	git tag -a -m msg2 t2 &&
	git checkout -b b2 master &&
	echo 0 >file &&
	git commit -m "second b2" file &&
	echo 00 >file &&
	git commit -m "another b2" file &&
	echo 000 >file &&
	git commit -m "yet another b2" file &&
	git checkout master
'

test_expect_success 'prompt - branch name' '
	printf " (master)" >expected &&
	__git_ps1 >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success SYMLINKS 'prompt - branch name - symlink symref' '
	printf " (master)" >expected &&
	test_when_finished "git checkout master" &&
	test_config core.preferSymlinkRefs true &&
	git checkout master &&
	__git_ps1 >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - unborn branch' '
	printf " (unborn)" >expected &&
	git checkout --orphan unborn &&
	test_when_finished "git checkout master" &&
	__git_ps1 >"$actual" &&
	test_cmp expected "$actual"
'

repo_with_newline='repo
with
newline'

if mkdir "$repo_with_newline" 2>/dev/null
then
	test_set_prereq FUNNYNAMES
else
	say 'Your filesystem does not allow newlines in filenames.'
fi

test_expect_success FUNNYNAMES 'prompt - with newline in path' '
	printf " (master)" >expected &&
	git init "$repo_with_newline" &&
	test_when_finished "rm -rf \"$repo_with_newline\"" &&
	mkdir "$repo_with_newline"/subdir &&
	(
		cd "$repo_with_newline/subdir" &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - detached head' '
	printf " ((%s...))" $(git log -1 --format="%h" --abbrev=13 b1^) >expected &&
	test_config core.abbrev 13 &&
	git checkout b1^ &&
	test_when_finished "git checkout master" &&
	__git_ps1 >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - describe detached head - contains' '
	printf " ((t2~1))" >expected &&
	git checkout b1^ &&
	test_when_finished "git checkout master" &&
	(
		GIT_PS1_DESCRIBE_STYLE=contains &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - describe detached head - branch' '
	printf " ((b1~1))" >expected &&
	git checkout b1^ &&
	test_when_finished "git checkout master" &&
	(
		GIT_PS1_DESCRIBE_STYLE=branch &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - describe detached head - describe' '
	printf " ((t1-1-g%s))" $(git log -1 --format="%h" b1^) >expected &&
	git checkout b1^ &&
	test_when_finished "git checkout master" &&
	(
		GIT_PS1_DESCRIBE_STYLE=describe &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - describe detached head - default' '
	printf " ((t2))" >expected &&
	git checkout --detach b1 &&
	test_when_finished "git checkout master" &&
	__git_ps1 >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - inside .git directory' '
	printf " (GIT_DIR!)" >expected &&
	(
		cd .git &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - deep inside .git directory' '
	printf " (GIT_DIR!)" >expected &&
	(
		cd .git/refs/heads &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - inside bare repository' '
	printf " (BARE:master)" >expected &&
	git init --bare bare.git &&
	test_when_finished "rm -rf bare.git" &&
	(
		cd bare.git &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - interactive rebase' '
	printf " (b1|REBASE-i 2/3)" >expected
	write_script fake_editor.sh <<-\EOF &&
		echo "exec echo" >"$1"
		echo "edit $(git log -1 --format="%h")" >>"$1"
		echo "exec echo" >>"$1"
	EOF
	test_when_finished "rm -f fake_editor.sh" &&
	test_set_editor "$TRASH_DIRECTORY/fake_editor.sh" &&
	git checkout b1 &&
	test_when_finished "git checkout master" &&
	git rebase -i HEAD^ &&
	test_when_finished "git rebase --abort"
	__git_ps1 >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - rebase merge' '
	printf " (b2|REBASE-m 1/3)" >expected &&
	git checkout b2 &&
	test_when_finished "git checkout master" &&
	test_must_fail git rebase --merge b1 b2 &&
	test_when_finished "git rebase --abort" &&
	__git_ps1 >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - rebase' '
	printf " (b2|REBASE 1/3)" >expected &&
	git checkout b2 &&
	test_when_finished "git checkout master" &&
	test_must_fail git rebase b1 b2 &&
	test_when_finished "git rebase --abort" &&
	__git_ps1 >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - merge' '
	printf " (b1|MERGING)" >expected &&
	git checkout b1 &&
	test_when_finished "git checkout master" &&
	test_must_fail git merge b2 &&
	test_when_finished "git reset --hard" &&
	__git_ps1 >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - cherry-pick' '
	printf " (master|CHERRY-PICKING)" >expected &&
	test_must_fail git cherry-pick b1 &&
	test_when_finished "git reset --hard" &&
	__git_ps1 >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - bisect' '
	printf " (master|BISECTING)" >expected &&
	git bisect start &&
	test_when_finished "git bisect reset" &&
	__git_ps1 >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - dirty status indicator - clean' '
	printf " (master)" >expected &&
	(
		GIT_PS1_SHOWDIRTYSTATE=y &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - dirty status indicator - dirty worktree' '
	printf " (master *)" >expected &&
	echo "dirty" >file &&
	test_when_finished "git reset --hard" &&
	(
		GIT_PS1_SHOWDIRTYSTATE=y &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - dirty status indicator - dirty index' '
	printf " (master +)" >expected &&
	echo "dirty" >file &&
	test_when_finished "git reset --hard" &&
	git add -u &&
	(
		GIT_PS1_SHOWDIRTYSTATE=y &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - dirty status indicator - dirty index and worktree' '
	printf " (master *+)" >expected &&
	echo "dirty index" >file &&
	test_when_finished "git reset --hard" &&
	git add -u &&
	echo "dirty worktree" >file &&
	(
		GIT_PS1_SHOWDIRTYSTATE=y &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - dirty status indicator - before root commit' '
	printf " (master #)" >expected &&
	(
		GIT_PS1_SHOWDIRTYSTATE=y &&
		cd otherrepo &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - dirty status indicator - shell variable unset with config disabled' '
	printf " (master)" >expected &&
	echo "dirty" >file &&
	test_when_finished "git reset --hard" &&
	test_config bash.showDirtyState false &&
	(
		sane_unset GIT_PS1_SHOWDIRTYSTATE &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - dirty status indicator - shell variable unset with config enabled' '
	printf " (master)" >expected &&
	echo "dirty" >file &&
	test_when_finished "git reset --hard" &&
	test_config bash.showDirtyState true &&
	(
		sane_unset GIT_PS1_SHOWDIRTYSTATE &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - dirty status indicator - shell variable set with config disabled' '
	printf " (master)" >expected &&
	echo "dirty" >file &&
	test_when_finished "git reset --hard" &&
	test_config bash.showDirtyState false &&
	(
		GIT_PS1_SHOWDIRTYSTATE=y &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - dirty status indicator - shell variable set with config enabled' '
	printf " (master *)" >expected &&
	echo "dirty" >file &&
	test_when_finished "git reset --hard" &&
	test_config bash.showDirtyState true &&
	(
		GIT_PS1_SHOWDIRTYSTATE=y &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - dirty status indicator - not shown inside .git directory' '
	printf " (GIT_DIR!)" >expected &&
	echo "dirty" >file &&
	test_when_finished "git reset --hard" &&
	(
		GIT_PS1_SHOWDIRTYSTATE=y &&
		cd .git &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - stash status indicator - no stash' '
	printf " (master)" >expected &&
	(
		GIT_PS1_SHOWSTASHSTATE=y &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - stash status indicator - stash' '
	printf " (master $)" >expected &&
	echo 2 >file &&
	git stash &&
	test_when_finished "git stash drop" &&
	git pack-refs --all &&
	(
		GIT_PS1_SHOWSTASHSTATE=y &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - stash status indicator - not shown inside .git directory' '
	printf " (GIT_DIR!)" >expected &&
	echo 2 >file &&
	git stash &&
	test_when_finished "git stash drop" &&
	(
		GIT_PS1_SHOWSTASHSTATE=y &&
		cd .git &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - untracked files status indicator - no untracked files' '
	printf " (master)" >expected &&
	(
		GIT_PS1_SHOWUNTRACKEDFILES=y &&
		cd otherrepo &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - untracked files status indicator - untracked files' '
	printf " (master %%)" >expected &&
	(
		GIT_PS1_SHOWUNTRACKEDFILES=y &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - untracked files status indicator - shell variable unset with config disabled' '
	printf " (master)" >expected &&
	test_config bash.showUntrackedFiles false &&
	(
		sane_unset GIT_PS1_SHOWUNTRACKEDFILES &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - untracked files status indicator - shell variable unset with config enabled' '
	printf " (master)" >expected &&
	test_config bash.showUntrackedFiles true &&
	(
		sane_unset GIT_PS1_SHOWUNTRACKEDFILES &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - untracked files status indicator - shell variable set with config disabled' '
	printf " (master)" >expected &&
	test_config bash.showUntrackedFiles false &&
	(
		GIT_PS1_SHOWUNTRACKEDFILES=y &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - untracked files status indicator - shell variable set with config enabled' '
	printf " (master %%)" >expected &&
	test_config bash.showUntrackedFiles true &&
	(
		GIT_PS1_SHOWUNTRACKEDFILES=y &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - untracked files status indicator - not shown inside .git directory' '
	printf " (GIT_DIR!)" >expected &&
	(
		GIT_PS1_SHOWUNTRACKEDFILES=y &&
		cd .git &&
		__git_ps1 >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - format string starting with dash' '
	printf -- "-master" >expected &&
	__git_ps1 "-%s" >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - pc mode' '
	printf "BEFORE: (master):AFTER" >expected &&
	printf "" >expected_output &&
	(
		__git_ps1 "BEFORE:" ":AFTER" >"$actual" &&
		test_cmp expected_output "$actual" &&
		printf "%s" "$PS1" >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - bash color pc mode - branch name' '
	printf "BEFORE: (${c_green}master${c_clear}):AFTER" >expected &&
	(
		GIT_PS1_SHOWCOLORHINTS=y &&
		__git_ps1 "BEFORE:" ":AFTER" >"$actual"
		printf "%s" "$PS1" >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - bash color pc mode - detached head' '
	printf "BEFORE: (${c_red}(%s...)${c_clear}):AFTER" $(git log -1 --format="%h" b1^) >expected &&
	git checkout b1^ &&
	test_when_finished "git checkout master" &&
	(
		GIT_PS1_SHOWCOLORHINTS=y &&
		__git_ps1 "BEFORE:" ":AFTER" &&
		printf "%s" "$PS1" >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - bash color pc mode - dirty status indicator - dirty worktree' '
	printf "BEFORE: (${c_green}master${c_clear} ${c_red}*${c_clear}):AFTER" >expected &&
	echo "dirty" >file &&
	test_when_finished "git reset --hard" &&
	(
		GIT_PS1_SHOWDIRTYSTATE=y &&
		GIT_PS1_SHOWCOLORHINTS=y &&
		__git_ps1 "BEFORE:" ":AFTER" &&
		printf "%s" "$PS1" >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - bash color pc mode - dirty status indicator - dirty index' '
	printf "BEFORE: (${c_green}master${c_clear} ${c_green}+${c_clear}):AFTER" >expected &&
	echo "dirty" >file &&
	test_when_finished "git reset --hard" &&
	git add -u &&
	(
		GIT_PS1_SHOWDIRTYSTATE=y &&
		GIT_PS1_SHOWCOLORHINTS=y &&
		__git_ps1 "BEFORE:" ":AFTER" &&
		printf "%s" "$PS1" >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - bash color pc mode - dirty status indicator - dirty index and worktree' '
	printf "BEFORE: (${c_green}master${c_clear} ${c_red}*${c_green}+${c_clear}):AFTER" >expected &&
	echo "dirty index" >file &&
	test_when_finished "git reset --hard" &&
	git add -u &&
	echo "dirty worktree" >file &&
	(
		GIT_PS1_SHOWCOLORHINTS=y &&
		GIT_PS1_SHOWDIRTYSTATE=y &&
		__git_ps1 "BEFORE:" ":AFTER" &&
		printf "%s" "$PS1" >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - bash color pc mode - dirty status indicator - before root commit' '
	printf "BEFORE: (${c_green}master${c_clear} ${c_green}#${c_clear}):AFTER" >expected &&
	(
		GIT_PS1_SHOWDIRTYSTATE=y &&
		GIT_PS1_SHOWCOLORHINTS=y &&
		cd otherrepo &&
		__git_ps1 "BEFORE:" ":AFTER" &&
		printf "%s" "$PS1" >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - bash color pc mode - inside .git directory' '
	printf "BEFORE: (${c_green}GIT_DIR!${c_clear}):AFTER" >expected &&
	echo "dirty" >file &&
	test_when_finished "git reset --hard" &&
	(
		GIT_PS1_SHOWDIRTYSTATE=y &&
		GIT_PS1_SHOWCOLORHINTS=y &&
		cd .git &&
		__git_ps1 "BEFORE:" ":AFTER" &&
		printf "%s" "$PS1" >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - bash color pc mode - stash status indicator' '
	printf "BEFORE: (${c_green}master${c_clear} ${c_lblue}\$${c_clear}):AFTER" >expected &&
	echo 2 >file &&
	git stash &&
	test_when_finished "git stash drop" &&
	(
		GIT_PS1_SHOWSTASHSTATE=y &&
		GIT_PS1_SHOWCOLORHINTS=y &&
		__git_ps1 "BEFORE:" ":AFTER" &&
		printf "%s" "$PS1" >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - bash color pc mode - untracked files status indicator' '
	printf "BEFORE: (${c_green}master${c_clear} ${c_red}%%${c_clear}):AFTER" >expected &&
	(
		GIT_PS1_SHOWUNTRACKEDFILES=y &&
		GIT_PS1_SHOWCOLORHINTS=y &&
		__git_ps1 "BEFORE:" ":AFTER" &&
		printf "%s" "$PS1" >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success 'prompt - zsh color pc mode' '
	printf "BEFORE: (%%F{green}master%%f):AFTER" >expected &&
	(
		ZSH_VERSION=5.0.0 &&
		GIT_PS1_SHOWCOLORHINTS=y &&
		__git_ps1 "BEFORE:" ":AFTER" >"$actual"
		printf "%s" "$PS1" >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_done
