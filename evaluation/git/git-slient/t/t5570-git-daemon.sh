#!/bin/sh

test_description='test fetching over git protocol'
. ./test-lib.sh

LIB_GIT_DAEMON_PORT=${LIB_GIT_DAEMON_PORT-5570}
. "$TEST_DIRECTORY"/lib-git-daemon.sh
start_git_daemon

test_expect_success 'setup repository' '
	git config push.default matching &&
	echo content >file &&
	git add file &&
	git commit -m one
'

test_expect_success 'create git-accessible bare repository' '
	mkdir "$GIT_DAEMON_DOCUMENT_ROOT_PATH/repo.git" &&
	(cd "$GIT_DAEMON_DOCUMENT_ROOT_PATH/repo.git" &&
	 git --bare init &&
	 : >git-daemon-export-ok
	) &&
	git remote add public "$GIT_DAEMON_DOCUMENT_ROOT_PATH/repo.git" &&
	git push public master:master
'

test_expect_success 'clone git repository' '
	git clone "$GIT_DAEMON_URL/repo.git" clone &&
	test_cmp file clone/file
'

test_expect_success 'fetch changes via git protocol' '
	echo content >>file &&
	git commit -a -m two &&
	git push public &&
	(cd clone && git pull) &&
	test_cmp file clone/file
'

test_expect_success 'remote detects correct HEAD' '
	git push public master:other &&
	(cd clone &&
	 git remote set-head -d origin &&
	 git remote set-head -a origin &&
	 git symbolic-ref refs/remotes/origin/HEAD > output &&
	 echo refs/remotes/origin/master > expect &&
	 test_cmp expect output
	)
'

test_expect_success 'prepare pack objects' '
	cp -R "$GIT_DAEMON_DOCUMENT_ROOT_PATH"/repo.git "$GIT_DAEMON_DOCUMENT_ROOT_PATH"/repo_pack.git &&
	(cd "$GIT_DAEMON_DOCUMENT_ROOT_PATH"/repo_pack.git &&
	 git --bare repack -a -d
	)
'

test_expect_success 'fetch notices corrupt pack' '
	cp -R "$GIT_DAEMON_DOCUMENT_ROOT_PATH"/repo_pack.git "$GIT_DAEMON_DOCUMENT_ROOT_PATH"/repo_bad1.git &&
	(cd "$GIT_DAEMON_DOCUMENT_ROOT_PATH"/repo_bad1.git &&
	 p=`ls objects/pack/pack-*.pack` &&
	 chmod u+w $p &&
	 printf %0256d 0 | dd of=$p bs=256 count=1 seek=1 conv=notrunc
	) &&
	mkdir repo_bad1.git &&
	(cd repo_bad1.git &&
	 git --bare init &&
	 test_must_fail git --bare fetch "$GIT_DAEMON_URL/repo_bad1.git" &&
	 test 0 = `ls objects/pack/pack-*.pack | wc -l`
	)
'

test_expect_success 'fetch notices corrupt idx' '
	cp -R "$GIT_DAEMON_DOCUMENT_ROOT_PATH"/repo_pack.git "$GIT_DAEMON_DOCUMENT_ROOT_PATH"/repo_bad2.git &&
	(cd "$GIT_DAEMON_DOCUMENT_ROOT_PATH"/repo_bad2.git &&
	 p=`ls objects/pack/pack-*.idx` &&
	 chmod u+w $p &&
	 printf %0256d 0 | dd of=$p bs=256 count=1 seek=1 conv=notrunc
	) &&
	mkdir repo_bad2.git &&
	(cd repo_bad2.git &&
	 git --bare init &&
	 test_must_fail git --bare fetch "$GIT_DAEMON_URL/repo_bad2.git" &&
	 test 0 = `ls objects/pack | wc -l`
	)
'

test_remote_error()
{
	do_export=YesPlease
	while test $# -gt 0
	do
		case $1 in
		-x)
			shift
			chmod -x "$GIT_DAEMON_DOCUMENT_ROOT_PATH/repo.git"
			;;
		-n)
			shift
			do_export=
			;;
		*)
			break
		esac
	done

	msg=$1
	shift
	cmd=$1
	shift
	repo=$1
	shift || error "invalid number of arguments"

	if test -x "$GIT_DAEMON_DOCUMENT_ROOT_PATH/$repo"
	then
		if test -n "$do_export"
		then
			: >"$GIT_DAEMON_DOCUMENT_ROOT_PATH/$repo/git-daemon-export-ok"
		else
			rm -f "$GIT_DAEMON_DOCUMENT_ROOT_PATH/$repo/git-daemon-export-ok"
		fi
	fi

	test_must_fail git "$cmd" "$GIT_DAEMON_URL/$repo" "$@" 2>output &&
	test_i18ngrep "fatal: remote error: $msg: /$repo" output &&
	ret=$?
	chmod +x "$GIT_DAEMON_DOCUMENT_ROOT_PATH/repo.git"
	(exit $ret)
}

msg="access denied or repository not exported"
test_expect_success 'clone non-existent' "test_remote_error    '$msg' clone nowhere.git    "
test_expect_success 'push disabled'      "test_remote_error    '$msg' push  repo.git master"
test_expect_success 'read access denied' "test_remote_error -x '$msg' fetch repo.git       "
test_expect_success 'not exported'       "test_remote_error -n '$msg' fetch repo.git       "

stop_git_daemon
start_git_daemon --informative-errors

test_expect_success 'clone non-existent' "test_remote_error    'no such repository'      clone nowhere.git    "
test_expect_success 'push disabled'      "test_remote_error    'service not enabled'     push  repo.git master"
test_expect_success 'read access denied' "test_remote_error -x 'no such repository'      fetch repo.git       "
test_expect_success 'not exported'       "test_remote_error -n 'repository not exported' fetch repo.git       "

stop_git_daemon
test_done
