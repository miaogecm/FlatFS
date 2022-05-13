#!/bin/sh

test_description=clone

. ./test-lib.sh

test_expect_success setup '

	rm -fr .git &&
	test_create_repo src &&
	(
		cd src &&
		>file &&
		git add file &&
		git commit -m initial &&
		echo 1 >file &&
		git add file &&
		git commit -m updated
	)

'

test_expect_success 'clone with excess parameters (1)' '

	rm -fr dst &&
	test_must_fail git clone -n src dst junk

'

test_expect_success 'clone with excess parameters (2)' '

	rm -fr dst &&
	test_must_fail git clone -n "file://$(pwd)/src" dst junk

'

test_expect_success C_LOCALE_OUTPUT 'output from clone' '
	rm -fr dst &&
	git clone -n "file://$(pwd)/src" dst >output 2>&1 &&
	test $(grep Clon output | wc -l) = 1
'

test_expect_success 'clone does not keep pack' '

	rm -fr dst &&
	git clone -n "file://$(pwd)/src" dst &&
	! test -f dst/file &&
	! (echo dst/.git/objects/pack/pack-* | grep "\.keep")

'

test_expect_success 'clone checks out files' '

	rm -fr dst &&
	git clone src dst &&
	test -f dst/file

'

test_expect_success 'clone respects GIT_WORK_TREE' '

	GIT_WORK_TREE=worktree git clone src bare &&
	test -f bare/config &&
	test -f worktree/file

'

test_expect_success 'clone creates intermediate directories' '

	git clone src long/path/to/dst &&
	test -f long/path/to/dst/file

'

test_expect_success 'clone creates intermediate directories for bare repo' '

	git clone --bare src long/path/to/bare/dst &&
	test -f long/path/to/bare/dst/config

'

test_expect_success 'clone --mirror' '

	git clone --mirror src mirror &&
	test -f mirror/HEAD &&
	test ! -f mirror/file &&
	FETCH="$(cd mirror && git config remote.origin.fetch)" &&
	test "+refs/*:refs/*" = "$FETCH" &&
	MIRROR="$(cd mirror && git config --bool remote.origin.mirror)" &&
	test "$MIRROR" = true

'

test_expect_success 'clone --mirror with detached HEAD' '

	( cd src && git checkout HEAD^ && git rev-parse HEAD >../expected ) &&
	git clone --mirror src mirror.detached &&
	( cd src && git checkout - ) &&
	GIT_DIR=mirror.detached git rev-parse HEAD >actual &&
	test_cmp expected actual

'

test_expect_success 'clone --bare with detached HEAD' '

	( cd src && git checkout HEAD^ && git rev-parse HEAD >../expected ) &&
	git clone --bare src bare.detached &&
	( cd src && git checkout - ) &&
	GIT_DIR=bare.detached git rev-parse HEAD >actual &&
	test_cmp expected actual

'

test_expect_success 'clone --bare names the local repository <name>.git' '

	git clone --bare src &&
	test -d src.git

'

test_expect_success 'clone --mirror does not repeat tags' '

	(cd src &&
	 git tag some-tag HEAD) &&
	git clone --mirror src mirror2 &&
	(cd mirror2 &&
	 git show-ref 2> clone.err > clone.out) &&
	test_must_fail grep Duplicate mirror2/clone.err &&
	grep some-tag mirror2/clone.out

'

test_expect_success 'clone to destination with trailing /' '

	git clone src target-1/ &&
	T=$( cd target-1 && git rev-parse HEAD ) &&
	S=$( cd src && git rev-parse HEAD ) &&
	test "$T" = "$S"

'

test_expect_success 'clone to destination with extra trailing /' '

	git clone src target-2/// &&
	T=$( cd target-2 && git rev-parse HEAD ) &&
	S=$( cd src && git rev-parse HEAD ) &&
	test "$T" = "$S"

'

test_expect_success 'clone to an existing empty directory' '
	mkdir target-3 &&
	git clone src target-3 &&
	T=$( cd target-3 && git rev-parse HEAD ) &&
	S=$( cd src && git rev-parse HEAD ) &&
	test "$T" = "$S"
'

test_expect_success 'clone to an existing non-empty directory' '
	mkdir target-4 &&
	>target-4/Fakefile &&
	test_must_fail git clone src target-4
'

test_expect_success 'clone to an existing path' '
	>target-5 &&
	test_must_fail git clone src target-5
'

test_expect_success 'clone a void' '
	mkdir src-0 &&
	(
		cd src-0 && git init
	) &&
	git clone "file://$(pwd)/src-0" target-6 2>err-6 &&
	! grep "fatal:" err-6 &&
	(
		cd src-0 && test_commit A
	) &&
	git clone "file://$(pwd)/src-0" target-7 2>err-7 &&
	! grep "fatal:" err-7 &&
	# There is no reason to insist they are bit-for-bit
	# identical, but this test should suffice for now.
	test_cmp target-6/.git/config target-7/.git/config
'

test_expect_success 'clone respects global branch.autosetuprebase' '
	(
		test_config="$HOME/.gitconfig" &&
		git config -f "$test_config" branch.autosetuprebase remote &&
		rm -fr dst &&
		git clone src dst &&
		cd dst &&
		actual="z$(git config branch.master.rebase)" &&
		test ztrue = $actual
	)
'

test_expect_success 'respect url-encoding of file://' '
	git init x+y &&
	git clone "file://$PWD/x+y" xy-url-1 &&
	git clone "file://$PWD/x%2By" xy-url-2
'

test_expect_success 'do not query-string-decode + in URLs' '
	rm -rf x+y &&
	git init "x y" &&
	test_must_fail git clone "file://$PWD/x+y" xy-no-plus
'

test_expect_success 'do not respect url-encoding of non-url path' '
	git init x+y &&
	test_must_fail git clone x%2By xy-regular &&
	git clone x+y xy-regular
'

test_expect_success 'clone separate gitdir' '
	rm -rf dst &&
	git clone --separate-git-dir realgitdir src dst &&
	test -d realgitdir/refs
'

test_expect_success 'clone separate gitdir: output' '
	echo "gitdir: `pwd`/realgitdir" >expected &&
	test_cmp expected dst/.git
'

test_expect_success 'clone from .git file' '
	git clone dst/.git dst2
'

test_expect_success 'fetch from .git gitfile' '
	(
		cd dst2 &&
		git fetch ../dst/.git
	)
'

test_expect_success 'fetch from gitfile parent' '
	(
		cd dst2 &&
		git fetch ../dst
	)
'

test_expect_success 'clone separate gitdir where target already exists' '
	rm -rf dst &&
	test_must_fail git clone --separate-git-dir realgitdir src dst
'

test_expect_success 'clone --reference from original' '
	git clone --shared --bare src src-1 &&
	git clone --bare src src-2 &&
	git clone --reference=src-2 --bare src-1 target-8 &&
	grep /src-2/ target-8/objects/info/alternates
'

test_expect_success 'clone with more than one --reference' '
	git clone --bare src src-3 &&
	git clone --bare src src-4 &&
	git clone --reference=src-3 --reference=src-4 src target-9 &&
	grep /src-3/ target-9/.git/objects/info/alternates &&
	grep /src-4/ target-9/.git/objects/info/alternates
'

test_expect_success 'clone from original with relative alternate' '
	mkdir nest &&
	git clone --bare src nest/src-5 &&
	echo ../../../src/.git/objects >nest/src-5/objects/info/alternates &&
	git clone --bare nest/src-5 target-10 &&
	grep /src/\\.git/objects target-10/objects/info/alternates
'

test_expect_success 'clone checking out a tag' '
	git clone --branch=some-tag src dst.tag &&
	GIT_DIR=src/.git git rev-parse some-tag >expected &&
	test_cmp expected dst.tag/.git/HEAD &&
	GIT_DIR=dst.tag/.git git config remote.origin.fetch >fetch.actual &&
	echo "+refs/heads/*:refs/remotes/origin/*" >fetch.expected &&
	test_cmp fetch.expected fetch.actual
'

setup_ssh_wrapper () {
	test_expect_success 'setup ssh wrapper' '
		write_script "$TRASH_DIRECTORY/ssh-wrapper" <<-\EOF &&
		echo >>"$TRASH_DIRECTORY/ssh-output" "ssh: $*" &&
		# throw away all but the last argument, which should be the
		# command
		while test $# -gt 1; do shift; done
		eval "$1"
		EOF
		GIT_SSH="$TRASH_DIRECTORY/ssh-wrapper" &&
		export GIT_SSH &&
		export TRASH_DIRECTORY &&
		>"$TRASH_DIRECTORY"/ssh-output
	'
}

expect_ssh () {
	test_when_finished '
		(cd "$TRASH_DIRECTORY" && rm -f ssh-expect && >ssh-output)
	' &&
	{
		case "$1" in
		none)
			;;
		*)
			echo "ssh: $1 git-upload-pack '$2'"
		esac
	} >"$TRASH_DIRECTORY/ssh-expect" &&
	(cd "$TRASH_DIRECTORY" && test_cmp ssh-expect ssh-output)
}

setup_ssh_wrapper

test_expect_success 'clone myhost:src uses ssh' '
	git clone myhost:src ssh-clone &&
	expect_ssh myhost src
'

test_expect_success NOT_MINGW,NOT_CYGWIN 'clone local path foo:bar' '
	cp -R src "foo:bar" &&
	git clone "foo:bar" foobar &&
	expect_ssh none
'

test_expect_success 'bracketed hostnames are still ssh' '
	git clone "[myhost:123]:src" ssh-bracket-clone &&
	expect_ssh myhost:123 src
'

counter=0
# $1 url
# $2 none|host
# $3 path
test_clone_url () {
	counter=$(($counter + 1))
	test_might_fail git clone "$1" tmp$counter &&
	expect_ssh "$2" "$3"
}

test_expect_success NOT_MINGW 'clone c:temp is ssl' '
	test_clone_url c:temp c temp
'

test_expect_success MINGW 'clone c:temp is dos drive' '
	test_clone_url c:temp none
'

#ip v4
for repo in rep rep/home/project 123
do
	test_expect_success "clone host:$repo" '
		test_clone_url host:$repo host $repo
	'
done

#ipv6
for repo in rep rep/home/project 123
do
	test_expect_success "clone [::1]:$repo" '
		test_clone_url [::1]:$repo ::1 $repo
	'
done
#home directory
test_expect_success "clone host:/~repo" '
	test_clone_url host:/~repo host "~repo"
'

test_expect_success "clone [::1]:/~repo" '
	test_clone_url [::1]:/~repo ::1 "~repo"
'

# Corner cases
for url in foo/bar:baz [foo]bar/baz:qux [foo/bar]:baz
do
	test_expect_success "clone $url is not ssh" '
		test_clone_url $url none
	'
done

#with ssh:// scheme
test_expect_success 'clone ssh://host.xz/home/user/repo' '
	test_clone_url "ssh://host.xz/home/user/repo" host.xz "/home/user/repo"
'

# from home directory
test_expect_success 'clone ssh://host.xz/~repo' '
	test_clone_url "ssh://host.xz/~repo" host.xz "~repo"
'

# with port number
test_expect_success 'clone ssh://host.xz:22/home/user/repo' '
	test_clone_url "ssh://host.xz:22/home/user/repo" "-p 22 host.xz" "/home/user/repo"
'

# from home directory with port number
test_expect_success 'clone ssh://host.xz:22/~repo' '
	test_clone_url "ssh://host.xz:22/~repo" "-p 22 host.xz" "~repo"
'

#IPv6
test_expect_success 'clone ssh://[::1]/home/user/repo' '
	test_clone_url "ssh://[::1]/home/user/repo" "::1" "/home/user/repo"
'

#IPv6 from home directory
test_expect_success 'clone ssh://[::1]/~repo' '
	test_clone_url "ssh://[::1]/~repo" "::1" "~repo"
'

#IPv6 with port number
test_expect_success 'clone ssh://[::1]:22/home/user/repo' '
	test_clone_url "ssh://[::1]:22/home/user/repo" "-p 22 ::1" "/home/user/repo"
'

#IPv6 from home directory with port number
test_expect_success 'clone ssh://[::1]:22/~repo' '
	test_clone_url "ssh://[::1]:22/~repo" "-p 22 ::1" "~repo"
'

test_expect_success 'clone from a repository with two identical branches' '

	(
		cd src &&
		git checkout -b another master
	) &&
	git clone src target-11 &&
	test "z$( cd target-11 && git symbolic-ref HEAD )" = zrefs/heads/another

'

test_expect_success 'shallow clone locally' '
	git clone --depth=1 --no-local src ssrrcc &&
	git clone ssrrcc ddsstt &&
	test_cmp ssrrcc/.git/shallow ddsstt/.git/shallow &&
	( cd ddsstt && git fsck )
'

test_done
