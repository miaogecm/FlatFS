#!/bin/sh
#
# Copyright (c) 2012 Felipe Contreras
#

test_description='test bash completion'

. ./lib-bash.sh

complete ()
{
	# do nothing
	return 0
}

# Be careful when updating this list:
#
# (1) The build tree may have build artifact from different branch, or
#     the user's $PATH may have a random executable that may begin
#     with "git-check" that are not part of the subcommands this build
#     will ship, e.g.  "check-ignore".  The tests for completion for
#     subcommand names tests how "check" is expanded; we limit the
#     possible candidates to "checkout" and "check-attr" to make sure
#     "check-attr", which is known by the filter function as a
#     subcommand to be thrown out, while excluding other random files
#     that happen to begin with "check" to avoid letting them get in
#     the way.
#
# (2) A test makes sure that common subcommands are included in the
#     completion for "git <TAB>", and a plumbing is excluded.  "add",
#     "filter-branch" and "ls-files" are listed for this.

GIT_TESTING_COMMAND_COMPLETION='add checkout check-attr filter-branch ls-files'

. "$GIT_BUILD_DIR/contrib/completion/git-completion.bash"

# We don't need this function to actually join words or do anything special.
# Also, it's cleaner to avoid touching bash's internal completion variables.
# So let's override it with a minimal version for testing purposes.
_get_comp_words_by_ref ()
{
	while [ $# -gt 0 ]; do
		case "$1" in
		cur)
			cur=${_words[_cword]}
			;;
		prev)
			prev=${_words[_cword-1]}
			;;
		words)
			words=("${_words[@]}")
			;;
		cword)
			cword=$_cword
			;;
		esac
		shift
	done
}

print_comp ()
{
	local IFS=$'\n'
	echo "${COMPREPLY[*]}" > out
}

run_completion ()
{
	local -a COMPREPLY _words
	local _cword
	_words=( $1 )
	test "${1: -1}" = ' ' && _words[${#_words[@]}+1]=''
	(( _cword = ${#_words[@]} - 1 ))
	__git_wrap__git_main && print_comp
}

# Test high-level completion
# Arguments are:
# 1: typed text so far (cur)
# 2: expected completion
test_completion ()
{
	if test $# -gt 1
	then
		printf '%s\n' "$2" >expected
	else
		sed -e 's/Z$//' >expected
	fi &&
	run_completion "$1" &&
	test_cmp expected out
}

# Test __gitcomp.
# The first argument is the typed text so far (cur); the rest are
# passed to __gitcomp.  Expected output comes is read from the
# standard input, like test_completion().
test_gitcomp ()
{
	local -a COMPREPLY &&
	sed -e 's/Z$//' >expected &&
	cur="$1" &&
	shift &&
	__gitcomp "$@" &&
	print_comp &&
	test_cmp expected out
}

# Test __gitcomp_nl
# Arguments are:
# 1: current word (cur)
# -: the rest are passed to __gitcomp_nl
test_gitcomp_nl ()
{
	local -a COMPREPLY &&
	sed -e 's/Z$//' >expected &&
	cur="$1" &&
	shift &&
	__gitcomp_nl "$@" &&
	print_comp &&
	test_cmp expected out
}

invalid_variable_name='${foo.bar}'

actual="$TRASH_DIRECTORY/actual"

test_expect_success 'setup for __gitdir tests' '
	mkdir -p subdir/subsubdir &&
	git init otherrepo
'

test_expect_success '__gitdir - from command line (through $__git_dir)' '
	echo "$TRASH_DIRECTORY/otherrepo/.git" >expected &&
	(
		__git_dir="$TRASH_DIRECTORY/otherrepo/.git" &&
		__gitdir >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success '__gitdir - repo as argument' '
	echo "otherrepo/.git" >expected &&
	__gitdir "otherrepo" >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success '__gitdir - remote as argument' '
	echo "remote" >expected &&
	__gitdir "remote" >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success '__gitdir - .git directory in cwd' '
	echo ".git" >expected &&
	__gitdir >"$actual" &&
	test_cmp expected "$actual"
'

test_expect_success '__gitdir - .git directory in parent' '
	echo "$(pwd -P)/.git" >expected &&
	(
		cd subdir/subsubdir &&
		__gitdir >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success '__gitdir - cwd is a .git directory' '
	echo "." >expected &&
	(
		cd .git &&
		__gitdir >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success '__gitdir - parent is a .git directory' '
	echo "$(pwd -P)/.git" >expected &&
	(
		cd .git/refs/heads &&
		__gitdir >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success '__gitdir - $GIT_DIR set while .git directory in cwd' '
	echo "$TRASH_DIRECTORY/otherrepo/.git" >expected &&
	(
		GIT_DIR="$TRASH_DIRECTORY/otherrepo/.git" &&
		export GIT_DIR &&
		__gitdir >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success '__gitdir - $GIT_DIR set while .git directory in parent' '
	echo "$TRASH_DIRECTORY/otherrepo/.git" >expected &&
	(
		GIT_DIR="$TRASH_DIRECTORY/otherrepo/.git" &&
		export GIT_DIR &&
		cd subdir &&
		__gitdir >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success '__gitdir - non-existing $GIT_DIR' '
	(
		GIT_DIR="$TRASH_DIRECTORY/non-existing" &&
		export GIT_DIR &&
		test_must_fail __gitdir
	)
'

test_expect_success '__gitdir - gitfile in cwd' '
	echo "$(pwd -P)/otherrepo/.git" >expected &&
	echo "gitdir: $TRASH_DIRECTORY/otherrepo/.git" >subdir/.git &&
	test_when_finished "rm -f subdir/.git" &&
	(
		cd subdir &&
		__gitdir >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success '__gitdir - gitfile in parent' '
	echo "$(pwd -P)/otherrepo/.git" >expected &&
	echo "gitdir: $TRASH_DIRECTORY/otherrepo/.git" >subdir/.git &&
	test_when_finished "rm -f subdir/.git" &&
	(
		cd subdir/subsubdir &&
		__gitdir >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success SYMLINKS '__gitdir - resulting path avoids symlinks' '
	echo "$(pwd -P)/otherrepo/.git" >expected &&
	mkdir otherrepo/dir &&
	test_when_finished "rm -rf otherrepo/dir" &&
	ln -s otherrepo/dir link &&
	test_when_finished "rm -f link" &&
	(
		cd link &&
		__gitdir >"$actual"
	) &&
	test_cmp expected "$actual"
'

test_expect_success '__gitdir - not a git repository' '
	(
		cd subdir/subsubdir &&
		GIT_CEILING_DIRECTORIES="$TRASH_DIRECTORY" &&
		export GIT_CEILING_DIRECTORIES &&
		test_must_fail __gitdir
	)
'

test_expect_success '__gitcomp - trailing space - options' '
	test_gitcomp "--re" "--dry-run --reuse-message= --reedit-message=
		--reset-author" <<-EOF
	--reuse-message=Z
	--reedit-message=Z
	--reset-author Z
	EOF
'

test_expect_success '__gitcomp - trailing space - config keys' '
	test_gitcomp "br" "branch. branch.autosetupmerge
		branch.autosetuprebase browser." <<-\EOF
	branch.Z
	branch.autosetupmerge Z
	branch.autosetuprebase Z
	browser.Z
	EOF
'

test_expect_success '__gitcomp - option parameter' '
	test_gitcomp "--strategy=re" "octopus ours recursive resolve subtree" \
		"" "re" <<-\EOF
	recursive Z
	resolve Z
	EOF
'

test_expect_success '__gitcomp - prefix' '
	test_gitcomp "branch.me" "remote merge mergeoptions rebase" \
		"branch.maint." "me" <<-\EOF
	branch.maint.merge Z
	branch.maint.mergeoptions Z
	EOF
'

test_expect_success '__gitcomp - suffix' '
	test_gitcomp "branch.me" "master maint next pu" "branch." \
		"ma" "." <<-\EOF
	branch.master.Z
	branch.maint.Z
	EOF
'

test_expect_success '__gitcomp - doesnt fail because of invalid variable name' '
	__gitcomp "$invalid_variable_name"
'

read -r -d "" refs <<-\EOF
maint
master
next
pu
EOF

test_expect_success '__gitcomp_nl - trailing space' '
	test_gitcomp_nl "m" "$refs" <<-EOF
	maint Z
	master Z
	EOF
'

test_expect_success '__gitcomp_nl - prefix' '
	test_gitcomp_nl "--fixup=m" "$refs" "--fixup=" "m" <<-EOF
	--fixup=maint Z
	--fixup=master Z
	EOF
'

test_expect_success '__gitcomp_nl - suffix' '
	test_gitcomp_nl "branch.ma" "$refs" "branch." "ma" "." <<-\EOF
	branch.maint.Z
	branch.master.Z
	EOF
'

test_expect_success '__gitcomp_nl - no suffix' '
	test_gitcomp_nl "ma" "$refs" "" "ma" "" <<-\EOF
	maintZ
	masterZ
	EOF
'

test_expect_success '__gitcomp_nl - doesnt fail because of invalid variable name' '
	__gitcomp_nl "$invalid_variable_name"
'

test_expect_success 'basic' '
	run_completion "git " &&
	# built-in
	grep -q "^add \$" out &&
	# script
	grep -q "^filter-branch \$" out &&
	# plumbing
	! grep -q "^ls-files \$" out &&

	run_completion "git f" &&
	! grep -q -v "^f" out
'

test_expect_success 'double dash "git" itself' '
	test_completion "git --" <<-\EOF
	--paginate Z
	--no-pager Z
	--git-dir=
	--bare Z
	--version Z
	--exec-path Z
	--exec-path=
	--html-path Z
	--man-path Z
	--info-path Z
	--work-tree=
	--namespace=
	--no-replace-objects Z
	--help Z
	EOF
'

test_expect_success 'double dash "git checkout"' '
	test_completion "git checkout --" <<-\EOF
	--quiet Z
	--ours Z
	--theirs Z
	--track Z
	--no-track Z
	--merge Z
	--conflict=
	--orphan Z
	--patch Z
	EOF
'

test_expect_success 'general options' '
	test_completion "git --ver" "--version " &&
	test_completion "git --hel" "--help " &&
	test_completion "git --exe" <<-\EOF &&
	--exec-path Z
	--exec-path=
	EOF
	test_completion "git --htm" "--html-path " &&
	test_completion "git --pag" "--paginate " &&
	test_completion "git --no-p" "--no-pager " &&
	test_completion "git --git" "--git-dir=" &&
	test_completion "git --wor" "--work-tree=" &&
	test_completion "git --nam" "--namespace=" &&
	test_completion "git --bar" "--bare " &&
	test_completion "git --inf" "--info-path " &&
	test_completion "git --no-r" "--no-replace-objects "
'

test_expect_success 'general options plus command' '
	test_completion "git --version check" "checkout " &&
	test_completion "git --paginate check" "checkout " &&
	test_completion "git --git-dir=foo check" "checkout " &&
	test_completion "git --bare check" "checkout " &&
	test_completion "git --exec-path=foo check" "checkout " &&
	test_completion "git --html-path check" "checkout " &&
	test_completion "git --no-pager check" "checkout " &&
	test_completion "git --work-tree=foo check" "checkout " &&
	test_completion "git --namespace=foo check" "checkout " &&
	test_completion "git --paginate check" "checkout " &&
	test_completion "git --info-path check" "checkout " &&
	test_completion "git --no-replace-objects check" "checkout "
'

test_expect_success 'git --help completion' '
	test_completion "git --help ad" "add " &&
	test_completion "git --help core" "core-tutorial "
'

test_expect_success 'setup for ref completion' '
	echo content >file1 &&
	echo more >file2 &&
	git add . &&
	git commit -m one &&
	git branch mybranch &&
	git tag mytag
'

test_expect_success 'checkout completes ref names' '
	test_completion "git checkout m" <<-\EOF
	master Z
	mybranch Z
	mytag Z
	EOF
'

test_expect_success 'show completes all refs' '
	test_completion "git show m" <<-\EOF
	master Z
	mybranch Z
	mytag Z
	EOF
'

test_expect_success '<ref>: completes paths' '
	test_completion "git show mytag:f" <<-\EOF
	file1 Z
	file2 Z
	EOF
'

test_expect_success 'complete tree filename with spaces' '
	echo content >"name with spaces" &&
	git add . &&
	git commit -m spaces &&
	test_completion "git show HEAD:nam" <<-\EOF
	name with spaces Z
	EOF
'

test_expect_success 'complete tree filename with metacharacters' '
	echo content >"name with \${meta}" &&
	git add . &&
	git commit -m meta &&
	test_completion "git show HEAD:nam" <<-\EOF
	name with ${meta} Z
	name with spaces Z
	EOF
'

test_expect_success 'send-email' '
	test_completion "git send-email --cov" "--cover-letter " &&
	test_completion "git send-email ma" "master "
'

test_expect_success 'complete files' '
	git init tmp && cd tmp &&
	test_when_finished "cd .. && rm -rf tmp" &&

	echo "expected" > .gitignore &&
	echo "out" >> .gitignore &&

	git add .gitignore &&
	test_completion "git commit " ".gitignore" &&

	git commit -m ignore &&

	touch new &&
	test_completion "git add " "new" &&

	git add new &&
	git commit -a -m new &&
	test_completion "git add " "" &&

	git mv new modified &&
	echo modify > modified &&
	test_completion "git add " "modified" &&

	touch untracked &&

	: TODO .gitignore should not be here &&
	test_completion "git rm " <<-\EOF &&
	.gitignore
	modified
	EOF

	test_completion "git clean " "untracked" &&

	: TODO .gitignore should not be here &&
	test_completion "git mv " <<-\EOF &&
	.gitignore
	modified
	EOF

	mkdir dir &&
	touch dir/file-in-dir &&
	git add dir/file-in-dir &&
	git commit -m dir &&

	mkdir untracked-dir &&

	: TODO .gitignore should not be here &&
	test_completion "git mv modified " <<-\EOF &&
	.gitignore
	dir
	modified
	untracked
	untracked-dir
	EOF

	test_completion "git commit " "modified" &&

	: TODO .gitignore should not be here &&
	test_completion "git ls-files " <<-\EOF
	.gitignore
	dir
	modified
	EOF

	touch momified &&
	test_completion "git add mom" "momified"
'

test_expect_failure 'complete with tilde expansion' '
	git init tmp && cd tmp &&
	test_when_finished "cd .. && rm -rf tmp" &&

	touch ~/tmp/file &&

	test_completion "git add ~/tmp/" "~/tmp/file"
'

test_done
