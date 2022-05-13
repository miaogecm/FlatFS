#!/bin/sh

test_description='Various filesystem issues'

. ./test-lib.sh

auml=$(printf '\303\244')
aumlcdiar=$(printf '\141\314\210')

if test_have_prereq CASE_INSENSITIVE_FS
then
	say "will test on a case insensitive filesystem"
	test_case=test_expect_failure
else
	test_case=test_expect_success
fi

if test_have_prereq UTF8_NFD_TO_NFC
then
	say "will test on a unicode corrupting filesystem"
	test_unicode=test_expect_failure
else
	test_unicode=test_expect_success
fi

test_have_prereq SYMLINKS ||
	say "will test on a filesystem lacking symbolic links"

if test_have_prereq CASE_INSENSITIVE_FS
then
test_expect_success "detection of case insensitive filesystem during repo init" '
	test $(git config --bool core.ignorecase) = true
'
else
test_expect_success "detection of case insensitive filesystem during repo init" '
	test_must_fail git config --bool core.ignorecase >/dev/null ||
	test $(git config --bool core.ignorecase) = false
'
fi

if test_have_prereq SYMLINKS
then
test_expect_success "detection of filesystem w/o symlink support during repo init" '
	test_must_fail git config --bool core.symlinks ||
	test "$(git config --bool core.symlinks)" = true
'
else
test_expect_success "detection of filesystem w/o symlink support during repo init" '
	v=$(git config --bool core.symlinks) &&
	test "$v" = false
'
fi

test_expect_success "setup case tests" '
	git config core.ignorecase true &&
	touch camelcase &&
	git add camelcase &&
	git commit -m "initial" &&
	git tag initial &&
	git checkout -b topic &&
	git mv camelcase tmp &&
	git mv tmp CamelCase &&
	git commit -m "rename" &&
	git checkout -f master
'

$test_case 'rename (case change)' '
	git mv camelcase CamelCase &&
	git commit -m "rename"
'

test_expect_success 'merge (case change)' '
	rm -f CamelCase &&
	rm -f camelcase &&
	git reset --hard initial &&
	git merge topic
'



test_expect_failure CASE_INSENSITIVE_FS 'add (with different case)' '
	git reset --hard initial &&
	rm camelcase &&
	echo 1 >CamelCase &&
	git add CamelCase &&
	camel=$(git ls-files | grep -i camelcase) &&
	test $(echo "$camel" | wc -l) = 1 &&
	test "z$(git cat-file blob :$camel)" = z1
'

test_expect_success "setup unicode normalization tests" '
	test_create_repo unicode &&
	cd unicode &&
	git config core.precomposeunicode false &&
	touch "$aumlcdiar" &&
	git add "$aumlcdiar" &&
	git commit -m initial &&
	git tag initial &&
	git checkout -b topic &&
	git mv $aumlcdiar tmp &&
	git mv tmp "$auml" &&
	git commit -m rename &&
	git checkout -f master
'

$test_unicode 'rename (silent unicode normalization)' '
	git mv "$aumlcdiar" "$auml" &&
	git commit -m rename
'

$test_unicode 'merge (silent unicode normalization)' '
	git reset --hard initial &&
	git merge topic
'

test_done
