#!/bin/sh

test_description='test diff with a bogus tree containing the null sha1'
. ./test-lib.sh

empty_tree=4b825dc642cb6eb9a060e54bf8d69288fbee4904

test_expect_success 'create bogus tree' '
	bogus_tree=$(
		printf "100644 fooQQQQQQQQQQQQQQQQQQQQQ" |
		q_to_nul |
		git hash-object -w --stdin -t tree
	)
'

test_expect_success 'create tree with matching file' '
	echo bar >foo &&
	git add foo &&
	good_tree=$(git write-tree)
	blob=$(git rev-parse :foo)
'

test_expect_success 'raw diff shows null sha1 (addition)' '
	echo ":000000 100644 $_z40 $_z40 A	foo" >expect &&
	git diff-tree $empty_tree $bogus_tree >actual &&
	test_cmp expect actual
'

test_expect_success 'raw diff shows null sha1 (removal)' '
	echo ":100644 000000 $_z40 $_z40 D	foo" >expect &&
	git diff-tree $bogus_tree $empty_tree >actual &&
	test_cmp expect actual
'

test_expect_success 'raw diff shows null sha1 (modification)' '
	echo ":100644 100644 $blob $_z40 M	foo" >expect &&
	git diff-tree $good_tree $bogus_tree >actual &&
	test_cmp expect actual
'

test_expect_success 'raw diff shows null sha1 (other direction)' '
	echo ":100644 100644 $_z40 $blob M	foo" >expect &&
	git diff-tree $bogus_tree $good_tree >actual &&
	test_cmp expect actual
'

test_expect_success 'raw diff shows null sha1 (reverse)' '
	echo ":100644 100644 $_z40 $blob M	foo" >expect &&
	git diff-tree -R $good_tree $bogus_tree >actual &&
	test_cmp expect actual
'

test_expect_success 'raw diff shows null sha1 (index)' '
	echo ":100644 100644 $_z40 $blob M	foo" >expect &&
	git diff-index $bogus_tree >actual &&
	test_cmp expect actual
'

test_expect_success 'patch fails due to bogus sha1 (addition)' '
	test_must_fail git diff-tree -p $empty_tree $bogus_tree
'

test_expect_success 'patch fails due to bogus sha1 (removal)' '
	test_must_fail git diff-tree -p $bogus_tree $empty_tree
'

test_expect_success 'patch fails due to bogus sha1 (modification)' '
	test_must_fail git diff-tree -p $good_tree $bogus_tree
'

test_expect_success 'patch fails due to bogus sha1 (other direction)' '
	test_must_fail git diff-tree -p $bogus_tree $good_tree
'

test_expect_success 'patch fails due to bogus sha1 (reverse)' '
	test_must_fail git diff-tree -R -p $good_tree $bogus_tree
'

test_expect_success 'patch fails due to bogus sha1 (index)' '
	test_must_fail git diff-index -p $bogus_tree
'

test_done
