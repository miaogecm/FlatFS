#!/bin/sh

test_description='combined and merge diff handle binary files and textconv'
. ./test-lib.sh

test_expect_success 'setup binary merge conflict' '
	echo oneQ1 | q_to_nul >binary &&
	git add binary &&
	git commit -m one &&
	echo twoQ2 | q_to_nul >binary &&
	git commit -a -m two &&
	git checkout -b branch-binary HEAD^ &&
	echo threeQ3 | q_to_nul >binary &&
	git commit -a -m three &&
	test_must_fail git merge master &&
	echo resolvedQhooray | q_to_nul >binary &&
	git commit -a -m resolved
'

cat >expect <<'EOF'
resolved

diff --git a/binary b/binary
index 7ea6ded..9563691 100644
Binary files a/binary and b/binary differ
resolved

diff --git a/binary b/binary
index 6197570..9563691 100644
Binary files a/binary and b/binary differ
EOF
test_expect_success 'diff -m indicates binary-ness' '
	git show --format=%s -m >actual &&
	test_cmp expect actual
'

cat >expect <<'EOF'
resolved

diff --combined binary
index 7ea6ded,6197570..9563691
Binary files differ
EOF
test_expect_success 'diff -c indicates binary-ness' '
	git show --format=%s -c >actual &&
	test_cmp expect actual
'

cat >expect <<'EOF'
resolved

diff --cc binary
index 7ea6ded,6197570..9563691
Binary files differ
EOF
test_expect_success 'diff --cc indicates binary-ness' '
	git show --format=%s --cc >actual &&
	test_cmp expect actual
'

test_expect_success 'setup non-binary with binary attribute' '
	git checkout master &&
	test_commit one text &&
	test_commit two text &&
	git checkout -b branch-text HEAD^ &&
	test_commit three text &&
	test_must_fail git merge master &&
	test_commit resolved text &&
	echo text -diff >.gitattributes
'

cat >expect <<'EOF'
resolved

diff --git a/text b/text
index 2bdf67a..2ab19ae 100644
Binary files a/text and b/text differ
resolved

diff --git a/text b/text
index f719efd..2ab19ae 100644
Binary files a/text and b/text differ
EOF
test_expect_success 'diff -m respects binary attribute' '
	git show --format=%s -m >actual &&
	test_cmp expect actual
'

cat >expect <<'EOF'
resolved

diff --combined text
index 2bdf67a,f719efd..2ab19ae
Binary files differ
EOF
test_expect_success 'diff -c respects binary attribute' '
	git show --format=%s -c >actual &&
	test_cmp expect actual
'

cat >expect <<'EOF'
resolved

diff --cc text
index 2bdf67a,f719efd..2ab19ae
Binary files differ
EOF
test_expect_success 'diff --cc respects binary attribute' '
	git show --format=%s --cc >actual &&
	test_cmp expect actual
'

test_expect_success 'setup textconv attribute' '
	echo "text diff=upcase" >.gitattributes &&
	git config diff.upcase.textconv "tr a-z A-Z <"
'

cat >expect <<'EOF'
resolved

diff --git a/text b/text
index 2bdf67a..2ab19ae 100644
--- a/text
+++ b/text
@@ -1 +1 @@
-THREE
+RESOLVED
resolved

diff --git a/text b/text
index f719efd..2ab19ae 100644
--- a/text
+++ b/text
@@ -1 +1 @@
-TWO
+RESOLVED
EOF
test_expect_success 'diff -m respects textconv attribute' '
	git show --format=%s -m >actual &&
	test_cmp expect actual
'

cat >expect <<'EOF'
resolved

diff --combined text
index 2bdf67a,f719efd..2ab19ae
--- a/text
+++ b/text
@@@ -1,1 -1,1 +1,1 @@@
- THREE
 -TWO
++RESOLVED
EOF
test_expect_success 'diff -c respects textconv attribute' '
	git show --format=%s -c >actual &&
	test_cmp expect actual
'

cat >expect <<'EOF'
resolved

diff --cc text
index 2bdf67a,f719efd..2ab19ae
--- a/text
+++ b/text
@@@ -1,1 -1,1 +1,1 @@@
- THREE
 -TWO
++RESOLVED
EOF
test_expect_success 'diff --cc respects textconv attribute' '
	git show --format=%s --cc >actual &&
	test_cmp expect actual
'

cat >expect <<'EOF'
diff --combined text
index 2bdf67a,f719efd..2ab19ae
--- a/text
+++ b/text
@@@ -1,1 -1,1 +1,1 @@@
- three
 -two
++resolved
EOF
test_expect_success 'diff-tree plumbing does not respect textconv' '
	git diff-tree HEAD -c -p >full &&
	tail -n +2 full >actual &&
	test_cmp expect actual
'

cat >expect <<'EOF'
diff --cc text
index 2bdf67a,f719efd..0000000
--- a/text
+++ b/text
@@@ -1,1 -1,1 +1,5 @@@
++<<<<<<< HEAD
 +THREE
++=======
+ TWO
++>>>>>>> MASTER
EOF
test_expect_success 'diff --cc respects textconv on worktree file' '
	git reset --hard HEAD^ &&
	test_must_fail git merge master &&
	git diff >actual &&
	test_cmp expect actual
'

test_done
