#!/bin/sh

test_description='push from/to a shallow clone'

. ./test-lib.sh

commit() {
	echo "$1" >tracked &&
	git add tracked &&
	git commit -m "$1"
}

test_expect_success 'setup' '
	git config --global transfer.fsckObjects true &&
	commit 1 &&
	commit 2 &&
	commit 3 &&
	commit 4 &&
	git clone . full &&
	(
	git init full-abc &&
	cd full-abc &&
	commit a &&
	commit b &&
	commit c
	) &&
	git clone --no-local --depth=2 .git shallow &&
	git --git-dir=shallow/.git log --format=%s >actual &&
	cat <<EOF >expect &&
4
3
EOF
	test_cmp expect actual &&
	git clone --no-local --depth=2 full-abc/.git shallow2 &&
	git --git-dir=shallow2/.git log --format=%s >actual &&
	cat <<EOF >expect &&
c
b
EOF
	test_cmp expect actual
'

test_expect_success 'push from shallow clone' '
	(
	cd shallow &&
	commit 5 &&
	git push ../.git +master:refs/remotes/shallow/master
	) &&
	git log --format=%s shallow/master >actual &&
	git fsck &&
	cat <<EOF >expect &&
5
4
3
2
1
EOF
	test_cmp expect actual
'

test_expect_success 'push from shallow clone, with grafted roots' '
	(
	cd shallow2 &&
	test_must_fail git push ../.git +master:refs/remotes/shallow2/master 2>err &&
	grep "shallow2/master.*shallow update not allowed" err
	) &&
	test_must_fail git rev-parse shallow2/master &&
	git fsck
'

test_expect_success 'add new shallow root with receive.updateshallow on' '
	test_config receive.shallowupdate true &&
	(
	cd shallow2 &&
	git push ../.git +master:refs/remotes/shallow2/master
	) &&
	git log --format=%s shallow2/master >actual &&
	git fsck &&
	cat <<EOF >expect &&
c
b
EOF
	test_cmp expect actual
'

test_expect_success 'push from shallow to shallow' '
	(
	cd shallow &&
	git --git-dir=../shallow2/.git config receive.shallowupdate true &&
	git push ../shallow2/.git +master:refs/remotes/shallow/master &&
	git --git-dir=../shallow2/.git config receive.shallowupdate false
	) &&
	(
	cd shallow2 &&
	git log --format=%s shallow/master >actual &&
	git fsck &&
	cat <<EOF >expect &&
5
4
3
EOF
	test_cmp expect actual
	)
'

test_expect_success 'push from full to shallow' '
	! git --git-dir=shallow2/.git cat-file blob `echo 1|git hash-object --stdin` &&
	commit 1 &&
	git push shallow2/.git +master:refs/remotes/top/master &&
	(
	cd shallow2 &&
	git log --format=%s top/master >actual &&
	git fsck &&
	cat <<EOF >expect &&
1
4
3
EOF
	test_cmp expect actual &&
	git cat-file blob `echo 1|git hash-object --stdin` >/dev/null
	)
'

if test -n "$NO_CURL" -o -z "$GIT_TEST_HTTPD"; then
	say 'skipping remaining tests, git built without http support'
	test_done
fi

. "$TEST_DIRECTORY"/lib-httpd.sh
start_httpd

test_expect_success 'push to shallow repo via http' '
	git clone --bare --no-local shallow "$HTTPD_DOCUMENT_ROOT_PATH/repo.git" &&
	(
	cd "$HTTPD_DOCUMENT_ROOT_PATH/repo.git" &&
	git config http.receivepack true
	) &&
	(
	cd full &&
	commit 9 &&
	git push $HTTPD_URL/smart/repo.git +master:refs/remotes/top/master
	) &&
	(
	cd "$HTTPD_DOCUMENT_ROOT_PATH/repo.git" &&
	git fsck &&
	git log --format=%s top/master >actual &&
	cat <<EOF >expect &&
9
4
3
EOF
	test_cmp expect actual
	)
'

test_expect_success 'push from shallow repo via http' '
	mv "$HTTPD_DOCUMENT_ROOT_PATH/repo.git" shallow-upstream.git &&
	git clone --bare --no-local full "$HTTPD_DOCUMENT_ROOT_PATH/repo.git" &&
	(
	cd "$HTTPD_DOCUMENT_ROOT_PATH/repo.git" &&
	git config http.receivepack true
	) &&
	commit 10 &&
	git push $HTTPD_URL/smart/repo.git +master:refs/remotes/top/master &&
	(
	cd "$HTTPD_DOCUMENT_ROOT_PATH/repo.git" &&
	git fsck &&
	git log --format=%s top/master >actual &&
	cat <<EOF >expect &&
10
1
4
3
2
1
EOF
	test_cmp expect actual
	)
'

stop_httpd
test_done
