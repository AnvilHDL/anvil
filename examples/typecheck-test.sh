# Type-checks should_pass/ and should_fail/. A should_fail test with a .err file
# must also reproduce that error output; UPDATE=1 rewrites the .err files.
set -e
cd "$(dirname "$0")"
dune build

should_pass_p=0
should_pass_t=0

should_fail_p=0
should_fail_t=0

for t in should_pass/*.anvil; do
    should_pass_t=$(expr $should_pass_t + 1)
    if dune exec --no-build anvil -- -just-check $@ $t > /dev/null; then
        should_pass_p=$(expr $should_pass_p + 1)
        echo "####### Test Passed: $t"
    else
        echo "####### Test Failed: $t"
    fi
done

actual=$(mktemp)
trap 'rm -f "$actual"' EXIT

for t in should_fail/*.anvil; do
    should_fail_t=$(expr $should_fail_t + 1)
    expected="${t%.anvil}.err"
    if dune exec --no-build anvil -- -just-check $@ $t > "$actual" 2>&1; then
        echo "####### Test Failed: $t (no error reported)"
    elif [ -n "$UPDATE" ]; then
        cp "$actual" "$expected"
        should_fail_p=$(expr $should_fail_p + 1)
        echo "####### Test Updated: $expected"
    elif [ ! -f "$expected" ] || diff -u "$expected" "$actual"; then
        should_fail_p=$(expr $should_fail_p + 1)
        echo "####### Test Passed: $t"
    else
        echo "####### Test Failed: $t (output differs from $expected)"
    fi
done

echo "######### should_pass = $should_pass_p/$should_pass_t"
echo "######### should_fail = $should_fail_p/$should_fail_t"
