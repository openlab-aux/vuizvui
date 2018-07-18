set -x
cd "$(mktemp -d)"

cat > "a.cs" <<EOF
using System;

public class a {
    public static string replaceMe(string foo) {
        Console.WriteLine("foo called: " + foo);
        return "foo";
    }
}
EOF

cat > "b.cs" <<EOF
using System;
using System.IO;

public class b {
    public static string replacement(string bar) {
        if (bar == "nope" || bar == "yikes")
            return "nope";
        Console.WriteLine("bar called: " + bar);
        return "bar";
    }

    public static void wrongFileStreamUse() {
        var fs = new FileStream("write_test.txt", FileMode.Open);
        if (fs.CanWrite)
            Console.WriteLine("can write");
        else
            Console.WriteLine("can not write");
    }
}
EOF

cat > "c.cs" <<EOF
using System;

public class c {
    public static string anotherReplacement(string foobar) {
        if (foobar == "nope")
            return "nope";
        Console.WriteLine("foobar called: " + foobar);
        return "foobar";
    }
}
EOF

cat > "test1.cs" <<EOF
class test1 {
    public static void unrelated() {
        b.replacement("yikes");
    }

    public static void Main() {
        a.replaceMe("xxx");
        b.replacement("nope");
        test1.unrelated();
    }
}
EOF

cat > "test2.cs" <<EOF
class test2 {
    public static void Main() {
        b.wrongFileStreamUse();
    }
}
EOF

mkdir subdir
mcs a.cs -target:library -out:subdir/a.dll
mcs b.cs -target:library -out:subdir/b.dll
mcs c.cs -target:library -out:subdir/c.dll

mcs test1.cs -r:subdir/a -r:subdir/b -out:subdir/test1.exe
mcs test2.cs -r:subdir/a -r:subdir/b -out:subdir/test2.exe

! "$out/bin/monogame-patcher" replace-call -i subdir/test1.exe \
    "System.String a::replaceMe(System.String)" \
    "System.String b::notfound(System.String)" \
    test1 2> /dev/null

"$out/bin/monogame-patcher" replace-call -i subdir/test1.exe \
    "System.String a::replaceMe(System.String)" \
    "System.String b::replacement(System.String)" \
    test1

test "$(mono subdir/test1.exe)" = "bar called: xxx"

"$out/bin/monogame-patcher" replace-call -i subdir/test1.exe -a subdir/c.dll \
    "System.String b::replacement(System.String)" \
    "System.String c::anotherReplacement(System.String)" \
    test1::Main

test "$(mono subdir/test1.exe)" = "foobar called: xxx"

echo foo > write_test.txt

test "$(mono subdir/test2.exe)" = "can write"

"$out/bin/monogame-patcher" fix-filestreams -i subdir/b.dll b

test "$(mono subdir/test2.exe)" = "can not write"

set +e
"$out/bin/monogame-patcher" --help &> /dev/null
ret=$?
set -e
if [ $ret -eq 0 ]; then
    echo "Running with --help should give exit status != 0 but was $ret" >&2
    exit 1
fi

"$out/bin/monogame-patcher" --help 2>&1 | grep -q fix-filestreams
