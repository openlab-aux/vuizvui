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
        if (bar == "nope")
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

cat > "test1.cs" <<EOF
class test1 {
    public static void Main() {
        a.replaceMe("xxx");
        b.replacement("nope");
    }
}
EOF

cat > "test2.cs" <<EOF
class test1 {
    public static void Main() {
        b.wrongFileStreamUse();
    }
}
EOF

mkdir subdir
mcs a.cs -target:library -out:subdir/a.dll
mcs b.cs -target:library -out:subdir/b.dll

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

echo foo > write_test.txt

test "$(mono subdir/test2.exe)" = "can write"

"$out/bin/monogame-patcher" fix-filestreams -i subdir/b.dll b

test "$(mono subdir/test2.exe)" = "can not write"

"$out/bin/monogame-patcher" --help 2>&1 | grep -q fix-filestreams
