# ** and/or

foo arg; and bar arg; or baz arg

foo arg
and bar arg
or baz arg

# ** Backgrounding

foo & bar

# ** For loops

for a in 1 2 3
    echo $a
end
foo; for a in 1 2 3; echo $a; end; foo

function name --description "Blah blah"
    for a in (seq 1 2 3)
        echo $a
    end

    if test $foo -lt $bar
        bar arg

        set steps (echo 3)

        for a in (seq 1 $steps)
            echo $a
            set foo bar
            fizz buzz
        end
    end
end

# ** Functions

function name -w arg --description "Foo bar" -v arg -e arg -a arg --on-event arg
    foo arg
end

# ** if

if foo arg
    bar arg
end

if false; echo 2; else if true; echo 2; end

# ** Indentation of strings containing keywords

# From https://github.com/wwwjfy/emacs-fish/issues/24#issuecomment-297616862

set -l msg 'This should not happen. Have you changed the cd function?'
printf (_ "$msg\n")

# ** Negation

if not foo arg
    bar arg
end

# ** Pipes

foo arg argh | bar arg argh | baz arg argh

foo \
| bar arg argh | bar arg argh | baz arg argh

function name
    foo arg argh | bar arg argh

    foo arg argh \
    | bar arg argh | baz arg argh | buzz arg argh
end

# ** Process substitution

foo arg argh (bar arg argh)

# ** Variables

set foo
set -l bar
set -el baz

echo $foo

echo $foo$bar

echo {$foo}{$bar}

# ** While loops

while foo arg argh
    bar arg argh
end
