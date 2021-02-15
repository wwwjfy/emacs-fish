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

switch $a
    case 1
        switch $a
            case 1
                if true
                    echo 1
                end
            case 2
                echo 2
        end
    case 2
        echo 2
end

# ** Indentation following escaped newlines

set -l variable \
    "Pretend this is a very long string."

# ***  Multiple consecutive lines with escaped newlines.

# See <https://github.com/wwwjfy/emacs-fish/issues/37>.

function print_status
    foo \
        bar \
        baz \
        quux \
        while true
            foo
        end
end

find . \
    -type f \
    -name '*.c'

# *** Alternating continued and non-continued lines

set -l var \
    (math "1 + 1")
set -l another_var \
    (math "2 + 2")

# *** Multiple comment lines with continued lines

function print_status
    foo \
        bar \
        baz \
        quux

    # Comment 1.
    # Comment 2.

    foo
    bar

    # Comment 3.
    foo

    # Comment 4.
    bar

    # Comment 5.
    foo
    # Comment 6.
    bar

    # Comment 7
    foo \
        bar
    # Comment 8.
    # Comment 9.
    foo \
        bar \
        baz
end
