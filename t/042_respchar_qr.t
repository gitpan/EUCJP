# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..20\n";

my $__FILE__ = __FILE__;

if ("������" =~ qr/^��/) {
    print qq{ok - 1 "������" =~ qr/^��/ $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 "������" =~ qr/^��/ $^X $__FILE__\n};
}

if ("������" !~ qr/^��/) {
    print qq{ok - 2 "������" !~ qr/^��/ $^X $__FILE__\n};
}
else {
    print qq{not ok - 2 "������" !~ qr/^��/ $^X $__FILE__\n};
}

if ("������" =~ qr/��$/) {
    print qq{ok - 3 "������" =~ qr/��\$/ $^X $__FILE__\n};
}
else {
    print qq{not ok - 3 "������" =~ qr/��\$/ $^X $__FILE__\n};
}

if ("������" !~ qr/��$/) {
    print qq{ok - 4 "������" !~ qr/��\$/ $^X $__FILE__\n};
}
else {
    print qq{not ok - 4 "������" !~ qr/��\$/ $^X $__FILE__\n};
}

if ("������" =~ qr/(��([������])��)/) {
    if ($1 eq "������") {
        if ($2 eq "��") {
            print qq{ok - 5 "������" =~ qr/(��([������])��)/ \$1=($1), \$2=($2) $^X $__FILE__\n};
        }
        else {
            print qq{not ok - 5 "������" =~ qr/(��([������])��)/ \$1=($1), \$2=($2) $^X $__FILE__\n};
        }
    }
    else {
        print qq{not ok - 5 "������" =~ qr/(��([������])��)/ \$1=($1), \$2=($2) $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 5 "������" =~ qr/(��([������])��)/ \$1=($1), \$2=($2) $^X $__FILE__\n};
}

if ("������" !~ qr/(��([������])��)/) {
    print qq{ok - 6 "������" !~ qr/(��([������])��)/ \$1=($1), \$2=($2) $^X $__FILE__\n};
}
else {
    print qq{not ok - 6 "������" !~ qr/(��([������])��)/ \$1=($1), \$2=($2) $^X $__FILE__\n};
}

if ("������" =~ qr/(����|����)/) {
    if ($1 eq "����") {
        print qq{ok - 7 "������" =~ qr/(����|����)/ \$1=($1) $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 7 "������" =~ qr/(����|����)/ \$1=($1) $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 7 "������" =~ qr/(����|����)/ \$1=($1) $^X $__FILE__\n};
}

if ("������" !~ qr/(����|����)/) {
    print qq{ok - 8 "������" !~ qr/(����|����)/ \$1=($1) $^X $__FILE__\n};
}
else {
    print qq{not ok - 8 "������" !~ qr/(����|����)/ \$1=($1) $^X $__FILE__\n};
}

if ("������" =~ qr/(����?)/) {
    if ($1 eq "����") {
        print qq{ok - 9 "������" =~ qr/(����?)/ \$1=($1) $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 9 "������" =~ qr/(����?)/ \$1=($1) $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 9 "������" =~ qr/(����?)/ \$1=($1) $^X $__FILE__\n};
}

if ("������" !~ qr/(����?)/) {
    print qq{ok - 10 "������" !~ qr/(����?)/ \$1=($1) $^X $__FILE__\n};
}
else {
    print qq{not ok - 10 "������" !~ qr/(����?)/ \$1=($1) $^X $__FILE__\n};
}

if ("������" =~ qr/(������?)/) {
    if ($1 eq "������") {
        print qq{ok - 11 "������" =~ qr/(������?)/ \$1=($1) $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 11 "������" =~ qr/(������?)/ \$1=($1) $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 11 "������" =~ qr/(������?)/ \$1=($1) $^X $__FILE__\n};
}

if ("������" !~ qr/(������?)/) {
    print qq{ok - 12 "������" !~ qr/(������?)/ \$1=($1) $^X $__FILE__\n};
}
else {
    print qq{not ok - 12 "������" !~ qr/(������?)/ \$1=($1) $^X $__FILE__\n};
}

if ("������" =~ qr/(����+)/) {
    if ($1 eq "������") {
        print qq{ok - 13 "������" =~ qr/(����+)/ \$1=($1) $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 13 "������" =~ qr/(����+)/ \$1=($1) $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 13 "������" =~ qr/(����+)/ \$1=($1) $^X $__FILE__\n};
}

if ("������" !~ qr/(����+)/) {
    print qq{ok - 14 "������" !~ qr/(����+)/ \$1=($1) $^X $__FILE__\n};
}
else {
    print qq{not ok - 14 "������" !~ qr/(����+)/ \$1=($1) $^X $__FILE__\n};
}

if ("������" =~ qr/(����*)/) {
    if ($1 eq "������") {
        print qq{ok - 15 "������" =~ qr/(����*)/ \$1=($1) $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 15 "������" =~ qr/(����*)/ \$1=($1) $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 15 "������" =~ qr/(����*)/ \$1=($1) $^X $__FILE__\n};
}

if ("������" !~ qr/(����*)/) {
    print qq{ok - 16 "������" !~ qr/(����*)/ \$1=($1) $^X $__FILE__\n};
}
else {
    print qq{not ok - 16 "������" !~ qr/(����*)/ \$1=($1) $^X $__FILE__\n};
}

if ("������" =~ qr/(��.)/) {
    if ($1 eq "����") {
        print qq{ok - 17 "������" =~ qr/(��.)/ \$1=($1) $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 17 "������" =~ qr/(��.)/ \$1=($1) $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 17 "������" =~ qr/(��.)/ \$1=($1) $^X $__FILE__\n};
}

if ("������" !~ qr/(��.)/) {
    print qq{ok - 18 "������" !~ qr/(��.)/ \$1=($1) $^X $__FILE__\n};
}
else {
    print qq{not ok - 18 "������" !~ qr/(��.)/ \$1=($1) $^X $__FILE__\n};
}

if ("������" =~ qr/(��.{2})/) {
    if ($1 eq "������") {
        print qq{ok - 19 "������" =~ qr/(��.{2})/ \$1=($1) $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 19 "������" =~ qr/(��.{2})/ \$1=($1) $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 19 "������" =~ qr/(��.{2})/ \$1=($1) $^X $__FILE__\n};
}

if ("������" !~ qr/(��.{2})/) {
    print qq{ok - 20 "������" !~ qr/(��.{2})/ \$1=($1) $^X $__FILE__\n};
}
else {
    print qq{not ok - 20 "������" !~ qr/(��.{2})/ \$1=($1) $^X $__FILE__\n};
}

__END__
