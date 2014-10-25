# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..3\n";

my $__FILE__ = __FILE__;

# tr///c
$a = "�������������������������ĥƥ�";
if ($a =~ tr/����������/����������/c) {
    print qq{ok - 1 \$a =~ tr/����������/����������/c ($a) $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 \$a =~ tr/����������/����������/c ($a) $^X $__FILE__\n};
}

# tr///d
$a = "�������������������������ĥƥ�";
if ($a =~ tr/����������//d) {
    print qq{ok - 2 \$a =~ tr/����������//d ($a) $^X $__FILE__\n};
}
else {
    print qq{not ok - 2 \$a =~ tr/����������//d ($a) $^X $__FILE__\n};
}

# tr///s
$a = "�������������������������ĥƥ�";
if ($a =~ tr/����������/��/s) {
    print qq{ok - 3 \$a =~ tr/����������/��/s ($a) $^X $__FILE__\n};
}
else {
    print qq{not ok - 3 \$a =~ tr/����������/��/s ($a) $^X $__FILE__\n};
}

__END__
