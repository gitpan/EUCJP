# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..1\n";

my $__FILE__ = __FILE__;

$a = "������";
if ($a =~ s/(����?)//) {
    if ($1 eq "����") {
        print qq{ok - 1 "������" =~ s/(����?)// \$1=($1) $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 1 "������" =~ s/(����?)// \$1=($1) $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 1 "������" =~ s/(����?)// \$1=($1) $^X $__FILE__\n};
}

__END__
