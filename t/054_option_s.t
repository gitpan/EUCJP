# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..1\n";

my $__FILE__ = __FILE__;

# s///x ��
$a = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
if ($a =~ s/ J K L /������/x) {
    if ($a eq "ABCDEFGHI������MNOPQRSTUVWXYZ") {
        print qq{ok - 1 \$a =~ s/ J K L /������/x ($a) $^X $__FILE__\n};
    }
    else {
        print qq{not ok - 1 \$a =~ s/ J K L /������/x ($a) $^X $__FILE__\n};
    }
}
else {
    print qq{not ok - 1 \$a =~ s/ J K L /������/x ($a) $^X $__FILE__\n};
}

__END__
