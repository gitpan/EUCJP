# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..2\n";

my $__FILE__ = __FILE__;

# /g �ʤ��Υ����顼����ƥ�����
my $success = "������" =~ qr/��/;
if ($success) {
    print qq{ok - 1 "������" =~ qr/��/ $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 "������" =~ qr/��/ $^X $__FILE__\n};
}

# /g �ʤ��Υꥹ�ȥ���ƥ�����
if (my($c1,$c2,$c3,$c4) = "���������������ĥƥ�" =~ qr/^...(.)(.)(.)(.)...$/) {
    if ("($c1)($c2)($c3)($c4)" eq "(��)(��)(��)(��)") {
        print "ok - 2 ($c1)($c2)($c3)($c4) $^X $__FILE__\n";
    }
    else {
        print "not ok - 2 ($c1)($c2)($c3)($c4) $^X $__FILE__\n";
    }
}
else {
    print "not ok - 2 ($c1)($c2)($c3)($c4) $^X $__FILE__\n";
}

__END__
