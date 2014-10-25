# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..8\n";

my $__FILE__ = __FILE__;

my $tno = 1;

# qr//i
if ("����������" !~ /a/i) {
    print qq{ok - $tno "����������" !~ /a/i $^X $__FILE__\n}
}
else {
    print qq{not ok - $tno "����������" !~ /a/i $^X $__FILE__\n}
}
$tno++;

# qr//m
if ("��������\n�������ĥƥ�" =~ qr/^��/m) {
    print qq{ok - $tno "��������\\n�������ĥƥ�" =~ qr/^��/m $^X $__FILE__\n};
}
else {
    print qq{not ok - $tno "��������\\n�������ĥƥ�" =~ qr/^��/m $^X $__FILE__\n};
}
$tno++;

# qr//o
@re = ("��","��");
for $i (1 .. 2) {
    $re = shift @re;
    if ("������" =~ qr/\Q$re\E/o) {
        print qq{ok - $tno "������" =~ qr/\\Q\$re\\E/o $^X $__FILE__\n};
    }
    else {
        if ($] =~ /^5\.006/) {
            print qq{ok - $tno # SKIP "������" =~ qr/\\Q\$re\\E/o $^X $__FILE__\n};
        }
        else {
            print qq{not ok - $tno "������" =~ qr/\\Q\$re\\E/o $^X $__FILE__\n};
        }
    }
    $tno++;
}

@re = ("��","��");
for $i (1 .. 2) {
    $re = shift @re;
    if ("������" !~ qr/\Q$re\E/o) {
        print qq{ok - $tno "������" !~ qr/\\Q\$re\\E/o $^X $__FILE__\n};
    }
    else {
        if ($] =~ /^5\.006/) {
            print qq{ok - $tno # SKIP "������" !~ qr/\\Q\$re\\E/o $^X $__FILE__\n};
        }
        else {
            print qq{not ok - $tno "������" !~ qr/\\Q\$re\\E/o $^X $__FILE__\n};
        }
    }
    $tno++;
}

# qr//s
if ("��\n��" =~ qr/��.��/s) {
    print qq{ok - $tno "��\\n��" =~ qr/��.��/s $^X $__FILE__\n};
}
else {
    print qq{not ok - $tno "��\\n��" =~ qr/��.��/s $^X $__FILE__\n};
}
$tno++;

# qr//x
if ("������" =~ qr/  ��  /x) {
    print qq{ok - $tno "������" =~ qr/  ��  /x $^X $__FILE__\n};
}
else {
    print qq{not ok - $tno "������" =~ qr/  ��  /x $^X $__FILE__\n};
}
$tno++;

__END__
