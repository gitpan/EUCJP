# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..10\n";

my $__FILE__ = __FILE__;

my $tno = 1;

# m//i
if ("����������" !~ /a/i) {
    print qq{ok - $tno "����������" !~ /a/i $^X $__FILE__\n}
}
else {
    print qq{not ok - $tno "����������" !~ /a/i $^X $__FILE__\n}
}
$tno++;

# m//m
if ("��������\n�������ĥƥ�" =~ m/^��/m) {
    print qq{ok - $tno "��������\\n�������ĥƥ�" =~ m/^��/m $^X $__FILE__\n};
}
else {
    print qq{not ok - $tno "��������\\n�������ĥƥ�" =~ m/^��/m $^X $__FILE__\n};
}
$tno++;

if ("����������\n�����ĥƥ�" =~ m/��$/m) {
    print qq{ok - $tno "����������\\n�����ĥƥ�" =~ m/��\$/m $^X $__FILE__\n};
}
else {
    print qq{not ok - $tno "����������\\n�����ĥƥ�" =~ m/��\$/m $^X $__FILE__\n};
}
$tno++;

if ("��������\n��\n�����ĥƥ�" =~ m/^��$/m) {
    print qq{ok - $tno "��������\\n��\\n�����ĥƥ�" =~ m/^��\$/m $^X $__FILE__\n};
}
else {
    print qq{not ok - $tno "��������\\n��\\n�����ĥƥ�" =~ m/^��\$/m $^X $__FILE__\n};
}
$tno++;

# m//o
@re = ("��","��");
for $i (1 .. 2) {
    $re = shift @re;
    if ("������" =~ m/\Q$re\E/o) {
        print qq{ok - $tno "������" =~ m/\\Q\$re\\E/o $^X $__FILE__\n};
    }
    else {
        if ($] =~ /^5\.006/) {
            print qq{ok - $tno # SKIP "������" =~ m/\\Q\$re\\E/o $^X $__FILE__\n};
        }
        else {
            print qq{not ok - $tno "������" =~ m/\\Q\$re\\E/o $^X $__FILE__\n};
        }
    }
    $tno++;
}

@re = ("��","��");
for $i (1 .. 2) {
    $re = shift @re;
    if ("������" !~ m/\Q$re\E/o) {
        print qq{ok - $tno "������" !~ m/\\Q\$re\\E/o $^X $__FILE__\n};
    }
    else {
        if ($] =~ /^5\.006/) {
            print qq{ok - $tno # SKIP "������" !~ m/\\Q\$re\\E/o $^X $__FILE__\n};
        }
        else {
            print qq{not ok - $tno "������" !~ m/\\Q\$re\\E/o $^X $__FILE__\n};
        }
    }
    $tno++;
}

# m//s
if ("��\n��" =~ m/��.��/s) {
    print qq{ok - $tno "��\\n��" =~ m/��.��/s $^X $__FILE__\n};
}
else {
    print qq{not ok - $tno "��\\n��" =~ m/��.��/s $^X $__FILE__\n};
}
$tno++;

# m//x
if ("������" =~ m/  ��  /x) {
    print qq{ok - $tno "������" =~ m/  ��  /x $^X $__FILE__\n};
}
else {
    print qq{not ok - $tno "������" =~ m/  ��  /x $^X $__FILE__\n};
}
$tno++;

__END__
