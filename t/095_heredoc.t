# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..6\n";

my $__FILE__ = __FILE__;

my $aaa = 'AAA';
my $bbb = 'BBB';

if (<<'END' eq "��\$aaa��\n" and <<'END' eq "ɽ\$bbbɽ\n") {
��$aaa��
END
ɽ$bbbɽ
END
    print qq{ok - 1 <<'END' and <<'END' $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 <<'END' and <<'END' $^X $__FILE__\n};
}

if (<<\END eq "��\$aaa��\n" and <<\END eq "ɽ\$bbbɽ\n") {
��$aaa��
END
ɽ$bbbɽ
END
    print qq{ok - 2 <<\\END and <<\\END $^X $__FILE__\n};
}
else {
    print qq{not ok - 2 <<\\END and <<\\END $^X $__FILE__\n};
}

if (<<END eq "��\L$aaa��\n" and <<END eq "ɽ\L$bbb\Eɽ\n") {
��\L$aaa��
END
ɽ\L$bbb\Eɽ
END
    print qq{ok - 3 <<END and <<END $^X $__FILE__\n};
}
else {
    print qq{not ok - 3 <<END and <<END $^X $__FILE__\n};
}

if (<<"END" eq "��\L$aaa��\n" and <<"END" eq "ɽ\L$bbb\Eɽ\n") {
��\L$aaa��
END
ɽ\L$bbb\Eɽ
END
    print qq{ok - 4 <<"END" and <<"END" $^X $__FILE__\n};
}
else {
    print qq{not ok - 4 <<"END" and <<"END" $^X $__FILE__\n};
}

if (<<'END' eq "��\$aaa��\n" and <<END eq "��$aaa��\n" and <<'END' eq "ɽ\$bbbɽ\n") {
��$aaa��
END
��$aaa��
END
ɽ$bbbɽ
END
    print qq{ok - 5 <<'END' and <<"END" and <<'END' $^X $__FILE__\n};
}
else {
    print qq{not ok - 5 <<'END' and <<"END" and <<'END' $^X $__FILE__\n};
}

if (<<END eq "��\L$aaa��\n" and <<'END' eq "ɽ\$bbbɽ\n" and <<END eq "ɽ\L$bbb\Eɽ\n") {
��\L$aaa��
END
ɽ$bbbɽ
END
ɽ\L$bbb\Eɽ
END
    print qq{ok - 6 <<END and <<'END' and <<END $^X $__FILE__\n};
}
else {
    print qq{not ok - 6 <<END and <<'END' and <<END $^X $__FILE__\n};
}

__END__
