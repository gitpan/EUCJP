# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..6\n";

my $__FILE__ = __FILE__;

# ��^�פȤ�������ɽ����Ȥä����

$_ = "������\n�££�\n�ãã�";
@_ = split(m/^/, $_);
if (join('', map {"($_)"} @_) eq "(������\n)(�££�\n)(�ãã�)") {
    print qq{ok - 1 \@_ = split(m/^/, \$\_) $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 \@_ = split(m/^/, \$\_) $^X $__FILE__\n};
}

$_ = "������1\n2�££�1\n2�ãã�";
@_ = split(m/^2/, $_);
if (join('', map {"($_)"} @_) eq "(������1\n)(�££�1\n)(�ãã�)") {
    print qq{ok - 2 \@_ = split(m/^2/, \$\_) $^X $__FILE__\n};
}
else {
    print qq{not ok - 2 \@_ = split(m/^2/, \$\_) $^X $__FILE__\n};
    print "<<", join('', map {"($_)"} @_), ">>\n";
}

$_ = "������1\n2�££�1\n2�ãã�";
@_ = split(m/^2/m, $_);
if (join('', map {"($_)"} @_) eq "(������1\n)(�££�1\n)(�ãã�)") {
    print qq{ok - 3 \@_ = split(m/^2/m, \$\_) $^X $__FILE__\n};
}
else {
    print qq{not ok - 3 \@_ = split(m/^2/m, \$\_) $^X $__FILE__\n};
    print "<<", join('', map {"($_)"} @_), ">>\n";
}

$_ = "������1\n2�££�1\n2�ãã�";
@_ = split(m/1^/, $_);
if (join('', map {"($_)"} @_) eq "(������1\n2�££�1\n2�ãã�)") {
    print qq{ok - 4 \@_ = split(m/1^/, \$\_) $^X $__FILE__\n};
}
else {
    print qq{not ok - 4 \@_ = split(m/1^/, \$\_) $^X $__FILE__\n};
}

$_ = "������1\n2�££�1\n2�ãã�";
@_ = split(m/1\n^2/, $_);
if (join('', map {"($_)"} @_) eq "(������)(�££�)(�ãã�)") {
    print qq{ok - 5 \@_ = split(m/1\\n^2/, \$\_) $^X $__FILE__\n};
}
else {
    print qq{not ok - 5 \@_ = split(m/1\\n^2/, \$\_) $^X $__FILE__\n};
    print "<<", join('', map {"($_)"} @_), ">>\n";
}

$_ = "������1\n2�££�1\n2�ãã�";
@_ = split(m/1\n^2/m, $_);
if (join('', map {"($_)"} @_) eq "(������)(�££�)(�ãã�)") {
    print qq{ok - 6 \@_ = split(m/1\\n^2/m, \$\_) $^X $__FILE__\n};
}
else {
    print qq{not ok - 6 \@_ = split(m/1\\n^2/m, \$\_) $^X $__FILE__\n};
    print "<<", join('', map {"($_)"} @_), ">>\n";
}

__END__
