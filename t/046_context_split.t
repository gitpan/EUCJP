# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..3\n";

my $__FILE__ = __FILE__;

$text = '�ɣϡ��ӣ٣ӡ��������������������ݣ����ݣ������ݣ�ݣ�衧����������';

local $^W = 0;

# 7.7 split�黻��(�ꥹ�ȥ���ƥ�����)
@_ = split(/��/, $text);
if (join('', map {"($_)"} @_) eq "(�ɣϡ��ӣ٣�)(������������)(�����ݣ����ݣ���)(�ݣ�ݣ��)(����������)") {
    print qq{ok - 1 \@_ = split(/��/, \$text); $^X $__FILE__\n};
}
else {
    print qq{not ok - 1 \@_ = split(/��/, \$text); $^X $__FILE__\n};
}

# 7.7 split�黻��(�����饳��ƥ�����)
my $a = split(/��/, $text);
if (join('', map {"($_)"} @_) eq "(�ɣϡ��ӣ٣�)(������������)(�����ݣ����ݣ���)(�ݣ�ݣ��)(����������)") {
    print qq{ok - 2 \$a = split(/��/, \$text); $^X $__FILE__\n};
}
else {
    print qq{not ok - 2 \$a = split(/��/, \$text); $^X $__FILE__\n};
}

# 7.7 split�黻��(void����ƥ�����)
split(/��/, $text);
if (join('', map {"($_)"} @_) eq "(�ɣϡ��ӣ٣�)(������������)(�����ݣ����ݣ���)(�ݣ�ݣ��)(����������)") {
    print qq{ok - 3 (void) split(/��/, \$text); $^X $__FILE__\n};
}
else {
    print qq{not ok - 3 (void) split(/��/, \$text); $^X $__FILE__\n};
}

__END__
