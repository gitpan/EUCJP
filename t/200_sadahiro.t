# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..2\n";

my $__FILE__ = __FILE__;

# ��������̻���Ҥ�ޤ�ѥ����� (�㤨�� C<.??>��C<\d*?>) �ϡ�
# ��ʸ����ȥޥå����뤳�Ȥ��Ǥ��ޤ�����C<jsplit()> �Υѥ�����Ȥ����Ѥ�����硢
# �Ȥ߹��ߤ� C<split()> ����ͽ�ۤ����ư��Ȱۤʤ뤳�Ȥ�����ޤ���

if (join('', map {"($_)"} split(/.??/, '������')) eq '(��)(��)(��)') {
    print "ok - 1 $^X $__FILE__ (join('', map {qq{(\$_)}} split(/.??/, '������')) eq '(��)(��)(��)')\n";
}
else {
    print "not ok - 1 $^X $__FILE__ (join('', map {qq{(\$_)}} split(/.??/, '������')) eq '(��)(��)(��)')\n";
}

if (join('', map {"($_)"} split(/\d*?/, '������')) eq '(��)(��)(��)') {
    print "ok - 2 $^X $__FILE__ (join('', map {qq{(\$_)}} split(/\\d*?/, '������')) eq '(��)(��)(��)')\n";
}
else {
    print "not ok - 2 $^X $__FILE__ (join('', map {qq{(\$_)}} split(/\\d*?/, '������')) eq '(��)(��)(��)')\n";
}

__END__

http://search.cpan.org/dist/EUC-JP-Regexp/
