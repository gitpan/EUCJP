# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use strict;
# use warnings;

use EUCJP;
print "1..1\n";

my $__FILE__ = __FILE__;

if ($^O !~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    print "ok - 1 # SKIP $^X $__FILE__\n";
    exit;
}

mkdir('hoge', 0777);
open(FILE,'>hoge/�ƥ��ȥ�����.txt') || die "Can't open file: hoge/�ƥ��ȥ�����.txt\n";
print FILE "1\n";
close(FILE);

my($fileName) = glob("./hoge/*");
# if ($fileName =~ /\Q������\E/) {
if ($fileName =~ /������/) {
    print "ok - 1 $^X $__FILE__\n";
}
else {
    print "not ok - 1 $^X $__FILE__\n";
}

unlink('hoge/�ƥ��ȥ�����.txt');
rmdir('hoge');

__END__

���Ȥ��С�./hoge�۲��ˡإƥ��ȥ�����.txt�٤Ȥ����ե����뤬���ä��Ȥ��ޤ���
��[�٤����̤�ʸ���������뤿��ˡ��إ������٤�\Q��\E�ǰϤ�Ǥߤޤ���

�����Σ��������ɤ�shiftjis��������shiftjis��ɸ�������Ϥ�shiftjis

�¹Է��
C:\test>perl $0
Unmatch
./hoge/�ƥ��ȥ�����.txt

���������嵭�Ǥϥޥå����ޤ���
�ʤ����Ȥ����ȡ� /\Q������\E/�ϡ�\Q�����ˡإ�������ʸ����ɾ�������Τǡ�
����Ū�ˡ�[�٤򥨥������פ����˲᤮�ޤ���

8/2(��) ��[Perl�Ρ���] ���ե�JIS�����Υե�����̾�˥ޥå����Ƥߤ�
http://d.hatena.ne.jp/chaichanPaPa/20080802/1217660826
