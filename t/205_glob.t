# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

my $__FILE__ = __FILE__;

use EUCJP;
print "1..1\n";

my $__FILE__ = __FILE__;

if ($^O !~ /\A (?: MSWin32 | NetWare | symbian | dos ) \z/oxms) {
    print "ok - 1 # SKIP $^X $__FILE__\n";
    exit;
}

open(FILE,'>F��ǽ') || die "Can't open file: F��ǽ\n";
print FILE "1\n";
close(FILE);
mkdir('D��ǽ', 0777);
open(FILE,'>D��ǽ/a.txt') || die "Can't open file: D��ǽ/a.txt\n";
print FILE "1\n";
close(FILE);
open(FILE,'>D��ǽ/b.txt') || die "Can't open file: D��ǽ/b.txt\n";
print FILE "1\n";
close(FILE);
open(FILE,'>D��ǽ/c.txt') || die "Can't open file: D��ǽ/c.txt\n";
print FILE "1\n";
close(FILE);
open(FILE,'>D��ǽ/F��ǽ') || die "Can't open file: D��ǽ/F��ǽ\n";
print FILE "1\n";
close(FILE);
mkdir('D��ǽ/D��ǽ', 0777);

my @file = glob('./*');
if (grep(/F��ǽ/, @file)) {
    if (grep(/D��ǽ/, @file)) {
        print "ok - 1 $^X $__FILE__\n";
    }
    else {
        print "not ok - 1 $^X $__FILE__\n";
    }
}
else {
    print "not ok - 1 $^X $__FILE__\n";
}

unlink('F��ǽ');
rmdir('D��ǽ/D��ǽ');
unlink('D��ǽ/a.txt');
unlink('D��ǽ/b.txt');
unlink('D��ǽ/c.txt');
unlink('D��ǽ/F��ǽ');
rmdir('D��ǽ');

__END__

Perl���/Windows�ǤΥե�����ѥ�
http://digit.que.ne.jp/work/wiki.cgi?Perl%E3%83%A1%E3%83%A2%2FWindows%E3%81%A7%E3%81%AE%E3%83%95%E3%82%A1%E3%82%A4%E3%83%AB%E3%83%91%E3%82%B9

�ե������Ϣ���ޥ�ɤ�ư���ǧ
�ֵ�ǽ�פ�����ǥ��쥯�ȥ�ǡ�glob('./*')�򤷤Ƥ⡢�ֵ�ǽ�פ��֤��ͤ˴ޤޤ�ʤ� 

