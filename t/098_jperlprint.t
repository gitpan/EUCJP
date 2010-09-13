# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{��} ne "\xa4\xa2";

use EUCJP;
print "1..1\n";

my $__FILE__ = __FILE__;

open(TMP,'>Kanji_xxx.tmp') || die "Can't open file: Kanji_xxx.tmp\n";
print TMP <<EOL;
������    align
abcde ��  align
��������  align
��        align
EOL
close(TMP);

$CAT = 'perl -e "print <>"';
if (`$CAT Kanji_xxx.tmp` eq <<EOL) {
������    align
abcde ��  align
��������  align
��        align
EOL
    print "ok - 1 $^X $__FILE__\n";
    unlink "Kanji_xxx.tmp";
}
else {
    print "not ok - 1 $^X $__FILE__\n";
}

__END__