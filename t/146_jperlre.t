# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{あ} ne "\xa4\xa2";

use EUCJP;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('ああう' =~ /(あ[い-え])/) {
    if ("$1" eq "あう") {
        print "ok - 1 $^X $__FILE__ ('ああう' =~ /あ[い-え]/).\n";
    }
    else {
        print "not ok - 1 $^X $__FILE__ ('ああう' =~ /あ[い-え]/).\n";
    }
}
else {
    print "not ok - 1 $^X $__FILE__ ('ああう' =~ /あ[い-え]/).\n";
}

__END__
