# This file is encoded in EUC-JP.
die "This file is not encoded in EUC-JP.\n" if q{あ} ne "\xa4\xa2";

use EUCJP;
print "1..1\n";

my $__FILE__ = __FILE__;

if ('あいいいいう' =~ /(.{3,4})/) {
    if ("$1" eq "あいいい") {
        print "ok - 1 $^X $__FILE__ ('あいいいいう' =~ /.{3,4}/).\n";
    }
    else {
        print "not ok - 1 $^X $__FILE__ ('あいいいいう' =~ /.{3,4}/).\n";
    }
}
else {
    print "not ok - 1 $^X $__FILE__ ('あいいいいう' =~ /.{3,4}/).\n";
}

__END__
