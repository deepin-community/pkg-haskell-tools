
use warnings;
use strict;

sub usage ($) {
	my $usage = shift;
	if ($ARGV[0] and ($ARGV[0] eq "--help" or $ARGV[0] eq "-h")) {
		print $usage;
		exit(1);
	}
}

sub manpage ($) {
	my $usage = shift;
	if ($ARGV[0] and ($ARGV[0] eq "--manpage")) {
		print $usage;
		exit(1);
	}
}

return 1;
