###############################################################################
## ----------------------------------------------------------------------------
## MCE extension for sharing data supporting threads and processes.
##
###############################################################################

package MCE::Shared;

use strict;
use warnings;
use 5.010001;

no warnings qw( threads recursion uninitialized once );

our $VERSION = '1.817';

## no critic (BuiltinFunctions::ProhibitStringyEval)
## no critic (Subroutines::ProhibitSubroutinePrototypes)
## no critic (TestingAndDebugging::ProhibitNoStrict)

use Scalar::Util qw( blessed refaddr );
use MCE::Shared::Server;

our @CARP_NOT = qw(
   MCE::Shared::Array   MCE::Shared::Condvar  MCE::Shared::Handle
   MCE::Shared::Hash    MCE::Shared::Minidb   MCE::Shared::Ordhash
   MCE::Shared::Queue   MCE::Shared::Scalar   MCE::Shared::Sequence
   MCE::Shared::Server  MCE::Shared::Object   MCE::Shared::Cache
);

sub import {
   no strict 'refs'; no warnings 'redefine';
   *{ caller().'::mce_open' } = \&open;

   return;
}

###############################################################################
## ----------------------------------------------------------------------------
## Share function.
##
###############################################################################

my ($_count, %_lkup) = (0);

sub share {
   shift if (defined $_[0] && $_[0] eq 'MCE::Shared');

   my $_params = ref $_[0] eq 'HASH' && ref $_[1] ? shift : {};
   my ($_class, $_ra, $_item) = (blessed($_[0]), refaddr($_[0]));

   # safety for circular references to not loop endlessly
   return $_lkup{ $_ra } if defined $_ra && exists $_lkup{ $_ra };

   $_count++;

   # blessed object, \@array, \%hash, or \$scalar
   if ( $_class && Scalar::Util::reftype($_[0]) ne 'GLOB' ) {
      _incr_count($_[0]), return $_[0] if $_[0]->can('SHARED_ID');

      _croak("Running MCE::Queue via MCE::Shared is not supported.\n",
             "A shared queue is possible via MCE::Shared->queue().\n\n")
         if ($_class eq 'MCE::Queue');

      $_params->{'class'} = $_class;
      $_item = MCE::Shared::Server::_new($_params, $_[0]);
   }
   elsif ( ref $_[0] eq 'ARRAY' ) {
      if ( tied(@{ $_[0] }) && tied(@{ $_[0] })->can('SHARED_ID') ) {
         _incr_count(tied(@{ $_[0] })), return tied(@{ $_[0] });
      }
      $_item = $_lkup{ $_ra } = MCE::Shared->array($_params, @{ $_[0] });
      @{ $_[0] } = ();  tie @{ $_[0] }, 'MCE::Shared::Object', $_item;
   }
   elsif ( ref $_[0] eq 'HASH' ) {
      if ( tied(%{ $_[0] }) && tied(%{ $_[0] })->can('SHARED_ID') ) {
         _incr_count(tied(%{ $_[0] })), return tied(%{ $_[0] });
      }
      $_item = $_lkup{ $_ra } = MCE::Shared->hash($_params, %{ $_[0] });
      %{ $_[0] } = ();  tie %{ $_[0] }, 'MCE::Shared::Object', $_item;
   }
   elsif ( ref $_[0] eq 'SCALAR' && !ref ${ $_[0] } ) {
      if ( tied(${ $_[0] }) && tied(${ $_[0] })->can('SHARED_ID') ) {
         _incr_count(tied(${ $_[0] })), return tied(${ $_[0] });
      }
      $_item = $_lkup{ $_ra } = MCE::Shared->scalar($_params, ${ $_[0] });
      undef ${ $_[0] }; tie ${ $_[0] }, 'MCE::Shared::Object', $_item;
   }

   # synopsis
   elsif ( ref $_[0] eq 'REF' ) {
      _croak('A "REF" type is not supported');
   }
   else {
      if ( ref $_[0] eq 'GLOB' ) {
         _incr_count(tied(*{ $_[0] })), return $_[0] if (
            tied(*{ $_[0] }) && tied(*{ $_[0] })->can('SHARED_ID')
         );
      }
      _croak('Synopsis: blessed object, \@array, \%hash, or \$scalar');
   }

   %_lkup = () unless --$_count;

   $_item;
}

###############################################################################
## ----------------------------------------------------------------------------
## Public functions.
##
###############################################################################

sub start { MCE::Shared::Server::_start() }
sub stop  { MCE::Shared::Server::_stop()  }

sub init {
   shift if ( defined $_[0] && $_[0] eq 'MCE::Shared' );
   MCE::Shared::Object::_init(@_);
}

sub cache {
   shift if ( defined $_[0] && $_[0] eq 'MCE::Shared' );
   require MCE::Shared::Cache unless $INC{'MCE/Shared/Cache.pm'};
   &share({}, MCE::Shared::Cache->new(_shared => 1, @_));
}
sub condvar {
   shift if ( defined $_[0] && $_[0] eq 'MCE::Shared' );
   require MCE::Shared::Condvar unless $INC{'MCE/Shared/Condvar.pm'};
   &share({}, MCE::Shared::Condvar->new(@_));
}
sub minidb {
   shift if ( defined $_[0] && $_[0] eq 'MCE::Shared' );
   require MCE::Shared::Minidb unless $INC{'MCE/Shared/Minidb.pm'};
   &share({}, MCE::Shared::Minidb->new(@_));
}
sub queue {
   shift if ( defined $_[0] && $_[0] eq 'MCE::Shared' );
   require MCE::Shared::Queue unless $INC{'MCE/Shared/Queue.pm'};
   &share({}, MCE::Shared::Queue->new(@_));
}
sub scalar {
   shift if ( defined $_[0] && $_[0] eq 'MCE::Shared' );
   require MCE::Shared::Scalar unless $INC{'MCE/Shared/Scalar.pm'};
   &share({}, MCE::Shared::Scalar->new(@_));
}
sub sequence {
   shift if ( defined $_[0] && $_[0] eq 'MCE::Shared' );
   require MCE::Shared::Sequence unless $INC{'MCE/Shared/Sequence.pm'};
   &share({}, MCE::Shared::Sequence->new(@_));
}

## 'num_sequence' is an alias for 'sequence'
*num_sequence = \&sequence;

sub array {
   shift if ( defined $_[0] && $_[0] eq 'MCE::Shared' );
   require MCE::Shared::Array unless $INC{'MCE/Shared/Array.pm'};

   my $_params = ref $_[0] eq 'HASH' ? shift : {};
   my $_item   = &share($_params, MCE::Shared::Array->new());

   if ( scalar @_ ) {
      $_params->{_DEEPLY_} = 1;
      for ( my $i = 0; $i <= $#_; $i += 1 ) {
         &_share($_params, $_item, $_[$i]) if ref($_[$i]);
      }
      $_item->assign(@_);
   }

   $_item;
}

sub handle {
   shift if ( defined $_[0] && $_[0] eq 'MCE::Shared' );
   require MCE::Shared::Handle unless $INC{'MCE/Shared/Handle.pm'};

   my $_item = &share( MCE::Shared::Handle->TIEHANDLE([]) );
   my $_fh   = \do { no warnings 'once'; local *FH };

   tie *{ $_fh }, 'MCE::Shared::Object', $_item;
   if ( @_ ) { $_item->OPEN(@_) or _croak("open error: $!"); }

   $_fh;
}

sub hash {
   shift if ( defined $_[0] && $_[0] eq 'MCE::Shared' );
   require MCE::Shared::Hash unless $INC{'MCE/Shared/Hash.pm'};

   my $_params = ref $_[0] eq 'HASH' ? shift : {};
   my $_item   = &share($_params, MCE::Shared::Hash->new());

   &_deeply_share_h($_params, $_item, @_) if @_;

   $_item;
}

sub open (@) {
   shift if ( defined $_[0] && $_[0] eq 'MCE::Shared' );
   require MCE::Shared::Handle unless $INC{'MCE/Shared/Handle.pm'};

   my $_item;

   if ( ref $_[0] eq 'GLOB' && tied *{ $_[0] } &&
        ref tied(*{ $_[0] }) eq 'MCE::Shared::Object' ) {
      $_item = tied *{ $_[0] };
   }
   elsif ( @_ ) {
      if ( ref $_[0] eq 'GLOB' && tied *{ $_[0] } ) {
         close $_[0] if defined ( fileno $_[0] );
      }
      $_item = &share( MCE::Shared::Handle->TIEHANDLE([]) );
      $_[0]  = \do { no warnings 'once'; local *FH };
      tie *{ $_[0] }, 'MCE::Shared::Object', $_item;
   }

   shift; _croak("Not enough arguments for open") unless @_;

   if ( !defined wantarray ) {
      $_item->OPEN(@_) or _croak("open error: $!");
   } else {
      $_item->OPEN(@_);
   }
}

sub ordhash {
   shift if ( defined $_[0] && $_[0] eq 'MCE::Shared' );
   require MCE::Shared::Ordhash unless $INC{'MCE/Shared/Ordhash.pm'};

   my $_params = ref $_[0] eq 'HASH' ? shift : {};
   my $_item   = &share($_params, MCE::Shared::Ordhash->new());

   &_deeply_share_h($_params, $_item, @_) if @_;

   $_item;
}

###############################################################################
## ----------------------------------------------------------------------------
## PDL sharing -- construction takes place under the shared server-process.
##
###############################################################################

if ( $INC{'PDL.pm'} ) {
   local $@; eval q{

      sub pdl_byte     { push @_, 'byte';     goto &_pdl_share }
      sub pdl_short    { push @_, 'short';    goto &_pdl_share }
      sub pdl_ushort   { push @_, 'ushort';   goto &_pdl_share }
      sub pdl_long     { push @_, 'long';     goto &_pdl_share }
      sub pdl_longlong { push @_, 'longlong'; goto &_pdl_share }
      sub pdl_float    { push @_, 'float';    goto &_pdl_share }
      sub pdl_double   { push @_, 'double';   goto &_pdl_share }
      sub pdl_ones     { push @_, 'ones';     goto &_pdl_share }
      sub pdl_sequence { push @_, 'sequence'; goto &_pdl_share }
      sub pdl_zeroes   { push @_, 'zeroes';   goto &_pdl_share }
      sub pdl_indx     { push @_, 'indx';     goto &_pdl_share }
      sub pdl          { push @_, 'pdl';      goto &_pdl_share }

      sub _pdl_share {
         shift if ( defined $_[0] && $_[0] eq 'MCE::Shared' );
         MCE::Shared::Server::_new({ 'class' => ':construct_pdl:' }, [ @_ ]);
      }
   };
}

###############################################################################
## ----------------------------------------------------------------------------
## Private functions.
##
###############################################################################

sub TIEARRAY  { shift; MCE::Shared->array(@_) }
sub TIESCALAR { shift; MCE::Shared->scalar(@_) }

sub TIEHASH {
   shift;
   my ($_cache, $_ordered);

   if ( ref $_[0] eq 'HASH' ) {
      if ( $_[0]->{'ordered'} || $_[0]->{'ordhash'} ) {
         $_ordered = 1; shift();
      } elsif ( exists $_[0]->{'max_age'} || exists $_[0]->{'max_keys'} ) {
         $_cache   = 1;
      }
   }
   else {
      if ( @_ < 3 && ( $_[0] eq 'ordered' || $_[0] eq 'ordhash' ) ) {
         $_ordered = $_[1]; splice(@_, 0, 2);
      } elsif ( @_ < 5 && ( $_[0] eq 'max_age' || $_[0] eq 'max_keys' ) ) {
         $_cache   = 1;
      }
   }

   if ( $_cache ) {
      MCE::Shared->cache(@_);
   } elsif ( $_ordered ) {
      MCE::Shared->ordhash(@_);
   } else {
      MCE::Shared->hash(@_);
   }
}

sub TIEHANDLE {
   require MCE::Shared::Handle unless $INC{'MCE/Shared/Handle.pm'};
   my $_item = &share( MCE::Shared::Handle->TIEHANDLE([]) ); shift;
   if ( @_ ) { $_item->OPEN(@_) or _croak("open error: $!"); }
   $_item;
}

sub _croak {
   $_count = 0, %_lkup = ();
   if ( defined $MCE::VERSION ) {
      goto &MCE::_croak;
   } else {
      require MCE::Shared::Base unless $INC{'MCE/Shared/Base.pm'};
      goto &MCE::Shared::Base::_croak;
   }
}

sub _deeply_share_h {
   my ( $_params, $_item ) = ( shift, shift );
   $_params->{_DEEPLY_} = 1;
   for ( my $i = 1; $i <= $#_; $i += 2 ) {
      &_share($_params, $_item, $_[$i]) if ref($_[$i]);
   }
   $_item->assign(@_);
   return;
}

sub _incr_count {
   # increments counter for safety during destroy
   MCE::Shared::Server::_incr_count($_[0]->SHARED_ID);
}

sub _share {
   $_[2] = &share($_[0], $_[2]);
   MCE::Shared::Object::_req2(
      'M~DEE', $_[1]->SHARED_ID()."\n", $_[2]->SHARED_ID()."\n"
   );
}

1;

__END__

###############################################################################
## ----------------------------------------------------------------------------
## Module usage.
##
###############################################################################

=head1 NAME

MCE::Shared - MCE extension for sharing data supporting threads and processes

=head1 VERSION

This document describes MCE::Shared version 1.817

=head1 SYNOPSIS

   # OO construction

   use MCE::Shared;

   my $ar = MCE::Shared->array( @list );
   my $ca = MCE::Shared->cache( max_keys => 500, max_age => 60 );
   my $cv = MCE::Shared->condvar( 0 );
   my $fh = MCE::Shared->handle( '>>', \*STDOUT );
   my $ha = MCE::Shared->hash( @pairs );
   my $oh = MCE::Shared->ordhash( @pairs );
   my $db = MCE::Shared->minidb();
   my $qu = MCE::Shared->queue( await => 1, fast => 0 );
   my $va = MCE::Shared->scalar( $value );
   my $se = MCE::Shared->sequence( $begin, $end, $step, $fmt );
   my $ob = MCE::Shared->share( $blessed_object );

   # open function in MCE::Shared 1.002 and later

   mce_open my $fh, ">", "/foo/bar.log" or die "open error: $!";

   # Tie construction

   use feature 'say';

   use MCE::Flow;
   use MCE::Shared;

   tie my $var, 'MCE::Shared', 'initial value';
   tie my @ary, 'MCE::Shared', qw( a list of values );
   tie my %ha,  'MCE::Shared', ( key1 => 'value', key2 => 'value' );
   tie my %oh,  'MCE::Shared', { ordered => 1 }, ( key1 => 'value' );
   tie my %ca,  'MCE::Shared', { max_keys => 500, max_age => 60 };

   tie my $cnt, 'MCE::Shared', 0;
   tie my @foo, 'MCE::Shared';
   tie my %bar, 'MCE::Shared';

   my $m1 = MCE::Mutex->new;

   mce_flow {
      max_workers => 4
   },
   sub {
      my ( $mce ) = @_;
      my ( $pid, $wid ) = ( MCE->pid, MCE->wid );

      ## Locking is required when multiple workers update the same element.
      ## This requires 2 trips to the manager process (fetch and store).

      $m1->synchronize( sub {
         $cnt += 1;
      });

      ## Locking is not necessary when updating unique elements.

      $foo[ $wid - 1 ] = $pid;
      $bar{ $pid }     = $wid;

      return;
   };

   say "scalar : $cnt";
   say " array : $_" for (@foo);
   say "  hash : $_ => $bar{$_}" for (sort keys %bar);

   # Output

   scalar : 4
    array : 37847
    array : 37848
    array : 37849
    array : 37850
     hash : 37847 => 1
     hash : 37848 => 2
     hash : 37849 => 3
     hash : 37850 => 4

=head1 DESCRIPTION

This module provides data sharing capabilities for L<MCE> supporting threads
and processes. L<MCE::Hobo> provides threads-like parallelization for running
code asynchronously.

C<MCE::Shared> enables extra functionality on systems with L<IO::FDPass>
installed. Without it, MCE::Shared is unable to send C<file descriptors> to
the shared-manager process. The following is a suggestion for systems without
C<IO::FDPass>. This limitation applies to sharing L<MCE::Shared::Condvar>,
L<MCE::Shared::Handle>, and L<MCE::Shared::Queue> only.

   use MCE::Shared;

   my $has_IO_FDPass = $INC{'IO/FDPass.pm'} ? 1 : 0;

   # Construct any shared Condvar(s), Handle(s), and Queue(s) first.
   # These contain GLOB handles where freezing is not allowed.

   my $cv1 = MCE::Shared->condvar();
   my $cv2 = MCE::Shared->condvar();

   my $q1  = MCE::Shared->queue();
   my $q2  = MCE::Shared->queue();

   # The shared-manager process knows of ( \*STDOUT, \*STDERR, \*STDIN ).
   # Therefore, okay to construct these after the manager is running.

   mce_open my $fh1, ">>", \*STDOUT;                  # ok
   mce_open my $fh2, "<", "/path/to/sequence.fasta";  # ok

   # Afterwards, start the shared-manager manually.

   MCE::Shared->start() unless $has_IO_FDPass;

Note that the shared-manager will start automatically for other classes,
shipped with C<MCE::Shared>. The following will fail if Perl lacks the
C<IO::FDPass> module.

   use MCE::Shared;

   my $h1 = MCE::Shared->hash();    # the shared-manager is started here
   my $q1 = MCE::Shared->queue();   # must have IO::FDPass to pass fd's
   my $cv = MCE::Shared->condvar(); # ditto
   my $h2 = MCE::Shared->ordhash();

The L<IO::FDPass> module is known to work reliably on most platforms.
Install 1.1 or later to rid of limitations.

   perl -MIO::FDPass -le "print 'Cheers! Perl has IO::FDPass.'"

=head1 DATA SHARING

=over 3

=item * array L<MCE::Shared::Array>

=item * cache L<MCE::Shared::Cache>

=item * condvar L<MCE::Shared::Condvar>

=item * handle L<MCE::Shared::Handle>

=item * hash L<MCE::Shared::Hash>

=item * minidb L<MCE::Shared::Minidb>

=item * ordhash L<MCE::Shared::Ordhash>

=item * queue L<MCE::Shared::Queue>

=item * scalar L<MCE::Shared::Scalar>

=item * sequence L<MCE::Shared::Sequence>

=back

Below, synopsis for sharing classes included with MCE::Shared.

   # short form

   use MCE::Shared;

   $ar = MCE::Shared->array( @list );
   $ca = MCE::Shared->cache( max_keys => 500, max_age => 60 );
   $cv = MCE::Shared->condvar( 0 );
   $fh = MCE::Shared->handle( ">>", \*STDOUT ); # see mce_open below
   $ha = MCE::Shared->hash( @pairs );
   $oh = MCE::Shared->ordhash( @pairs );
   $db = MCE::Shared->minidb();
   $qu = MCE::Shared->queue( await => 1, fast => 0 );
   $va = MCE::Shared->scalar( $value );
   $se = MCE::Shared->sequence( $begin, $end, $step, $fmt );

   mce_open my $fh, ">>", \*STDOUT or die "open error: $!";

   # long form, must include class module

   use MCE::Shared::Array;
   use MCE::Shared::Cache;
   use MCE::Shared::Hash;
   use MCE::Shared::Minidb;
   use MCE::Shared::Ordhash;
   use MCE::Shared::Queue;
   use MCE::Shared::Scalar;

   $ar = MCE::Shared->share( MCE::Shared::Array->new( ... ) );
   $ca = MCE::Shared->share( MCE::Shared::Cache->new( ... ) );
   $ha = MCE::Shared->share( MCE::Shared::Hash->new( ... ) );
   $db = MCE::Shared->share( MCE::Shared::Minidb->new( ... ) );
   $oh = MCE::Shared->share( MCE::Shared::Ordhash->new( ... ) );
   $qu = MCE::Shared->share( MCE::Shared::Queue->new( ... ) );
   $va = MCE::Shared->share( MCE::Shared::Scalar->new( ... ) );

The restriction for sharing classes not included with MCE::Shared
is that the object must not have file-handles nor code-blocks.

   use Hash::Ordered;

   $oh = MCE::Shared->share( Hash::Ordered->new( ... ) );

=over 3

=item open ( filehandle, expr )

=item open ( filehandle, mode, expr )

=item open ( filehandle, mode, reference )

In version 1.002 and later, constructs a new object by opening the file
whose filename is given by C<expr>, and associates it with C<filehandle>.
When omitting error checking at the application level, MCE::Shared emits
a message and stop if open fails.

See L<MCE::Shared::Handle> for chunk IO demonstrations.

   # non-shared or local construction for use by a single process

   use MCE::Shared::Handle;

   MCE::Shared::Handle->open( my $fh, "<", "file.log" ) or die "$!";
   MCE::Shared::Handle::open  my $fh, "<", "file.log"   or die "$!";

   mce_open my $fh, "<", "file.log" or die "$!";

   # construction for sharing with other threads and processes

   use MCE::Shared;

   MCE::Shared->open( my $fh, "<", "file.log" ) or die "$!";
   MCE::Shared::open  my $fh, "<", "file.log"   or die "$!";

   mce_open my $fh, "<", "file.log" or die "$!";

Simple examples to open a file for reading:

   # mce_open is exported by MCE::Shared or MCE::Shared::Handle.
   # It creates a shared file handle with MCE::Shared present
   # or a non-shared handle otherwise.

   mce_open my $fh, "< input.txt"     or die "open error: $!";
   mce_open my $fh, "<", "input.txt"  or die "open error: $!";
   mce_open my $fh, "<", \*STDIN      or die "open error: $!";

and for writing:

   mce_open my $fh, "> output.txt"    or die "open error: $!";
   mce_open my $fh, ">", "output.txt" or die "open error: $!";
   mce_open my $fh, ">", \*STDOUT     or die "open error: $!";

=item num_sequence

C<num_sequence> is an alias for C<sequence>.

=back

=head1 DEEPLY SHARING

The following is a demonstration for a shared tied-hash variable. Before
venturing into the actual code, notice the dump function making a call to
C<export> explicitly for objects of type C<MCE::Shared::Object>. This is
necessary in order to retrieve the data from the shared-manager process.

The C<export> method is described later under the Common API section.

   sub _dump {
      require Data::Dumper unless $INC{'Data/Dumper.pm'};
      no warnings 'once';

      local $Data::Dumper::Varname  = 'VAR';
      local $Data::Dumper::Deepcopy = 1;
      local $Data::Dumper::Indent   = 1;
      local $Data::Dumper::Purity   = 1;
      local $Data::Dumper::Sortkeys = 0;
      local $Data::Dumper::Terse    = 0;

      ( ref $_[0] eq 'MCE::Shared::Object' )
         ? print Data::Dumper::Dumper( $_[0]->export ) . "\n"
         : print Data::Dumper::Dumper( $_[0] ) . "\n";
   }

   use MCE::Shared;

   tie my %abc, 'MCE::Shared';

   my @parents = qw( a b c );
   my @children = qw( 1 2 3 4 );

   for my $parent ( @parents ) {
      for my $child ( @children ) {
         $abc{ $parent }{ $child } = 1;
      }
   }

   _dump( tied( %abc ) );

   # Output

   $VAR1 = bless( {
     'c' => bless( {
       '1' => '1',
       '4' => '1',
       '3' => '1',
       '2' => '1'
     }, 'MCE::Shared::Hash' ),
     'a' => bless( {
       '1' => '1',
       '4' => '1',
       '3' => '1',
       '2' => '1'
     }, 'MCE::Shared::Hash' ),
     'b' => bless( {
       '1' => '1',
       '4' => '1',
       '3' => '1',
       '2' => '1'
     }, 'MCE::Shared::Hash' )
   }, 'MCE::Shared::Hash' );

Dereferencing provides hash-like behavior for C<hash> and C<ordhash>.
Array-like behavior is allowed for C<array>, not shown below.

   use MCE::Shared;

   my $abc = MCE::Shared->hash;

   my @parents = qw( a b c );
   my @children = qw( 1 2 3 4 );

   for my $parent ( @parents ) {
      for my $child ( @children ) {
         $abc->{ $parent }{ $child } = 1;
      }
   }

   _dump( $abc );

Each level in a deeply structure requires a separate trip to the shared-manager
process. The included C<MCE::Shared::Minidb> module provides optimized methods
for working with hash of hashes C<HoH> and/or hash of arrays C<HoA>. As such,
do the following when performance is desired.

   use MCE::Shared;

   my $abc = MCE::Shared->minidb;

   my @parents = qw( a b c );
   my @children = qw( 1 2 3 4 );

   for my $parent ( @parents ) {
      for my $child ( @children ) {
         $abc->hset( $parent, $child, 1 );
      }
   }

   _dump( $abc );

For further reading, see L<MCE::Shared::Minidb>.

=head1 OBJECT SHARING

=over 3

=item share

This class method transfers the blessed-object to the shared-manager
process and returns a C<MCE::Shared::Object> containing the C<SHARED_ID>.
For classes not included with C<MCE::Shared>, the object must not contain
any C<GLOB>'s or C<CODE_REF>'s or the transfer will fail.

   use MCE::Shared;
   use MCE::Shared::Ordhash;

   my $oh1 = MCE::Shared->share( MCE::Shared::Ordhash->new() );
   my $oh2 = MCE::Shared->ordhash();       # same thing

   $oh1->assign( @pairs );
   $oh2->assign( @pairs );

   use Hash::Ordered;

   my ($ho_shared, $ho_nonshared);

   $ho_shared = MCE::Shared->share( Hash::Ordered->new() );
   $ho_shared->push( @pairs );

   $ho_nonshared = $ho_shared->export();   # back to non-shared
   $ho_nonshared = $ho_shared->destroy();  # including destruction

The following provides long and short forms for constructing a shared array,
hash, or scalar object.

   use MCE::Shared;

   use MCE::Shared::Array;    # Loading helper classes is not necessary
   use MCE::Shared::Hash;     # when using the shorter form.
   use MCE::Shared::Scalar;

   my $a1 = MCE::Shared->share( MCE::Shared::Array->new( @list ) );
   my $a3 = MCE::Shared->share( [ @list ] );  # sugar syntax
   my $a2 = MCE::Shared->array( @list );

   my $h1 = MCE::Shared->share( MCE::Shared::Hash->new( @pairs ) );
   my $h3 = MCE::Shared->share( { @pairs } ); # sugar syntax
   my $h2 = MCE::Shared->hash( @pairs );

   my $s1 = MCE::Shared->share( MCE::Shared::Scalar->new( 20 ) );
   my $s2 = MCE::Shared->share( \do{ my $o = 20 } );
   my $s4 = MCE::Shared->scalar( 20 );

=back

=head1 PDL SHARING

=over 3

=item * pdl_byte

=item * pdl_short

=item * pdl_ushort

=item * pdl_long

=item * pdl_longlong

=item * pdl_float

=item * pdl_double

=item * pdl_ones

=item * pdl_sequence

=item * pdl_zeroes

=item * pdl_indx

=item * pdl

=back

C<pdl_byte>, C<pdl_short>, C<pdl_ushort>, C<pdl_long>, C<pdl_longlong>,
C<pdl_float>, C<pdl_double>, C<pdl_ones>, C<pdl_sequence>, C<pdl_zeroes>,
C<pdl_indx>, and C<pdl> are sugar syntax for PDL construction take place
under the shared-manager process.

   use PDL;                 # must load PDL before MCE::Shared
   use MCE::Shared;

   # makes extra copy/transfer and unnecessary destruction
   my $ob1 = MCE::Shared->share( zeroes( 256, 256 ) );

   # do this instead, efficient
   my $ob1 = MCE::Shared->zeroes( 256, 256 );

=over 3

=item ins_inplace

The C<ins_inplace> method applies to shared PDL objects. It supports
three forms for writing elements back to the PDL object, residing under
the shared-manager process.

   # --- action taken by the shared-manager process
   # ins_inplace(  1 arg  ):  ins( inplace( $this ), $what, 0, 0 );
   # ins_inplace(  2 args ):  $this->slice( $arg1 ) .= $arg2;
   # ins_inplace( >2 args ):  ins( inplace( $this ), $what, @coords );

   # --- use case
   $o->ins_inplace( $result );                    #  1 arg
   $o->ins_inplace( ":,$start:$stop", $result );  #  2 args
   $o->ins_inplace( $result, 0, $seq_n );         # >2 args

Operations such as C< + 5 > will not work on shared PDL objects. At this
time, the OO interface is the only mechanism for communicating with the
PDL piddle. For example, call C<slice>, C<sever>, or C<copy> to fetch
elements. Call C<ins_inplace> to update elements.

   # make a shared PDL piddle
   my $b = MCE::Shared->pdl_sequence(20,20);

   # fetch, add 10 to row 2 only
   my $res1 = $b->slice(":,1:1") + 10;
   $b->ins_inplace($res1, 0, 1);

   # fetch, add 10 to rows 4 and 5
   my $res2 = $b->slice(":,3:4") + 10;
   $b->ins_inplace($res2, 0, 3);

   # make non-shared object, export-destroy the shared object
   $b = $b->destroy;

   print "$b\n";

The following provides parallel demonstrations using C<MCE::Flow>.

   use PDL;  # must load PDL before MCE::Shared

   use MCE::Flow;
   use MCE::Shared;

   my $a = MCE::Shared->pdl_sequence(20,20);
   my $b = MCE::Shared->pdl_zeroes(20,20);

   # with chunking disabled

   mce_flow_s {
      max_workers => 4, chunk_size => 1
   },
   sub {
      my $row = $_;
      my $result = $a->slice(":,$row:$row") + 5;
      $b->ins_inplace($result, 0, $row);
   }, 0, 20 - 1;

   # with chunking enabled

   mce_flow_s {
      max_workers => 4, chunk_size => 5, bounds_only => 1
   },
   sub {
      my ($row1, $row2) = @{ $_ };
      my $result = $a->slice(":,$row1:$row2") + 5;
      $b->ins_inplace($result, 0, $row1);
   }, 0, 20 - 1;

   # make non-shared object, export-destroy the shared object

   $b = $b->destroy;

   print "$b\n";

See also L<PDL::ParallelCPU> and L<PDL::Parallel::threads>. For further
reading, the MCE-Cookbook on Github provides two PDL demonstrations.

L<https://github.com/marioroy/mce-cookbook>

=back

=head1 COMMON API

=over 3

=item blessed

Returns the real C<blessed> name, provided by the shared-manager process.

   use Scalar::Util qw(blessed);
   use MCE::Shared;

   use MCE::Shared::Ordhash;
   use Hash::Ordered;

   my $oh1 = MCE::Shared->share( MCE::Shared::Ordhash->new() );
   my $oh2 = MCE::Shared->share( Hash::Ordered->new() );

   print blessed($oh1), "\n";    # MCE::Shared::Object
   print blessed($oh2), "\n";    # MCE::Shared::Object

   print $oh1->blessed(), "\n";  # MCE::Shared::Ordhash
   print $oh2->blessed(), "\n";  # Hash::Ordered

=item destroy

Exports optionally, but destroys the shared object entirely from the
shared-manager process.

   my $exported_ob = $shared_ob->destroy();

   $shared_ob; # becomes undef

=item export ( keys )

=item export

Exports the shared object as a non-shared object. One must export when passing
the object into any dump routine. Otherwise, the C<shared_id value> and
C<blessed name> is all one will see.

   use MCE::Shared;
   use MCE::Shared::Ordhash;

   sub _dump {
      require Data::Dumper unless $INC{'Data/Dumper.pm'};
      no warnings 'once';

      local $Data::Dumper::Varname  = 'VAR';
      local $Data::Dumper::Deepcopy = 1;
      local $Data::Dumper::Indent   = 1;
      local $Data::Dumper::Purity   = 1;
      local $Data::Dumper::Sortkeys = 0;
      local $Data::Dumper::Terse    = 0;

      print Data::Dumper::Dumper($_[0]) . "\n";
   }

   my $oh1 = MCE::Shared->share( MCE::Shared::Ordhash->new() );
   my $oh2 = MCE::Shared->ordhash();  # same thing

   _dump($oh1);
      # bless( [ 1, 'MCE::Shared::Ordhash' ], 'MCE::Shared::Object' )

   _dump($oh2);
      # bless( [ 2, 'MCE::Shared::Ordhash' ], 'MCE::Shared::Object' )

   _dump( $oh1->export );  # dumps object structure and content
   _dump( $oh2->export );

C<export> can optionally take a list of indices/keys for what to export.
This applies to shared array, hash, and ordhash.

   use MCE::Shared;

   my $h1 = MCE::Shared->hash(           # shared hash
      qw/ I Heard The Bluebirds Sing by Marty Robbins /
        # k v     k   v         k    v  k     v
   );

   my $h2 = $h1->export( qw/ I The / );  # non-shared hash

   _dump($h2);

   # Output

   $VAR1 = bless( {
     'I' => 'Heard',
     'The' => 'Bluebirds'
   }, 'MCE::Shared::Hash' );

=item next

The C<next> method provides parallel iteration between workers for shared
C<array>, C<hash>, C<ordhash>, and C<sequence>. In list context, returns the
next key-value pair. This applies to C<array>, C<hash>, and C<ordhash>.
In scalar context, returns the next item. The C<undef> value is returned
after iteration has completed.

Internally, the list of keys to return is set when the closure is constructed.
Later keys added to the shared array or hash are not included. Subsequently,
the C<undef> value is returned for deleted keys.

The following example iterates through a shared array in parallel.

   use MCE::Hobo;
   use MCE::Shared;

   my $ob = MCE::Shared->array( 'a' .. 'j' );

   sub demo1 {
      my ( $id ) = @_;
      while ( my ( $index, $value ) = $ob->next ) {
         print "$id: [ $index ] $value\n";
         sleep 1;
      }
   }

   sub demo2 {
      my ( $id ) = @_;
      while ( defined ( my $value = $ob->next ) ) {
         print "$id: $value\n";
         sleep 1;
      }
   }

   MCE::Hobo->new( \&demo2, $_ ) for 1 .. 3;

   # ... do other work ...

   MCE::Hobo->waitall();

   # Output

   1: a
   2: b
   3: c
   2: f
   1: d
   3: e
   2: g
   3: i
   1: h
   2: j

The form is similar for C<sequence>. For large sequences, the C<bounds_only>
option is recommended. Also, specify C<chunk_size> accordingly. This reduces
the amount of traffic to and from the shared-manager process.

   use MCE::Hobo;
   use MCE::Shared;

   my $N   = shift || 4_000_000;
   my $pi  = MCE::Shared->scalar( 0.0 );

   my $seq = MCE::Shared->sequence(
      { chunk_size => 200_000, bounds_only => 1 }, 0, $N - 1
   );

   sub compute_pi {
      my ( $wid ) = @_;

      while ( my ( $beg, $end ) = $seq->next ) {
         my ( $_pi, $t ) = ( 0.0 );
         for my $i ( $beg .. $end ) {
            $t = ( $i + 0.5 ) / $N;
            $_pi += 4.0 / ( 1.0 + $t * $t );
         }
         $pi->incrby( $_pi );
      }

      return;
   }

   MCE::Hobo->create( \&compute_pi, $_ ) for ( 1 .. 8 );

   # ... do other stuff ...

   MCE::Hobo->waitall();

   printf "pi = %0.13f\n", $pi->get / $N;

   # Output

   3.1415926535898

=item rewind ( index, [, index, ... ] )

=item rewind ( key, [, key, ... ] )

=item rewind ( "query string" )

Rewinds the parallel iterator for L<MCE::Shared::Array>, L<MCE::Shared::Hash>,
or L<MCE::Shared::Ordhash> when no arguments are given. Otherwise, resets the
iterator with given criteria. The syntax for C<query string> is described in
the shared module.

   # array
   $ar->rewind;

   $ar->rewind( 0, 1 );
   $ar->rewind( "val eq some_value" );
   $ar->rewind( "key >= 50 :AND val =~ /sun|moon|air|wind/" );
   $ar->rewind( "val eq sun :OR val eq moon :OR val eq foo" );
   $ar->rewind( "key =~ /$pattern/" );

   while ( my ( $index, $value ) = $ar->next ) {
      ...
   }

   # hash, ordhash
   $oh->rewind;

   $oh->rewind( "key1", "key2" );
   $oh->rewind( "val eq some_value" );
   $oh->rewind( "key eq some_key :AND val =~ /sun|moon|air|wind/" );
   $oh->rewind( "val eq sun :OR val eq moon :OR val eq foo" );
   $oh->rewind( "key =~ /$pattern/" );

   while ( my ( $key, $value ) = $oh->next ) {
      ...
   }

=item rewind ( { options }, begin, end [, step, format ] )

=item rewind ( begin, end [, step, format ] )

Rewinds the parallel iterator for L<MCE::Shared::Sequence> when no arguments
are given. Otherwise, resets the iterator with given criteria.

   $seq->rewind;

   $seq->rewind( { chunk_size => 10, bounds_only => 1 }, 1, 100 );

   while ( my ( $beg, $end ) = $seq->next ) {
      for my $i ( $beg .. $end ) {
         ...
      }
   }

   $seq->rewind( 1, 100 );

   while ( defined ( my $num = $seq->next ) ) {
      ...
   }

=item store ( key, value )

Deep-sharing a non-blessed structure recursively is possible with C<store>,
an alias to C<STORE>.

   use MCE::Shared;

   my $h1 = MCE::Shared->hash();
   my $h2 = MCE::Shared->hash();

   # auto-shares deeply
   $h1->store( 'key', [ 0, 2, 5, { 'foo' => 'bar' } ] );
   $h2->{key}[3]{foo} = 'baz';   # via auto-vivification

   my $v1 = $h1->get('key')->get(3)->get('foo');  # bar
   my $v2 = $h2->get('key')->get(3)->get('foo');  # baz
   my $v3 = $h2->{key}[3]{foo};                   # baz

=back

=head1 SERVER API

=over 3

=item init

This method is called automatically by each MCE or Hobo worker immediately
after being spawned. The effect is extra parallelism during inter-process
communication. The optional ID (an integer) is modded internally in a
round-robin fashion.

   MCE::Shared->init();
   MCE::Shared->init( ID );

=item start

Starts the shared-manager process. This is done automatically.

   MCE::Shared->start();

=item stop

Stops the shared-manager process, wiping all shared data content. This is
called by the C<END> block automatically when the script terminates.

   MCE::Shared->stop();

=back

=head1 LOCKING

Application-level advisory locking is possible with L<MCE::Mutex>.

   use strict;
   use warnings;

   use MCE::Hobo;
   use MCE::Mutex;
   use MCE::Shared;

   my $mutex = MCE::Mutex->new();

   tie my $cntr, 'MCE::Shared', 0;

   sub work {
      for ( 1 .. 1000 ) {
         $mutex->lock;

         # The next statement involves 2 IPC ops ( get and set ).
         # Thus, locking is required.
         $cntr++;

         $mutex->unlock;
      }
   }

   MCE::Hobo->create('work') for ( 1 .. 8 );

   MCE::Hobo->waitall;

   print $cntr, "\n"; # 8000

However, locking is not necessary when using the OO interface. This is possible
as MCE::Shared is implemented using a single-point of entry for commands sent
to the shared-manager process. Furthermore, the shared classes include sugar
methods for combining set and get in a single operation.

   use strict;
   use warnings;

   use MCE::Hobo;
   use MCE::Shared;

   my $cntr = MCE::Shared->scalar( 0 );

   sub work {
      for ( 1 .. 1000 ) {
         # The next statement increments the value without having
         # to call set and get explicitly.
         $cntr->incr;
      }
   }

   MCE::Hobo->create('work') for ( 1 .. 8 );

   MCE::Hobo->waitall;

   print $cntr->get, "\n"; # 8000

Another possibility when running threads is locking via L<threads::shared>.

   use strict;
   use warnings;

   use threads;
   use threads::shared;

   use MCE::Flow;
   use MCE::Shared;

   my $mutex : shared;

   tie my $cntr, 'MCE::Shared', 0;

   sub work {
      for ( 1 .. 1000 ) {
         lock $mutex;

         # the next statement involves 2 IPC ops ( get and set )
         # thus, locking is required
         $cntr++;
      }
   }

   MCE::Flow->run( { max_workers => 8 }, \&work );

   MCE::Flow->finish;

   print $cntr, "\n"; # 8000

Of the three demonstrations, the OO interface yields the best performance.
This is from the lack of locking at the application level. The results were
obtained from a MacBook Pro (Haswell) running at 2.6 GHz, 1600 MHz RAM.

   CentOS 7.2 VM

      -- Perl v5.16.3
      MCE::Mutex .... : 0.528 secs.
      OO Interface .. : 0.062 secs.
      threads::shared : 0.545 secs.

   FreeBSD 10.0 VM

      -- Perl v5.16.3
      MCE::Mutex .... : 0.367 secs.
      OO Interface .. : 0.083 secs.
      threads::shared : 0.593 secs.

   Mac OS X 10.11.6 ( Host OS )

      -- Perl v5.18.2
      MCE::Mutex .... : 0.397 secs.
      OO Interface .. : 0.070 secs.
      threads::shared : 0.463 secs.

   Solaris 11.2 VM

      -- Perl v5.12.5 installed with the OS
      MCE::Mutex .... : 0.895 secs.
      OO Interface .. : 0.099 secs.
      threads::shared :              Perl not built to support threads

      -- Perl v5.22.2 built with threads support
      MCE::Mutex .... : 0.788 secs.
      OO Interface .. : 0.086 secs.
      threads::shared : 0.895 secs.

   Windows 7 VM

      -- Perl v5.22.2
      MCE::Mutex .... : 1.045 secs.
      OO Interface .. : 0.312 secs.
      threads::shared : 1.061 secs.

Beginning with MCE::Shared 1.809, the C<pipeline> method provides another way.
Included in C<Array>, C<Cache>, C<Hash>, C<Minidb>, and C<Ordhash>, it combines
multiple commands for the object to be processed serially. For shared objects,
the call is made atomically due to single IPC to the shared-manager process.

The C<pipeline> method is fully C<wantarray>-aware and receives a list of
commands and their arguments. In scalar or list context, it returns data from
the last command in the pipeline.

   use MCE::Mutex;
   use MCE::Shared;

   my $mutex = MCE::Mutex->new();
   my $oh = MCE::Shared->ordhash();
   my @vals;

   # mutex locking

   $mutex->lock;
   $oh->set( foo => "a_a" );
   $oh->set( bar => "b_b" );
   $oh->set( baz => "c_c" );
   @vals = $oh->mget( qw/ foo bar baz / );
   $mutex->unlock;

   # pipeline, same thing done atomically

   @vals = $oh->pipeline(
      [ "set", foo => "a_a" ],
      [ "set", bar => "b_b" ],
      [ "set", baz => "c_c" ],
      [ "mget", qw/ foo bar baz / ]
   );

   # ( "a_a", "b_b", "c_c" )

There is also C<pipeline_ex>, same as C<pipeline>, but returns data for every
command in the pipeline.

   @vals = $oh->pipeline_ex(
      [ "set", foo => "a_a" ],
      [ "set", bar => "b_b" ],
      [ "set", baz => "c_c" ]
   );

   # ( "a_a", "b_b", "c_c" )

=head1 REQUIREMENTS

MCE::Shared requires Perl 5.10.1 or later. The L<IO::FDPass> module is highly
recommended on UNIX and Windows. This module does not install it by default.

=head1 SOURCE AND FURTHER READING

The source, cookbook, and examples are hosted at GitHub.

=over 3

=item * L<https://github.com/marioroy/mce-shared>

=item * L<https://github.com/marioroy/mce-cookbook>

=item * L<https://github.com/marioroy/mce-examples>

=back

=head1 INDEX

L<MCE|MCE>, L<MCE::Hobo>

=head1 AUTHOR

Mario E. Roy, S<E<lt>marioeroy AT gmail DOT comE<gt>>

=head1 COPYRIGHT AND LICENSE

Copyright (C) 2016-2017 by Mario E. Roy

MCE::Shared is released under the same license as Perl.

See L<http://dev.perl.org/licenses/> for more information.

=cut

