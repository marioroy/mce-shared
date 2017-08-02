###############################################################################
## ----------------------------------------------------------------------------
## A threads-like parallelization module with managed capabilities.
##
###############################################################################

use strict;
use warnings;

use 5.010001;

no warnings qw( threads recursion uninitialized once redefine );

package MCE::Hobo;

our $VERSION = '1.826';

## no critic (BuiltinFunctions::ProhibitStringyEval)
## no critic (Subroutines::ProhibitExplicitReturnUndef)
## no critic (Subroutines::ProhibitSubroutinePrototypes)
## no critic (TestingAndDebugging::ProhibitNoStrict)

use if $^O ne 'MSWin32', 'POSIX', 'WNOHANG';
use Time::HiRes 'sleep';
use MCE::Shared ();
use bytes;

use overload (
   q(==)    => \&equal,
   q(!=)    => sub { !equal(@_) },
   fallback => 1
);

no overloading;

sub import {
   no strict 'refs'; no warnings 'redefine';
   *{ caller().'::mce_async' } = \&async;
   return;
}

use constant {
   _WNOHANG => $INC{'POSIX.pm'} ? &WNOHANG : 1
};

my ( $_MNGD, $_DATA, $_DELY, $_LIST ) = ( {}, {}, {}, {} );

my $_freeze = MCE::Shared::Server::_get_freeze();
my $_thaw   = MCE::Shared::Server::_get_thaw();

my $_has_threads = $INC{'threads.pm'} ? 1 : 0;
my $_tid = $_has_threads ? threads->tid() : 0;
my $_uid = 0;

sub CLONE {
   $_tid = threads->tid(), &_clear() if $_has_threads;
}

sub _clear {
   %{ $_LIST } = ();
}

sub _uid {
   $_uid = 1000 if ( ++$_uid > 2e9 );
   $_uid;
}

###############################################################################
## ----------------------------------------------------------------------------
## Init routine.
##
###############################################################################

bless my $_SELF = { MGR_ID => "$$.$_tid", WRK_ID => $$ }, __PACKAGE__;

sub init {
   _croak('Usage: MCE::Hobo->init()') if ref($_[0]);
   shift if ( defined $_[0] && $_[0] eq __PACKAGE__ );

   my $pkg  = "$$.$_tid.".( caller eq __PACKAGE__ ? caller(1) : caller );
      $pkg .= '.'.&_uid() if defined wantarray;

   # -- options --------------------------------------------------------
   # max_workers on_start on_finish hobo_timeout posix_exit spawn_delay
   # -------------------------------------------------------------------

   my $mngd = $_MNGD->{$pkg} = { @_ };  @_ = ();

   $mngd->{MGR_ID} = "$$.$_tid", $mngd->{PKG} = $pkg,
   $mngd->{WRK_ID} =  $$;

   $mngd->{max_workers} = delete $mngd->{max_procs}
      if ( exists $mngd->{max_procs} );

   &_force_reap($pkg), $_DATA->{$pkg}->clear() if exists $_LIST->{$pkg};

   if ( !exists $_LIST->{$pkg} ) {
      $_LIST->{ $pkg } = MCE::Hobo::_ordhash->new();
      $_DELY->{ $pkg } = MCE::Shared->share( MCE::Hobo::_delay->new() );
      $_DATA->{ $pkg } = MCE::Shared->share( MCE::Hobo::_hash->new() );
      $_DATA->{"$pkg:seed"} = int(rand() * 1e9);
      $_DATA->{"$pkg:id"  } = 0;
   }
   if ( !exists $mngd->{posix_exit} ) {
      $mngd->{posix_exit} = 1 if (
         ( $_has_threads && $_tid ) || $INC{'Mojo/IOLoop.pm'} ||
         $INC{'Curses.pm'} || $INC{'CGI.pm'} || $INC{'FCGI.pm'} ||
         $INC{'Prima.pm'} || $INC{'Tk.pm'} || $INC{'Wx.pm'} ||
         $INC{'Gearman/Util.pm'} || $INC{'Gearman/XS.pm'} ||
         $INC{'Coro.pm'} || $INC{'Win32/GUI.pm'}
      );
   }
   if ( exists $mngd->{max_workers} ) {
      _max_workers($mngd, $mngd->{max_workers});
   }

   if ( defined wantarray ) {
      bless $mngd, 'MCE::Hobo::Manager';
      Scalar::Util::weaken($_MNGD->{$pkg});
      $mngd->{MNGD} = 1;
   }

   $mngd;
}

###############################################################################
## ----------------------------------------------------------------------------
## 'new', 'async (mce_async)', and 'create' for threads-like similarity.
##
###############################################################################

## 'new' and 'tid' are aliases for 'create' and 'pid' respectively.

*new = \&create, *tid = \&pid;

## Use "goto" trick to avoid pad problems from 5.8.1 (fixed in 5.8.2)
## Tip found in threads::async.

sub async (&;@) {
   unless ( $_[0] eq __PACKAGE__ || Scalar::Util::blessed($_[0]) ) {
      unshift @_, __PACKAGE__;
   }
   goto &create;
}

sub create {
   my $mngd = ref $_[0] ? $_[0] : $_MNGD->{ "$$.$_tid.".caller() } // do {
      # Unless defined, construct $mngd internally on first use.
      init(); $_MNGD->{ "$$.$_tid.".caller() };
   };
   _croak("Nested spawning is possible, but requires the child process\n".
          "to construct it's own by calling ->init, then spawn workers")
      if ( $mngd->{MNGD} && $mngd->{MGR_ID} ne "$$.$_tid" );

   shift;

   # ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~

   my $self = bless ref $_[0] eq 'HASH' ? { %{ shift() } } : { }, __PACKAGE__;

   $self->{MGR_ID} = $mngd->{MGR_ID}, $self->{PKG} = $mngd->{PKG};

   my $func = shift; $func = caller().'::'.$func
      if ( !ref $func && length $func && index($func,':') < 0 );

   if ( !defined $func && !exists $self->{no_fcn} ) {
      local $\; print {*STDERR} "code function is not specified or valid\n";
      return undef;
   }

   my ( $list, $max_workers, $pkg ) = (
      $_LIST->{ $mngd->{PKG} }, $mngd->{max_workers}, $mngd->{PKG}
   );

   $_DATA->{"$pkg:id"} = 0
      if ( ( my $id = ++$_DATA->{"$pkg:id"} ) > 2e9 );

   # Reap completed hobos.
   if ( $max_workers || defined wantarray || $mngd->{on_finish} ) {
      local $!;
      for my $wrk_id ( keys %{ $list->[0] } ) {
         waitpid($wrk_id, _WNOHANG) or next;
         _reap_hobo($list->del($wrk_id));
      }
   }

   # Wait for a slot if saturated.
   $self->wait_one()
      if ( $max_workers && keys %{ $list->[0] } >= $max_workers );

   # ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~ ~~~

   my @args = @_; @_ = ();  # To avoid (Scalars leaked: N) messages
   my $pid  = fork();

   if ( !defined $pid ) {                                # error
      local $\; print {*STDERR} "fork error: $!\n";

      return undef;
   }
   elsif ( $pid ) {                                      # parent
      $self->{WRK_ID} = $pid, $list->set($pid, $self);
      $mngd->{on_start}->($pid, $self->{ident}) if $mngd->{on_start};
      sleep $mngd->{spawn_delay} if $mngd->{spawn_delay};

      return $self;
   }

   %{ $_LIST } = (), $_SELF = $self;                     # child

   if ( UNIVERSAL::can('Prima', 'cleanup') ) {
      no warnings 'redefine'; local $@; eval '*Prima::cleanup = sub {}';
   }

   MCE::Shared::init($id);

   # Sets the seed of the base generator uniquely between workers.
   # The new seed is computed using the current seed and ID value.
   # One may set the seed at the application level for predictable
   # results. Ditto for Math::Random.

   srand( abs($_DATA->{"$pkg:seed"} - ($id * 100000)) % 2147483560 );

   if ( $INC{'Math/Random.pm'} ) {
      my $cur_seed = Math::Random::random_get_seed();
      my $new_seed = ($cur_seed < 1073741781)
         ? $cur_seed + ((abs($id) * 10000) % 1073741780)
         : $cur_seed - ((abs($id) * 10000) % 1073741780);

      Math::Random::random_set_seed($new_seed, $new_seed);
   }

   _dispatch($mngd, $func, \@args);
}

###############################################################################
## ----------------------------------------------------------------------------
## Public methods.
##
###############################################################################

sub equal {
   _croak('Usage: $hobo->equal()') if ( ref $_[0] && $_[0]->{MNGD} );
   return 0 unless ( ref $_[0] && ref $_[1] );

   ( $_[0]->{PKG   } == $_[1]->{PKG   } &&
     $_[0]->{WRK_ID} == $_[1]->{WRK_ID} ) ? 1 : 0;
}

sub error {
   _croak('Usage: $hobo->error()')
      if ( !ref( my $self = $_[0] ) || $_[0]->{MNGD} );

   $self->join() if ( !exists $self->{JOINED} );
   $self->{ERROR} || undef;
}

sub exit {
   shift if ( defined $_[0] && $_[0] eq __PACKAGE__ );

   my ( $self ) = ( ref $_[0] ? shift : $_SELF );
   my ( $pkg, $wrk_id ) = ( $self->{PKG}, $self->{WRK_ID} );

   if ( $wrk_id == $$ && $self->{MGR_ID} eq "$$.$_tid" ) {
      MCE::Hobo->finish('MCE');
      CORE::exit(@_);
   }
   elsif ( $wrk_id == $$ ) {
      alarm 0; my ( $exit_status, @res ) = @_; $? = $exit_status // 0;
      $_DATA->{$pkg}->set('R'.$wrk_id, @res ? $_freeze->(\@res) : '');
      die "Hobo exited ($?)\n" unless ( exists $_SELF->{no_fcn} );
      _exit($?);
   }

   return $self if ( exists $self->{JOINED} );

   if ( exists $_DATA->{$pkg} ) {
      sleep 0.015 until $_DATA->{$pkg}->exists('S'.$wrk_id);
   } else {
      sleep 0.030;
   }

   if ($^O eq 'MSWin32') {
      CORE::kill('KILL', $wrk_id) if CORE::kill('ZERO', $wrk_id);
   } else {
      CORE::kill('QUIT', $wrk_id) if CORE::kill('ZERO', $wrk_id);
   }

   $self;
}

sub finish {
   _croak('Usage: MCE::Hobo->finish()') if ref($_[0]);
   my $pkg = ( defined $_[1] ) ? $_[1] : caller();

   if ( $pkg eq 'MCE' ) {
      for my $key ( keys %{ $_LIST } ) { MCE::Hobo->finish($key); }
   }
   elsif ( exists $_LIST->{$pkg} ) {
      return if $MCE::Signal::KILLED;

      if ( exists $_DELY->{$pkg} ) {
         &_force_reap($pkg);
         delete($_DELY->{$pkg}), delete($_DATA->{"$pkg:seed"}),
         delete($_LIST->{$pkg}), delete($_DATA->{"$pkg:id"}),
         delete($_MNGD->{$pkg}), delete($_DATA->{ $pkg });
      }
   }

   @_ = ();

   return;
}

sub is_joinable {
   _croak('Usage: $hobo->is_joinable()')
      if ( !ref( my $self = $_[0] ) || $_[0]->{MNGD} );

   my ( $wrk_id, $pkg ) = ( $self->{WRK_ID}, $self->{PKG} );

   if ( $wrk_id == $$ ) {
      '';
   }
   elsif ( $self->{MGR_ID} eq "$$.$_tid" ) {
      return undef if ( exists $self->{JOINED} );
      local $!;
      ( waitpid($wrk_id, _WNOHANG) == 0 ) ? '' : do {
         _reap_hobo($_LIST->{$pkg}->del($self->{WRK_ID}));
         1;
      };
   }
   else {
      $_DATA->{$pkg}->exists('R'.$wrk_id) ? 1 : '';
   }
}

sub is_running {
   _croak('Usage: $hobo->is_running()')
      if ( !ref( my $self = $_[0] ) || $_[0]->{MNGD} );

   my ( $wrk_id, $pkg ) = ( $self->{WRK_ID}, $self->{PKG} );

   if ( $wrk_id == $$ ) {
      1;
   }
   elsif ( $self->{MGR_ID} eq "$$.$_tid" ) {
      return undef if ( exists $self->{JOINED} );
      local $!;
      ( waitpid($wrk_id, _WNOHANG) == 0 ) ? 1 : do {
         _reap_hobo($_LIST->{$pkg}->del($self->{WRK_ID}));
         '';
      };
   }
   else {
      $_DATA->{$pkg}->exists('R'.$wrk_id) ? '' : 1;
   }
}

sub join {
   _croak('Usage: $hobo->join()')
      if ( !ref( my $self = $_[0] ) || $_[0]->{MNGD} );

   if ( exists $self->{JOINED} ) {
      return ( defined wantarray )
         ? wantarray ? @{ $self->{RESULT} } : $self->{RESULT}->[-1] : ();
   }

   my ( $wrk_id, $pkg ) = ( $self->{WRK_ID}, $self->{PKG} );

   if ( $wrk_id == $$ ) {
      _croak('Cannot join self');
   }
   elsif ( $self->{MGR_ID} eq "$$.$_tid" ) {
      local $!; waitpid($wrk_id, 0);
      _reap_hobo($_LIST->{$pkg}->del($wrk_id));
   }
   else {
      sleep 0.3 until ( $_DATA->{$pkg}->exists('R'.$wrk_id) );
      _reap_hobo($self);
   }

   ( defined wantarray )
      ? wantarray ? @{ $self->{RESULT} } : $self->{RESULT}->[-1]
      : ();
}

sub kill {
   _croak('Usage: $hobo->kill()')
      if ( !ref( my $self = $_[0] ) || $_[0]->{MNGD} );

   my ( $wrk_id, $pkg, $signal ) = ( $self->{WRK_ID}, $self->{PKG}, $_[1] );

   if ( $wrk_id == $$ ) {
      CORE::kill($signal || 'INT', $$);
      return $self;
   }
   if ( $self->{MGR_ID} eq "$$.$_tid" ) {
      return $self if ( exists $self->{JOINED} );
      if ( exists $_DATA->{$pkg} ) {
         sleep 0.015 until $_DATA->{$pkg}->exists('S'.$wrk_id);
      } else {
         sleep 0.030;
      }
   }

   CORE::kill($signal || 'INT', $wrk_id) if CORE::kill('ZERO', $wrk_id);

   $self;
}

sub list {
   my $pkg = ref $_[0] ? $_[0]->{PKG} : "$$.$_tid.".caller();

   ( exists $_LIST->{$pkg} ) ? $_LIST->{$pkg}->vals() : ();
}

sub list_joinable {
   my $pkg = ref $_[0] ? $_[0]->{PKG} : "$$.$_tid.".caller();
   return () unless ( my $list = $_LIST->{$pkg} );

   local ($!, $?, $_);

   map {
      ( waitpid($_->{WRK_ID}, _WNOHANG) == 0 ) ? () : do {
         _reap_hobo($list->del($_->{WRK_ID}));
         $_;
      };
   }
   $list->vals();
}

sub list_running {
   my $pkg = ref $_[0] ? $_[0]->{PKG} : "$$.$_tid.".caller();
   return () unless ( my $list = $_LIST->{$pkg} );

   local ($!, $?, $_);

   map {
      ( waitpid($_->{WRK_ID}, _WNOHANG) == 0 ) ? $_ : do {
         _reap_hobo($list->del($_->{WRK_ID}));
         ();
      };
   }
   $list->vals();
}

sub pending {
   my $pkg = ref $_[0] ? $_[0]->{PKG} : "$$.$_tid.".caller();

   ( exists $_LIST->{$pkg} ) ? $_LIST->{$pkg}->len() : 0;
}

sub pid {
   ref($_[0]) ? $_[0]->{WRK_ID} : $_SELF->{WRK_ID};
}

sub result {
   _croak('Usage: $hobo->result()')
      if ( !ref( my $self = $_[0] ) || $_[0]->{MNGD} );

   return $self->join() if ( !exists $self->{JOINED} );
   wantarray ? @{ $self->{RESULT} } : $self->{RESULT}->[-1];
}

sub self {
   ( ref $_[0] ) ? ( $_[0]->{MNGD} ) ? undef : $_[0] : $_SELF;
}

sub wait_all {
   my $pkg = ref $_[0] ? $_[0]->{PKG} : "$$.$_tid.".caller();

   return wantarray ? () : 0
      if ( !exists $_LIST->{$pkg} || !$_LIST->{$pkg}->len() );

   local $_; ( wantarray )
      ? map { $_->join(); $_ } $_LIST->{$pkg}->vals()
      : map { $_->join(); () } $_LIST->{$pkg}->vals();
}

*waitall = \&wait_all; # compatibility

sub wait_one {
   my $pkg = ref $_[0] ? $_[0]->{PKG} : "$$.$_tid.".caller();

   return undef unless ( my $list = $_LIST->{$pkg} );
   return undef if ( !$list->len() || !exists $_DATA->{$pkg} );

   my $self; local $!;

   while () {
      my $wrk_id = CORE::wait();

      return undef if ( $wrk_id == -1 );        # no child processes
      last if ( $self = $list->del($wrk_id) );  # our child process

      for my $key ( keys %{ $_LIST } ) {        # other child process
         _reap_hobo($_LIST->{$key}->del($wrk_id)), last
            if ( $key ne $pkg && $_LIST->{$key}->exists($wrk_id) );
      }
   }

   _reap_hobo($self);

   $self;
}

*waitone = \&wait_one; # compatibility

sub yield {
   my $pkg = ref $_[0] ? shift->{PKG} : do {
      shift if ( defined $_[0] && $_[0] eq __PACKAGE__ );
      "$$.$_tid.".caller();
   };
   return unless ( my $mngd = $_MNGD->{$pkg} );

   if ( $mngd->{CHILD} ) {
      ( $INC{'Coro/AnyEvent.pm'} )
         ? Coro::AnyEvent::sleep( $_DELY->{$pkg}->seconds(@_) )
         : sleep $_DELY->{$pkg}->seconds(@_);
   }
   else {
      local $_; $_->join() for list_joinable($mngd);
   }

   return;
}

###############################################################################
## ----------------------------------------------------------------------------
## Private methods.
##
###############################################################################

sub _croak {
   goto &MCE::_croak if $INC{'MCE.pm'};
   require MCE::Shared::Base unless $INC{'MCE/Shared/Base.pm'};
   goto &MCE::Shared::Base::_croak;
}

sub _dispatch {
   my ( $mngd, $func, $args ) = @_;
   $mngd->{CHILD} = 1, $mngd->{WRK_ID} = $_SELF->{WRK_ID} = $$;

   $ENV{PERL_MCE_IPC} = 'win32' if ($^O eq 'MSWin32');
   $SIG{TERM} = $SIG{INT} = $SIG{HUP} = \&_trap;
   $SIG{QUIT} = \&_quit;

   # IO::Handle->autoflush not available in older Perl.
   {
      local $!;
      select(( select(*STDERR), $| = 1 )[0]) if defined(fileno *STDERR);
      select(( select(*STDOUT), $| = 1 )[0]) if defined(fileno *STDOUT);
   }

   # Run.
   $? = 0, $_DATA->{ $_SELF->{PKG} }->set('S'.$$, '');
   return 0 unless ( defined $func ); # e.g. $pm->start and next

   local $SIG{'ALRM'} = sub { alarm 0; die "Hobo timed out\n" };

   my $hobo_timeout = ( exists $_SELF->{hobo_timeout} )
      ? $_SELF->{hobo_timeout} : $mngd->{hobo_timeout};

   my @res = eval {
      no strict 'refs'; alarm( $hobo_timeout || 0 );
      $func->( @{ $args } );
   };
   alarm 0;

   _exit($?) if ( $@ && $@ =~ /^Hobo exited \(\S+\)$/ );

   if ( $@ ) {
      chomp( my $err = $@ );
      $? = 1, $_DATA->{ $_SELF->{PKG} }->set('S'.$$, $err);
      warn "Hobo $$ terminated abnormally: reason $err\n" if (
         $err ne "Hobo timed out" && !$mngd->{on_finish}
      );
   }

   $_DATA->{ $_SELF->{PKG} }->set('R'.$$, @res ? $_freeze->(\@res) : '');

   _exit($?);
}

sub _exit {
   my ( $exit_status ) = @_;

   # Check nested Hobo workers not yet joined.
   MCE::Hobo->finish('MCE') if ( keys %{ $_LIST } > 0 && !$_SELF->{SIGNALED} );

   # Exit child process.
   $SIG{__DIE__}  = sub { } unless $_tid;
   $SIG{__WARN__} = sub { };

   threads->exit($exit_status) if ( $_has_threads && $^O eq 'MSWin32' );

   $SIG{HUP} = $SIG{INT} = $SIG{QUIT} = $SIG{TERM} = sub {
      $SIG{$_[0]} = $SIG{INT} = sub { };
      CORE::kill($_[0], getppid()) if ( $_[0] eq 'INT' && $^O ne 'MSWin32' );
      CORE::kill('KILL', $$);
   };

   my $posix_exit = ( exists $_SELF->{posix_exit} )
      ? $_SELF->{posix_exit} : $_MNGD->{ $_SELF->{PKG} }{posix_exit};

   if ( $posix_exit && $^O ne 'MSWin32' ) {
      eval { MCE::Mutex::Channel::_destroy() };
      POSIX::_exit($exit_status);
   }

   CORE::exit($exit_status);
}

sub _force_reap {
   my ( $count, $pkg ) = ( 0, @_ );
   return unless ( exists $_LIST->{$pkg} && $_LIST->{$pkg}->len() );

   for my $hobo ( $_LIST->{$pkg}->vals() ) {
      if ( $hobo->is_running() ) {
         CORE::kill('KILL', $hobo->pid())
            if CORE::kill('ZERO', $hobo->pid());
         $count++;
      }
   }

   $_LIST->{$pkg}->clear();

   warn "Finished with active Hobos [$pkg] ($count)\n"
      if ($count && $^O ne 'MSWin32');

   return;
}

sub _max_workers {
   my $mngd = shift;

   if ( @_ ) {
      my $cpus = $_[0];
      if ( !defined $cpus ) {
         delete $mngd->{max_workers};
      } else {
         $cpus = MCE::Util::get_ncpu() if $cpus eq 'auto';
         $cpus = 1 if $cpus !~ /^\d+$/ || $cpus < 1;
         $mngd->{max_workers} = $cpus;
      }
   }

   $mngd->{max_workers};
}

sub _quit {
   my ( $name ) = @_;
   $_SELF->{SIGNALED} = 1, $name =~ s/^SIG//;

   $SIG{$name} = sub {}, CORE::kill($name, -$$)
      if ( exists $SIG{$name} );

   _exit(0);
}

sub _reap_hobo {
   my ( $hobo ) = @_;
   local @_ = $_DATA->{ $hobo->{PKG} }->_get_hobo_data( $hobo->{WRK_ID} );

   ( $hobo->{ERROR}, $hobo->{RESULT}, $hobo->{JOINED} ) =
      ( pop // '', length $_[0] ? $_thaw->(pop) : [], 1 );

   if ( my $on_finish = $_MNGD->{ $hobo->{PKG} }{on_finish} ) {
      my ( $exit, $err ) = ( $? // 0, $hobo->{ERROR} );
      my ( $code, $sig ) = ( $exit >> 8, $exit & 0x7f );

      if ( ( $code > 100 || $sig == 9 ) && !$err ) {
         $code = 2, $sig = 1,  $err = 'received SIGHUP'  if $code == 101;
         $code = 2, $sig = 2,  $err = 'received SIGINT'  if $code == 102;
         $code = 2, $sig = 15, $err = 'received SIGTERM' if $code == 115;
         $code = 2, $sig = 9,  $err = 'received SIGKILL' if $sig  == 9;
      }

      $on_finish->(
         $hobo->{WRK_ID}, $code, $hobo->{ident}, $sig, $err,
         @{ $hobo->{RESULT} }
      );
   }

   return;
}

sub _trap {
   my ( $exit_status, $name ) = ( 2, @_ );
   $_SELF->{SIGNALED} = 1, $name =~ s/^SIG//;

   $SIG{$name} = sub {}, CORE::kill($name, -$$)
      if ( exists $SIG{$name} );

   if    ( $name eq 'HUP'  ) { $exit_status = 101 }
   elsif ( $name eq 'INT'  ) { $exit_status = 102 }
   elsif ( $name eq 'TERM' ) { $exit_status = 115 }

   _exit($exit_status);
}

###############################################################################
## ----------------------------------------------------------------------------
## Delay, hash, and ordhash implementations suited for MCE::Hobo.
##
###############################################################################

package MCE::Hobo::_delay;

sub new {
   my ( $class, $delay ) = @_;

   if ( !defined $delay ) {
      $delay = ($^O =~ /mswin|mingw|msys|cygwin/i) ? 0.015 : 0.008;
   }

   bless [ $delay, Time::HiRes::time() + $delay ], $class;
}

sub seconds {
   my ( $self, $how_long ) = @_;
   my ( $time, $delay, $next ) = ( Time::HiRes::time(), @{ $self } );

   $how_long = 0.004007 if ( defined $how_long && $how_long < 0.004007 );
   my $adj = defined $how_long ? $how_long - $delay : 0;

   if ( $next + $adj > $time ) {
      $self->[1] += $delay + $adj;
      return $next + $adj - $time;
   }

   $self->[1] = $time + $delay + $adj;

   return 0;
}

package MCE::Hobo::_hash;

sub new    { bless {}, shift; }
sub clear  { %{ $_[0] } = (); }
sub exists { exists $_[0]->{ $_[1] }; }
sub set    { $_[0]->{ $_[1] } = $_[2]; }

package MCE::Hobo::_ordhash;

sub new    { my $gcnt = 0; bless [ {}, [], {}, \$gcnt ], shift; }
sub exists { exists $_[0]->[0]{ $_[1] }; }
sub get    { $_[0]->[0]{ $_[1] }; }
sub len    { scalar keys %{ $_[0]->[0] }; }

sub clear {
   %{ $_[0]->[0] } = @{ $_[0]->[1] } = %{ $_[0]->[2] } = ();
   ${ $_[0]->[3] } = 0;

   return;
}

sub del {
   my ( $data, $keys, $indx, $gcnt ) = @{ $_[0] };
   $keys->[ delete $indx->{ $_[1] } // return undef ] = undef;

   if ( ++${ $gcnt } > @{ $keys } * 0.667 ) {
      my $i; $i = ${ $gcnt } = 0;
      for my $k ( @{ $keys } ) {
         $keys->[ $i ] = $k, $indx->{ $k } = $i++ if ( defined $k );
      }
      splice @{ $keys }, $i;
   }

   delete $data->{ $_[1] };
}

sub set {
   my ( $key, $data, $keys, $indx ) = ( $_[1], @{ $_[0] } );
   $data->{ $key } = $_[2], $indx->{ $key } = @{ $keys };
   push @{ $keys }, "$key";

   return;
}

sub vals {
   my ( $self ) = @_;

   ${ $self->[3] }
      ? @{ $self->[0] }{ grep defined($_), @{ $self->[1] } }
      : @{ $self->[0] }{ @{ $self->[1] } };
}

###############################################################################
## ----------------------------------------------------------------------------
## Manager class.
##
###############################################################################

package MCE::Hobo::Manager;

use base 'MCE::Hobo';

sub AUTOLOAD {
   my $self = shift;
   my $FCN  = substr($MCE::Hobo::Manager::AUTOLOAD,
              rindex($MCE::Hobo::Manager::AUTOLOAD, ':') + 1);

   my $opt = $FCN;  $opt =~ s/^run_// or $opt =~ s/^set_//;

   if ( $opt eq 'max_workers' || $opt eq 'max_procs' ) {
      return MCE::Hobo::_max_workers($self, @_);
   }
   if ( $opt eq 'hobo_timeout' || $opt eq 'spawn_delay' ) {
      $self->{$opt} = $_[0] if ( @_ && $_[0] =~ /^[\d\.]+$/ );
      return $self->{$opt};
   }
   if ( $opt eq 'on_start' || $opt eq 'on_finish' ) {
      $self->{$opt} = $_[0] if ( @_ && ( ref $_[0] eq 'CODE' || !$_[0] ) );
      return $self->{$opt};
   }
   if ( $opt eq 'posix_exit' ) {
      $self->{$opt} = $_[0] if ( @_ && $_[0] =~ /^[01]$/ );
      return $self->{$opt};
   }

   MCE::Hobo::_croak(
      "Can't locate object method \"$FCN\" via package \"MCE::Hobo::Manager\""
   );
}

sub DESTROY {
   if ( $_[0]->{MGR_ID} eq "$$.$_tid" ) {
      MCE::Hobo::_force_reap( my $pkg = $_[0]->{PKG} );
      delete($_DELY->{$pkg}), delete($_DATA->{"$pkg:seed"}),
      delete($_LIST->{$pkg}), delete($_DATA->{"$pkg:id"}),
      delete($_MNGD->{$pkg}), delete($_DATA->{ $pkg });
   }
   return;
}

sub finish    { shift->exit( @_ ); }
sub is_child  { $_[0]->{CHILD} ? 1 : ''; }
sub is_parent { $_[0]->{CHILD} ? '' : 1; }

sub running_procs {
   return $_[0]->pending() unless wantarray;
   local $_; map { $_->pid } $_[0]->list();
}

sub start {
   ( @_ == 2 )
      ? shift->create( { no_fcn => 1, ident => shift } )
      : shift->create( { no_fcn => 1 } );
}

sub start_child {
   ( @_ >= 3 )
      ? shift->create( { ident => shift }, @_ )
      : shift->create( @_ );
}

sub wait_for_available_procs {
   my ( $self, $nbr ) = @_;
   my $max_workers = $self->{max_workers};

   $nbr ||= 1;  $nbr = 1 if ( $nbr < 0 );
   $nbr   = $max_workers if ( $nbr > $max_workers );
   $self->wait_one() until ( $max_workers - $self->pending() >= $nbr );

   return;
}

*reap_finished_children = \&MCE::Hobo::yield;
*wait_children = \&MCE::Hobo::yield; # compatibility
*wait_all_children = \&MCE::Hobo::wait_all;

sub set_waitpid_blocking_sleep { 0; } # no-op
sub waitpid_blocking_sleep     { 0; }

1;

__END__

###############################################################################
## ----------------------------------------------------------------------------
## Module usage.
##
###############################################################################

=head1 NAME

MCE::Hobo - A threads-like parallelization module with managed capabilities

=head1 VERSION

This document describes MCE::Hobo version 1.826

=head1 SYNOPSIS

   use MCE::Hobo;

   MCE::Hobo->create( sub { print "Hello from hobo\n" } )->join();

   sub parallel {
      my ($arg1) = @_;
      print "Hello again, $arg1\n" if defined($arg1);
      print "Hello again, $_\n"; # same thing
   }

   MCE::Hobo->create( \&parallel, $_ ) for 1 .. 3;

   my @hobos    = MCE::Hobo->list();
   my @running  = MCE::Hobo->list_running();
   my @joinable = MCE::Hobo->list_joinable();
   my @count    = MCE::Hobo->pending();

   # Joining is orderly, e.g. hobo1 is joined first, hobo2, hobo3.
   $_->join() for @hobos;

   # Joining occurs immediately as hobo(s) complete execution.
   1 while MCE::Hobo->wait_one();

   my $hobo = mce_async { foreach (@files) { ... } };

   $hobo->join();

   if ( my $err = $hobo->error() ) {
      warn "Hobo error: $err\n";
   }

   # Get a hobo's object
   $hobo = MCE::Hobo->self();

   # Get a hobo's ID
   $pid = MCE::Hobo->pid();  # $$
   $pid = $hobo->pid();
   $pid = MCE::Hobo->tid();  # tid is an alias for pid
   $pid = $hobo->tid();

   # Test hobo objects
   if ( $hobo1 == $hobo2 ) {
      ...
   }

   # Test for within the child or parent
   if ( MCE::Hobo->in_child() ) {
      ...
   }

   # Give other hobos a chance to run
   MCE::Hobo->yield();
   MCE::Hobo->yield(0.05);

   # Return context, wantarray aware
   my ($value1, $value2) = $hobo->join();
   my $value = $hobo->join();

   # Check hobo's state
   if ( $hobo->is_running() ) {
      sleep 1;
   }
   if ( $hobo->is_joinable() ) {
      $hobo->join();
   }

   # Send a signal to a hobo
   $hobo->kill('SIGUSR1');

   # Exit a hobo
   MCE::Hobo->exit();

=head1 DESCRIPTION

A Hobo is a migratory worker inside the machine that carries the
asynchronous gene. Hobos are equipped with C<threads>-like capability
for running code asynchronously. Unlike threads, each hobo is a unique
process to the underlying OS. The IPC is managed by C<MCE::Shared>,
which runs on all the major platforms including Cygwin.

An exception was made on the Windows platform to spawn threads versus
children in C<MCE::Hobo> 1.807 until 1.816. For consistency, the 1.817
release reverts back to spawning children on all supported platforms.

C<MCE::Hobo> may be used as a standalone or together with C<MCE>
including running alongside C<threads>.

   use MCE::Hobo;
   use MCE::Shared;

   # synopsis: head -20 file.txt | perl script.pl

   my $ifh = MCE::Shared->handle( "<", \*STDIN  );  # shared
   my $ofh = MCE::Shared->handle( ">", \*STDOUT );
   my $ary = MCE::Shared->array();

   sub parallel_task {
      my ( $id ) = @_;

      while ( <$ifh> ) {
         printf {$ofh} "[ %4d ] %s", $., $_;

       # $ary->[ $. - 1 ] = "[ ID $id ] read line $.\n" );  # dereferencing
         $ary->set( $. - 1, "[ ID $id ] read line $.\n" );  # faster via OO
      }
   }

   my $hobo1 = MCE::Hobo->new( "parallel_task", 1 );
   my $hobo2 = MCE::Hobo->new( \&parallel_task, 2 );
   my $hobo3 = MCE::Hobo->new( sub { parallel_task(3) } );

   $_->join for MCE::Hobo->list();  # ditto: MCE::Hobo->wait_all();

   # search array (total one round-trip via IPC)
   my @vals = $ary->vals( "val =~ / ID 2 /" );

   print {*STDERR} join("", @vals);

=head1 API DOCUMENTATION

=over 3

=item $hobo = MCE::Hobo->create( FUNCTION, ARGS )

=item $hobo = MCE::Hobo->new( FUNCTION, ARGS )

This will create a new hobo that will begin execution with function as the
entry point, and optionally ARGS for list of parameters. It will return the
corresponding MCE::Hobo object, or undef if hobo creation failed.

I<FUNCTION> may either be the name of a function, an anonymous subroutine, or
a code ref.

   my $hobo = MCE::Hobo->create( "func_name", ... );
       # or
   my $hobo = MCE::Hobo->create( sub { ... }, ... );
       # or
   my $hobo = MCE::Hobo->create( \&func, ... );

=item $hobo = MCE::Hobo->create( { options }, FUNCTION, ARGS )

Options may be specified via a hash structure. At this time, C<posix_exit> and
C<hobo_timeout> are the only options supported. Set C<posix_exit> to avoid all
END and destructor processing. Set C<hobo_timeout>, in number of seconds, if
you want the hobo process to terminate after some time. The default is C<0>
for no timeout.

Many modules on CPAN are not thread-safe nor safe to use with many processes.
The C<posix_exit> option must be set explicitly if your application is crashing,
due to a module with a C<DESTROY> or C<END> block not accounting for the process
ID C<$$.$tid> the object was constructed under: e.g. C<Cache::BDB>.

Constructing a Hobo inside a thread implies C<posix_exit => 1> or if present
CGI, FCGI, Coro, Curses, Gearman::Util, Gearman::XS, Mojo::IOLoop, Prima, Tk,
Wx, or Win32::GUI.

   my $hobo1 = MCE::Hobo->create( { posix_exit => 1 }, sub {
      ...
   } );

   $hobo1->join;

   my $hobo2 = MCE::Hobo->create( { hobo_timeout => 3 }, sub {
      sleep 1 for ( 1 .. 9 );
   } );

   $hobo2->join;

   if ( $hobo2->error() eq "Hobo timed out\n" ) {
      ...
   }

The C<new()> method is an alias for C<create()>.

=item mce_async { BLOCK } ARGS;

=item mce_async { BLOCK };

C<mce_async> runs the block asynchronously similarly to C<MCE::Hobo->create()>.
It returns the hobo object, or undef if hobo creation failed.

   my $hobo = mce_async { foreach (@files) { ... } };

   $hobo->join();

   if ( my $err = $hobo->error() ) {
      warn("Hobo error: $err\n");
   }

=item $hobo->join()

This will wait for the corresponding hobo to complete its execution. In
non-voided context, C<join()> will return the value(s) of the entry point
function.

The context (void, scalar or list) for the return value(s) for C<join> is
determined at the time of joining and mostly C<wantarray> aware.

   my $hobo1 = MCE::Hobo->create( sub {
      my @res = qw(foo bar baz);
      return (@res);
   });

   my @res1 = $hobo1->join();  # ( foo, bar, baz )
   my $res1 = $hobo1->join();  #   baz

   my $hobo2 = MCE::Hobo->create( sub {
      return 'foo';
   });

   my @res2 = $hobo2->join();  # ( foo )
   my $res2 = $hobo2->join();  #   foo

=item $hobo1->equal( $hobo2 )

Tests if two hobo objects are the same hobo or not. Hobo comparison is based
on process IDs. This is overloaded to the more natural forms.

    if ( $hobo1 == $hobo2 ) {
        print("Hobos are the same\n");
    }
    # or
    if ( $hobo1 != $hobo2 ) {
        print("Hobos differ\n");
    }

=item $hobo->error()

Hobos are executed in an C<eval> context. This method will return C<undef>
if the hobo terminates I<normally>. Otherwise, it returns the value of
C<$@> associated with the hobo's execution status in its C<eval> context.

=item $hobo->exit()

This sends C<'SIGQUIT'> to the hobo object, notifying hobo to exit. It returns
the hobo object to allow for method chaining. It is important to join later if
not immediately to not leave a zombie or defunct process.

   $hobo->exit()->join();

   ...

   $hobo->join();  # later

=item MCE::Hobo->exit()

A hobo can be exited at any time by calling C<MCE::Hobo->exit()>.
This behaves the same as C<exit(status)> when called from the main process.

=item MCE::Hobo->finish()

This class method is called automatically by C<END>, but may be called
explicitly. Two shared objects to C<MCE::Shared> are destroyed. An error is
emitted via croak if there are active hobos not yet joined.

   MCE::Hobo->create( 'task1', $_ ) for 1 .. 4;

   $_->join for MCE::Hobo->list();

   MCE::Hobo->create( 'task2', $_ ) for 1 .. 4;

   $_->join for MCE::Hobo->list();

   MCE::Hobo->create( 'task3', $_ ) for 1 .. 4;

   $_->join for MCE::Hobo->list();

   MCE::Hobo->finish();

=item MCE::Hobo->in_child()

Returns C<true> if within the child or a C<false> value if the parent.

Current API available since 1.827.

=item $hobo->is_running()

Returns true if a hobo is still running.

=item $hobo->is_joinable()

Returns true if the hobo has finished running and not yet joined.

=item $hobo->kill( 'SIG...' )

Sends the specified signal to the hobo. Returns the hobo object to allow for
method chaining. As with C<exit>, it is important to join eventually if not
immediately to not leave a zombie or defunct process.

   $hobo->kill('SIG...')->join();

The following is a parallel demonstration comparing C<MCE::Shared> against
C<Redis> and C<Redis::Fast> on a Fedora 23 VM. Joining begins after all
workers have been notified to quit.

   use Time::HiRes qw(time);

   use Redis;
   use Redis::Fast;

   use MCE::Hobo;
   use MCE::Shared;

   my $redis = Redis->new();
   my $rfast = Redis::Fast->new();
   my $array = MCE::Shared->array();

   sub parallel_redis {
      my ($_redis) = @_;
      my ($count, $quit, $len) = (0, 0);

      # instead, use a flag to exit loop
      $SIG{'QUIT'} = sub { $quit = 1 };

      while () {
         $len = $_redis->rpush('list', $count++);
         last if $quit;
      }

      $count;
   }

   sub parallel_array {
      my ($count, $quit, $len) = (0, 0);

      # do not exit from inside handler
      $SIG{'QUIT'} = sub { $quit = 1 };

      while () {
         $len = $array->push($count++);
         last if $quit;
      }

      $count;
   }

   sub benchmark_this {
      my ($desc, $num_hobos, $timeout, $code, @args) = @_;
      my ($start, $total) = (time(), 0);

      MCE::Hobo->new($code, @args) for 1..$num_hobos;
      sleep $timeout;

      # joining is not immediate; ok
      $_->kill('QUIT') for MCE::Hobo->list();

      # joining later; ok
      $total += $_->join() for MCE::Hobo->list();

      printf "$desc <> duration: %0.03f secs, count: $total\n",
         time() - $start;

      sleep 0.2;
   }

   benchmark_this('Redis      ', 8, 5.0, \&parallel_redis, $redis);
   benchmark_this('Redis::Fast', 8, 5.0, \&parallel_redis, $rfast);
   benchmark_this('MCE::Shared', 8, 5.0, \&parallel_array);

=item MCE::Hobo->list()

Returns a list of all hobos not yet joined.

   @hobos = MCE::Hobo->list();

=item MCE::Hobo->list_running()

Returns a list of all hobos that are still running.

   @hobos = MCE::Hobo->list_running();

=item MCE::Hobo->list_joinable()

Returns a list of all hobos that have completed running. Thus, ready to be
joined without blocking.

   @hobos = MCE::Hobo->list_joinable();

=item MCE::Hobo->pending()

Returns a count of all hobos not yet joined.

   $count = MCE::Hobo->pending();

=item $hobo->result()

Returns the result obtained by C<join>, C<wait_one>, or C<wait_all>. If the
process has not yet exited, waits for the corresponding hobo to complete its
execution.

   use MCE::Hobo;
   use Time::HiRes qw(sleep);

   sub task {
      my ($id) = @_;
      sleep $id * 0.333;
      return $id;
   }

   MCE::Hobo->create('task', $_) for ( reverse 1 .. 3 );

   # 1 while MCE::Hobo->wait_one();

   while ( my $hobo = MCE::Hobo->wait_one() ) {
      my $err = $hobo->error() // 'no error';
      my $res = $hobo->result();
      my $pid = $hobo->pid();

      print "[$pid] $err : $res\n";
   }

Like C<join> described above, the context (void, scalar or list) for the
return value(s) is determined at the time C<result> is called and mostly
C<wantarray> aware.

   my $hobo1 = MCE::Hobo->create( sub {
      my @res = qw(foo bar baz);
      return (@res);
   });

   my @res1 = $hobo1->result();  # ( foo, bar, baz )
   my $res1 = $hobo1->result();  #   baz

   my $hobo2 = MCE::Hobo->create( sub {
      return 'foo';
   });

   my @res2 = $hobo2->result();  # ( foo )
   my $res2 = $hobo2->result();  #   foo

=item MCE::Hobo->self()

Class method that allows a hobo to obtain it's own I<MCE::Hobo> object.

=item $hobo->pid()

=item $hobo->tid()

Returns the ID of the hobo.

   pid: $$  process id
   tid: $$  alias for pid

=item MCE::Hobo->pid()

=item MCE::Hobo->tid()

Class methods that allows a hobo to obtain its own ID.

   pid: $$  process id
   tid: $$  alias for pid

=item MCE::Hobo->wait_one()

=item MCE::Hobo->wait_all()

Meaningful for the manager process only, waits for one or all hobos to
complete execution. Afterwards, returns the corresponding hobo(s).
If a hobo doesn't exist, returns the C<undef> value or an empty list
for C<wait_one> and C<wait_all> respectively.

The C<waitone> and C<waitall> methods are aliases since 1.827 for
backwards compatibility.

   use MCE::Hobo;
   use Time::HiRes qw(sleep);

   sub task {
      my $id = shift;
      sleep $id * 0.333;
      return $id;
   }

   MCE::Hobo->create('task', $_) for ( reverse 1 .. 3 );

   # join, traditional use case
   $_->join() for MCE::Hobo->list();

   # wait_one, simplistic use case
   1 while MCE::Hobo->wait_one();

   # wait_one
   while ( my $hobo = MCE::Hobo->wait_one() ) {
      my $err = $hobo->error() // 'no error';
      my $res = $hobo->result();
      my $pid = $hobo->pid();

      print "[$pid] $err : $res\n";
   }

   # wait_all
   my @hobos = MCE::Hobo->wait_all();

   for ( @hobos ) {
      my $err = $_->error() // 'no error';
      my $res = $_->result();
      my $pid = $_->pid();

      print "[$pid] $err : $res\n";
   }

=item MCE::Hobo->yield( [ floating_seconds ] )

Prior API till 1.826.

Let this hobo yield CPU time to other hobos. By default, the class method calls
C<sleep(0.008)> on UNIX and C<sleep(0.015)> on Windows including Cygwin.

   MCE::Hobo->yield();
   MCE::Hobo->yield(0.05);

   # total run time: 0.25 seconds, sleep occurs in parallel

   MCE::Hobo->create( sub { MCE::Hobo->yield(0.25) } ) for 1 .. 4;
   MCE::Hobo->wait_all();

Current API available since 1.827.

Give other hobos a chance to run, optionally for given time. Yield behaves
similarly to MCE's interval option for throttling hobos from running too
fast. Another use case is preventing hobos from initiating many connections
simultaneously. See the next section for fetching URLs in parallel.

   # total run time: 1.00 second

   MCE::Hobo->create( sub { MCE::Hobo->yield(0.25) } ) for 1 .. 4;
   MCE::Hobo->wait_all();

=back

=head1 PARALLEL HTTP REQUESTS USING ANYEVENT

This demonstration constructs two queues, two handles, starts the
shared-manager process if needed, and spawns four hobo workers.
For this demonstration, am chunking 64 URLs per job. In reality,
one may run with 200 workers and chunk 300 URLs on a 24-way box.

 # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 # perl demo.pl              -- all output
 # perl demo.pl  >/dev/null  -- mngr/hobo output
 # perl demo.pl 2>/dev/null  -- show results only
 #
 # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 use strict;
 use warnings;

 use AnyEvent;
 use AnyEvent::HTTP;
 use Time::HiRes qw( time );

 use MCE::Hobo;
 use MCE::Shared;

 my $que = MCE::Shared->queue();
 my $ret = MCE::Shared->queue();

 # Construct shared handles to serialize output from many hobos
 # writing simultaneously. This prevents garbled output.

 mce_open my $OUT, ">>", \*STDOUT or die "open error: $!";
 mce_open my $ERR, ">>", \*STDERR or die "open error: $!";

 # Must start the shared-server manually when Perl lacks IO::FDPass.
 # This restriction applies to MCE::Shared::{ Condvar,Handle,Queue }.
 # MCE::Shared loads IO::FDPass automatically if available.

 MCE::Shared->start() unless $INC{'IO/FDPass.pm'};

 # Spawn workers early for minimum memory consumption.

 MCE::Hobo->create({ posix_exit => 1 }, 'task', $_) for 1 .. 4;

 # Obtain or generate input data for hobos to process.

 my ( $count, @urls ) = ( 0 );

 push @urls, map { "http://127.0.0.$_/"   } 1..254;
 push @urls, map { "http://192.168.0.$_/" } 1..254; # 508 URLs total

 while ( @urls ) {
     my @chunk = splice(@urls, 0, 64);
     $que->enqueue( { ID => ++$count, INPUT => \@chunk } );
 }

 # So that workers leave the loop after consuming the queue.

 $que->end();

 # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 # Loop for the manager process. The manager may do other work if
 # need be and periodically check $ret->pending() not shown here.
 #
 # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 my $start = time;

 printf {$ERR} "Mngr - entering loop\n";

 while ( $count ) {
     my ( $result, $failed ) = $ret->dequeue( 2 );

     # Remove ID from result, so not treated as a URL item.

     printf {$ERR} "Mngr - received job %s\n", delete $result->{ID};

     # Display the URL and the size captured.

     foreach my $url ( keys %{ $result } ) {
         printf {$OUT} "%s: %d\n", $url, length($result->{$url})
             if $result->{$url};  # url has content
     }

     # Display URLs the hobo worker could not reach.

     if ( @{ $failed } ) {
         foreach my $url ( @{ $failed } ) {
             print {$OUT} "Failed: $url\n";
         }
     }

     # Decrement the count.

     $count--;
 }

 MCE::Hobo->wait_all();

 printf {$ERR} "Mngr - exiting loop\n\n";
 printf {$ERR} "Duration: %0.3f seconds\n\n", time - $start;

 exit;

 # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
 # Hobos enqueue two items ( $result and $failed ) per each job
 # for the manager process. Likewise, the manager process dequeues
 # two items above. Optionally, Hobos add ID to result.
 #
 # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

 sub task {
     my ( $id ) = @_;
     printf {$ERR} "Hobo $id entering loop\n";

     while ( my $job = $que->dequeue() ) {
         my ( $result, $failed ) = ( { ID => $job->{ID} }, [ ] );

         # Walk URLs, provide a hash and array refs for data.

         printf {$ERR} "Hobo $id running  job $job->{ID}\n";
         walk( $job, $result, $failed );

         # Send results to the manager process.

         $ret->enqueue( $result, $failed );
     }

     printf {$ERR} "Hobo $id exiting loop\n";
 }

 sub walk {
     my ( $job, $result, $failed ) = @_;

     # Yielding is critical when running an event loop in parallel.
     # Not doing so means that the app may reach contention points
     # with the firewall and likely impose unnecessary hardship at
     # the OS level. The idea here is not to have multiple workers
     # initiate HTTP requests to a batch of URLs at the same time.
     # Yielding in 1.827+ behaves more like scatter for the worker
     # to run solo in a fraction of time.

     MCE::Hobo->yield( 0.03 );   # MCE::Hobo 1.827

     my $cv = AnyEvent->condvar();

     # Populate the hash ref for URLs it could reach.
     # Do not mix AnyEvent timeout and Hobo timeout.
     # Choose to do the event timeout if available.

     foreach my $url ( @{ $job->{INPUT} } ) {
         $cv->begin();

         http_get $url, timeout => 2, sub {
             my ( $data, $headers ) = @_;
             $result->{$url} = $data;
             $cv->end();
         };
     }

     $cv->recv();

     # Populate the array ref for URLs it could not reach.

     foreach my $url ( @{ $job->{INPUT} } ) {
         push @{ $failed }, $url unless (exists $result->{ $url });
     }

     return;
 }

 __END__

 $ perl demo.pl

 Hobo 1 entering loop
 Hobo 2 entering loop
 Hobo 3 entering loop
 Mngr - entering loop
 Hobo 2 running  job 2
 Hobo 3 running  job 3
 Hobo 1 running  job 1
 Hobo 4 entering loop
 Hobo 4 running  job 4
 Hobo 2 running  job 5
 Mngr - received job 2
 Hobo 3 running  job 6
 Mngr - received job 3
 Hobo 1 running  job 7
 Mngr - received job 1
 Hobo 4 running  job 8
 Mngr - received job 4
 http://192.168.0.1/: 3729
 Hobo 2 exiting loop
 Mngr - received job 5
 Hobo 3 exiting loop
 Mngr - received job 6
 Hobo 1 exiting loop
 Mngr - received job 7
 Hobo 4 exiting loop
 Mngr - received job 8
 Mngr - exiting loop

 Duration: 4.131 seconds

=head1 CROSS-PLATFORM TEMPLATE FOR BINARY EXECUTABLE

Making an executable is possible with the L<PAR::Packer> module.
On the Windows platform, threads, threads::shared, and exiting via
threads are all necessary for the binary to exit successfully.

   # https://metacpan.org/pod/PAR::Packer
   # https://metacpan.org/pod/pp
   #
   #   pp -o demo.exe demo.pl
   #   ./demo.exe

   use strict;
   use warnings;

   use if $^O eq "MSWin32", "threads";
   use if $^O eq "MSWin32", "threads::shared";

   use Time::HiRes (); # include minimum dependencies for MCE::Hobo
   use Storable ();

   use IO::FDPass ();  # optional: for MCE::Shared->condvar, handle, queue
   use Sereal ();      # optional: faster serialization, may omit Storable

   use MCE::Hobo;      # 1.808 or later on Windows
   use MCE::Shared;

   my $seq_a = MCE::Shared->sequence( 1, 30 );

   sub task {
      my ( $id ) = @_;
      while ( defined ( my $num = $seq_a->next() ) ) {
         print "$id: $num\n";
      }
   }

   MCE::Hobo->new( \&task, $_ ) for 1 .. 2;
   MCE::Hobo->wait_all();

   threads->exit(0) if $INC{"threads.pm"};

=head1 CREDITS

The inspiration for C<MCE::Hobo> comes from wanting C<threads>-like behavior
for processes. Both can run side-by-side including safe-use by MCE workers.
Likewise, the documentation resembles C<threads>.

The inspiration for C<wait_all> and C<wait_one> comes from the
C<Parallel::WorkUnit> module.

=head1 SEE ALSO

=over 3

=item * L<forks>

=item * L<forks::BerkeleyDB>

=item * L<Parallel::ForkManager>

=item * L<Parallel::Loops>

=item * L<Parallel::Prefork>

=item * L<Parallel::WorkUnit>

=item * L<Proc::Fork>

=item * L<Thread::Tie>

=item * L<threads>

=back

=head1 INDEX

L<MCE|MCE>, L<MCE::Shared>

=head1 AUTHOR

Mario E. Roy, S<E<lt>marioeroy AT gmail DOT comE<gt>>

=cut

