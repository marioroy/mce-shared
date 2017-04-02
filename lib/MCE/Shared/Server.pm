###############################################################################
## ----------------------------------------------------------------------------
## Server/Object packages for MCE::Shared.
##
###############################################################################

package MCE::Shared::Server;

use strict;
use warnings;

use 5.010001;

no warnings qw( threads recursion uninitialized numeric once );

our $VERSION = '1.821';

## no critic (BuiltinFunctions::ProhibitStringyEval)
## no critic (Subroutines::ProhibitExplicitReturnUndef)
## no critic (TestingAndDebugging::ProhibitNoStrict)
## no critic (InputOutput::ProhibitTwoArgOpen)

use Carp ();
use Time::HiRes qw( sleep time );
use Scalar::Util qw( blessed weaken );
use Socket qw( SOL_SOCKET SO_RCVBUF );
use bytes;

no overloading;

my ($_has_threads, $_spawn_child, $_freeze, $_thaw);

BEGIN {
   local $@; local $SIG{__DIE__};

   if ($^O eq 'MSWin32' && !$INC{'threads.pm'}) {
      eval 'use threads; use threads::shared';
   }
   elsif ($INC{'threads.pm'} && !$INC{'threads/shared.pm'}) {
      eval 'use threads::shared';
   }

   $_has_threads = $INC{'threads.pm'} ? 1 : 0;
   $_spawn_child = $_has_threads ? 0 : 1;

   eval 'use IO::FDPass' if !$INC{'IO/FDPass.pm'} && $^O ne 'cygwin';
   eval 'PDL::no_clone_skip_warning()' if $INC{'PDL.pm'};
   eval 'use PDL::IO::Storable' if $INC{'PDL.pm'};

   if (!exists $INC{'PDL.pm'}) {
      eval '
         use Sereal::Encoder 3.015 qw( encode_sereal );
         use Sereal::Decoder 3.015 qw( decode_sereal );
      ';
      if ( !$@ ) {
         my $_encoder_ver = int( Sereal::Encoder->VERSION() );
         my $_decoder_ver = int( Sereal::Decoder->VERSION() );
         if ( $_encoder_ver - $_decoder_ver == 0 ) {
            $_freeze = sub { encode_sereal( @_, { freeze_callbacks => 1 } ) };
            $_thaw   = \&decode_sereal;
         }
      }
   }

   if (!defined $_freeze) {
      require Storable;
      $_freeze = \&Storable::freeze;
      $_thaw   = \&Storable::thaw;
   }

   return;
}

use MCE::Util ();
use MCE::Mutex;

use constant {
   # Max data channels. This cannot be greater than 8 on MSWin32.
   DATA_CHANNELS => ($^O eq 'MSWin32') ? 8 : 12,

   MAX_DQ_DEPTH  => 192,  # Maximum dequeue notifications
   WA_ARRAY      => 1,    # Wants list

   SHR_M_NEW => 'M~NEW',  # New share
   SHR_M_CID => 'M~CID',  # ClientID request
   SHR_M_DEE => 'M~DEE',  # Deeply shared
   SHR_M_INC => 'M~INC',  # Increment count
   SHR_M_OBJ => 'M~OBJ',  # Object request
   SHR_M_OB0 => 'M~OB0',  # Object request - thaw'less
   SHR_M_OB1 => 'M~OB1',  # Object request - thaw'less
   SHR_M_OB2 => 'M~OB2',  # Object request - thaw'less
   SHR_M_OB3 => 'M~OB3',  # Object request - thaw'less
   SHR_M_DES => 'M~DES',  # Destroy request
   SHR_M_EXP => 'M~EXP',  # Export request
   SHR_M_INX => 'M~INX',  # Iterator next
   SHR_M_IRW => 'M~IRW',  # Iterator rewind
   SHR_M_STP => 'M~STP',  # Stop server

   SHR_O_CVB => 'O~CVB',  # Condvar broadcast
   SHR_O_CVS => 'O~CVS',  # Condvar signal
   SHR_O_CVT => 'O~CVT',  # Condvar timedwait
   SHR_O_CVW => 'O~CVW',  # Condvar wait
   SHR_O_CLO => 'O~CLO',  # Handle CLOSE
   SHR_O_OPN => 'O~OPN',  # Handle OPEN
   SHR_O_REA => 'O~REA',  # Handle READ
   SHR_O_RLN => 'O~RLN',  # Handle READLINE
   SHR_O_PRI => 'O~PRI',  # Handle PRINT
   SHR_O_WRI => 'O~WRI',  # Handle WRITE
   SHR_O_QUA => 'O~QUA',  # Queue await
   SHR_O_QUD => 'O~QUD',  # Queue dequeue
   SHR_O_QUN => 'O~QUN',  # Queue dequeue non-blocking
   SHR_O_PDL => 'O~PDL',  # PDL::ins inplace(this),what,coords
   SHR_O_FCH => 'O~FCH',  # A,H,OH,S FETCH
   SHR_O_CLR => 'O~CLR',  # A,H,OH CLEAR
};

###############################################################################
## ----------------------------------------------------------------------------
## Private functions.
##
###############################################################################

my ($_SVR, %_all, %_obj, %_ob2, %_ob3, %_itr, %_new) = (undef);
my ($_next_id, $_is_client, $_init_pid, $_svr_pid) = (0, 1);
my $LF = "\012"; Internals::SvREADONLY($LF, 1);

my $_is_MSWin32 = ($^O eq 'MSWin32') ? 1 : 0;
my $_tid = $_has_threads ? threads->tid() : 0;
my $_oid = "$$.$_tid";

my %_iter_allow = (qw/
   MCE::Shared::Array   1
   MCE::Shared::Hash    1
   MCE::Shared::Ordhash 1
   Hash::Ordered        1
/);

sub _croak { goto &Carp::croak }
sub  CLONE { $_tid = threads->tid() if $_has_threads }

END {
   MCE::Hobo->finish('MCE') if $INC{'MCE/Hobo.pm'};

   _stop();
}

{
   my $_handler_cnt : shared = 0;

   sub _trap {
      my $_sig_name = $_[0];
      $MCE::Shared::Server::KILLED = 1;

      $SIG{INT} = $SIG{__DIE__} = $SIG{__WARN__} = $SIG{$_[0]} = sub { };
      lock $_handler_cnt if $INC{'threads/shared.pm'};

      if (++$_handler_cnt == 1) {
         CORE::kill($_sig_name, $_is_MSWin32 ? -$$ : -getpgrp);

         if ($_sig_name eq 'PIPE') {
            for my $_i (1..2) { sleep 0.015 }
         } else {
            for my $_i (1..3) { sleep 0.060 }
         }

         CORE::kill('QUIT', $_is_MSWin32 ? -$$ : -getpgrp)
            if ($_sig_name eq 'PIPE' && $INC{'MCE/Hobo.pm'});

         ($_is_MSWin32)
            ? CORE::kill('KILL', -$$, $$)
            : CORE::kill('INT', -getpgrp);

         CORE::kill('KILL', -$$, $$)
            if ($_sig_name ne 'PIPE' && $INC{'MCE/Hobo.pm'});
      }

      for my $_i (1..5) { sleep 0.060 }

      CORE::exit($?);
   }
}

sub _new {
   my ($_class, $_deeply, %_hndls) = ($_[0]->{class}, $_[0]->{_DEEPLY_});

   unless ($_svr_pid) {
      # Minimum support for environments without IO::FDPass.
      # Must share Condvar and Queue before others.
      return _share(@_)
         if (!$INC{'IO/FDPass.pm'} && $_class =~
               /^MCE::Shared::(?:Condvar|Queue)$/
         );
      _start();
   }

   if ($_class =~ /^MCE::Shared::(?:Condvar|Queue)$/) {
      if (!$INC{'IO/FDPass.pm'}) {
         _croak(
            "\nSharing a $_class object while the server is running\n" .
            "requires the IO::FDPass module.\n\n"
         );
      }
      for my $_k (qw(
         _qw_sock _qr_sock _aw_sock _ar_sock _cw_sock _cr_sock _mutex
         _mutex_0 _mutex_1 _mutex_2 _mutex_3 _mutex_4 _mutex_5
      )) {
         if (defined $_[1]->{ $_k }) {
            $_hndls{ $_k } = delete $_[1]->{ $_k };
            $_[1]->{ $_k } = undef;
         }
      }
   }

   my ($_buf, $_id, $_len);

   my $_chn = ($_has_threads)
      ? $_tid % $_SVR->{_data_channels} + 1
      : abs($$) % $_SVR->{_data_channels} + 1;

   my $_DAT_LOCK   = $_SVR->{'_mutex_'.$_chn};
   my $_DAT_W_SOCK = $_SVR->{_dat_w_sock}[0];
   my $_DAU_W_SOCK = $_SVR->{_dat_w_sock}[$_chn];

   local $\ = undef if (defined $\);
   local $/ = $LF if ($/ ne $LF);

   $_DAT_LOCK->lock();
   print {$_DAT_W_SOCK} SHR_M_NEW.$LF . $_chn.$LF;

   $_buf = $_freeze->(shift);  print {$_DAU_W_SOCK} length($_buf).$LF, $_buf;
   $_buf = $_freeze->([ @_ ]); print {$_DAU_W_SOCK} length($_buf).$LF, $_buf;
   undef $_buf;

   print {$_DAU_W_SOCK} (keys %_hndls ? 1 : 0).$LF;
   <$_DAU_W_SOCK>;

   if (keys %_hndls) {
      for my $_k (qw( _qw_sock _qr_sock _aw_sock _cw_sock )) {
         if (exists $_hndls{ $_k }) {
            IO::FDPass::send( fileno $_DAU_W_SOCK, fileno $_hndls{ $_k } );
            <$_DAU_W_SOCK>;
         }
      }
   }

   chomp($_id = <$_DAU_W_SOCK>);
   if (keys %_hndls) {
      $_all{ $_id } = $_class;
      $_obj{ $_id } = \%_hndls;
   }

   chomp($_len = <$_DAU_W_SOCK>);
   read $_DAU_W_SOCK, $_buf, $_len;
   $_DAT_LOCK->unlock();

   unless ($_deeply) {
      # for auto-destroy
      $_new{ $_id } = $_has_threads ? $$ .'.'. $_tid : $$;
   }

   return $_thaw->($_buf);
}

sub _incr_count {
   return unless $_svr_pid;

   my $_chn = ($_has_threads)
      ? $_tid % $_SVR->{_data_channels} + 1
      : abs($$) % $_SVR->{_data_channels} + 1;

   my $_DAT_LOCK   = $_SVR->{'_mutex_'.$_chn};
   my $_DAT_W_SOCK = $_SVR->{_dat_w_sock}[0];
   my $_DAU_W_SOCK = $_SVR->{_dat_w_sock}[$_chn];

   local $\ = undef if (defined $\);
   local $/ = $LF if ($/ ne $LF);

   $_DAT_LOCK->lock();
   print {$_DAT_W_SOCK} SHR_M_INC.$LF . $_chn.$LF;
   print {$_DAU_W_SOCK} $_[0].$LF;
   <$_DAU_W_SOCK>;

   $_DAT_LOCK->unlock();

   return;
}

sub _share {
   my ($_params, $_item) = (shift, shift);
   my ($_id, $_class) = (++$_next_id, delete $_params->{'class'});

   if ($_class eq ':construct_pdl:') {
      local $@; local $SIG{__DIE__};

      $_class = 'PDL', $_item = eval q{
         use PDL; my $_func = pop @{ $_item };

         if    ($_func eq 'byte'    ) { byte     (@{ $_item }) }
         elsif ($_func eq 'short'   ) { short    (@{ $_item }) }
         elsif ($_func eq 'ushort'  ) { ushort   (@{ $_item }) }
         elsif ($_func eq 'long'    ) { long     (@{ $_item }) }
         elsif ($_func eq 'longlong') { longlong (@{ $_item }) }
         elsif ($_func eq 'float'   ) { float    (@{ $_item }) }
         elsif ($_func eq 'double'  ) { double   (@{ $_item }) }
         elsif ($_func eq 'ones'    ) { ones     (@{ $_item }) }
         elsif ($_func eq 'sequence') { sequence (@{ $_item }) }
         elsif ($_func eq 'zeroes'  ) { zeroes   (@{ $_item }) }
         elsif ($_func eq 'indx'    ) { indx     (@{ $_item }) }
         else                         { pdl      (@{ $_item }) }
      };
   }

   $_all{ $_id } = $_class; $_ob3{ "$_id:count" } = 1;

   if ($_class eq 'MCE::Shared::Handle') {
      require Symbol unless $INC{'Symbol.pm'};
      $_obj{ $_id } = Symbol::gensym();
      bless $_obj{ $_id }, 'MCE::Shared::Handle';
   }
   else {
      $_obj{ $_id } = $_item;
   }

   my $self = bless [ $_id, $_class ], 'MCE::Shared::Object';
   $_ob2{ $_id } = $_freeze->($self);

   if ( my $_code = $_obj{ $_id }->can('_shared_init') ) {
      $_code->($_obj{ $_id });
   }

   return $self;
}

sub _start {
   return if $_svr_pid;

   $SIG{HUP} = $SIG{INT} = $SIG{PIPE} = $SIG{QUIT} = $SIG{TERM} = \&_trap
      if (!$_is_MSWin32 && !$INC{'MCE/Signal.pm'} && !$INC{'threads.pm'});

   $_init_pid = "$$.$_tid"; local $_;

   my $_data_channels = ($_oid eq $_init_pid) ? DATA_CHANNELS : 4;
   $_SVR = { _data_channels => $_data_channels };

   MCE::Util::_sock_pair($_SVR, qw(_dat_r_sock _dat_w_sock), $_)
      for (0 .. $_data_channels);
   $_SVR->{'_mutex_'.$_} = MCE::Mutex->new( impl => 'Channel' )
      for (1 .. $_data_channels);

   setsockopt($_SVR->{_dat_r_sock}[0], SOL_SOCKET, SO_RCVBUF, 4096)
      if ($^O ne 'aix' && $^O ne 'linux');

   MCE::Shared::Object::_start();

   if ($_spawn_child) {
      $_svr_pid = fork();
      _loop() if (defined $_svr_pid && $_svr_pid == 0);
   }
   else {
      $_svr_pid = threads->create(\&_loop);
      $_svr_pid->detach() if (defined $_svr_pid);
   }

   _croak("cannot start the shared-manager process: $!")
      unless (defined $_svr_pid);

   return;
}

sub _stop {
   return unless ($_is_client && $_init_pid && $_init_pid eq "$$.$_tid");

   return if ($INC{'MCE/Signal.pm'} && $MCE::Signal::KILLED);
   return if ($MCE::Shared::Server::KILLED);

   MCE::Hobo->finish('MCE') if $INC{'MCE/Hobo.pm'};

   local ($!, $?); %_all = (), %_obj = ();

   if (defined $_svr_pid) {
      my $_DAT_W_SOCK = $_SVR->{_dat_w_sock}[0];

      local $\ = undef if (defined $\);
      local $/ = $LF if ($/ ne $LF);

      print {$_DAT_W_SOCK} SHR_M_STP.$LF.'0'.$LF;  <$_DAT_W_SOCK>;
      ref $_svr_pid ? sleep(0.09) : waitpid($_svr_pid, 0);

      MCE::Util::_destroy_socks($_SVR, qw( _dat_w_sock _dat_r_sock ));

      for my $_i (1 .. $_SVR->{_data_channels}) {
         delete $_SVR->{'_mutex_'.$_i};
      }

      MCE::Shared::Object::_stop();

      $_init_pid = $_svr_pid = undef;
   }

   return;
}

sub _destroy {
   my ($_lkup, $_item, $_id) = @_;

   # safety for circular references to not destroy dangerously
   return if exists $_ob3{ "$_id:count" } && --$_ob3{ "$_id:count" } > 0;

   # safety for circular references to not loop endlessly
   return if exists $_lkup->{ $_id };

   $_lkup->{ $_id } = 1;

   if (exists $_ob3{ "$_id:deeply" }) {
      for my $_oid (keys %{ $_ob3{ "$_id:deeply" } }) {
         _destroy($_lkup, $_obj{ $_oid }, $_oid);
      }
      delete $_ob3{ "$_id:deeply" };
   }
   elsif ($_all{ $_id } eq 'MCE::Shared::Scalar') {
      if (blessed($_item->get())) {
         my $_oid = $_item->get()->SHARED_ID();
         _destroy($_lkup, $_obj{ $_oid }, $_oid);
      }
      undef ${ $_obj{ $_id } };
   }
   elsif ($_all{ $_id } eq 'MCE::Shared::Handle') {
      close $_obj{ $_id } if defined(fileno($_obj{ $_id }));
   }

   weaken( delete $_obj{ $_id } ) if ( exists $_obj{ $_id } );
   weaken( delete $_itr{ $_id } ) if ( exists $_itr{ $_id } );

   delete($_ob2{ $_id }), delete($_ob3{ "$_id:count" }),
   delete($_all{ $_id }), delete($_itr{ "$_id:args"  });

   return;
}

###############################################################################
## ----------------------------------------------------------------------------
## Server loop.
##
###############################################################################

sub _exit {
   $SIG{__DIE__}  = sub { } unless $_tid;
   $SIG{__WARN__} = sub { };

   # Wait for the main thread to exit.
   sleep 3.0 if ( $_is_MSWin32 || ($_has_threads && $INC{'Tk.pm'}) );

   threads->exit(0) if ( !$_spawn_child || ($_has_threads && $_is_MSWin32) );

   CORE::kill('KILL', $$);
}

sub _loop {
   $_is_client = 0;

   $SIG{HUP} = $SIG{INT} = $SIG{QUIT} = $SIG{TERM} = sub {
      $SIG{INT} = $SIG{$_[0]} = sub { };

      CORE::kill($_[0], $_is_MSWin32 ? -$$ : -getpgrp);
      for my $_i (1..15) { sleep 0.060 }

      CORE::kill('KILL', $$);
      CORE::exit(255);
   };

   $SIG{PIPE} = sub {
      $SIG{PIPE} = $SIG{INT} = $SIG{QUIT} = $SIG{TERM} = sub { };

      for my $_o ( values %_obj ) {
         close $_o if ref($_o) eq 'MCE::Shared::Handle' && defined fileno($_o);
      }

      CORE::kill('PIPE', $_is_MSWin32 ? -$$ : -getpgrp), _exit();
   };

   my $_running_inside_eval = $^S;

   $SIG{__DIE__} = sub {
      if (!defined $^S || $^S) {
         if ( ($INC{'threads.pm'} && threads->tid() != 0) ||
               $ENV{'PERL_IPERL_RUNNING'} ||
               $_running_inside_eval
         ) {
            # thread env or running inside IPerl, check stack trace
            my $_t = Carp::longmess(); $_t =~ s/\teval [^\n]+\n$//;
            if ( $_t =~ /^(?:[^\n]+\n){1,7}\teval / ||
                 $_t =~ /\n\teval [^\n]+\n\t(?:eval|Try)/ )
            {
               CORE::die(@_);
            }
         }
         else {
            # normal env, trust $^S
            CORE::die(@_);
         }
      }

      $SIG{INT} = $SIG{__DIE__} = $SIG{__WARN__} = sub { };
      my $_die_msg = (defined $_[0]) ? $_[0] : '';
      print {*STDERR} $_die_msg;

      CORE::kill('INT', $_is_MSWin32 ? -$$ : -getpgrp);

      ($_spawn_child && !$_is_MSWin32)
         ? CORE::kill('KILL', $$)
         : CORE::exit($?);
   };

   local $\ = undef; local $/ = $LF; $| = 1;

   my ($_id, $_fn, $_wa, $_key, $_len, $_le2, $_le3, $_ret, $_func);
   my ($_DAU_R_SOCK, $_CV, $_Q, $_cnt, $_pending, $_t, $_frozen);
   my ($_client_id, $_done) = (0, 0);

   my $_DAT_R_SOCK = $_SVR->{_dat_r_sock}[0];
   my $_channels   = $_SVR->{_dat_r_sock};

   my $_warn0 = sub {
      if ( $_wa ) {
         my $_buf = $_freeze->([ ]);
         print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
      }
   };

   my $_warn1 = sub {
      warn "Can't locate object method \"$_[0]\" via package \"$_[1]\"\n";
      if ( $_wa ) {
         my $_buf = $_freeze->([ ]);
         print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
      }
   };

   my $_warn2 = sub {
      warn "Can't locate object method \"$_[0]\" via package \"$_[1]\"\n";
   };

   my $_fetch = sub {
      if ( ref($_[0]) ) {
         my $_buf = ( blessed($_[0]) && $_[0]->can('SHARED_ID') )
            ? $_ob2{ $_[0]->[0] } || $_freeze->($_[0])
            : $_freeze->($_[0]);
         print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
      }
      elsif ( defined $_[0] ) {
         print {$_DAU_R_SOCK} length($_[0]).'0'.$LF, $_[0];
      }
      else {
         print {$_DAU_R_SOCK} '-1'.$LF;
      }

      return;
   };

   my $_iterator = sub {
      if (!exists $_itr{ $_id }) {

         # MCE::Shared::{ Array, Hash, Ordhash }, Hash::Ordered
         if ($_iter_allow{ $_all{ $_id } } && $_obj{ $_id }->can('keys')) {
            my @_keys = ( exists $_itr{ "$_id:args" } )
               ? $_obj{ $_id }->keys( @{ $_itr{ "$_id:args" } } )
               : $_obj{ $_id }->keys;

            $_itr{ $_id } = sub {
               my $_key = shift @_keys;
               if ( !defined $_key ) {
                  print {$_DAU_R_SOCK} '-1'.$LF;
                  return;
               }
               my $_buf = $_freeze->([ $_key, $_obj{ $_id }->get($_key) ]);
               print {$_DAU_R_SOCK} length($_buf).$LF, $_buf;
            };
         }

         # Not supported
         else {
            print {$_DAU_R_SOCK} '-1'.$LF;
            return;
         }
      }

      $_itr{ $_id }->();

      return;
   };

   # --------------------------------------------------------------------------

   my %_output_function = (

      SHR_M_NEW.$LF => sub {                      # New share
         my ($_buf, $_params, $_class, $_args, $_fd, $_item);

         chomp($_len = <$_DAU_R_SOCK>);
         read $_DAU_R_SOCK, $_buf, $_len;

         $_params = $_thaw->($_buf);
         $_class  = $_params->{'class'};

         if (!exists $INC{ join('/',split(/::/,$_class)).'.pm' }) {
            local $@; local $SIG{__DIE__};
            eval "use $_class ()";
         }

         chomp($_len = <$_DAU_R_SOCK>);
         read $_DAU_R_SOCK, $_buf, $_len;
         $_args = $_thaw->($_buf); undef $_buf;

         chomp($_len = <$_DAU_R_SOCK>);
         print {$_DAU_R_SOCK} $LF;

         if ($_len) {
            for my $_k (qw( _qw_sock _qr_sock _aw_sock _cw_sock )) {
               if (exists $_args->[0]->{ $_k }) {
                   delete $_args->[0]->{ $_k };
                   $_fd = IO::FDPass::recv(fileno $_DAU_R_SOCK); $_fd >= 0
                     or _croak("cannot receive file handle: $!");

                   open $_args->[0]->{ $_k }, "+<&=$_fd"
                     or _croak("cannot convert file discriptor to handle: $!");

                   print {$_DAU_R_SOCK} $LF;
               }
            }
         }

         $_item = _share($_params, @{ $_args });
         print {$_DAU_R_SOCK} $_item->SHARED_ID().$LF;

         $_buf = $_freeze->($_item);
         print {$_DAU_R_SOCK} length($_buf).$LF, $_buf;

         return;
      },

      SHR_M_CID.$LF => sub {                      # ClientID request
         print {$_DAU_R_SOCK} (++$_client_id).$LF;
         $_client_id = 0 if ($_client_id > 2e9);

         return;
      },

      SHR_M_DEE.$LF => sub {                      # Deeply shared
         chomp(my $_id1 = <$_DAU_R_SOCK>),
         chomp(my $_id2 = <$_DAU_R_SOCK>);

         $_ob3{ "$_id1:deeply" }->{ $_id2 } = 1;

         return;
      },

      SHR_M_INC.$LF => sub {                      # Increment count
         chomp($_id = <$_DAU_R_SOCK>);

         $_ob3{ "$_id:count" }++;
         print {$_DAU_R_SOCK} $LF;

         return;
      },

      SHR_M_OBJ.$LF => sub {                      # Object request
         my $_buf;

         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_fn  = <$_DAU_R_SOCK>),
         chomp($_wa  = <$_DAU_R_SOCK>),
         chomp($_len = <$_DAU_R_SOCK>);

         read($_DAU_R_SOCK, $_buf, $_len);

         my $_var  = $_obj{ $_id } || do { return $_warn0->($_fn) };
         my $_code = $_var->can($_fn) || do {
            return $_warn1->($_fn, blessed($_var));
         };

         if ( $_wa == WA_ARRAY ) {
            my @_ret = $_code->($_var, @{ $_thaw->($_buf) });
            my $_buf = $_freeze->(\@_ret);
            print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
         }
         elsif ( $_wa ) {
            my $_ret = $_code->($_var, @{ $_thaw->($_buf) });
            if ( !ref($_ret) && defined($_ret) ) {
               print {$_DAU_R_SOCK} length($_ret).'0'.$LF, $_ret;
            } else {
               my $_buf = $_freeze->([ $_ret ]);
               print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
            }
         }
         else {
            $_code->($_var, @{ $_thaw->($_buf) });
         }

         return;
      },

      SHR_M_OB0.$LF => sub {                      # Object request - thaw'less
         chomp($_id = <$_DAU_R_SOCK>),
         chomp($_fn = <$_DAU_R_SOCK>),
         chomp($_wa = <$_DAU_R_SOCK>);

         my $_var  = $_obj{ $_id } || do { return $_warn0->($_fn) };
         my $_code = $_var->can($_fn) || do {
            return $_warn1->($_fn, blessed($_var));
         };

         if ( $_wa == WA_ARRAY ) {
            my @_ret = $_code->($_var);
            my $_buf = $_freeze->(\@_ret);
            print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
         }
         elsif ( $_wa ) {
            my $_ret = $_code->($_var);
            if ( !ref($_ret) && defined($_ret) ) {
               print {$_DAU_R_SOCK} length($_ret).'0'.$LF, $_ret;
            } else {
               my $_buf = $_freeze->([ $_ret ]);
               print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
            }
         }
         else {
            $_code->($_var);
         }

         return;
      },

      SHR_M_OB1.$LF => sub {                      # Object request - thaw'less
         my $_arg1;

         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_fn  = <$_DAU_R_SOCK>),
         chomp($_wa  = <$_DAU_R_SOCK>),
         chomp($_len = <$_DAU_R_SOCK>);

         read($_DAU_R_SOCK, $_arg1, $_len);

         my $_var  = $_obj{ $_id } || do { return $_warn0->($_fn) };
         my $_code = $_var->can($_fn) || do {
            return $_warn1->($_fn, blessed($_var));
         };

         if ( $_wa == WA_ARRAY ) {
            my @_ret = $_code->($_var, $_arg1);
            my $_buf = $_freeze->(\@_ret);
            print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
         }
         elsif ( $_wa ) {
            my $_ret = $_code->($_var, $_arg1);
            if ( !ref($_ret) && defined($_ret) ) {
               print {$_DAU_R_SOCK} length($_ret).'0'.$LF, $_ret;
            } else {
               my $_buf = $_freeze->([ $_ret ]);
               print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
            }
         }
         else {
            $_code->($_var, $_arg1);
         }

         return;
      },

      SHR_M_OB2.$LF => sub {                      # Object request - thaw'less
         my ($_arg1, $_arg2);

         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_fn  = <$_DAU_R_SOCK>),
         chomp($_wa  = <$_DAU_R_SOCK>),
         chomp($_len = <$_DAU_R_SOCK>),
         chomp($_le2 = <$_DAU_R_SOCK>);

         read($_DAU_R_SOCK, $_arg1, $_len),
         read($_DAU_R_SOCK, $_arg2, $_le2);

         my $_var  = $_obj{ $_id } || do { return $_warn0->($_fn) };
         my $_code = $_var->can($_fn) || do {
            return $_warn1->($_fn, blessed($_var));
         };

         if ( $_wa == WA_ARRAY ) {
            my @_ret = $_code->($_var, $_arg1, $_arg2);
            my $_buf = $_freeze->(\@_ret);
            print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
         }
         elsif ( $_wa ) {
            my $_ret = $_code->($_var, $_arg1, $_arg2);
            if ( !ref($_ret) && defined($_ret) ) {
               print {$_DAU_R_SOCK} length($_ret).'0'.$LF, $_ret;
            } else {
               my $_buf = $_freeze->([ $_ret ]);
               print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
            }
         }
         else {
            $_code->($_var, $_arg1, $_arg2);
         }

         return;
      },

      SHR_M_OB3.$LF => sub {                      # Object request - thaw'less
         my ($_arg1, $_arg2, $_arg3);

         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_fn  = <$_DAU_R_SOCK>),
         chomp($_wa  = <$_DAU_R_SOCK>),
         chomp($_len = <$_DAU_R_SOCK>),
         chomp($_le2 = <$_DAU_R_SOCK>),
         chomp($_le3 = <$_DAU_R_SOCK>);

         read($_DAU_R_SOCK, $_arg1, $_len),
         read($_DAU_R_SOCK, $_arg2, $_le2),
         read($_DAU_R_SOCK, $_arg3, $_le3);

         my $_var  = $_obj{ $_id } || do { return $_warn0->($_fn) };
         my $_code = $_var->can($_fn) || do {
            return $_warn1->($_fn, blessed($_var));
         };

         if ( $_wa == WA_ARRAY ) {
            my @_ret = $_code->($_var, $_arg1, $_arg2, $_arg3);
            my $_buf = $_freeze->(\@_ret);
            print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
         }
         elsif ( $_wa ) {
            my $_ret = $_code->($_var, $_arg1, $_arg2, $_arg3);
            if ( !ref($_ret) && defined($_ret) ) {
               print {$_DAU_R_SOCK} length($_ret).'0'.$LF, $_ret;
            } else {
               my $_buf = $_freeze->([ $_ret ]);
               print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
            }
         }
         else {
            $_code->($_var, $_arg1, $_arg2, $_arg3);
         }

         return;
      },

      SHR_M_DES.$LF => sub {                      # Destroy request
         chomp($_id = <$_DAU_R_SOCK>);

         local $SIG{__DIE__};
         local $SIG{__WARN__};

         local $@; eval {
            $_ret = (exists $_all{ $_id }) ? '1' : '0';
            _destroy({}, $_obj{ $_id }, $_id) if $_ret;
         };

         return;
      },

      SHR_M_EXP.$LF => sub {                      # Export request
         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_len = <$_DAU_R_SOCK>);

         read($_DAU_R_SOCK, my($_keys), $_len) if $_len;

         if (exists $_obj{ $_id }) {
            my $_buf;

            # MCE::Shared::{ Array, Hash, Ordhash }, Hash::Ordered
            if ($_iter_allow{ $_all{ $_id } } && $_obj{ $_id }->can('clone')) {
               $_buf = ($_len)
                  ? $_freeze->($_obj{ $_id }->clone(@{ $_thaw->($_keys) }))
                  : $_freeze->($_obj{ $_id });
            }

            # MCE::Shared::{ Condvar, Queue }
            elsif ( $_all{ $_id } =~ /^MCE::Shared::(?:Condvar|Queue)$/ ) {
               my %_ret = %{ $_obj{ $_id } }; bless \%_ret, $_all{ $_id };
               delete @_ret{ qw(
                  _qw_sock _qr_sock _aw_sock _ar_sock _cw_sock _cr_sock _mutex
                  _mutex_0 _mutex_1 _mutex_2 _mutex_3 _mutex_4 _mutex_5
               ) };
               $_buf = $_freeze->(\%_ret);
            }

            # Other
            else {
               $_buf = $_freeze->($_obj{ $_id });
            }

            print {$_DAU_R_SOCK} length($_buf).$LF, $_buf;
            undef $_buf;
         }
         else {
            print {$_DAU_R_SOCK} '-1'.$LF;
         }

         return;
      },

      SHR_M_INX.$LF => sub {                      # Iterator next
         chomp($_id = <$_DAU_R_SOCK>);

         my $_var = $_obj{ $_id };

         if ( my $_code = $_var->can('next') ) {
            my $_buf = $_freeze->([ $_code->( $_var ) ]);
            print {$_DAU_R_SOCK} length($_buf).$LF, $_buf;
         }
         else {
            $_iterator->();
         }

         return;
      },

      SHR_M_IRW.$LF => sub {                      # Iterator rewind
         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_len = <$_DAU_R_SOCK>);

         read $_DAU_R_SOCK, my($_buf), $_len;

         my $_var  = $_obj{ $_id };
         my @_args = @{ $_thaw->($_buf) };

         if (my $_code = $_var->can('rewind')) {
            $_code->( $_var, @_args );
         }
         else {
            weaken( delete $_itr{ $_id } ) if ( exists $_itr{ $_id } );
            if ( @_args ) {
               $_itr{ "$_id:args" } = \@_args;
            } else {
               delete $_itr{ "$_id:args" };
            }
         }

         print {$_DAU_R_SOCK} $LF;

         return;
      },

      SHR_M_STP.$LF => sub {                      # Stop server
         $_done = 1;

         return;
      },

      # -----------------------------------------------------------------------

      SHR_O_CVB.$LF => sub {                      # Condvar broadcast
         chomp($_id = <$_DAU_R_SOCK>);

         $_CV = $_obj{ $_id };

         for my $_i (1 .. $_CV->{_count}) {
            1 until syswrite($_CV->{_cw_sock}, $LF) || ($! && !$!{'EINTR'});
         }

         $_CV->{_count} = 0;

         print {$_DAU_R_SOCK} $LF;

         return;
      },

      SHR_O_CVS.$LF => sub {                      # Condvar signal
         chomp($_id = <$_DAU_R_SOCK>);

         $_CV = $_obj{ $_id };

         if ( $_CV->{_count} >= 0 ) {
            1 until syswrite($_CV->{_cw_sock}, $LF) || ($! && !$!{'EINTR'});
            $_CV->{_count} -= 1;
         }

         print {$_DAU_R_SOCK} $LF;

         return;
      },

      SHR_O_CVT.$LF => sub {                      # Condvar timedwait
         chomp($_id = <$_DAU_R_SOCK>);

         $_CV = $_obj{ $_id };
         $_CV->{_count} -= 1;

         print {$_DAU_R_SOCK} $LF;

         return;
      },

      SHR_O_CVW.$LF => sub {                      # Condvar wait
         chomp($_id = <$_DAU_R_SOCK>);

         $_CV = $_obj{ $_id };
         $_CV->{_count} += 1;

         print {$_DAU_R_SOCK} $LF;

         return;
      },

      SHR_O_CLO.$LF => sub {                      # Handle CLOSE
         chomp($_id = <$_DAU_R_SOCK>);

         close $_obj{ $_id } if defined fileno($_obj{ $_id });
         print {$_DAU_R_SOCK} '1'.$LF;

         return;
      },

      SHR_O_OPN.$LF => sub {                      # Handle OPEN
         my ($_fd, $_buf, $_err); local $!;

         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_fd  = <$_DAU_R_SOCK>),
         chomp($_len = <$_DAU_R_SOCK>);

         read $_DAU_R_SOCK, $_buf, $_len;
         print {$_DAU_R_SOCK} $LF;

         if ($_fd > 2) {
            $_fd = IO::FDPass::recv(fileno $_DAU_R_SOCK); $_fd >= 0
               or _croak("cannot receive file handle: $!");
         }

         my $_args = $_thaw->($_buf);

         close $_obj{ $_id } if defined fileno($_obj{ $_id });

         if (@{ $_args } == 2) {
            open( $_obj{ $_id }, "$_args->[0]", $_args->[1] )
               or do { $_err = 0+$! };
         } else {
            open( $_obj{ $_id }, $_args->[0] )
               or do { $_err = 0+$! };
         }

         print {$_DAU_R_SOCK} $_err.$LF;

         return;
      },

      SHR_O_REA.$LF => sub {                      # Handle READ
         my ($_a3, $_auto);

         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_a3  = <$_DAU_R_SOCK>),
         chomp($_len = <$_DAU_R_SOCK>);

         if (lc(substr $_a3, -1, 1) eq 'm') {
            $_auto = 1, chop $_a3; $_a3 *= 1024 * 1024;
         } elsif (lc(substr $_a3, -1, 1) eq 'k') {
            $_auto = 1, chop $_a3; $_a3 *= 1024;
         }

         local $/; read($_DAU_R_SOCK, $/, $_len) if $_len;
         my ($_fh, $_buf) = ($_obj{ $_id }); local ($!, $.);

         # support special case; e.g. $/ = "\n>" for bioinformatics
         # anchoring ">" at the start of line

         if (!$_auto) {
            $. = 0, $_ret = read($_fh, $_buf, $_a3);
         }
         elsif (!eof($_fh)) {
            if (length $/ > 1 && substr($/, 0, 1) eq "\n") {
               $_len = length($/) - 1;

               if (tell $_fh) {
                  $_buf = substr($/, 1);
                  $_ret = read($_fh, $_buf, $_a3, length($_buf));
               } else {
                  $_ret = read($_fh, $_buf, $_a3);
               }

               if (defined $_ret) {
                  $.    += 1 if eof($_fh);
                  $_buf .= readline($_fh);

                  substr($_buf, -$_len, $_len, '')
                     if (substr($_buf, -$_len) eq substr($/, 1));
               }
            }
            elsif (defined ($_ret = read($_fh, $_buf, $_a3))) {
               $.    += 1 if eof($_fh);
               $_buf .= readline($_fh);
            }
         }
         else {
            $_buf = '', $_ret = 0;
         }

         if (defined $_ret) {
            print {$_DAU_R_SOCK} "$.$LF" . length($_buf).$LF, $_buf;
         } else {
            print {$_DAU_R_SOCK} "$.$LF" . ( (0+$!) * -1 ).$LF;
         }

         return;
      },

      SHR_O_RLN.$LF => sub {                      # Handle READLINE
         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_len = <$_DAU_R_SOCK>);

         local $/; read($_DAU_R_SOCK, $/, $_len) if $_len;
         my ($_fh, $_buf) = ($_obj{ $_id }); local ($!, $.);

         # support special case; e.g. $/ = "\n>" for bioinformatics
         # anchoring ">" at the start of line

         if (length $/ > 1 && substr($/, 0, 1) eq "\n" && !eof($_fh)) {
            $_len = length($/) - 1;

            if (tell $_fh) {
               $_buf = substr($/, 1), $_buf .= readline($_fh);
            } else {
               $_buf = readline($_fh);
            }

            substr($_buf, -$_len, $_len, '')
               if (substr($_buf, -$_len) eq substr($/, 1));
         }
         else {
            $_buf = readline($_fh);
         }

         if (defined $_buf) {
            print {$_DAU_R_SOCK} "$.$LF" . length($_buf).$LF, $_buf;
         } else {
            print {$_DAU_R_SOCK} "$.$LF" . ( (0+$!) * -1 ).$LF;
         }

         return;
      },

      SHR_O_PRI.$LF => sub {                      # Handle PRINT
         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_len = <$_DAU_R_SOCK>);

         read $_DAU_R_SOCK, my($_buf), $_len;
         print {$_obj{ $_id }} $_buf;

         return;
      },

      SHR_O_WRI.$LF => sub {                      # Handle WRITE
         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_len = <$_DAU_R_SOCK>);

         read $_DAU_R_SOCK, my($_buf), $_len;

         my $_wrote = 0;

         WRITE: {
            $_wrote += ( syswrite (
               $_obj{ $_id }, length($_buf) - $_wrote, $_wrote
            )) || do {
               if ( $! ) {
                  redo WRITE if $!{'EINTR'};
                  print {$_DAU_R_SOCK} ''.$LF;

                  return;
               }
            };
         }

         print {$_DAU_R_SOCK} $_wrote.$LF;

         return;
      },

      SHR_O_QUA.$LF => sub {                      # Queue await
         chomp($_id = <$_DAU_R_SOCK>),
         chomp($_t  = <$_DAU_R_SOCK>);

         $_Q = $_obj{ $_id };
         $_Q->{_tsem} = $_t;

         if ($_Q->pending() <= $_t) {
            1 until syswrite($_Q->{_aw_sock}, $LF) || ($! && !$!{'EINTR'});
         } else {
            $_Q->{_asem} += 1;
         }

         print {$_DAU_R_SOCK} $LF;

         return;
      },

      SHR_O_QUD.$LF => sub {                      # Queue dequeue
         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_cnt = <$_DAU_R_SOCK>);

         $_cnt = 0 if ($_cnt == 1);
         $_Q = $_obj{ $_id };

         my (@_items, $_buf);

         if ($_cnt) {
            my $_pending = @{ $_Q->{_datq} };

            if ($_pending < $_cnt && scalar @{ $_Q->{_heap} }) {
               for my $_h (@{ $_Q->{_heap} }) {
                  $_pending += @{ $_Q->{_datp}->{$_h} };
               }
            }
            $_cnt = $_pending if $_pending < $_cnt;

            for my $_i (1 .. $_cnt) { push @_items, $_Q->_dequeue() }
         }
         else {
            $_buf = $_Q->_dequeue();
         }

         if ($_Q->{_fast}) {
            # The 'fast' option may reduce wait time, thus run faster
            if ($_Q->{_dsem} <= 1) {
               $_pending = $_Q->pending();
               $_pending = int($_pending / $_cnt) if ($_cnt);
               if ($_pending) {
                  $_pending = MAX_DQ_DEPTH if ($_pending > MAX_DQ_DEPTH);
                  for my $_i (1 .. $_pending) {
                     1 until syswrite($_Q->{_qw_sock}, $LF) || ($! && !$!{'EINTR'});
                  }
               }
               $_Q->{_dsem} = $_pending;
            }
            else {
               $_Q->{_dsem} -= 1;
            }
         }
         else {
            # Otherwise, never to exceed one byte in the channel
            if ($_Q->_has_data()) {
               1 until syswrite($_Q->{_qw_sock}, $LF) || ($! && !$!{'EINTR'});
            }
         }

         if ($_Q->{_ended} && !$_Q->_has_data()) {
            1 until syswrite($_Q->{_qw_sock}, $LF) || ($! && !$!{'EINTR'});
         }

         if ($_cnt) {
            $_buf = $_freeze->(\@_items);
            print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
         }
         elsif (defined $_buf) {
            if (!ref($_buf)) {
               print {$_DAU_R_SOCK} length($_buf).'0'.$LF, $_buf;
            } else {
               $_buf = $_freeze->([ $_buf ]);
               print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
            }
         }
         else {
            print {$_DAU_R_SOCK} '-1'.$LF;
         }

         if ($_Q->{_await} && $_Q->{_asem} && $_Q->pending() <= $_Q->{_tsem}) {
            for my $_i (1 .. $_Q->{_asem}) {
               1 until syswrite($_Q->{_aw_sock}, $LF) || ($! && !$!{'EINTR'});
            }
            $_Q->{_asem} = 0;
         }

         $_Q->{_nb_flag} = 0;

         return;
      },

      SHR_O_QUN.$LF => sub {                      # Queue dequeue non-blocking
         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_cnt = <$_DAU_R_SOCK>);

         $_Q = $_obj{ $_id };

         if ($_cnt == 1) {
            my $_buf = $_Q->_dequeue();

            if (defined $_buf) {
               if (!ref($_buf)) {
                  print {$_DAU_R_SOCK} length($_buf).'0'.$LF, $_buf;
               } else {
                  $_buf = $_freeze->([ $_buf ]);
                  print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
               }
            }
            else {
               print {$_DAU_R_SOCK} '-1'.$LF;
            }
         }
         else {
            my @_items;
            my $_pending = @{ $_Q->{_datq} };

            if ($_pending < $_cnt && scalar @{ $_Q->{_heap} }) {
               for my $_h (@{ $_Q->{_heap} }) {
                  $_pending += @{ $_Q->{_datp}->{$_h} };
               }
            }
            $_cnt = $_pending if $_pending < $_cnt;

            for my $_i (1 .. $_cnt) { push @_items, $_Q->_dequeue() }

            if ($_cnt) {
               my $_buf = $_freeze->(\@_items);
               print {$_DAU_R_SOCK} length($_buf).'1'.$LF, $_buf;
            } else {
               print {$_DAU_R_SOCK} '-1'.$LF;
            }
         }

         if ($_Q->{_await} && $_Q->{_asem} && $_Q->pending() <= $_Q->{_tsem}) {
            for my $_i (1 .. $_Q->{_asem}) {
               1 until syswrite($_Q->{_aw_sock}, $LF) || ($! && !$!{'EINTR'});
            }
            $_Q->{_asem} = 0;
         }

         $_Q->{_nb_flag} = $_Q->_has_data() ? 1 : 0;

         return;
      },

      SHR_O_PDL.$LF => sub {                      # PDL::ins inplace(this),...
         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_len = <$_DAU_R_SOCK>);

         read $_DAU_R_SOCK, my($_buf), $_len;

         if ($_all{ $_id } eq 'PDL') {
            local @_ = @{ $_thaw->($_buf) };
            if (@_ == 1) {
               # ins( inplace( $this ), $what, 0, 0 );
               ins( inplace( $_obj{ $_id } ), @_, 0, 0 );
            }
            elsif (@_ == 2) {
               # $this->slice( $arg1 ) .= $arg2;
               $_obj{ $_id }->slice( $_[0] ) .= $_[1];
            }
            elsif (@_ > 2) {
               # ins( inplace( $this ), $what, @coords );
               ins( inplace( $_obj{ $_id } ), @_ );
            }
         }

         return;
      },

      SHR_O_FCH.$LF => sub {                      # A,H,OH,S FETCH
         chomp($_id  = <$_DAU_R_SOCK>),
         chomp($_fn  = <$_DAU_R_SOCK>),
         chomp($_len = <$_DAU_R_SOCK>);

         read($_DAU_R_SOCK, $_key, $_len) if $_len;

         my $_var = $_obj{ $_id } || do {
            print {$_DAU_R_SOCK} '-1'.$LF;
            return;
         };

         if ( my $_code = $_var->can($_fn) ) {
            $_len ? $_fetch->($_code->($_var, $_key))
                  : $_fetch->($_code->($_var));
         }
         else {
            $_warn2->($_fn, blessed($_obj{ $_id }));
            print {$_DAU_R_SOCK} '-1'.$LF;
         }

         return;
      },

      SHR_O_CLR.$LF => sub {                      # A,H,OH CLEAR
         chomp($_id = <$_DAU_R_SOCK>),
         chomp($_fn = <$_DAU_R_SOCK>);

         my $_var = $_obj{ $_id } || do {
            return;
         };

         if ( my $_code = $_var->can($_fn) ) {
            if (exists $_ob3{ "$_id:deeply" }) {
               my $_keep = { $_id => 1 };
               for my $_oid (keys %{ $_ob3{ "$_id:deeply" } }) {
                  _destroy($_keep, $_obj{ $_oid }, $_oid);
               }
               delete $_ob3{ "$_id:deeply" };
            }
            $_code->($_var);
         }
         else {
            $_warn2->($_fn, blessed($_obj{ $_id }));
         }

         return;
      },

   );

   # --------------------------------------------------------------------------

   # Call on hash function.

   if ($_is_MSWin32) {
      # The normal loop hangs on Windows when processes/threads start/exit.
      # Using ioctl() properly, http://www.perlmonks.org/?node_id=780083

      my $_val_bytes = "\x00\x00\x00\x00";
      my $_ptr_bytes = unpack( 'I', pack('P', $_val_bytes) );
      my ($_count, $_nbytes, $_start) = (1);

      while (!$_done) {
         $_start = time;

         # MSWin32 FIONREAD
         IOCTL: ioctl($_DAT_R_SOCK, 0x4004667f, $_ptr_bytes);

         unless ($_nbytes = unpack('I', $_val_bytes)) {
            if ($_count) {
                # delay after a while to not consume a CPU core
                $_count = 0 if ++$_count % 50 == 0 && time - $_start > 0.030;
            } else {
                sleep 0.030;
            }
            goto IOCTL;
         }

         $_count = 1;

         do {
            sysread($_DAT_R_SOCK, $_func, 8);
            $_done = 1, last() unless length($_func) == 8;
            $_DAU_R_SOCK = $_channels->[ substr($_func, -2, 2, '') ];

            $_output_function{$_func}();

         } while (($_nbytes -= 8) >= 8);
      }
   }

   else {
      while (!$_done) {
         $_func = <$_DAT_R_SOCK>;
         last() unless length($_func) == 6;
         $_DAU_R_SOCK = $_channels->[ <$_DAT_R_SOCK> ];

         $_output_function{$_func}();
      }
   }

   # Exit loop.

   for my $_o ( values %_obj ) {
      close $_o if ref($_o) eq 'MCE::Shared::Handle' && defined fileno($_o);
   }

   eval q{ print $_DAT_R_SOCK $LF };

   _exit();
}

###############################################################################
## ----------------------------------------------------------------------------
## Object package.
##
###############################################################################

package MCE::Shared::Object;

use strict;
use warnings;

use 5.010001;

no warnings qw( threads recursion uninitialized numeric once );

use Time::HiRes qw( sleep );
use Scalar::Util qw( looks_like_number );
use MCE::Shared::Base;
use bytes;

use constant {
   _ID    => 0, _CLASS => 1, _DREF   => 2, _ITER => 3,  # shared object
   _UNDEF => 0, _ARRAY => 1, _SCALAR => 2,              # wantarray

   MUTEX_LOCKS => 6,  # Number of mutex locks for 1st level defense
                      # against many workers waiting to dequeue
};

my %_hash_deref_allow = (qw/
   MCE::Shared::Cache   1
   MCE::Shared::Hash    1
   MCE::Shared::Ordhash 1
   Hash::Ordered        1
/);

use overload (
   q("")    => \&MCE::Shared::Base::_stringify,
   q(0+)    => \&MCE::Shared::Base::_numify,
   q(@{})   => sub {
      no overloading;
      $_[0]->[_DREF] || do {
         return $_[0] if $_[0]->[_CLASS] ne 'MCE::Shared::Array';
         # no circular reference to original, therefore no memory leaks
         tie my @a, __PACKAGE__, bless([ $_[0]->[_ID] ], __PACKAGE__);
         $_[0]->[_DREF] = \@a;
      };
   },
   q(%{})   => sub {
      $_[0]->[_DREF] || do {
         return $_[0] unless $_hash_deref_allow{ $_[0]->[_CLASS] };
         # no circular reference to original, will not memory leak
         tie my %h, __PACKAGE__, bless([ $_[0]->[_ID] ], __PACKAGE__);
         $_[0]->[_DREF] = \%h;
      };
   },
   q(${})   => sub {
      $_[0]->[_DREF] || do {
         return $_[0] if $_[0]->[_CLASS] ne 'MCE::Shared::Scalar';
         # no circular reference to original, ditto...
         tie my $s, __PACKAGE__, bless([ $_[0]->[_ID] ], __PACKAGE__);
         $_[0]->[_DREF] = \$s;
      };
   },
   fallback => 1
);

no overloading;

my ($_DAT_LOCK, $_DAT_W_SOCK, $_DAU_W_SOCK, $_chn, $_dat_ex, $_dat_un);

my $_blessed = \&Scalar::Util::blessed;
my $_rdy     = \&MCE::Util::_sock_ready;

BEGIN {
   $_dat_ex = sub {
      _croak(
         "\nPlease start the shared-manager process manually when ready.\n",
         "Or see section labeled \"Extra Functionality\" in MCE::Shared.\n\n"
      );
   };
}

# Hook for threads.

sub CLONE {
   $_tid = threads->tid() if $_has_threads;
   &_init($_tid)          if $_tid;
}

# Private functions.

sub DESTROY {
   return unless ($_is_client && defined $_svr_pid && defined $_[0]);

   my $_id = $_[0]->[_ID];

   if (exists $_new{ $_id }) {
      my $_pid = $_has_threads ? $$ .'.'. $_tid : $$;

      if ($_new{ $_id } eq $_pid) {
         return if ($INC{'MCE/Signal.pm'} && $MCE::Signal::KILLED);
         return if ($MCE::Shared::Server::KILLED);

         delete($_new{ $_id }), _req2('M~DES', $_id.$LF, '');
      }
   }

   return;
}

sub _croak {
   goto &MCE::Shared::Base::_croak;
}
sub SHARED_ID { $_[0]->[_ID] }

sub TIEARRAY  { $_[1] }
sub TIEHANDLE { $_[1] }
sub TIEHASH   { $_[1] }
sub TIESCALAR { $_[1] }

sub _start {
   $_chn        = 1;
   $_DAT_LOCK   = $_SVR->{'_mutex_'.$_chn};
   $_DAT_W_SOCK = $_SVR->{_dat_w_sock}[0];
   $_DAU_W_SOCK = $_SVR->{_dat_w_sock}[$_chn];

   # inlined for performance
   $_dat_ex = sub {
      1 until sysread($_DAT_LOCK->{_r_sock}, my($_b), 1) || ($! && !$!{'EINTR'});
   };
   $_dat_un = sub {
      1 until syswrite($_DAT_LOCK->{_w_sock}, '0') || ($! && !$!{'EINTR'});
   };

   return;
}

sub _stop {
   $_DAT_LOCK = $_DAT_W_SOCK = $_DAU_W_SOCK = $_chn = $_dat_un = undef;

   $_dat_ex = sub {
      _croak(
         "\nPlease start the shared-manager process manually when ready.\n",
         "Or see section labeled \"Extra Functionality\" in MCE::Shared.\n\n"
      );
   };

   return;
}

sub _get_client_id {
   my $_ret;

   local $\ = undef if (defined $\);
   local $/ = $LF if ($/ ne $LF);

   $_dat_ex->();
   print {$_DAT_W_SOCK} 'M~CID'.$LF . $_chn.$LF;
   chomp($_ret = <$_DAU_W_SOCK>);
   $_dat_un->();

   return $_ret;
}

sub _init {
   return unless defined $_SVR;

   my $_id = $_[0] // &_get_client_id();
      $_id = $$ if ( $_id !~ /\d+/ );

   $_chn        = abs($_id) % $_SVR->{_data_channels} + 1;
   $_DAT_LOCK   = $_SVR->{'_mutex_'.$_chn};
   $_DAU_W_SOCK = $_SVR->{_dat_w_sock}[$_chn];

   %_new = ();

   return $_id;
}

###############################################################################
## ----------------------------------------------------------------------------
## Private routines.
##
###############################################################################

# Called by AUTOLOAD, SCALAR, STORE, and set.

sub _auto {
   my $_wa = !defined wantarray ? _UNDEF : wantarray ? _ARRAY : _SCALAR;

   local $\ = undef if (defined $\);

   if ( @_ == 2 ) {
      $_dat_ex->();
      print {$_DAT_W_SOCK} 'M~OB0'.$LF . $_chn.$LF;
      print {$_DAU_W_SOCK} $_[1]->[_ID].$LF . $_[0].$LF . $_wa.$LF;
   }
   elsif ( @_ == 3 && !ref($_[2]) && defined($_[2]) ) {
      $_dat_ex->();
      print {$_DAT_W_SOCK} 'M~OB1'.$LF . $_chn.$LF;
      print {$_DAU_W_SOCK} $_[1]->[_ID].$LF . $_[0].$LF . $_wa.$LF .
         length($_[2]).$LF, $_[2];
   }
   elsif ( @_ == 4 && !ref($_[3]) && defined($_[3])
                   && !ref($_[2]) && defined($_[2]) ) {
      $_dat_ex->();
      print {$_DAT_W_SOCK} 'M~OB2'.$LF . $_chn.$LF;
      print {$_DAU_W_SOCK} $_[1]->[_ID].$LF . $_[0].$LF . $_wa.$LF .
         length($_[2]).$LF . length($_[3]).$LF . $_[2], $_[3];
   }
   elsif ( @_ == 5 && !ref($_[4]) && defined($_[4])
                   && !ref($_[3]) && defined($_[3])
                   && !ref($_[2]) && defined($_[2]) ) {
      $_dat_ex->();
      print {$_DAT_W_SOCK} 'M~OB3'.$LF . $_chn.$LF;
      print {$_DAU_W_SOCK} $_[1]->[_ID].$LF . $_[0].$LF . $_wa.$LF .
         length($_[2]).$LF . length($_[3]).$LF . length($_[4]).$LF .
         $_[2] . $_[3], $_[4];
   }
   else {
      my ( $_fn, $_id, $_tmp ) = ( shift, shift()->[_ID], $_freeze->([ @_ ]) );
      my $_buf = $_id.$LF . $_fn.$LF . $_wa.$LF . length($_tmp).$LF;

      $_dat_ex->();
      print {$_DAT_W_SOCK} 'M~OBJ'.$LF . $_chn.$LF;
      print {$_DAU_W_SOCK} $_buf, $_tmp;
   }

   if ( $_wa ) {
      my ($_len, $_frozen, $_buf);

      local $/ = $LF if ($/ ne $LF);
      chomp($_len = <$_DAU_W_SOCK>);

      $_frozen = chop($_len);
      read $_DAU_W_SOCK, $_buf, $_len;
      $_dat_un->();

      return ( $_wa != _ARRAY )
         ? $_frozen ? $_thaw->($_buf)[0] : $_buf
         : @{ $_thaw->($_buf) };
   }

   $_dat_un->();
}

# Called by CLOSE, await, broadcast, signal, timedwait, wait, and rewind.

sub _req1 {
   local $\ = undef if (defined $\);
   local $/ = $LF if ($/ ne $LF);

   $_dat_ex->();
   print {$_DAT_W_SOCK} $_[0].$LF . $_chn.$LF;
   print {$_DAU_W_SOCK} $_[1];

   chomp(my $_ret = <$_DAU_W_SOCK>);
   $_dat_un->();

   $_ret;
}

# Called by DESTROY, PRINT, PRINTF, STORE, destroy, ins_inplace, and set.

sub _req2 {
   local $\ = undef if (defined $\);

   $_dat_ex->();
   print {$_DAT_W_SOCK} $_[0].$LF . $_chn.$LF;
   print {$_DAU_W_SOCK} $_[1], $_[2];
   $_dat_un->();

   1;
}

# Called by export.

sub _req3 {
   local $\ = undef if (defined $\);
   local $/ = $LF if ($/ ne $LF);

   $_dat_ex->();
   print {$_DAT_W_SOCK} $_[0].$LF . $_chn.$LF;
   print {$_DAU_W_SOCK} $_[1], $_[2];

   chomp(my $_len = <$_DAU_W_SOCK>);

   if ($_len < 0) { $_dat_un->(); return undef; }

   read $_DAU_W_SOCK, my($_buf), $_len;
   $_dat_un->();

   my $_obj = $_thaw->($_buf);
   undef $_buf;

   $_obj;
}

# Called by dequeue and dequeue_nb.

sub _req4 {
   local $\ = undef if (defined $\);
   local $/ = $LF if ($/ ne $LF);

   $_dat_ex->();
   print {$_DAT_W_SOCK} $_[0].$LF . $_chn.$LF;
   print {$_DAU_W_SOCK} $_[1];
   $_[3]->{'_mutex_'.( $_chn % MUTEX_LOCKS )}->unlock() if $_[3];

   chomp(my $_len = <$_DAU_W_SOCK>);

   $_dat_un->(), return if ($_len < 0);

   my $_frozen = chop($_len);
   read $_DAU_W_SOCK, my($_buf), $_len;
   $_dat_un->();

   ($_[2] == 1)
      ? ($_frozen) ? $_thaw->($_buf)[0] : $_buf
      : @{ $_thaw->($_buf) };
}

# Called by CLEAR and clear.

sub _req5 {
   my ( $_fn, $self ) = @_;
   local $\ = undef if (defined $\);
   local $/ = $LF if ($/ ne $LF);

   delete $self->[_ITER] if defined $self->[_ITER];

   $_dat_ex->();
   print {$_DAT_W_SOCK} 'O~CLR'.$LF . $_chn.$LF;
   print {$_DAU_W_SOCK} $self->[_ID].$LF . $_fn.$LF;
   $_dat_un->();

   return;
}

# Called by FETCH and get.

sub _req6 {
   local $\ = undef if (defined $\);
   local $/ = $LF if ($/ ne $LF);

   $_dat_ex->();
   print {$_DAT_W_SOCK} 'O~FCH'.$LF . $_chn.$LF;
   print {$_DAU_W_SOCK} $_[1]->[_ID].$LF . $_[0].$LF . length($_[2]).$LF, $_[2];

   chomp(my $_len = <$_DAU_W_SOCK>);

   if ($_len < 0) { $_dat_un->(); return undef; }

   my $_frozen = chop($_len);
   read $_DAU_W_SOCK, my($_buf), $_len;
   $_dat_un->();

   $_frozen ? $_thaw->($_buf) : $_buf;
}

###############################################################################
## ----------------------------------------------------------------------------
## Common methods.
##
###############################################################################

our $AUTOLOAD;

sub AUTOLOAD {
   # $AUTOLOAD = MCE::Shared::Object::<method_name>
   my $_fn = substr($AUTOLOAD, 21);

   # save this method for future calls
   no strict 'refs';
   *$AUTOLOAD = sub { _auto($_fn, @_) };

   goto &{ $AUTOLOAD };
}

# blessed ( )

sub blessed {
   $_[0]->[_CLASS];
}

# destroy ( )

sub destroy {
   my $_id   = $_[0]->[_ID];
   my $_item = (defined wantarray) ? $_[0]->export() : undef;
   my $_pid  = $_has_threads ? $$ .'.'. $_tid : $$;

   delete($_all{ $_id }), delete($_obj{ $_id });

   if (defined $_svr_pid && exists $_new{ $_id } && $_new{ $_id } eq $_pid) {
      delete($_new{ $_id }), _req2('M~DES', $_id.$LF, '');
   }

   $_[0] = undef;
   $_item;
}

# export ( key [, key, ... ] )
# export ( )

sub export {
   my $_ob   = shift;
   my $_id   = $_ob->[_ID];
   my $_lkup = ref($_[0]) eq 'HASH' ? shift : {};

   # safety for circular references to not loop endlessly
   return $_lkup->{ $_id } if exists $_lkup->{ $_id };

   my $_tmp   = @_ ? $_freeze->([ @_ ]) : '';
   my $_buf   = $_id.$LF . length($_tmp).$LF;
   my $_class = $_ob->[_CLASS];

   if (!exists $INC{ join('/',split(/::/,$_class)).'.pm' }) {
      local $@; local $SIG{__DIE__};
      eval "use $_class ()";
   }

   my $_item = $_lkup->{ $_id } = _req3('M~EXP', $_buf, $_tmp);
   my $_data; local $_;

   if ( $_class eq 'MCE::Shared::Array' ) {
      ## no critic
      map { $_ = $_->export($_lkup) if $_blessed->($_) && $_->can('export') }
         @{ $_item };
   }
   elsif ( $_class eq 'MCE::Shared::Scalar' ) {
      if ($_blessed->($_item->get()) && $_item->get()->can('export')) {
         $_item->set($_item->get()->export($_lkup));
      }
   }
   else {
      if    ( $_class eq 'MCE::Shared::Hash'    ) { $_data = $_item      }
      elsif ( $_class eq 'MCE::Shared::Ordhash' ) { $_data = $_item->[0] }
      elsif ( $_class eq 'MCE::Shared::Cache'   ) { $_data = $_item->[0] }
      elsif ( $_class eq 'Hash::Ordered'        ) { $_data = $_item->[0] }

      ## no critic
      map { $_ = $_->export($_lkup) if $_blessed->($_) && $_->can('export') }
         values %{ $_data } if defined( $_data );
   }

   $_item;
}

# iterator ( index, [, index, ... ] )
# iterator ( key, [, key, ... ] )
# iterator ( "query string" )
# iterator ( )

sub iterator {
   my ( $self, @keys ) = @_;
   my $pkg = $self->blessed();

   # MCE::Shared::{ Array, Hash, Ordhash }, Hash::Ordered
   if ( $_iter_allow{ $pkg } && eval qq{ $pkg->can('keys') } ) {
      if ( ! @keys ) {
         @keys = $self->keys;
      }
      elsif ( @keys == 1 && $keys[0] =~ /^(?:key|val)[ ]+\S\S?[ ]+\S/ ) {
         @keys = $self->keys($keys[0]);
      }
      return sub {
         return unless @keys;
         my $key = shift @keys;
         return ( $key => $self->get($key) );
      };
   }

   # Not supported
   else {
      return sub { };
   }
}

# rewind ( begin, end, [ step, format ] )  # Sequence
# rewind ( index [, index, ... ] )         # Array
# rewind ( key [, key, ... ] )             # Hash, Ordhash
# rewind ( "query string" )                # Array, Hash, Ordhash
# rewind ( )

sub rewind {
   my $_id  = shift()->[_ID];
   my $_buf = $_freeze->([ @_ ]);
   _req1('M~IRW', $_id.$LF . length($_buf).$LF . $_buf);

   return;
}

# next ( )

sub next {
   local $\ = undef if (defined $\);
   local $/ = $LF if ($/ ne $LF);

   $_dat_ex->();
   print {$_DAT_W_SOCK} 'M~INX'.$LF . $_chn.$LF;
   print {$_DAU_W_SOCK} $_[0]->[_ID].$LF;

   chomp(my $_len = <$_DAU_W_SOCK>);

   if ($_len < 0) { $_dat_un->(); return; }
   read $_DAU_W_SOCK, my($_buf), $_len;
   $_dat_un->();

   wantarray ? @{ $_thaw->($_buf) } : $_thaw->($_buf)[-1];
}

###############################################################################
## ----------------------------------------------------------------------------
## Methods optimized for Condvar.
##
###############################################################################

# lock ( )

sub lock {
   return unless ( my $_CV = $_obj{ $_[0]->[_ID] } );
   return unless ( exists $_CV->{_mutex} );

   $_CV->{_mutex}->lock;
}

# unlock ( )

sub unlock {
   return unless ( my $_CV = $_obj{ $_[0]->[_ID] } );
   return unless ( exists $_CV->{_mutex} );

   $_CV->{_mutex}->unlock;
}

# broadcast ( floating_seconds )
# broadcast ( )

sub broadcast {
   my $_id = $_[0]->[_ID];
   return unless ( my $_CV = $_obj{ $_id } );
   return unless ( exists $_CV->{_cr_sock} );

   sleep($_[1]) if defined $_[1];

   _req1('O~CVB', $_id.$LF);
   $_CV->{_mutex}->unlock();

   sleep(0);
}

# signal ( floating_seconds )
# signal ( )

sub signal {
   my $_id = $_[0]->[_ID];
   return unless ( my $_CV = $_obj{ $_id } );
   return unless ( exists $_CV->{_cr_sock} );

   sleep($_[1]) if defined $_[1];

   _req1('O~CVS', $_id.$LF);
   $_CV->{_mutex}->unlock();

   sleep(0);
}

# timedwait ( floating_seconds )

sub timedwait {
   my $_id = $_[0]->[_ID];
   my $_timeout = $_[1];

   return unless ( my $_CV = $_obj{ $_id } );
   return unless ( exists $_CV->{_cr_sock} );
   return $_[0]->wait() unless $_timeout;

   _croak('Condvar: timedwait (timeout) is not an integer')
      if (!looks_like_number($_timeout) || int($_timeout) != $_timeout);

   _req1('O~CVW', $_id.$LF);
   $_CV->{_mutex}->unlock();

   local $@; eval {
      local $SIG{ALRM} = sub { die "alarm clock restart\n" };
      alarm $_timeout unless $_is_MSWin32;

      die "alarm clock restart\n"
         if $_is_MSWin32 && $_rdy->($_CV->{_cr_sock}, $_timeout);

      1 until sysread($_CV->{_cr_sock}, my($_b), 1) || ($! && !$!{'EINTR'});

      alarm 0;
   };

   alarm 0;

   if ($@) {
      chomp($@), _croak($@) unless $@ eq "alarm clock restart\n";
      _req1('O~CVT', $_id.$LF);

      return '';
   }

   return 1;
}

# wait ( )

sub wait {
   my $_id = $_[0]->[_ID];
   return unless ( my $_CV = $_obj{ $_id } );
   return unless ( exists $_CV->{_cr_sock} );

   _req1('O~CVW', $_id.$LF);
   $_CV->{_mutex}->unlock();

   $_rdy->($_CV->{_cr_sock}) if $_is_MSWin32;
   1 until sysread($_CV->{_cr_sock}, my($_b), 1) || ($! && !$!{'EINTR'});

   return 1;
}

###############################################################################
## ----------------------------------------------------------------------------
## Methods optimized for Handle.
##
###############################################################################

sub CLOSE {
   _req1('O~CLO', $_[0]->[_ID].$LF);
}

sub OPEN {
   my ($_id, $_fd, $_buf) = (shift()->[_ID]);
   return unless defined $_[0];

   if (ref $_[-1] && Scalar::Util::reftype($_[-1]) ne 'GLOB') {
      _croak("open error: not a GLOB reference");
   }
   elsif (@_ == 2 && ref $_[1] && defined($_fd = fileno($_[1]))) {
      $_buf = $_freeze->([ $_[0]."&=$_fd" ]);
   }
   elsif (!ref $_[-1]) {
      $_fd  = ($_[-1] =~ /&=(\d+)$/) ? $1 : -1;
      $_buf = $_freeze->([ @_ ]);
   }
   else {
      _croak("open error: unsupported use-case");
   }

   if ($_fd > 2 && !$INC{'IO/FDPass.pm'}) {
      _croak(
         "\nSharing a handle object while the server is running\n",
         "requires the IO::FDPass module.\n\n"
      );
   }

   local $\ = undef if (defined $\);
   local $/ = $LF if ($/ ne $LF);

   $_dat_ex->();
   print {$_DAT_W_SOCK} 'O~OPN'.$LF . $_chn.$LF;
   print {$_DAU_W_SOCK} $_id.$LF . $_fd.$LF . length($_buf).$LF . $_buf;
   <$_DAU_W_SOCK>;

   IO::FDPass::send( fileno $_DAU_W_SOCK, fileno $_fd ) if ($_fd > 2);
   chomp(my $_err = <$_DAU_W_SOCK>);
   $_dat_un->();

   if ($_err) {
      $! = $_err; '';
   } else {
      $! = 0; 1;
   }
}

sub READ {
   local $\ = undef if (defined $\);

   $_dat_ex->();
   print {$_DAT_W_SOCK} 'O~REA'.$LF . $_chn.$LF;
   print {$_DAU_W_SOCK} $_[0]->[_ID].$LF . $_[2].$LF . length($/).$LF . $/;

   local $/ = $LF if ($/ ne $LF);
   chomp(my $_ret = <$_DAU_W_SOCK>);
   chomp(my $_len = <$_DAU_W_SOCK>);

   if ($_len) {
      if ($_len < 0) {
         $_dat_un->(); $. = 0, $! = $_len * -1;
         return undef;
      }
      (defined $_[3])
         ? read($_DAU_W_SOCK, $_[1], $_len, $_[3])
         : read($_DAU_W_SOCK, $_[1], $_len);
   }
   else {
      my $_ref = \$_[1];
      if (defined $_[3]) {
         substr($$_ref, $_[3], length($$_ref) - $_[3], '');
      } else {
         $$_ref = '';
      }
   }

   $_dat_un->(); $. = $_ret, $! = 0;
   $_len;
}

sub READLINE {
   local $\ = undef if (defined $\);

   $_dat_ex->();
   print {$_DAT_W_SOCK} 'O~RLN'.$LF . $_chn.$LF;
   print {$_DAU_W_SOCK} $_[0]->[_ID].$LF . length($/).$LF . $/;

   local $/ = $LF if ($/ ne $LF); my $_buf;
   chomp(my $_ret = <$_DAU_W_SOCK>);
   chomp(my $_len = <$_DAU_W_SOCK>);

   if ($_len) {
      if ($_len < 0) {
         $_dat_un->(); $. = 0, $! = $_len * -1;
         return undef;
      }
      read($_DAU_W_SOCK, $_buf, $_len);
   }

   $_dat_un->(); $. = $_ret, $! = 0;
   $_buf;
}

sub PRINT {
   my $_id  = shift()->[_ID];
   my $_buf = join(defined $, ? $, : "", @_);

   $_buf .= $\ if defined $\;

   (length $_buf)
      ? _req2('O~PRI', $_id.$LF . length($_buf).$LF, $_buf)
      : 1;
}

sub PRINTF {
   my $_id  = shift()->[_ID];
   my $_buf = sprintf(shift, @_);

   (length $_buf)
      ? _req2('O~PRI', $_id.$LF . length($_buf).$LF, $_buf)
      : 1;
}

sub WRITE {
   my $_id  = shift()->[_ID];
   local $\ = undef if (defined $\);
   local $/ = $LF if ($/ ne $LF);

   if (@_ == 1 || (@_ == 2 && $_[1] == length($_[0]))) {
      $_dat_ex->();
      print {$_DAT_W_SOCK} 'O~WRI'.$LF . $_chn.$LF;
      print {$_DAU_W_SOCK} $_id.$LF . length($_[0]).$LF, $_[0];
   }
   else {
      my $_buf = substr($_[0], ($_[2] || 0), $_[1]);
      $_dat_ex->();
      print {$_DAT_W_SOCK} 'O~WRI'.$LF . $_chn.$LF;
      print {$_DAU_W_SOCK} $_id.$LF . length($_buf).$LF, $_buf;
   }

   chomp(my $_ret = <$_DAU_W_SOCK>);
   $_dat_un->();

   (length $_ret) ? $_ret : undef;
}

###############################################################################
## ----------------------------------------------------------------------------
## Methods optimized for Queue.
##
###############################################################################

sub await {
   my $_id = shift()->[_ID];
   return unless ( my $_Q = $_obj{ $_id } );
   return unless ( exists $_Q->{_qr_sock} );

   my $_t = shift || 0;

   _croak('Queue: (await) is not enabled for this queue')
      unless (exists $_Q->{_ar_sock});
   _croak('Queue: (await threshold) is not an integer')
      if (!looks_like_number($_t) || int($_t) != $_t);

   $_t = 0 if ($_t < 0);
   _req1('O~QUA', $_id.$LF . $_t.$LF);

   $_rdy->($_Q->{_ar_sock}) if $_is_MSWin32;
   1 until sysread($_Q->{_ar_sock}, my($_b), 1) || ($! && !$!{'EINTR'});

   return;
}

sub dequeue {
   my $_id = shift()->[_ID];
   return unless ( my $_Q = $_obj{ $_id } );
   return unless ( exists $_Q->{_qr_sock} );

   my $_cnt = shift;

   if (defined $_cnt && $_cnt ne '1') {
      _croak('Queue: (dequeue count argument) is not valid')
         if (!looks_like_number($_cnt) || int($_cnt) != $_cnt || $_cnt < 1);
   } else {
      $_cnt = 1;
   }

   $_Q->{'_mutex_'.( $_chn % MUTEX_LOCKS )}->lock();
   $_rdy->($_Q->{_qr_sock}) if $_is_MSWin32;
   1 until sysread($_Q->{_qr_sock}, my($_b), 1) || ($! && !$!{'EINTR'});

   _req4('O~QUD', $_id.$LF . $_cnt.$LF, $_cnt, $_Q);
}

sub dequeue_nb {
   my $_id = shift()->[_ID];
   return unless ( my $_Q = $_obj{ $_id } );
   return unless ( exists $_Q->{_qr_sock} );

   my $_cnt = shift;

   if ($_Q->{_fast}) {
      warn "Queue: (dequeue_nb) is not allowed for fast => 1\n";
      return;
   }
   if (defined $_cnt && $_cnt ne '1') {
      _croak('Queue: (dequeue_nb count argument) is not valid')
         if (!looks_like_number($_cnt) || int($_cnt) != $_cnt || $_cnt < 1);
   } else {
      $_cnt = 1;
   }

   _req4('O~QUN', $_id.$LF . $_cnt.$LF, $_cnt, undef);
}

###############################################################################
## ----------------------------------------------------------------------------
## Methods optimized for:
##  MCE::Shared::{ Array, Hash, Ordhash, Scalar } and similar.
##
###############################################################################

if ($INC{'PDL.pm'}) {
   local $@; eval q{
      sub ins_inplace {
         my $_id = shift()->[_ID];
         if (@_) {
            my $_tmp = $_freeze->([ @_ ]);
            my $_buf = $_id.$LF . length($_tmp).$LF;
            _req2('O~PDL', $_buf, $_tmp);
         }
         return;
      }
   };
}

sub CLEAR { _req5('CLEAR', @_) }
sub clear { _req5('clear', @_) }

sub FETCH { _req6('FETCH', @_) }
sub get   { _req6('get'  , @_) }

sub FIRSTKEY {
   my ( $self ) = @_;
   $self->[_ITER] = [ $self->keys ];
   shift @{ $self->[_ITER] };
}

sub NEXTKEY {
   shift @{ $_[0]->[_ITER] };
}

sub SCALAR {
   _auto('SCALAR', @_);
}

sub STORE {
   if (ref $_[2]) {
      $_[2] = MCE::Shared::share({ _DEEPLY_ => 1 }, $_[2]);
      _req2('M~DEE', $_[0]->[_ID].$LF, $_[2]->SHARED_ID().$LF);
   }
   _auto('STORE', @_);
   1;
}

sub set {
   if ($_blessed->($_[2]) && $_[2]->can('SHARED_ID')) {
      _req2('M~DEE', $_[0]->[_ID].$LF, $_[2]->SHARED_ID().$LF);
      delete $_new{ $_[2]->SHARED_ID() };
   }
   _auto('set', @_);
   $_[-1];
}

{
   no strict 'refs';
   *{ __PACKAGE__.'::store' } = \&STORE;
}

1;

__END__

###############################################################################
## ----------------------------------------------------------------------------
## Module usage.
##
###############################################################################

=head1 NAME

MCE::Shared::Server - Server/Object packages for MCE::Shared

=head1 VERSION

This document describes MCE::Shared::Server version 1.821

=head1 DESCRIPTION

The core engine for L<MCE::Shared>. See documentation there.

=head1 INDEX

L<MCE|MCE>, L<MCE::Hobo>, L<MCE::Shared>

=head1 AUTHOR

Mario E. Roy, S<E<lt>marioeroy AT gmail DOT comE<gt>>

=cut

