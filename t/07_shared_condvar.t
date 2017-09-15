#!/usr/bin/env perl

use strict;
use warnings;

use Test::More;

BEGIN {
   use_ok 'MCE::Hobo';
   use_ok 'MCE::Shared';
   use_ok 'MCE::Shared::Condvar';
}

## MCE::Shared loads IO::FDPass automatically when available.
## On the Windows platform, the script may exit with a non-zero
## value (5) without it.

if ( $^O eq 'MSWin32' && !exists $INC{'IO/FDPass.pm'} ) {
   done_testing;
   exit 0;
}

my $cv = MCE::Shared->condvar();

## signal - --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

{
   ok( 1, "shared condvar, spawning an asynchronous process" );

   my $proc = MCE::Hobo->new( sub {
      sleep(1); $cv->lock; $cv->signal; 1;
   });

   $cv->lock;
   $cv->wait;

   ok( 1, "shared condvar, we've come back from the process" );
   is( $proc->join, 1, 'shared condvar, check if process came back correctly' );
}

## lock, set, get, unlock - --- --- --- --- --- --- --- --- --- --- --- --- ---

{
   my $data = 'beautiful skies, ...';

   $cv->lock;

   my $proc = MCE::Hobo->new( sub {
      $cv->lock;
      $cv->get eq $data;
   });

   $cv->set($data);
   $cv->unlock;

   ok( $proc->join, 'shared condvar, check if process sees the same value' );
}

## timedwait, wait, broadcast - --- --- --- --- --- --- --- --- --- --- --- ---

{
   my @procs; my $start = time();

   push @procs, MCE::Hobo->new( sub { $cv->timedwait(10); 1 } );
   push @procs, MCE::Hobo->new( sub { $cv->timedwait(20); 1 } );
   push @procs, MCE::Hobo->new( sub { $cv->wait; 1 } );

   sleep(2); $cv->broadcast;

   ok( $procs[0]->join, 'shared condvar, check broadcast to process1' );
   ok( $procs[1]->join, 'shared condvar, check broadcast to process2' );
   ok( $procs[2]->join, 'shared condvar, check broadcast to process3' );

   cmp_ok(
      time() - $start, '<', 8,
      'shared condvar, check processes exited timely'
   );
}

## the rest --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- ---

$cv->set(20);

is( $cv->len(), 2, 'shared condvar, check length' );
is( $cv->incr(), 21, 'shared condvar, check incr' );
is( $cv->decr(), 20, 'shared condvar, check decr' );
is( $cv->incrby(4), 24, 'shared condvar, check incrby' );
is( $cv->decrby(4), 20, 'shared condvar, check decrby' );
is( $cv->getincr(), 20, 'shared condvar, check getincr' );
is( $cv->get(), 21, 'shared condvar, check value after getincr' );
is( $cv->getdecr(), 21, 'shared condvar, check getdecr' );
is( $cv->get(), 20, 'shared condvar, check value after getdecr' );
is( $cv->append('ba'), 4, 'shared condvar, check append' );
is( $cv->get(), '20ba', 'shared condvar, check value after append' );
is( $cv->getset('foo'), '20ba', 'shared condvar, check getset' );
is( $cv->get(), 'foo', 'shared condvar, check value after getset' );

done_testing;

