#!/usr/bin/env perl

use strict;
use warnings;
use utf8;
use open qw(:std :utf8);

use Test::More;

BEGIN {
   use_ok 'MCE::Flow';
   use_ok 'MCE::Shared';
   use_ok 'MCE::Shared::Queue';
}

MCE::Flow->init(
   max_workers => 1
);

###############################################################################

## Queues must be shared first before anything else or it will not work.
## The reason is for the socket handles to be in place before starting the
## server. Sharing a hash or array will start the server automatically.

my $q1 = MCE::Shared->queue( type => $MCE::Shared::Queue::FIFO );
my $q2 = MCE::Shared->queue( type => $MCE::Shared::Queue::LIFO );

my $q3 = MCE::Shared->queue(
   porder => $MCE::Shared::Queue::HIGHEST,
   type   => $MCE::Shared::Queue::FIFO
);

my $q4 = MCE::Shared->queue(
   porder => $MCE::Shared::Queue::LOWEST,
   type   => $MCE::Shared::Queue::FIFO
);

my $q;

## One must explicitly start the shared-server for condvars and queues.
## Not necessary otherwise, if IO::FDPass is available.

MCE::Shared->start() unless $INC{'IO/FDPass.pm'};

###############################################################################

## https://sacred-texts.com/cla/usappho/sph02.htm (VI)

my $sappho_text =
  "καὶ γάρ αἰ φεύγει, ταχέωσ διώξει,
   αἰ δὲ δῶρα μὴ δέκετ ἀλλά δώσει,
   αἰ δὲ μὴ φίλει ταχέωσ φιλήσει,
   κωὐκ ἐθέλοισα." . "Ǣ";

my $translation =
  "For if now she flees, quickly she shall follow
   And if she spurns gifts, soon shall she offer them
   Yea, if she knows not love, soon shall she feel it
   Even reluctant.";

sub check_clear {
   my ($description) = @_;
   is( $q->_get_aref(5), undef, $description );
}

sub check_enqueuep {
   my ($description) = @_;
   is( join('', @{ $q->_get_aref(5) }), '1234', $description );
}

sub check_insertp {
   my ($description, $expected) = @_;
   is( join('', @{ $q->_get_aref(5) }), $expected, $description );
}

sub check_pending {
   my ($description, $pending) = @_;
   is( $pending, 14, $description );
}

sub check_unicode_in {
   my ($description) = @_;
   is( join('', @{ $q->_get_aref(5) }), $sappho_text, $description );
}

sub check_unicode_out {
   my ($description, $value) = @_;
   is( $value, $sappho_text, $description );
}

sub check {
   my ($description, $expected, $value) = @_;
   is( $value, $expected, $description );
}

###############################################################################

##  FIFO tests

$q = $q1;

sub check_dequeue_fifo {
   my (@r) = @_;
   is( join('', @r), '123', 'fifo, check dequeue' );
   is( join('', @{ $q->_get_aref(5) }), '4', 'fifo, check array' );
}

mce_flow sub {
   my ($mce) = @_;
   my $w; # effect is waiting for the check (MCE->do) to complete

   $q->enqueuep(5, '1', '2');
   $q->enqueuep(5, '3');
   $q->enqueuep(5, '4');

   $w = MCE->do('check_enqueuep', 'fifo, check enqueuep');

   my @r = $q->dequeue(2);
   push @r, $q->dequeue;

   $w = MCE->do('check_dequeue_fifo', @r);

   $q->clear;

   $w = MCE->do('check_clear', 'fifo, check clear');

   $q->enqueuep(5, 'a', 'b', 'c', 'd');

   $q->insertp(5,   1, 'e', 'f');
   $q->insertp(5,   3, 'g');
   $q->insertp(5,  -2, 'h');
   $q->insertp(5,   7, 'i');
   $q->insertp(5,   9, 'j');
   $q->insertp(5,  20, 'k');
   $q->insertp(5, -10, 'l');
   $q->insertp(5, -12, 'm');
   $q->insertp(5, -20, 'n');

   $w = MCE->do('check_insertp', 'fifo, check insertp', 'nmalefgbhcidjk');
   $w = MCE->do('check_pending', 'fifo, check pending', $q->pending());

   $w = MCE->do('check', 'fifo, check peekp at head     ',   'n', $q->peekp(5     ));
   $w = MCE->do('check', 'fifo, check peekp at index   0',   'n', $q->peekp(5,   0));
   $w = MCE->do('check', 'fifo, check peekp at index   2',   'a', $q->peekp(5,   2));
   $w = MCE->do('check', 'fifo, check peekp at index  13',   'k', $q->peekp(5,  13));
   $w = MCE->do('check', 'fifo, check peekp at index  20', undef, $q->peekp(5,  20));
   $w = MCE->do('check', 'fifo, check peekp at index  -2',   'j', $q->peekp(5,  -2));
   $w = MCE->do('check', 'fifo, check peekp at index -13',   'm', $q->peekp(5, -13));
   $w = MCE->do('check', 'fifo, check peekp at index -14',   'n', $q->peekp(5, -14));
   $w = MCE->do('check', 'fifo, check peekp at index -15', undef, $q->peekp(5, -15));
   $w = MCE->do('check', 'fifo, check peekp at index -20', undef, $q->peekp(5, -20));

   $q->clear;

   $q->enqueuep(5, $sappho_text);
   $w = MCE->do('check_unicode_in',  'fifo, check unicode enqueuep');
   $w = MCE->do('check_unicode_out', 'fifo, check unicode dequeue', $q->dequeue);

   $q->insertp(5, 0, $sappho_text);
   $w = MCE->do('check_unicode_out', 'fifo, check unicode peekp', $q->peekp(5, 0));
   $w = MCE->do('check_unicode_out', 'fifo, check unicode insertp', $q->dequeue_nb);

   $q->enqueuep(5, $sappho_text);
   $w = MCE->do('check_unicode_out', 'fifo, check unicode dequeue_timed', $q->dequeue_timed);

   return;
};

MCE::Flow->finish;

###############################################################################

##  LIFO tests

$q = $q2;

sub check_dequeue_lifo {
   my (@r) = @_;
   is( join('', @r), '432', 'lifo, check dequeue' );
   is( join('', @{ $q->_get_aref(5) }), '1', 'lifo, check array' );
}

mce_flow sub {
   my ($mce) = @_;
   my $w; # effect is waiting for the check (MCE->do) to complete

   $q->enqueuep(5, '1', '2');
   $q->enqueuep(5, '3');
   $q->enqueuep(5, '4');

   $w = MCE->do('check_enqueuep', 'lifo, check enqueuep');

   my @r = $q->dequeue(2);
   push @r, $q->dequeue;

   $w = MCE->do('check_dequeue_lifo', @r);

   $q->clear;

   $w = MCE->do('check_clear', 'lifo, check clear');

   $q->enqueuep(5, 'a', 'b', 'c', 'd');

   $q->insertp(5,   1, 'e', 'f');
   $q->insertp(5,   3, 'g');
   $q->insertp(5,  -2, 'h');
   $q->insertp(5,   7, 'i');
   $q->insertp(5,   9, 'j');
   $q->insertp(5,  20, 'k');
   $q->insertp(5, -10, 'l');
   $q->insertp(5, -12, 'm');
   $q->insertp(5, -20, 'n');

   $w = MCE->do('check_insertp', 'lifo, check insertp', 'kjaibhcgefldmn');
   $w = MCE->do('check_pending', 'lifo, check pending', $q->pending());

   $w = MCE->do('check', 'lifo, check peekp at head     ',   'n', $q->peekp(5     ));
   $w = MCE->do('check', 'lifo, check peekp at index   0',   'n', $q->peekp(5,   0));
   $w = MCE->do('check', 'lifo, check peekp at index   2',   'd', $q->peekp(5,   2));
   $w = MCE->do('check', 'lifo, check peekp at index  13',   'k', $q->peekp(5,  13));
   $w = MCE->do('check', 'lifo, check peekp at index  20', undef, $q->peekp(5,  20));
   $w = MCE->do('check', 'lifo, check peekp at index  -2',   'j', $q->peekp(5,  -2));
   $w = MCE->do('check', 'lifo, check peekp at index -13',   'm', $q->peekp(5, -13));
   $w = MCE->do('check', 'lifo, check peekp at index -14',   'n', $q->peekp(5, -14));
   $w = MCE->do('check', 'lifo, check peekp at index -15', undef, $q->peekp(5, -15));
   $w = MCE->do('check', 'lifo, check peekp at index -20', undef, $q->peekp(5, -20));

   $q->clear;

   $q->enqueuep(5, $sappho_text);
   $w = MCE->do('check_unicode_in',  'lifo, check unicode enqueuep');
   $w = MCE->do('check_unicode_out', 'lifo, check unicode dequeue', $q->dequeue);

   $q->insertp(5, 0, $sappho_text);
   $w = MCE->do('check_unicode_out', 'lifo, check unicode peekp', $q->peekp(5, 0));
   $w = MCE->do('check_unicode_out', 'lifo, check unicode insertp', $q->dequeue_nb);

   $q->enqueuep(5, $sappho_text);
   $w = MCE->do('check_unicode_out', 'lifo, check unicode dequeue_timed', $q->dequeue_timed);

   return;
};

MCE::Flow->finish;

###############################################################################

##  HIGHEST priority tests, mix-mode (normal and priority)

$q = $q3;

mce_flow sub {
   my ($mce) = @_;

   $q->enqueuep(5, 'a', 'b');    # priority queue
   $q->enqueuep(7, 'e', 'f');    # priority queue
   $q->enqueue (   'i', 'j');    # normal   queue
   $q->enqueuep(8, 'g', 'h');    # priority queue
   $q->enqueuep(6, 'c', 'd');    # priority queue

   my @h = $q->heap;

   MCE->do('check', 'highest, check heap', '8765', join('', @h));
   MCE->do('check', 'highest, check peekh at index  0', '8', $q->peekh( 0));
   MCE->do('check', 'highest, check peekh at index -2', '6', $q->peekh(-2));

   my @r = $q->dequeue(10);

   MCE->do('check', 'highest, check dequeue', 'ghefcdabij', join('', @r));

   return;
};

MCE::Flow->finish;

###############################################################################

##  LOWEST priority tests, mix-mode (normal and priority)

$q = $q4;

mce_flow sub {
   my ($mce) = @_;

   $q->enqueuep(5, 'a', 'b');    # priority queue
   $q->enqueuep(7, 'e', 'f');    # priority queue
   $q->enqueue (   'i', 'j');    # normal   queue
   $q->enqueuep(8, 'g', 'h');    # priority queue
   $q->enqueuep(6, 'c', 'd');    # priority queue

   my @h = $q->heap;

   MCE->do('check', 'lowest, check heap', '5678', join('', @h));
   MCE->do('check', 'lowest, check peekh at index  0', '5', $q->peekh( 0));
   MCE->do('check', 'lowest, check peekh at index -2', '7', $q->peekh(-2));

   my @r = $q->dequeue(10);

   MCE->do('check', 'lowest, check dequeue', 'abcdefghij', join('', @r));

   return;
};

MCE::Flow->finish;

done_testing;

