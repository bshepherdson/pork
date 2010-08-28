#!/usr/bin/perl

use strict;
use warnings;
use Curses;


initscr();
cbreak();
noecho();
nonl();

my ($rows, $cols);
getmaxyx(stdscr, $rows, $cols);

print STDERR "$rows x $cols\n";

# turn the top line into the status line
my $status_line = newwin(1, $cols, 0, 0);
my $main_win    = newwin($rows-1, $cols, 1, 0);

scrollok($main_win, 1);

# move to the bottom of the main window
move($main_win, $rows-2, 0);

# now accept lines of user input and store the first 30 characters of his last line into the status line
my $str;
while(1) {
	my $c = getch($main_win);
	print STDERR ord($c)."\n";

	if(ord($c) == 127) { # backspace
		chop($str);
		my ($r,$c);
		getyx($main_win, $r, $c);
		addch($main_win, $r, $c-1, " ");
		move($main_win, $r, $c-1);
		refresh($main_win);
	} elsif($c eq "\r") { # newline
		clear($status_line);
		addstr($status_line, 0, 0, $str);
		refresh($status_line);
		$str = '';
		scroll($main_win);
		move($main_win, $rows-2, 0);
	} else {
		addch($main_win, $c);
		$str .= $c;
	}
}


endwin();



