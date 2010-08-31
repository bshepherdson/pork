#!/usr/bin/perl

use strict;
use warnings;
use integer; # force integer operations
use feature "switch"; # access Perl 6 given/when syntax

use Curses;


if(@ARGV < 1) {
    print "Usage: $0 <story file>\n";
    exit 1;
}


# GENERAL NOTES
# * I use Fog Creek-style semantic Hungarian notation, as follows:
#   * w  = unsigned word
#   * sw = signed word
#   * b  = unsigned byte
#   * sb = signed byte
#   * ba = byte address (unsigned word)
#   * wa = word address (unsigned word, only used in abbreviation table)
#   * pa = packed address (unsigned word)
#   * ix = index
#   * t  = table (in the Z-machine sense), pat is the packed address of a table, etc.
#   * f  = flag, boolean value
#   * p  = Perl value


# alphabets
my @alphabets = ( [ undef, undef, undef, undef, undef, undef, 'a'..'z' ],
                  [ undef, undef, undef, undef, undef, undef, 'A'..'Z' ],
                  [ undef, undef, undef, undef, undef, undef, ' ', "\n",
                  '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
				  '.', ',', '!', '?', '_', '#', '\'', '"', 
				  '/', '\\', '-', ':', '(', ')' ] );

my %alphabet_table = ( qw( a 6 b 7 c 8 d 9 e 10 f 11 g 12 h 13 i 14 j 15 k 16 l 17 m 18 n 19 o 20 p 21 q 22 r 23 s 24 t 25 u 26 v 27 w 28 x 29 y 30 z 31 A 38 B 39 C 40 D 41 E 42 F 43 G 44 H 45 I 46 J 47 K 48 L 49 M 50 N 51 O 52 P 53 Q 54 R 55 S 56 T 57 U 58 V 59 W 60 X 61 Y 62 Z 63 0 72 1 73 2 74 3 75 4 76 5 77 6 78 7 79 8 80 9 81 ),
 ( ' ' => 0, "\n" => 71, '.' => 82, ',' => 83, '!' => 84, '?' => 85, '_' => 86, '#' => 87, "'" => 88, '"' => 89, '/' => 90, "\\" => 91, '-' => 92, ':' => 93, '(' => 94, ')' => 95 ) );


# these opcode names are not the same as in the spec tables, it's TYPE:HEX rather than TYPE:WHOLE_BYTE
# WARNING: use lowercase hex, as that's how it's printed by (s)printf's %x
my %store_opcodes;
for(qw( 2OP:8 2OP:9 2OP:f 2OP:10 2OP:11 2OP:12 2OP:13 2OP:14 2OP:15 2OP:16 2OP:17 2OP:18 2OP:19 1OP:1 1OP:2 1OP:3 1OP:4 1OP:8 1OP:e 1OP:f VAR:0 VAR:7 VAR:c VAR:16 VAR:17 VAR:18 EXT:0 EXT:1 EXT:2 EXT:3 EXT:4 EXT:9 EXT:a EXT:13) ){
	$store_opcodes{$_}++;
}

# same as above, but for branching instructions
my %branch_opcodes;
for(qw( 2OP:1 2OP:2 2OP:3 2OP:4 2OP:5 2OP:6 2OP:7 2OP:a 1OP:0 1OP:1 1OP:2 0OP:5 0OP:d 0OP:f VAR:17 VAR:1f EXT:6 EXT:18 EXT:1b )){
	$branch_opcodes{$_}++;
}

# initialized in a helper function, due to its length.
my %opcodes;

# Z-machine state
my %header; # stores all the parts of the header by name
my @mem; # main memory
my @stack; # the Z-machine user stack
my $sp;    # stack pointer for the user stack

# format of the routine stack: { sp ($#stack at the time of the call), return_addr (next instruction to execute on return), return_dest (variable to store the return value in, -1 for nowhere), locals (array of local variables, 0 is a dummy) }
my @routine_stack; # the interpreter's routine state stack, holding return addresses and local variables, plus the stack pointer at the start of the routine
my $rsp = -1; # gives the active index of the ROUTINE stack -- points at, rather than above, the top of the stack
my $pc; # the program counter
my $quit = 0;


my $MAXINT = ~(1 << 16); # 65,535
my $NEGATOR = 1 << 16;


# header names
# version
# abbreviations
# alphabet_table


#initialize opcodes
initialize_opcodes();

# initialize curses
initscr();
cbreak();
noecho();
nonl();

# set up the windows: a one-liner at the top and a main window
my ($rows, $cols);
getmaxyx(stdscr, $rows, $cols);
my $status_line = newwin(1, $cols, 0, 0);
my $main_window = newwin($rows-1, $cols, 1, 0);

scrollok($main_window, 1);

cursor_to_bottom();


# seed with the time, initially
srand();

# load up the story file
read_story_file($ARGV[0]);

$pc = get_word($header{main}); # it's a byte address, which is what I wanted

# and get things rolling by executing at main
while(!$quit) {
	decode_instr($pc);
}

endwin(); # curses cleanup

#################################################################
# reading story files
#################################################################

sub read_story_file {
	my $filename = shift;

	open(my($story), $filename) or die "Can't open story file $filename!\n";
	binmode $story;

	while(! eof($story)) {
		push @mem, getc($story);
	}

	close($story);

	# populate the %header hash with all the values from there that we're going to use
	# NOTE: These are READ-ONLY! Writing to them won't write back into memory.
	$header{version} = $mem[0];
	$header{flags1}  = $mem[1];
	$header{highmem} = get_word(4);
	$header{main}    = get_word(6);
	$header{dictionary} = get_word(8);
	$header{objects} = get_word(0xA);
	$header{globals} = get_word(0xC);
	$header{staticmem} = get_word(0xE);
	$header{flags2}    = get_word(0x10);
	$header{abbreviations} = get_word(0x18);
	$header{story_length}  = get_word(0x1A);
	$header{checksum}      = get_word(0x1C);
	$header{routine_offset} = get_word(0x28);
	$header{strings_offset} = get_word(0x2a);

	if($header{version} >= 5) {
		$header{alphabet_table} = get_word(0x34);
		if($header{alphabet_table}) {
			%alphabet_table = ();
			my $table = $header{alphabet_table};
			for(my $i = 0; $i < 26; $i++) {
				$alphabet_table{ $mem[ $table+$i ] } = $i+6;
				$alphabet_table{ $mem[ $table+$i+26 ] } = $i+38;
				$alphabet_table{ $mem[ $table+$i+52 ] } = $i+70;
			}

			$alphabet_table{' '} = 0; # force space to be encoded as 0

		}
	}


	# now set header values that are in the interpreter's control
	if($header{version} <= 3) {
		clear_bit($mem[1], 4); # status line is available
		clear_bit($mem[1], 5); # screen splitting is not available
		clear_bit($mem[1], 6); # variable-width font is not default
	} else {
		$mem[1] = 0b0001_0000; # no fancy features but fixed-width fonts
	}

	clear_bit($mem[0x11], 0);
	clear_bit($mem[0x10], 7);
	clear_bit($mem[0x10], 5);
	clear_bit($mem[0x10], 4);
	clear_bit($mem[0x10], 3);

	# and the screen info
	my ($rows, $cols);
	getmaxyx(stdscr, $rows, $cols);

	$mem[0x20] = $rows; # screen height in lines
	$mem[0x21] = $cols; # screen width in columns
	set_word(0x22, $cols); # screen width in units (characters, pre-6)
	set_word(0x24, $rows); # screen height in unit (characters, pre-6)
	$mem[0x26] = 1;
	$mem[0x27] = 1;

	# these two instructions are Store in 5+
	if($header{version} >= 5) {
		$store_opcodes{"0OP:9"}++; # catch
		$store_opcodes{"VAR:4"}++; # read
	}

}


#################################################################
# curses and screens
#################################################################

sub cursor_to_bottom {
	move($main_window, $rows-2, 0);
}

# takes a maximum number of characters and whether to read a whole line (1) or just one character (0)
# returns what the user typed either way
sub read_mode {
	my ($max, $line) = @_;

	if($line) {
		my $str = "";
		while(1) {
			my $c = getch($main_window);

			if(ord($c) == 127) { # backspace
				chop($str);
				my ($r,$c);
				getyx($main_window, $r, $c);
				addch($main_window, $r, $c-1, " ");
				move($main_window, $r, $c-1);
				refresh($main_window);
			} elsif($c eq "\r") { # newline
				scroll($main_window);
				move($main_window, $rows-2, 0);
				break;
			} else {
				if(length($str) < $max){
					addch($main_window, $c);
					$str .= $c;
				}
			}
		}

		return $str;
	} else { # single character
		my $c = getch($main_window);
		return $c;
	}
}


#################################################################
# instruction implementations
#################################################################

sub op_add { # signed 16-bit addition
	my ($op, $store, $a, $b) = @_;

	write_var($store, arg2p_s($a) + arg2p_s($b));
}

sub op_and { # bitwise AND
	my ($op, $store, $a, $b) = @_;

	write_var($store, arg2p($a) & arg2p($b));
}

sub op_art_shift { # arithmetic shift. left on positive, right on negative.
	check_version(5, -1);

	my ($op, $store, $number, $places) = @_;

	# need to treat these as unsigned because Perl's shifts are unsigned
	my $p_number = arg2p($number);
	my $p_places = arg2p($places);

	if($p_places >= 0) {
		$p_number <<= $p_places;
	} elsif($p_places < 0) {
		my $f_neg = bit(15, $p_number);
		$p_number >>= -$p_places;

		# now extend the sign bit if it was negative
		if($f_neg){
			for((16+$p_places)..15) { # should be 13..15 for 3 places, for example
				set_bit($p_number, $_);
			}
		}
	}

	write_var($store, $p_number);
}


sub op_buffer_mode {
	die "buffer_mode is not implemented yet";
}

sub op_call_n {
	my ($opcode, @ops) = @_;
	imp_call_master($opcode, -1, @ops);
}
sub op_call_s {
	imp_call_master(@_);
}

sub imp_call_master {
	my ($opcode, $store, @ops) = @_;

	my $frame = { sp => $sp, locals => [], return_addr => $pc, return_dest => $store };
	push @routine_stack, $frame;
	$rsp++;

	# the first arg in all cases
	my $pa_routine = arg2p(shift @ops); # shift off the routine
	my $a_routine  = unpack_addr($pa_routine);

	my $b_locals = $mem[$a_routine];

	if($header{version} <= 4) {
		for(0..$b_locals) {
			$frame->{locals}->[$_] = get_word($a_routine + 1 + 2*$_);
		}
	} else {
		for(0..$b_locals) {
			$frame->{locals}->[$_] = 0;
		}
	}

	# store the args over top of the locals, stopping when either runs out
	for(my $i = 0; $i < @ops && $i < $b_locals; $i++) {
		$frame->{locals}->[$i] = arg2p($ops[$i]);
	}

	# move the pc
	$pc = $a_routine + 1 + 2*$b_locals;

	# and execution continues with the normal loop
}


sub op_pop__catch {
	if($header{version} >= 5) { # catch
		my ($opcode, $store) = @_;
		write_var($store, $rsp);
	} else { # pop
		pop_stack();
	}
}


sub op_check_arg_count {
	my ($opcode, $branch_on, $branch_offset, $number) = @_;
	my $p_number = arg2p($number);

	return if $rsp < 0; # not in a routine, just do nothing
	if(defined($routine_stack[$rsp]->{locals}->[$p_number]) == $branch_on) {
		branch($branch_offset);
	}

	# otherwise it does nothing
}


sub op_clear_attr {
	my ($opcode, $object, $attr) = @_;
	imp_handle_attr($object, $attr, 0);
}

# hacky code to handle object attributes
# if $to is 1, attr is set. if 0, attr is cleared. if -1, attr value is returned
sub imp_handle_attr {
	my ($object, $attr, $to) = @_;
	my $p_object = arg2p($object);
	my $p_attr   = arg2p($attr);

	my $obj = get_object_addr($p_object);

	my $bits = $header{version} >= 4 ? 48 : 32;

	my $byte = $attr >> 3;
	# now flip attr around and find the target bit
	$attr = $bits - $attr - 1;
	my $bit  = $attr % 8;

	if($to == 1){
		set_bit($mem[$obj+$byte], $bit);
	} elsif($to == 0) {
		clear_bit($mem[$obj+$byte], $bit);
	} elsif($to == -1) {
		return bit($mem[$obj+$byte], $bit);
	}
}


# NOTE: This instruction is called "table" but the description says "bytes".
# I assume these are not Z-machine tables per se, just arrays, and that $p_size is a number of bytes to be blindly copied
sub op_copy_table {
	my ($opcode, $first, $second, $size) = @_;
	my ($p_first, $p_second) = map { arg2p($_) } ($first, $second);
	my $p_size = arg2p_s($size);

	if($p_second == 0) {
		for(my $i = 0; $i < $p_size; $i++) {
			$mem[$first + $i] = 0;
		}
	} else {
		my $backwards = 0;
		if($p_size < 0) {
			$p_size = abs $p_size;
		} elsif($p_first < $p_second && $p_second < $p_first + $p_size) { 
			$backwards = 1;
		}

		if($backwards){
			for(my $i = $p_size-1; $i >= 0; $i--) {
				$mem[$second + $i] = $mem[$first + $i];
			}
		} else {
			for(my $i = 0; $i < $p_size; $i++) {
				$mem[$second + $i] = $mem[$first + $i];
			}
		}
	}
}


sub op_dec {
	my ($opcode, $var) = @_;
	my $p_var = arg2p($var);

	my $p_value = from_signed(read_var($p_var));
	write_var($p_value-1);
}

sub op_dec_chk {
	my ($opcode, $branch_on, $branch_offset, $var, $below) = @_;

	my $p_var = arg2p($var);
	my $p_below = arg2p_s($below);

	my $p_value = from_signed(read_var($p_var));
	$p_value--;
	write_var($p_value);

	if(($p_value < $below) == $branch_on) {
		branch($branch_offset);
	}
}

sub op_div {
	my ($opcode, $store, $a, $b) = @_;

	my $p_a = arg2p_s($a);
	my $p_b = arg2p_s($b);

	die "Division by zero!" if $p_b == 0;

	write_var($store, $p_a / $p_b);
}


sub op_encode_text {
	my $opcode = shift;
	my ($p_zscii_text, $p_length, $p_from, $p_dest) = map { arg2p($_) } @_;

	my @chars;
	for(my $i = $p_from; $i < $p_from+$p_length; $i++) {
		push @chars, zscii_chr($mem[$p_zscii_text + $i]);
	}

	my @ws = encode_str(\@chars, dictionary_length());

	for(0..$#ws) {
		set_word($p_dest + 2*$_, $ws[$_]);
	}
}

sub op_erase_line {
	die "erase_line not implemented";
}


sub op_get_child {
	my ($opcode, $store, $branch_on, $branch_offset, $obj) = @_;

	my $p_obj = arg2p($obj);

	my $a_obj = get_object($p_obj);
	my $child;
	if($header{version} <= 3) {
		$child = $mem[ $a_obj + 4 + 2 ];
	} else {
		$child = get_word($a_obj + 6 + 4);
	}

	write_var($store, $child);

	branch($branch_offset) if $branch_on == !!$child; # branch if child isn't 0
}


sub op_get_cursor {
	my ($opcode, $array) = @_;
	my $p_array = arg2p($array);

	my ($row, $col);
	getyx($main_window, $row, $col);
	set_word($array, $row);
	set_word($array+2, $col);
}


sub op_get_next_prop {
	my ($opcode, $store, $obj, $prop) = @_;

	my $p_obj = arg2p($obj);
	my $p_prop = arg2p($prop);

	my $a_obj = get_object($p_obj);
	my $p_prop_struct = get_prop($a_obj, $prop, 0);

	if(!defined($p_prop_struct)) { # no such property
		die "No property $p_prop for object $p_obj, ack!";
	}

	my $p_nextprop = build_prop($p_prop_struct->{next});

	write_var($store, $p_nextprop->{number});
}


sub op_get_parent {
	my ($opcode, $store, $obj) = @_;

	my $p_obj = arg2p($obj);
	my $a_obj = get_object($p_obj);

	my $p_parent;

	if($header{version} <= 3){
		$p_parent = $mem[$a_obj + 4];
	} else {
		$p_parent = get_word($a_obj + 6);
	}

	write_var($store, $p_parent);
}


sub op_get_prop {
	my ($opcode, $store, $obj, $propnum) = @_;
	my $p_obj = arg2p($obj);
	my $p_propnum = arg2p($propnum);

	my $a_obj = get_object($p_obj);
	my $prop  = get_prop($a_obj, $p_propnum, 1);

	my $value;
	if($prop->{size} == 1) {
		$value = $mem[$prop->{data}];
	} elsif ($prop->{size} == 2) {
		$value = get_word($prop->{data});
	} else {
		die "get_prop called on property with length > 2";
	}

	write_var($store, $value);
}


sub op_get_prop_addr {
	my ($opcode, $store, $obj, $propnum) = @_;
	
	my $prop = args2prop($obj, $propnum, 0);

	if(defined($prop)){
		write_var($store, $prop->{data});
	} else {
		write_var($store, 0);
	}
}

sub op_get_prop_len {
	my ($opcode, $store, $obj, $propnum) = @_;
	
	my $prop = args2prop($obj, $propnum, 0);

	if(defined($prop)){
		write_var($store, $prop->{size});
	} else {
		die "Attempt to get_prop_len on object without requested property.";
	}
}

sub op_get_sibling {
	my ($opcode, $store, $branch_on, $branch_offset, $obj) = @_;

	my $a_obj = args2obj($obj);

	my $p_sibling;
	if($header{version} <= 3){
		$p_sibling = $mem[$a_obj + 5];
	} else {
		$p_sibling = get_word($a_obj + 8);
	}

	if($p_sibling == $branch_on) {
		branch($branch_offset);
	}
}


sub op_inc {
	my ($opcode, $var) = @_;

	my $p_var = arg2p($var);

	my $value = from_signed(read_var($p_var));
	write_var($value+1);
}

sub op_inc_chk {
	my ($opcode, $branch_on, $branch_offset, $var, $above) = @_;

	my $p_var = arg2p($var);
	my $p_above = arg2p_s($var);

	my $value = from_signed(read_value($p_var));
	$value--;
	write_value($p_var, $value);

	if(!!$branch_on == ($value > $above)){
		branch($branch_offset);
	}
}


sub op_input_stream {
	die "input_stream not implemented yet";
}

sub op_insert_obj {
	my ($opcode, $obj, $dest) = @_;

	my $p_obj = arg2p($obj);
	my $p_dest = arg2p($dest);

	# detach the moving object from its current location
	remove_object($p_obj);

	# and attach it to the new destination
	my $child = get_child($p_dest);
	set_child($p_dest, $p_obj);
	set_parent($p_obj, $p_dest);
	set_sibling($p_obj, $child);
}


sub op_je {
	jump(sub { my ($a, $b) = @_; $a == $b }, @_);
}
sub op_jg {
	jump(sub { my ($a, $b) = @_; $a > $b }, @_);
}
sub op_jl {
	jump(sub { my ($a, $b) = @_; $a < $b }, @_);
}

sub op_jin {
	my ($opcode, $branch_on, $branch_offset, $a, $b) = @_;

	my $p_a = arg2p($a);
	my $p_b = arg2p($b);

	if(!!$branch_on == (get_parent($p_a) == $p_b)) {
		branch($branch_offset);
	}
}

sub op_jump {
	my ($opcode, $address) = @_;

	$pc = arg2p($address);
}

sub op_jz {
	my ($opcode, $branch_on, $branch_offset, $a) = @_;
	my $p_a = arg2p($a);

	if(!!$branch_on == ($p_a == 0)){
		branch($branch_offset);
	}
}


sub op_load {
	my ($opcode, $store, $source) = @_;
	my $p_source = arg2p($source);

	write_var($store, read_var($p_source));
}

sub op_loadb {
	my ($opcode, $store, $array, $index) = @_;
	my $p_array = arg2p($array);
	my $p_index = arg2p($index);

	write_var($store, $mem[$p_array + $p_index]);
}

sub op_loadw {
	my ($opcode, $store, $array, $index) = @_;
	my $p_array = arg2p($array);
	my $p_index = arg2p($index);

	write_var($store, get_word($p_array + 2*$p_index));
}


sub op_log_shift {
	my ($opcode, $store, $num, $places) = @_;
	my $p_num = arg2p($num);
	my $p_places = arg2p($places);

	$p_num = ($places < 0 ? $p_num >> -$places : $p_num << $places);
	write_var($store, $p_num);
}


sub op_mod {
	my ($opcode, $store, $a, $b) = @_;
	my $p_a = arg2p_s($a);
	my $p_b = arg2p_s($b);

	if($p_b == 0){
		die "Attempt to mod by 0.";
	}

	write_var($store, $p_a % $p_b);
}

sub op_mul {
	my ($opcode, $store, $a, $b) = @_;
	my $p_a = arg2p_s($a);
	my $p_b = arg2p_s($b);

	write_var($store, $p_a * $p_b);
}

sub op_new_line {
	my ($opcode) = @_;

	print_perl_str("\n");
}

sub op_nop {
	# do nothing
}

sub op_not {
	my ($opcode, $store, $value) = @_;
	my $p_value = arg2p($value);

	write_var($store, (~$p_value) & 0xffff);
}

sub op_or {
	my ($opcode, $store, $a, $b) = @_;
	my $p_a = arg2p($a);
	my $p_b = arg2p($b);

	write_var($store, $a | $b);
}

sub op_output_stream {
	die "output_stream infiltrated";
}

sub op_piracy {
	my ($opcode, $branch_on, $branch_offset) = @_;

	branch($branch_offset);
}


sub op_print {
	my ($opcode) = @_;

	print_literal_string();
}

sub op_print_addr {
	my ($opcode, $addr) = @_;
	my $p_addr = arg2p($addr);

	print_perl_str(decode_str($addr));
}

sub op_print_char {
	my ($opcode, $char) = @_;
	my $p_char = arg2p($char);

	print_perl_str(zscii_chr($p_char));
}


sub op_print_obj {
	my ($opcode, $obj) = @_;

	print_object($obj);
}

sub op_print_paddr {
	my ($opcode, $addr) = @_;

	my $pa = arg2p($addr);
	print_perl_string(decode_str(unpack_addr($pa)));
}

sub op_print_ret {
	my ($opcode) = @_;

	print_literal_str();

	return_(1);
}


sub op_print_table {
	die "print_table is not implemented";
}

sub op_print_unicode {
	die "print_unicode is not implemented";
}

sub op_pull {
	my ($opcode, $var) = @_;

	my $p_var = arg2p($var);
	my $value = pop_stack();
	write_var($p_var, $value);
}

sub op_push {
	my ($opcode, $value) = @_;
	my $p_value = arg2p($value);

	push_stack($p_value);
}

sub op_put_prop {
	my ($opcode, $obj, $propnum, $value) = @_;
	my $p_value = arg2p($value);

	my $prop = args2prop($obj, $propnum, 0);

	die "put_prop: property not defined" unless defined($prop);

	if($prop->{size} > 2) {
		die "put_prop: property is larger than 2 bytes";
	} elsif($prop->{size} == 2) {
		set_word($prop->{data}, $p_value);
	} elsif($prop->{size} == 1) {
		$mem[ $prop->{data} ] = 0xff & $p_value;
	}
}

sub op_quit {
	my ($opcode) = @_;

	$quit = 1; # the main loop will quit on the next pass
}

sub op_random {
	my ($opcode, $store, $range) = @_;
	my $p_range = arg2p_s($range);

	my $ret = 0;
	if($p_range < 0) {
		srand($p_range);
	} elsif($p_range > 0) {
		$ret = int(rand($p_range))+1;
	} else {
		srand(); # re-seed using the time/pid/etc.
	}

	write_var($store, $ret);
}

# TODO: Implement the timeout functionality -- right now it's ignored
sub op_read {
	my ($opcode, $store, $text, $parse, $time, $routine);
	if($header{version} <= 3) {
		($opcode, $text, $parse) = @_;
	} elsif($header{version} == 4) {
		($opcode, $text, $parse, $time, $routine) = @_;
	} else { # 5 and up
		($opcode, $store, $text, $parse, $time, $routine) = @_;
	}

	if($header{version} <= 3) {
		update_status_line();
	}

	my $a_text = arg2p($text);
	my $max_chars;
	my $a_chars; # address where characters go

	# in version 5 and later, byte 1 contains the number of characters typed so far
	if($header{version} >= 5){
		$max_chars = $mem[$a_text] - $mem[$a_text+1];
		$a_chars = $a_text + 2 + $mem[$a_text+1]; # skipping over any text already present
	} else {
		$max_chars = $mem[$a_text] - 1; # "minus 1" in the spec... not clear. erring on the side of not overflowing
		$a_chars = $a_text+1;
	}

	# read a whole line, with a maximum of $max_chars characters of input
	my $str = read_mode($max_chars, 1);
	$str = lc $str; # force it to lowercase

	# copy it into the text buffer starting at $a_chars;
	my @chars = split //, $str;
	for my $i (0..$#chars) {
		$mem[$a_chars + $i] = zscii_ord($chars[$i]);
	}

	# set the length byte in V5+
	if($header{version} >= 5) {
		$mem[$a_text+1] += scalar(@chars);
	} else { # and in lower versions, write the 0 byte at the end
		$mem[$a_text+scalar(@chars)] = 0;
	}

	# and now the lexing and matching
	my $a_parse = arg2p($parse);
	if($header{version} < 5 || $a_parse > 0) { # version 5+ can skip parsing if a_parse is 0
		my $max_words = $mem[$a_parse];

		my $a_dict_hdr = $header{dictionary};

		my $num_seps = $mem[$a_dict_hdr];
		my @word_seps = map { zscii_chr($_) } @mem[$a_dict_hdr+1..$a_dict_hdr+$num_seps];

		my @words;
		my @chars = split //, $str;

		my $buf = '';
		my $start = 0;
		for my $i (0..$#chars) {
			given($chars[$i]) {
				when( [ ' ', @word_seps ] ) {
					if($buf ne '') {
						push @words, [ $start, $buf, length($buf) ];
						$buf = '';
					}

					if($_ ne ' ') {
						push @words, [ $i, $_, 1 ];
					}
				}
				default {
					$start = $i if $buf eq ''; # move start if this is the first letter of the word
					$buf .= $_;
				}
			}
		}
		push @words, $buf unless $buf eq '';

		
		# now begin searching the dictionary for the words.
		my $entry_len = $mem[$a_dict_hdr + $num_seps + 1]; # in bytes
		my $num_entries = get_word($a_dict_hdr + $num_seps + 2);
		my $a_first = $a_dict_hdr + $num_seps + 4; # skip over: n, n seps, 1 byte and 1 word

		my @words_parsed;
		my $wpw = $header{version} >= 4 ? 3 : 2; # "words per word"


		for(@words) {
			my @enc = encode_str($_->[1]);

			my $ix_left = 0;
			my $ix_right = $num_entries - 1;

			my $found = 0;
			my $dir = 0;
			while(!$found) {
				my $ix_mid = ($ix_right - $ix_left)/2;
				my $a_mid = $a_first + $entry_len * $ix_mid;

				# compare with mid
				for(my $i = 0; $i < $wpw; $i++){
					given($enc[$i] <=> get_word($a_mid + $i*2)) {
						when(-1) { # enc word comes before this one
							$ix_right = $ix_mid;
						}
						when(1)  { # enc word comes after this one
							$ix_left = $ix_mid;
						}
						when(0) { # matches
							if($i == $wpw-1) { # found it exactly
								push @words_parsed, [ $a_mid, $_->[2], $_->[0] ]; # address, length, location in command
								$found = 1;
							} # otherwise check the next word too
						}
					}
				}
			}
		}

		# now we have the list of parsed words in @words_parsed
		# so write them into the buffer

		$max_words = $mem[$a_parse];
		my $words_written = 0;
		for($words_written = 0; $words_written < $max_words && $words_written < @words_parsed; $words_written++) {

			my $w = $words_parsed[$words_written];
			my $a = $a_parse + 1 + 4*$words_written;
			set_word($a, $w->[0]); # write the address
			$mem[$a+1] = $w->[1];  # write the number of letters
			$mem[$a+2] = $w->[2];  # and the location in the text
		}


		$mem[$a_parse] = $words_written;


		if($header{version} >= 5) {
			write_var($store, 10); # write a terminator of 10 (newline) into the buffer
			# TODO: That won't always be true once interrupts are on. and there's something in v5 about different EOL characters?
		}

	}
}


sub op_read_char {
	die "read_char is not implemented";
}

sub op_remove_obj {
	my ($opcode, $obj) = @_;

	my $p_obj = arg2p($obj);
	remove_object($p_obj);
}

sub op_restart {
	die "restart not implemented";
}

sub op_restore {
	die "restore not implemented";
}

sub op_restore_undo {
	die "restore_undo not implemented";
}

sub op_ret {
	my ($opcode, $value) = @_;
	my $p_value = arg2p($value);

	return_($p_value);
}

sub op_ret_popped {
	my ($opcode) = @_;

	my $val = pop_stack();
	return_($val);
}

sub op_rfalse {
	my ($opcode) = @_;
	return_(0);
}

sub op_rtrue {
	my ($opcode) = @_;
	return_(1);
}

sub op_save {
	die "save not implemented";
}

sub op_save_undo {
	die "save_undo not implemented";
}

sub op_scan_table {
	die "scan_table is not implemented";
}


sub op_set_attr {
	my ($opcode, $object, $attr) = @_;
	imp_handle_attr($object, $attr, 1);
}

sub op_set_colour {
	die "set_colour is not implemented";
}

sub op_set_cursor {
	die "set_cursor is not implemented";
}

sub op_set_font {
	die "set_font is not implemented";
}

sub op_set_text_style {
	die "set_text_style is not implemented";
}

sub op_set_window {
	die "set_window is not implemented";
}

sub op_show_status {
	my ($opcode) = @_;

	if($header{version} == 3) { # version 3 only
		update_status_line();
	} else { # nop in later versions, Wishbringer v5 r23 contains it by accident
		# nop
	}
}

sub op_sound_effect {
	die "sound_effect is not implemented";
}

sub op_split_window { 
	die "split_window is not implemented";
}


sub op_store {
	my ($opcode, $var, $value) = @_;

	my $p_var = arg2p($var);
	my $p_value = arg2p($value);

	write_var($p_var, $p_value);
}

sub op_storeb {
	my ($opcode, $array, $index, $value) = @_;
	my $p_array = arg2p($array);
	my $p_index = arg2p($index);
	my $p_value = arg2p($value);

	$mem[$p_array + $p_index] = 0xff & $p_value;
}

sub op_storew {
	my ($opcode, $array, $index, $value) = @_;
	my $p_array = arg2p($array);
	my $p_index = arg2p($index);
	my $p_value = arg2p($value);

	set_word($p_array + 2*$p_index, $value);
}


sub op_sub {
	my ($opcode, $store, $a, $b) = @_;
	my $p_a = arg2p_s($a);
	my $p_b = arg2p_s($b);

	write_var($store, $p_a - $p_b);
}

sub op_test {
	my ($opcode, $branch_on, $branch_offset, $bitmap, $flags) = @_;
	my $p_bitmap = arg2p($bitmap);
	my $p_flags  = arg2p($flags);

	if($branch_on == (($p_bitmap & $p_flags) == $p_flags)) {
		branch($branch_offset);
	}
}


sub op_test_attr {
	my ($opcode, $branch_on, $branch_offset, $obj, $attr) = @_;
	
	my $value = imp_handle_attr($obj, $attr, -1);

	if($branch_on == $value) {
		branch($branch_offset);
	}
}


sub op_throw {
	my ($opcode, $value, $frame) = @_;
	my $p_value = arg2p($value);
	my $p_frame = arg2p($frame);

	# move the rsp
	$rsp = $p_frame;
	# and then return
	return_($p_value);
}


sub tokenise {
	die "tokenise is not implemented";
	#my ($opcode, $text, $parse, $dictionary, $flag) = @_;
}


# just a nop for now, assumes the checksums pass
# TODO: actually implement the checksums
sub verify {
	my ($opcode, $branch_on, $branch_offset) = @_;

	if($branch_on){
		branch($branch_offset);
	}
}



#################################################################
# objects and properties
#################################################################

# complex operation to detach an object from the tree and give it parent 0.
# it keeps its children, and is removed neatly
# object /number/, not address
sub remove_object {
	my ($obj) = @_;
	# so this is complex as fuck:
	# * go up to parent
	# * go down to child at the head of the linked list of siblings
	# * walk down, remembering the previous as well as current
	# * stop on reaching this object, then set the previous one's sibling to this one's sibling
	# * now set the parent of this one to 0

	my $parent = get_parent($obj);
	my $prev = 0;
	my $child = get_child($parent);

	# special case for being the first child
	if($child == $obj){
		set_child($parent, get_sibling($obj));
		set_parent($obj, 0);
		return;
	}

	do {
		$prev = $child;
		$child = get_sibling($child);
	} until ($child == $obj);

	set_sibling($prev, get_sibling($child));
	set_parent($obj, 0);
}


# helper function for one of the common patterns
# takes an object number in argument form and return the byte address of the object
sub args2obj {
	my ($obj) = @_;
	my $p_obj = arg2p($obj);
	return get_object($p_obj);
}

# helper function for one of the common patterns
# takes an object number and a property number in argument form, plus the default flag, and returns the prop object or undef
sub args2prop {
	my ($obj, $num, $default) = @_;
	my $p_obj = arg2p($obj);
	my $p_num = arg2p($num);

	my $a_obj = get_object($p_obj);
	return get_prop($a_obj, $p_num, $default);
}


# given the address of an object, returns the number of its parent
sub get_parent {
	my ($obj) = @_;
	
	return get_related_obj($obj, 0);
}

sub get_sibling {
	my ($obj) = @_;
	return get_related_obj($obj, 1);
}

sub get_child {
	my ($obj) = @_;
	return get_related_obj($obj, 2);
}

sub get_related_obj {
	my ($obj, $index) = @_;

	my $a_obj = get_object($obj);
	my $related;
	if($header{version} <= 3) {
		$related = $mem[$a_obj + 4 + $index];
	} else {
		$related = get_word($a_obj + 6 + 2*$index);
	}

	return $related;
}

# given the address of an object and the number of another, changes the first to point to the second
sub set_parent {
	my ($obj, $to) = @_;
	
	set_related_obj($obj, 0, $to);
}

sub set_sibling {
	my ($obj, $to) = @_;
	set_related_obj($obj, 1, $to);
}

sub set_child {
	my ($obj, $to) = @_;
	set_related_obj($obj, 2, $to);
}

sub set_related_obj {
	my ($obj, $index, $to) = @_;

	my $a_obj = get_object($obj);
	if($header{version} <= 3) {
		$mem[$a_obj + 4 + $index] = $to;
	} else {
		set_word($a_obj + 6 + 2*$index, $to);
	}
}


# takes an object number, returns the address of the object's fields
sub get_object {
	my ($obj) = @_;

	my $a = $header{objects};
	$a += $header{version} <= 3 ? 31 : 63;

	# now $a is the beginning of the object tree proper
	my $obj_size = $header{version} <= 3 ? 9 : 14;

	return $a + $obj_size * ($obj-1);
}

# builds a property struct in the following format, given the address of a property:
# { number => this property's number, data => pointer to the start of the data, size => the number of data bytes, next => pointer to the next property }
sub build_prop {
	my ($a_prop) = @_;

	if($header{version} <= 3) {
		my $prop = {};
		my $b_size = $mem[$a_prop];

		return undef unless $b_size;

		$prop->{size}   = $b_size >> 5 + 1;
		$prop->{number} = $b_size & 0b1_1111;
		$prop->{data}   = $a_prop + 1;
		$prop->{next}   = $a_prop + 1 + $prop->{size};
		return $prop;
	} else {
		my $prop = {};
		my $b_size = $mem[$a_prop];

		if(bit($b_size, 7)){
			$prop->{number} = $b_size & 0b11_1111;
			$prop->{size}   = $mem[$a_prop+1] & 0b11_1111;
			if($prop->{size} == 0){
				$prop->{size} = 64;
			}

			$prop->{data} = $a_prop + 2;
			$prop->{next} = $a_prop + 2 + $prop->{size};
		} else {
			$prop->{number} = $b_size & 0b11_1111;
			$prop->{size}   = bit($b_size, 6) ? 2 : 1;
			$prop->{data}   = $a_prop + 1;
			$prop->{next}   = $a_prop + 1 + $prop->{size};
		}

		return $prop;
	}
}


# takes an object address, and a property number, and retrieves the property structure (as given in build_prop above)
# there is an optional third argument that chooses whether to use the default if not found. defaults to 1. returns undef otherwise.
sub get_prop {
	my ($a_obj, $number, $default) = @_;
	$default = 1 unless defined($default);

	my $a_prop = get_word($a_obj  + ($header{version} <= 3 ? 7 : 12));

	my $prop = build_prop($a_prop);
	# properties are stored in descending numerical order
	while($prop && $prop->{number} && $prop->{number} > $number){
		$prop = build_prop($prop->{next});
	}

	# now that it's done, there are several possibilities for what we've found:
	if($prop->{number} == $number) { # found it
		return $prop;
	} elsif($prop->{number} < $number) { # passed without finding it
		if($default){
			# the defaults are stored in the header of the object tree
			$prop = { number => $number, next => undef, data => $header{objects} + ($number-1)*2, size => 2 };
			return $prop;
		} else {
			return undef;
		}
	}
}











	



#################################################################
# instruction utilities
#################################################################


sub return_ {
	my ($ret_val) = @_;

	my $state = $routine_stack[$rsp];

	# move the stack pointer
	$sp = $state->{sp};
	# and the pc
	$pc = $state->{return_addr};

	# now move $rsp down so store_var will use the return target's locals rather than mine
	$rsp--;
	
	# delete locals so they get garbage-collected
	$state->{locals} = undef;

	if($state->{return_dest} >= 0) {
		store_var($state->{return_dest}, $ret_val);
	}
}


sub print_literal_str {
	my $str = decode_str($pc);
	print_perl_str($str);

	# now move pc
	while(!bit( get_word($pc), 15)){ 
		$pc += 2;
	}
	$pc += 2;
}


# takes a sub ref followed by normal op arguments
sub jump {
	my ($test, $opcode, $branch_on, $branch_offset, $a, $b) = @_;

	my $p_a = arg2p_s($a);
	my $p_b = arg2p_s($b);

	my $result = $test->($p_a, $p_b);
	if(!!$result == !!$branch_on){
		branch($branch_offset);
	}
}

# returns the number of characters per dictionary entry
sub dictionary_length {
	return $header{version} <= 3 ? 6 : 9;
}

# unpacks a packed addr into a Perl value (it might be longer than 16-bit) holding a byte addr
sub unpack_addr {
	my ($pa) = @_;

	if($header{version} <= 3){
		return $pa*2;
	} elsif(4 <= $header{version} && $header{version} <= 5) {
		return $pa*4;
	} elsif($header{version} == 8) {
		return $pa*8;
	} else {
		die "Unsupported version; can't unpack address";
	}
}

# dies if the version is not in the specified range
# use -1 for "never"
# that is (-1, 3) is any version up to and including 3
# and (5, -1) is 5 or later
sub check_version {
	my ($start, $stop) = @_;

	if($start < 0) {
		die "Illegal instruction: only supported in versions up to $stop" unless $header{version} <= $stop;
	} elsif ($stop < 0) {
		die "Illegal instruction: only supported in versions $start and later" unless $header{version} >= $start;
	} else {
		die "Illegal instruction: only supported in version between $start and $stop inclusive" unless $start <= $header{version} && $header{version} <= $stop;
	}

	# otherwise it returns silently
}


# decoding of arguments
# arg types: 0 = large constant, 1 = small constant, 2 = var by value, 3 = omitted (3 should never happen)
sub arg2p {
	my ($arg) = @_;
	my ($type, $value) = @$arg;

	given($type){
		when(0) { return $value; }
		when(1) { return $value; }
		when(2) { return read_var($value); }
		when(3) { die "CAN'T HAPPEN: Got 'omitted' operand type"; }
	}
}

sub arg2p_s {
	my ($arg) = @_;
	my ($type, $value) = @$arg;
	
	given($type){
		when(0) { return from_signed($value); }
		when(1) { return from_signed_b($value); }
		when(2) { return from_signed(read_var($value)); }
		when(3) { die "CAN'T HAPPEN: Got 'omitted' operand type"; }
	}
}

# reading and writing vars
# takes a Perl value, writes a Z-machine value
sub write_var {
	my ($var, $value) = @_; 

	if($var == 0) {
		push_stack($value);
	} elsif(0 < $var && $var <= 15) { # local variable
		if($rsp < 0) { # attempting to access a local var from main
			die "Attempting to access a local variable ($var) from main";
		}

		my $frame = $routine_stack[$rsp];
		die "Illegal attempt to write non-existent local var $var" unless defined($frame->{locals}->[$var]);
		$frame->{locals}->[$var] = to_word($value);
	} else { # global
		set_word($header{globals} + $var - 0x10, to_word($value));
	}
}

# returns a Z-machine value
sub read_var {
	my ($var) = @_;

	if($var == 0) {
		return pop_stack($var);
	} elsif(0 < $var && $var <= 15) { # locals
		if($rsp < 0) { # attempt to access a local var from main
			die "Attempting to access a loval variable ($var) from main";
		}

		my $frame = $routine_stack[$rsp];
		return $frame->{locals}->[$var];
	} else { # global
		return get_word($header{globals} + $var - 0x10);
	}

	return undef; # can't happen
}


sub decode_instr {
	my ($a) = @_;

	my $b = $mem[$a];
	my $form = bits($b, 6..7);
	my @op_types; # 0 = large constant, 1 = small constant, 2 = var by value, 3 = omitted
	my $opcode;
	my $a_ops; # the index where ops begin

	my $instr_type; # 0OP, 1OP, 2OP, VAR, EXT

	given($form){
		when(3) { # variable
			$opcode = bits($b, 0..4);

			my $op_bytes = ( $opcode == 12 || $opcode == 26 ) ? 2 : 1;

			my @raw_op_types;
			if($op_bytes == 2){
				push @raw_op_types, @mem[$a+1, $a+2];
			} else {
				push @raw_op_types, $mem[$a+1];
			}

			for(@raw_op_types){
				push @op_types, bits($_, 6,7), bits($_, 4,5), bits($_, 2,3), bits($_, 0,1);
			}

			$a_ops = $a + 1 + $op_bytes;
			$instr_type = "VAR";
		}
		when(2) { # short, or extended
			if($header{version} >= 5 && $b == 190) { # extended
				$opcode = $mem[$a+1];
				my $b_ops = $mem[$a+2]; # the third byte of the instruction, after the first and the extended opcode
				push @op_types, bits($b_ops, 6,7), bits($b_ops, 4,5), bits($b_ops, 2,3), bits($b_ops, 0,1);

				$a_ops = $a + 3;
				$instr_type = "EXT";
			} else { # short
				$opcode = bits($b, 0..3);

				my $op_type = bits($b, 4, 5);
				push @op_types, $op_type;

				$a_ops = $a + 1;
				$instr_type = $op_type == 3 ? "0OP" : "1OP";
			}
		}
		default { # long form
			$opcode = bits($b, 0..4);

			my @long_types = ( 1, 2 ); # 0 means small constant, 1 means variable
			push @op_types, $long_types[ bit($b, 6) ], $long_types[ bit($b, 5) ];

			$a_ops = $a + 1;
			$instr_type = "2OP";
		}
	}

	# remove "omitted" op types
	@op_types = grep { $_ != 3 } @op_types;

	# now retrieve the ops matching those sizes
	my @ops;
	for(@op_types){
		if($_ == 0){
			push @ops, [ $_, get_word($a_ops) ];
			$a_ops += 2;
		} else {
			push @ops, [ $_, $mem[$a_ops] ];
			$a_ops++;
		}
	}

	# build the interpreter-internal opcode, eg; 2OP:165
	my $interp_opcode = sprintf("%s:%x", $instr_type, $opcode);

	# "store" instructions require one byte given the variable number to store the result
	my $store = -1;
	if(exists($store_opcodes{$interp_opcode})){
		$store = $mem[$a_ops++]; # get that last byte, and move $a_ops to the instruction after
	}

	my $f_branch = 0;
	my $branch_on;
	my $branch_offset;
	if(exists($branch_opcodes{$interp_opcode})) {
		$f_branch = 1;

		my $b = $mem[$a_ops];
		$branch_on = bit($b, 7);

		if(bit($b, 6)) { # one byte
			$a_ops++;
			$branch_offset = bits($b, 0..5); # this is unsigned, 0-63
		} else {
			my $w = get_word($a_ops);
			$w &= 0b0011_1111_1111_1111; # mask off the two info bits
			# now perform sign extension to convert to 16 bits
			# NOTE: This assumes the value is 14-bit 2's-complement. if it's not, this will be wildly broken and make things crash almost at once
			if(bit($w, 13)) {
				$w |= 0b1100_0000_0000_0000;
			}
			$branch_offset = $w;
			$a_ops += 2;
		}
	}

	# update the pc
	$pc = $a_ops;

	# now, call the op in question with these arguments
	# the calling convention is (interp_opcode, store, branch_on, branch_offset, ops), omitting store and branch when not present

	my @args = ($interp_opcode);
	if($store >= 0) {
		push @args, $store;
	}
	if($f_branch){
		push @args, $branch_on, $branch_offset;
	}
	push @args, @ops;

	$opcodes{$interp_opcode}->(@args);
}



#################################################################
# utility functions
#################################################################

sub bit {
    my ($v, $n) = @_;

    return !!($v & (1 << $n)); # !!-normalized mask
}

sub set_bit {
	my ($v, $n) = @_;

	$_[0] = $v | (1 << $n);
}

sub clear_bit {
	my ($v, $n) = @_;

	$_[0] = $v & ~(1 << $n);
}



sub bits {
    my $v = shift;
    my @bits = @_;

    my $mask = 0;
    for(@bits){
        $mask |= 1 << $_;
    }

    $v &= $mask;

    while(!bit($v, 0)){
        $v >>= 1;
    }
}



# tables are always words -- this returns the unsigned word
sub tlookup {
	my ($addr, $index) = @_;

	my $len = get_word($addr);

	if($index >= $len) {
		print STDERR sprintf("Table lookup overflow: addr 0x%08u, length %d, index %d!\n", $addr, $len, $index);
		return 0;
	}

	return get_word( $addr + 2*($index+1) );
}


sub decode_str {
    my ($addr) = @_;

	# start by extracting all the chars into an array first
	my @c;

    while(1) {
		my $w = get_word($addr);
        push @c, bits($w, 10..14), bits($w, 5..9), bits($w, 0..4);

		break if bit($w, 15);
	}

	my $str = '';
	my $alpha = 0;

	for(my $i = 0; $i < @c; $i++) {
		given($c[$i]) {
			when([1,2,3]) { # abbreviations!
				break unless $i < @c-1; # end if there's an incomplete abbreviation. this is legal, but ignored.

				my $abbrev = 32*($_-1) + $c[$i+1];
				$str .= decode_str( get_word( tlookup($header{abbreviations}, $abbrev) ) );
				$i++; # skip the abbreviation number
			}
			when(4) {
				$alpha += 1;
			}
			when(5) {
				$alpha += 2;
			}
			default {
				if($_ == 6 && $alpha == 2) {
					break unless $i < @c-2; # end if there's an incomplete literal. this is legal, but ignored

					my $zscii_code = $c[$i+1] << 5 + $c[$i+2];
					$str .= zscii_chr($zscii_code);
					$i += 2; # skip over the ZSCII literal
				} elsif($_ == 0) {
					$str .= ' '; # space is special-cased
				} else {
					if($header{version} >= 5 && $header{alphabet_table}){ # use custom table
						my $index = 26*$alpha + $_ - 6;
						$str .= zscii_chr($mem[$header{alphabet_table} + $index]);
					} else { # use default table
						$str .= $alphabets[$alpha]->[$_-6];
					}
				}
			}
		}

		$alpha = 0; # reset alpha to A0
	}

	return $str;
}


# str is given as either an array reference of characters or a Perl string
# length is optional; if it is omitted then there will be no extra padding or truncation
sub encode_str {
	my ($str, $length) = @_;

	my @chars;
	if(ref($str)){
		@chars = @$str;
	} else {
		@chars = split //, $str;
	}

	# if length is set, we're encoding for the dictionary, so it needs to be lowercased
	if($length) {
		@chars = map { lc } @chars;
	}

	my @enc_chars;
	for(@chars){
		push @enc_chars, encode_char($_);
	}

	# pad with 5s to fill out the length
	if($length > 0) {
		while(@enc_chars < $length) {
			push @enc_chars, 5;
		}
	}

	# then pad with 5s to fill out a full word
	while(@enc_chars % 3){
		push @enc_chars, 5;
	}

	# now, finally, we can encode this shit
	my @encoded;
	while(@enc_chars){
		my $a = shift @enc_chars;
		my $b = shift @enc_chars;
		my $c = shift @enc_chars;

		my $w = $c | ($b << 5) | ($a << 10);
		if(!@enc_chars){
			$w |= 0x80_00; # set 'last' bit
		}
		push @encoded, $w;
	}

	return @encoded;
}


sub encode_char {
	my $c = shift;

	if(exists($alphabet_table{$c})){
		my $enc = $alphabet_table{$c};
		my $alpha = $enc / 32;
		$enc %= 32;
		my @ret;
		if($alpha == 1){
			push @ret, 4;
		} elsif($alpha = 2){
			push @ret, 5;
		}
		push @ret, $enc;

		return @ret;
	}

	my $enc = zscii_ord($c);
	my $up = $enc >> 5;        # upper 5 (really 3) bits
	my $down = $enc & 0b11111; # lower 5 bits
	return ( 5, 6, $up, $down );
}


# used for output only!
# TODO: Support for extra characters 155-251, using Unicode. VIOLATES 3.8.5.4.1. Currently prints ?s for these in all cases.
sub zscii_chr {
	my $c = shift;

	given($c){
		when(0)  { return "\0"; }
		when(13) { return "\n"; }
		when(155..251) { return "?"; }
		default {
			return chr($c);
		}
	}
}

# used for input only!
# TODO: Support for extra characters 155-251, using Unicode. VIOLATES 3.8.5.4.1.
sub zscii_ord {
	my $c = shift;

	given($c){
		when("\0") { return 0; }
		when("\177") { return 8; } # delete
		when("\n") { return 13; }
		default {
			return ord($c);
		}
	}
}


sub get_word { # unsigned!
    my ($addr) = @_;

    return $mem[$addr] << 8 + $mem[$addr+1];
}

sub get_word_s { # signed
    my ($addr) = @_;

	return from_signed(get_word($addr));
}

sub from_signed {
	my ($w) = @_;

    return bit($w, 15) ? - ($NEGATOR - $w) : $w;
}

sub from_signed_b {
	my ($b) = @_;

	return bit($b, 7) ? - (256 - $b) : $b;
}


sub set_word { # unsigned. truncates to 16-bit values
	my ($ba, $w) = @_;

	my $w2 = to_word($w); # convert to Z-machine word

	$mem[$ba]   = ($w2 >> 8) % 0x100;
	$mem[$ba+1] = $w2 % 0x100;
}

# note, to_word is idempotent. if a value is signed on the first pass, it becomes unsigned for the second, and is therefore unchanged.
sub to_word {
	my $sw = shift;

	$sw = ($sw < 0 ? $NEGATOR - $sw : $sw);

	return $sw % 0x1_0000;
}


######################################################
# opcode registry
######################################################

sub initialize_opcodes {
	%opcodes = (
		"2OP:1"   => \&op_je,
		"2OP:2"   => \&op_jl,
		"2OP:3"   => \&op_jg,
		"2OP:4"   => \&op_dec_check,
		"2OP:5"   => \&op_inc_check,
		"2OP:6"   => \&op_jin,
		"2OP:7"   => \&op_test,
		"2OP:8"   => \&op_or,
		"2OP:9"   => \&op_and,
		"2OP:a"   => \&op_test_attr,
		"2OP:b"   => \&op_set_attr,
		"2OP:c"   => \&op_clear_attr,
		"2OP:d"   => \&op_store,
		"2OP:e"   => \&op_insert_obj,
		"2OP:f"   => \&op_loadw,
		"2OP:10"  => \&op_loadb,
		"2OP:11"  => \&op_get_prop,
		"2OP:12"  => \&op_get_prop_addr,
		"2OP:13"  => \&op_get_next_prop,
		"2OP:14"  => \&op_add,
		"2OP:15"  => \&op_sub,
		"2OP:16"  => \&op_mul,
		"2OP:17"  => \&op_div,
		"2OP:18"  => \&op_mod,
		"2OP:19"  => \&op_call_2s,
		"2OP:1a"  => \&op_call_2n,
		"2OP:1b"  => \&op_set_colour,
		"2OP:1c"  => \&op_throw,

		"1OP:0"   => \&op_jz,
		"1OP:1"   => \&op_get_sibling,
		"1OP:2"   => \&op_get_child,
		"1OP:3"   => \&op_get_parent,
		"1OP:4"   => \&op_get_prop_len,
		"1OP:5"   => \&op_inc,
		"1OP:6"   => \&op_dec,
		"1OP:7"   => \&op_print_addr,
		"1OP:8"   => \&op_call_s,
		"1OP:9"   => \&op_remove_obj,
		"1OP:a"   => \&op_print_obj,
		"1OP:b"   => \&op_ret,
		"1OP:c"   => \&op_jump,
		"1OP:d"   => \&op_print_paddr,
		"1OP:e"   => \&op_load,
		"1OP:f"   => \&op_not__call_1n,

		"0OP:0"   => \&op_rtrue,
		"0OP:1"   => \&op_rfalse,
		"0OP:2"   => \&op_print,
		"0OP:3"   => \&op_print_ret,
		"0OP:4"   => \&op_nop,
		"0OP:5"   => \&op_save,
		"0OP:6"   => \&op_restore,
		"0OP:7"   => \&op_restart,
		"0OP:8"   => \&op_ret_popped,
		"0OP:9"   => \&op_pop__catch,
		"0OP:a"   => \&op_quit,
		"0OP:b"   => \&op_new_line,
		"0OP:c"   => \&op_show_status,
		"0OP:d"   => \&op_verify,
		"0OP:e"   => sub { unimp("extended", @_); },
		"0OP:f"   => sub { unimp("piracy", @_); },

		"VAR:0"   => \&op_call_s,
		"VAR:1"   => \&op_storew,
		"VAR:2"   => \&op_storeb,
		"VAR:3"   => \&op_put_prop,
		"VAR:4"   => \&op_read,
		"VAR:5"   => \&op_print_char,
		"VAR:6"   => \&op_print_num,
		"VAR:7"   => \&op_random,
		"VAR:8"   => \&op_push,
		"VAR:9"   => \&op_pull,
		"VAR:a"   => sub { unimp("split_window", @_); },
		"VAR:b"   => sub { unimp("set_window", @_); },
		"VAR:c"   => \&op_call_s,
		"VAR:d"   => sub { unimp("erase_window", @_); },
		"VAR:e"   => \&op_erase_line,
		"VAR:f"   => \&op_set_cursor,
		"VAR:10"  => sub { unimp("get_cursor_array", @_); },
		"VAR:11"  => \&op_set_text_style,
		"VAR:12"  => \&op_buffer_mode,
		"VAR:13"  => \&op_output_stream,
		"VAR:14"  => \&op_input_stream,
		"VAR:15"  => sub { unimp("sound_effect", @_); },
		"VAR:16"  => \&op_read_char,
		"VAR:17"  => \&op_scan_table,
		"VAR:18"  => \&op_not,
		"VAR:19"  => \&op_call_n,
		"VAR:1a"  => \&op_call_n,
		"VAR:1b"  => \&op_tokenize,
		"VAR:1c"  => \&op_encode_text,
		"VAR:1d"  => \&op_copy_table,
		"VAR:1e"  => \&op_print_table,
		"VAR:1f"  => \&op_check_arg_count,

		"EXT:0"   => \&op_save,
		"EXT:1"   => \&op_restore,
		"EXT:2"   => \&op_log_shift,
		"EXT:3"   => \&op_art_shift,
		"EXT:4"   => \&op_set_font,
		"EXT:9"   => \&op_save_undo,
		"EXT:a"   => \&op_restore_undo,
		"EXT:b"   => sub { unimp("print_unicode", @_); },
		"EXT:c"   => sub { unimp("check_unicode", @_); }
	);

}

