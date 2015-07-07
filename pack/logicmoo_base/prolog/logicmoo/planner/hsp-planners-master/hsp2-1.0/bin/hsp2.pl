#!/usr/bin/env perl -w


# read arguments and HSPHOME
$problem = $ARGV[0] or die "usage: HSP <problem.pddl> <domain.pddl>\n";
$domain = $ARGV[1] or die "usage: HSP <problem.pddl> <domain.pddl>\n";
$hsphome = $ENV{HSPHOME} or die "set \$HSPHOME shell variable before ruuning this script\n";
# $home = $ENV{HOME};


# prepare schedules
$number_schedules = 3;
$schedule[0] = '-w 5 -S [backward,h1plus,180000]';
$stdout[0] = "/tmp/hsp-s0-stdout";
$stderr[0] = "/tmp/hsp-s0-stderr";
$schedule[1] = '-w 5 -S [backward,h2max,180000]';
$stdout[1] = "/tmp/hsp-s1-stdout";
$stderr[1] = "/tmp/hsp-s1-stderr";
$schedule[2] = '-w 5 -S [forward,h1plus,1800000]';
$stdout[2] = "/tmp/hsp-s2-stdout";
$stderr[2] = "/tmp/hsp-s2-stderr";

# fork processes
for( $i = 0; $i < $number_schedules; ++$i )
{
    if( $pid = fork )
    {
	#parent
	$alive{$pid} = 1;
	$index{$pid} = $i;
        #print "child $pid spawned\n";
    }
    else
    {
	# child
	exec( "exec $hsphome/bin/hsp2 $schedule[$i] $problem $domain > $stdout[$i] 2> $stderr[$i]" )
	    or die "exec: $!\n";
    }
}


# wait for them
$solved = 0;
$p = $number_schedules;
while( $p > 0 )
{
    $child = wait;
    $status = $?;
    #print "child $child ended with status $status\n";
    $alive{$child} = 0;

    # if successful, kill other childs
    if( $status == 0 )
    {
        $i = 0;
	foreach $pid (%alive)
	{
	    if( $i % 2 == 0 )
	    { 
	    	if( ($pid > 1) && ($alive{$pid} == 1) )
		{
		    #print "killing pid $pid. Index is $index{$pid}\n";
		    $alive{$pid} = 0;
		    kill 1, $pid;
		    --$p;
	    	}
	    }
            ++$i;
	}

	--$p;
	$solved = 1;
	#print "$child finished. Index is $index{$child}\n";
	$output = `cat $stdout[$index{$child}]`;
	system( "cat $stderr[$index{$child}] >&2" );
    }
    else
    {
	--$p;
    }
}


# print solution
if( $solved == 1 )
{
    $childtime{0} = `grep "REGISTER: main" $stderr[0]`;
    $childtime{1} = `grep "REGISTER: main" $stderr[1]`;
    $childtime{2} = `grep "REGISTER: main" $stderr[2]`;
    ($name,$time,$length,$plan) = split(/,/,$output,4);
    #($a,$b,$c,$t0,$d) = split(/ /,$childtime{0},5);
    #($a,$b,$c,$t1,$d) = split(/ /,$childtime{1},5);
    #($a,$b,$c,$t2,$d) = split(/ /,$childtime{2},5);
    #$time = 0;
    #$newtime = $t0 + $t1 + $t2;
    #print $name.",".$newtime.",".$length.",".$plan;
    print $name.",".$time.",".$length.",".$plan;
}
unlink( $stderr[0] );
unlink( $stdout[0] );
unlink( $stderr[1] );
unlink( $stdout[1] );
unlink( $stderr[2] );
unlink( $stdout[2] );

