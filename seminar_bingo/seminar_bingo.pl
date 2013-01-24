#!/usr/bin/perl

#generate geeky bingo cards for annoying seminar speakers with. 

use strict;
use warnings;
use Getopt::Long;

my ($help);
my $numberCards=5;
&GetOptions( 
    "h|help"              => \$help,
    "nc|numcards"         => \$numberCards
    );

if( $help ) {
    &help();
    exit(1);
}

open(IN, "< words.txt") or die ("FATAL: Couldn't open words.txt\n[$!]");
my @words = <IN>; 
close(IN); 
my $tables=""; 
for(my $i=0; $i<$numberCards; $i++){
    my @sample=@words;
    my @sampled=(); 
    
    for(my $j=0; $j<25; $j++){
	my $numLeft = scalar(@sample);
	my $word=splice( @sample, int(rand($numLeft)), 1);
	chomp($word);
	push(@sampled, $word);
    }
    
    $tables .= printCard(\@sampled, $i);
}

printLatex($tables); 

exit(0);

######################################################################

sub printCard {
    
    my $sample = shift;
    my $cardNum=shift;
    my $table="
\\begin{tabular}{ |c|c|c|c|c| }
  \\hline
  \\multicolumn{5}{|c|}{Seminar Bingo $cardNum} \\\\
  \\hline
";
    my $column=0; 
    while(my $word=pop(@{$sample})){
	my $sep="&";
	$sep=" " if ($column==4);
	$table.="$word $sep "; 
	
	$column++; 
	if($column==5){
	    $table.="\\\\\n\\hline\n";
	    $column=0;
	}
	
    }
    $table.="\\end{tabular}\n\n\n\\clearpage\n\\newpage\n\n\n";
    
    print "$table";
    return $table; 
    
}

######################################################################
sub printLatex {
    my $tables=shift; 
    
    my $latex="\\documentclass[a4paper,12pt]{article}
\\pagestyle{empty}
\\usepackage{rotating}
\\linespread{1.2}

\\begin{document}

";
    
    $latex.=$tables;
    
    $latex.="\n\n\\end{document}\n";
    
    open(OUT, "> seminar_bingo.tex") or die ("FATAL: Couldn't open seminar_bingo.tex\n[$!]");
    print OUT $latex; 
    close(OUT);
    
    system("pdflatex seminar_bingo.tex") and die "FATAL: failed to execute [pdflatex seminar_bingo.tex]\n[$!]";
}

######################################################################
sub help {
    print STDERR <<EOF;

seminar_bingo.pl: generate 

Usage:   seminar_bingo.pl 
Options:       -h|--help                     Show this help.
               -nc|--numcards <num>          Gnerate <num> bingo cards [Default:$numberCards] 
EOF
}













