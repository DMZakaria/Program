# Convert csv files

# Help :
# perl script.pl -exp "output name" "hour" "sensor code" 
# Don't forget to create Output directory


use strict;
use warnings;


my $sep = ","; # separateur des valeurs
my $rep = "Output"; # repertoire des fichiers en sortie
my $file = shift @ARGV; # nom du fichier en argument
my $modeexp = 0; 
my $nomSortie;
my $heure;
my $codeCapteur;

if(!$file)
{
print "Precisez le nom du fichier\n";
exit;
}

if($file eq "-exp")
{
	$nomSortie = shift @ARGV;
	$heure = shift @ARGV;
    $codeCapteur = shift @ARGV;
	$modeexp = 1;
}

# test si le repertoire de sortie existe

chdir $rep or die "Le repertoire de sortie $rep n'existe pas";
chdir "..";


sub convertir1fichierData
{
# ouverture fichiers entree et sortie
	my $file = $heure."_data_".$codeCapteur.".csv";
	 $file =~ /(.+\/)(.+\.csv)/;
	my $file_simple = $2;
	my $skip_lines = 3001;
	open my $in, '<', $file or die "Impossible de lire le fichier: $file $!";
	open my $out, '>', $nomSortie."_data.csv" or die "Impossible d'ouvrir le fichier de sortie:  $!";

	$_ = <$in>;

	# premiere ligne
	print $out "X,Y,Z,Temp\n";
	while( <$in> )
	{
		if(/(-?\d+),(-?\d+),(-?\d+),(-?\d+),(-?\d+)/)
		{   
			next if $. < $skip_lines;
			my $tmp = ($4.$5);
			print $out $2.$sep.$3.$sep.$4.$sep.($5/10)."\n";
			
		}
		else
		{print $out "Erreur: ".$_;}
	}
	close $out;
}

sub convertir1fichierAut
{
	
	# ouverture fichiers entree et sortie
	my $file = $heure."_aut_".$codeCapteur.".csv";
	 $file =~ /(.+\/)(.+\.csv)/;
	my $file_simple = $2;
	open my $in, '<', $file or die "Impossible de lire le fichier: $file $!";
	open my $out, '>', $nomSortie."_aut.csv" or die "Impossible d'ouvrir le fichier de sortie:  $!";

	$_ = <$in>;

	# premiere ligne
	print $out "Time,ParaSymp,Symp\n";
	while( <$in> )
	{
		if(/(-?\d+),(-?\d+),(-?\d+),(-?\d+),(-?\d+),(-?\d+)/)
		{
			
			print $out $2.$sep.$3.".".$4.$sep.$5.".".$6."\n";
		}
		else
		{print $out "Erreur: ".$_;}
	}
	close $out;
}

sub convertir1fichierRR
{
	
	# ouverture fichiers entree et sortie
	my $file = $heure."_rr_".$codeCapteur.".csv";
	 $file =~ /(.+\/)(.+\.csv)/;
	my $file_simple = $2;
	open my $in, '<', $file or die "Impossible de lire le fichier: $file $!";
	open my $out, '>', $nomSortie."_rr.csv" or die "Impossible d'ouvrir le fichier de sortie:  $!";

	$_ = <$in>;

	# premiere ligne
	print $out "RR,HR\n";
	while( <$in> )
	{
		if(/(-?\d+),(-?\d+),(-?\d+),(-?\d+),(-?\d+)/)
		{
			
			print $out $3.".".$4.$sep.$5."\n";
		}
		else
		{print $out "Erreur: ".$_;}
	}
	close $out;
}


if($modeexp)
{
	convertir1fichierAut();
	convertir1fichierRR();
	convertir1fichierData();
}
