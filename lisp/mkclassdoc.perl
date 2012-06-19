######################################################################
# mkclassdoc.perl -- Extract information from class documentation
# 
# Copyright 2012 Dewey M. Sasser  All Rights Reserved
#         Unpublished, Confidential and Proprietary
# 
# Author          : Dewey M. Sasser
# Created On      : Thu Mar 07 15:58:48 1996
# Status          : $State: Exp $
# Keywords        : 
# PURPOSE
# 	|>Description of modules purpose<|
# 
# TABLE OF CONTENTS
#   PrintSupers() -- print superclasses
#   PrintFunctions() -- Print all the functions
#   PrintMembers() -- 
#   PrintFunctionType() -- print all functions of a particular type
#   PrintMemberType() -- 
# 
#  OPTIONS
#    Update Tests:  no
# 
# $RCSfile: mkclassdoc.perl,v $
# $Revision: 1.2 $
######################################################################


$protection=2;

$ProtectionNames[0]="public";
$ProtectionNames[1]="protected";
$ProtectionNames[2]="private";

while (<>)
{
    s#/\*.*\*/##g;
    s#//.*$##g;
    s/\{//g;
    s/\};//g;
    next if /^[ \t]*$/;
    chop;

    if(/^class[ \t]*([^:]*)(:[ \t*]public[ \t]*(.*))?/)
    {
	$name=$1;
	$classname=(reverse split(/[ \t]+/,$name))[0];
	/:[ \t]*public[ \t]*(.*)/;
	$sups=$1;
	@superclasses=split(/[ \t,]+/,$sups) if $sups;		     
	next;
    }

    if(/^[ \t]*protected:/)
    {
	$protection=1;
	next;

    }

    if(/^[ \t]*private:/)
    {
	$protection=2;
	next;

    }
    if(/^[ \t]*public:/)
    {
	$protection=0;
	next;

    }

    if(/\(/)
    {
	push(@{$Functions[$protection]},$_);
	next;
    }

    push(@{$Members[$protection]},$_);
}

print "( $classname ";

&PrintSupers($classname);
&PrintFunctions($classname);
&PrintMembers($classname);

print ")\n";


######################################################################
# PrintSupers() -- print superclasses
# AUTHOR:    Dewey M. Sasser
# VISIBILITY:    PUBLIC | PRIVATE
# SYNOPSIS
# DESCRIPTION
#   |>Description of function<|
# ARGUMENTS:    NONE
# RETURNS
#   (something perlish) -- 
# NOTES
#   |><|
# CAVEATS AND BUGS
#   |><|
 
sub PrintSupers {
    local($name)=$_[0];

    print "(";
    print join(" ", @superclasses) if @superclasses;
    print ")\n";
}

######################################################################
# PrintFunctions() -- Print all the functions
# AUTHOR:    Dewey M. Sasser
# VISIBILITY:    PUBLIC | PRIVATE
# SYNOPSIS
# DESCRIPTION
#   |>Description of function<|
# ARGUMENTS:    NONE
# RETURNS
#   (something perlish) -- 
# NOTES
#   |><|
# CAVEATS AND BUGS
#   |><|
 
sub PrintFunctions {
    local($name)=$_[0];

    print " (\n";

    for($i=0;$i<3;$i++)
    {
	print "  (\n";
	print "   ", $ProtectionNames[$i];
	print "\n   ";
	&PrintFunctionType($name,$i);
	print "  )\n"
    }

    print " )\n";
}

######################################################################
# PrintMembers() -- 
# AUTHOR:    Dewey M. Sasser
# VISIBILITY:    PUBLIC | PRIVATE
# SYNOPSIS
# DESCRIPTION
#   |>Description of function<|
# ARGUMENTS:    NONE
# RETURNS
#   (something perlish) -- 
# NOTES
#   |><|
# CAVEATS AND BUGS
#   |><|
 

sub PrintMembers {
    local($name)=$_[0];

    print "( ";

    for($i=0;$i<3;$i++)
    {
	print "(";
	print $ProtectionNames[$i];
	print " ";
	&PrintMemberType($name,$i);
	print ")"
    }

    print ") ";
}


######################################################################
# PrintFunctionType() -- print all functions of a particular type
# AUTHOR:    Dewey M. Sasser
# VISIBILITY:    PUBLIC | PRIVATE
# SYNOPSIS
# DESCRIPTION
#   |>Description of function<|
# ARGUMENTS:    NONE
# RETURNS
#   (something perlish) -- 
# NOTES
#   |><|
# CAVEATS AND BUGS
#   |><|
 
sub PrintFunctionType {
    local($name,$type)=@_;
    print "    (";
    foreach $name (@{$Functions[$type]})
    {
	print "   \"$name\"\n";
    }
    print "    )\n";
}

######################################################################
# PrintMemberType() -- 
# AUTHOR:    Dewey M. Sasser
# VISIBILITY:    PUBLIC | PRIVATE
# SYNOPSIS
# DESCRIPTION
#   |>Description of function<|
# ARGUMENTS:    NONE
# RETURNS
#   (something perlish) -- 
# NOTES
#   |><|
# CAVEATS AND BUGS
#   |><|
 
sub PrintMemberType {
    local($name,$type)=@_;
    print "    (";
    foreach $name (@{$Members[$type]})
    {
	print "   \"$name\"\n";
    }
    print "    )\n";
}
    
