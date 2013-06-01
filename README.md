---------------------------------------------------
||	Readme file for DNAScript 2.0
|| 	Joseph Rock
---------------------------------------------------

First and foremost, I apologize for the excessive commenting in the source.


Source code contains four .scala files:

DNADatum: A class definition for the only data type found in DNAScript

DNARunner: The class used to evaluate DNAScript code

DNAScope: The class that does all of the magic lexing/parsing for the language. Oh boy!

DNASpecialScopes: A .SCALA container file for definitions of For and While loops, as well as If statements
___________________________________________________
___________________________________________________
___________________________________________________


To run a DNAScript program, boot up your command line and enter:
> scala DNARunner.scala [string of code]

where [string of code] refers to a literal string of DNAScript

___________________________________________________
___________________________________________________
___________________________________________________

Here's an illustration of DNAScript: 

Iterated factorial program (hooray for clichés!)

:pseudocode
int n = 0
for 1:10
  n <- add n 1
  int j = n
  int product = 1
  boolean condition = j > 1
  while(condition)
    product <- multiply product j
    j <- sub j 1
  intprint product

:structured code with pseudocode
TAAAAAAAAAAAAAAATA AAAAAAAAAAAAAAAAAA   int n = 0
ACCAAAAAAAAAAAAAAC AAAAAAAAAAAAAAAAGG TACAAAAAAAAAAAAAAA for 1:10
 CAAAAAAAAAAAAAAATA GAAAAAAAAAAAAAAATA AAAAAAAAAAAAAAAAAC n <- add n 1
 TAAAAAAAAAAAAAAAGA GAAAAAAAAAAAAAAATA int j = n
 TAAAAAAAAAAAAAAACA AAAAAAAAAAAAAAAAAC int product = 1
 TCAAAAAAAAAAAAACCA CCTAAAAAAAAAAAAAGA AAAAAAAAAAAAAAAAAC boolean condition = j > 1
 AACAAAAAAAAAAAACCA TACAAAAAAAAAAAAAAA while(condition)
  CATAAAAAAAAAAAAACA GAAAAAAAAAAAAAAACA GAAAAAAAAAAAAAAAGA  product <- multiply product j
  CACAAAAAAAAAAAAAGA GAAAAAAAAAAAAAAAGA AAAAAAAAAAAAAAAAAC j <- sub j 1
 ATTAAAAAAAAAAAAAAA //end While
 TTAAAAAAAAAAAAAACA intprint product
ATTAAAAAAAAAAAAAAA //end For

:structured code
TAAAAAAAAAAAAAAATA AAAAAAAAAAAAAAAAAA
ACCAAAAAAAAAAAAAAC AAAAAAAAAAAAAAAAGG TACAAAAAAAAAAAAAAA
 CAAAAAAAAAAAAAAATA GAAAAAAAAAAAAAAATA AAAAAAAAAAAAAAAAAC
 TAAAAAAAAAAAAAAAGA GAAAAAAAAAAAAAAATA
 TAAAAAAAAAAAAAAACA AAAAAAAAAAAAAAAAAC
 TCAAAAAAAAAAAAACCA CCTAAAAAAAAAAAAAGA AAAAAAAAAAAAAAAAAC
 AACAAAAAAAAAAAACCA TACAAAAAAAAAAAAAAA
  CATAAAAAAAAAAAAACA GAAAAAAAAAAAAAAACA GAAAAAAAAAAAAAAAGA 
  CACAAAAAAAAAAAAAGA GAAAAAAAAAAAAAAAGA AAAAAAAAAAAAAAAAAC
 ATTAAAAAAAAAAAAAAA
 TTAAAAAAAAAAAAAACA
ATTAAAAAAAAAAAAAAA

:destructured code
TAAAAAAAAAAAAAAATAAAAAAAAAAAAAAAAAAAACCAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAGGTACAAAAAAAAAAAAAAACAAAAAAAAAAAAAAATAGAAAAAAAAAAAAAAATAAAAAAAAAAAAAAAAAACTAAAAAAAAAAAAAAAGAGAAAAAAAAAAAAAAATATAAAAAAAAAAAAAAACAAAAAAAAAAAAAAAAAACTCAAAAAAAAAAAAACCACCTAAAAAAAAAAAAAGAAAAAAAAAAAAAAAAAACAACAAAAAAAAAAAACCATACAAAAAAAAAAAAAAACATAAAAAAAAAAAAACAGAAAAAAAAAAAAAAACAGAAAAAAAAAAAAAAAGACACAAAAAAAAAAAAAGAGAAAAAAAAAAAAAAAGAAAAAAAAAAAAAAAAAACATTAAAAAAAAAAAAAAATTAAAAAAAAAAAAAACAATTAAAAAAAAAAAAAAA
