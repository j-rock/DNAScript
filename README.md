---------------------------------------------------
||DNAScript 
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

Currently DNARunner.scala is broken. The dependencies are somehow screwed up. Otherwise...

To run a DNAScript program, boot up your command line and enter:
> scala DNARunner.scala [string of code]

where [string of code] refers to a literal string of DNAScript