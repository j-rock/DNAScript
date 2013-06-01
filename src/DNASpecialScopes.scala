/**
 * DNASpecialScopes is contains definitions for If, While, and For control structures.
 * For information on DNAScoping, look at the DNAScope class.
 * 
 * Joseph Rock
 * Jan. 21, 2013
 * Version 2.0
 */


import scala.collection.mutable.HashMap


/**
 * If scope will dynamically evaluate a DNADatum as a boolean.
 * If the boolean is true, it will parse the passed codon instructions
 */
class IfScope(codons: String, var varias: List[HashMap[String, DNADatum]] , booleanDNA: DNADatum)
	extends DNAScope(codons, varias){
  
  override def start() {
    varias = (new HashMap[String, DNADatum]) :: varias
    
    val (name1, name2) = booleanDNA.getInvolvedData
    val (datum1, datum2) = (find(name1), find(name2))
    val evaluableBoolean = booleanDNA.asBoolean(datum1, datum2)
    
    if(evaluableBoolean) parse(codons) else ()
  }
}

/**
 * A While scope will dynamically evaluate a DNADatum as a boolean.
 * Until the boolean is false, it will parse the passed codon instructions.
 */
class WhileScope(codons: String, var varias: List[HashMap[String, DNADatum]], booleanDNA: DNADatum)
	extends DNAScope(codons, varias){
  
  override def start() {
    varias = (new HashMap[String, DNADatum]) :: varias
    
	var (name1, name2) = booleanDNA.getInvolvedData
	var (datum1, datum2) = (find(name1), find(name2))
	var evaluableBoolean: Boolean = booleanDNA.asBoolean(datum1, datum2)
	
    while(evaluableBoolean){
		parse(codons)
        var (nam1, nam2) = booleanDNA.getInvolvedData
	    var (datm1, datm2) = (find(nam1), find(nam2))
		evaluableBoolean = booleanDNA.asBoolean(datm1, datm2)
    }
  }
}

/**
 * For scope will repeat the passed codon instructions until the
 * starting index has been incremented to beyond the ending index
 * That is an inclusive range on the starting and ending indices.
 */
class ForScope(codons: String, var varias: List[HashMap[String, DNADatum]], start: Int, end: Int)
	extends DNAScope(codons, varias){
  
  override def start() {
    varias = (new HashMap[String, DNADatum]) :: varias
    for(i <- start to end) parse(codons)
  }
}

