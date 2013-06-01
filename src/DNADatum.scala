/**
 * DNADatum is the fundamental data type for DNAScript.
 * It stores a value that could be treated as an integer (Int), String, or boolean (Boolean)
 * The singleton object contains integer-codon conversions and String-codon methods
 * 
 * Joseph Rock
 * Jan. 21, 2013
 * Version 2.0
 */
  
class DNADatum(var value: String) {
  
  def getValue() = value
  
  def set(nVal:String) { value = nVal }
  
  /**
   * Returns integral value of six-block contained in the DNADatum
   */
  def asInt() = DNADatum.block2Int(value)
  
  /**
   * Traverses value of this DNADatum and converts each six block to a String character.
   * These characters are concatenated in order into the final String returned.
   */
  def asString(): String = {
    var asString = ""
    for(i <- 0 to value.length()/18) {
      val intValue = DNADatum.block2Int(value.substring(18*i, 18*(i+1)))
      asString += DNADatum.getAlphabetCharacter(intValue)
    }
    asString
  }
  
  /**
   * For dynamic invocation of the boolean expression,
   * the DNADatum should have its caller pass the DNADatums (forgive the pluralization)
   * involved inside this DNADAtum via method getInvolvedData.
   * If this DNADatum is not a proper boolean, val1 should be null.
   * In which case, the boolean evaluation will test if the first valid
   * six block is zero. 0 returns false, Non-zeros return true.
   */
  def asBoolean(val1: DNADatum, val2: DNADatum): Boolean = {
    if(val1 == null) this.asInt() != 0
    else value match {
      		case s if s startsWith "GCT" => val1.asInt() < val2.asInt() //GCT is the < particle
      		case t if t startsWith "GCC" => val1.asInt() <= val2.asInt() //GCC is the <= particle
      		case r if r startsWith "GCG" => val1.asInt() != val2.asInt() //etc.
      		case q if q startsWith "CCA" => val1.asInt() >= val2.asInt()
      		case p if p startsWith "CCC" => val1.asInt() == val2.asInt()
      		case m if m startsWith "CCT" => val1.asInt() > val2.asInt()
      		case _ => this.asInt() != 0
    	}
  }
  
  /**
   * Retrieves the names or integer literals buried within the value of this DNADatum.
   * This is specifically for dynamic invocation of the boolean expression herein.
   * If this DNADatum is not built for boolean expression, it will return empty Strings
   */
  def getInvolvedData(): (String, String) = {
    if(value.length() != 36) ("", "") else value.splitAt(18)
  }
  
  override def toString() = asInt() + "/" + asString()
}




object DNADatum {
  
  def main(args: Array[String]) {
  }
  
  private val alphabet = Array("", " ", "A","B","C","D","E","F",
      "G","H","I","J","K","L","M",
      "N","O","P","Q","R","S","T",
      "U","V","W","X","Y","Z",
      "a","b","c","d","e","f",
      "g","h","i","j","k","l","m",
      "n","o","p","q","r","s","t",
      "u","v","w","x","y","z",
      "!","\"","#","$","%","&","(",
      ")","*","+","-",":",";","<",
      ">","=","?","@","[","]","{",
      "}","~","^","_"
  )
        
  def getAlphabetCharacter(index: Int) = alphabet(index % alphabet.length)
  
  
  /**
   * Takes a six-block of triplets (String) and converts its integral value (Int)
   * Max integer value in DNALanguage -> ***TTTTTTTTTTTTTTT (which would be 1073741823 == 2^30-1)
   *  where *** refers to any proper triplet
   * Precondition: The passed String is composed of six valid triplets (no more, no less)
   */
  def block2Int(sixblock: String): Int = {
    val importantParts = sixblock.substring(3)
	var amount = 0
	for(i <- 0 to 4) {
	   amount += triplet2Int(importantParts.substring(i*3, i*3+3)) * (1 << (24 - 6*i))
	}
    amount
  }
  
  /**
   * Takes a String of 3 codons (A, G, T, or C) and returns integral value (Int)
   * AAA is 0, AAC is 1, ... , TTG is 62, TTT is 63.
   * Precondition: passed String is a valid triplet.
   */
  def triplet2Int(triplet: String): Int = {
    var amount = 0
    for(i <- 0 to 2) {
      amount += codon2Int(triplet.substring(i, i+1)) * (1 << (4 - 2*i))
    }
    amount
  }
  
  /**
   * Takes a valid codon (String of "A", "G", "T", or "C") and returns integral value (Int)
   * A is 0, C is 1, G is 2, and T is 3. This can be remembered by lexicographic order.
   * Precondition: passed codon is a valid codon
   */
  def codon2Int(codon: String): Int = {
    codon match {
      case "A" => 0
      case "C" => 1
      case "G" => 2
      case "T" => 3
    }
  }
  
  /**
   * Takes an Int mod 4 and returns a valid codon (String)
   * If 0 returns "A", 1 returns "C", 2 returns "G", and 3 returns "T"
   */
  def int2Codon(num: Int): String = {
    (num % 4) match {
      case 0 => "A"
      case 1 => "C"
      case 2 => "G"
      case 3 => "T"
    }
  }
  
  /**
   * Takes an Int mod 64 and returns a valid triplet (String)
   * 0 will return "AAA" and 63 will return "TTT"
   */
  def int2Triplet(num: Int): String = {
    val number = num % 64
    int2Codon(number >> 4) + int2Codon(number >> 2) + int2Codon(number % 4)
  }
  
  /**
   * Takes an Int and returns a valid six block of triplets (String)
   * 0 will return "AAAAAAAAAAAAAAAAAA"
   * (1 << 30) - 1 will return "AAATTTTTTTTTTTTTTT" etc.
   */
  def int2Block(num: Int): String = {
    val number = num % ((1 << 30));
    "AAA" + int2Triplet(number >> 24) + int2Triplet(number >> 18) +
    	int2Triplet(number >> 12) + int2Triplet(number >> 6) + int2Triplet(number % 64);
  }
  
  
}