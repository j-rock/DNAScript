/**
 * DNAScope is a lexical scope class. It contains a String of codon instructions,
 * and it has a List of HashMaps that use variable names (Strings) to retrieve DNADatum objects.
 * 
 * Each DNAScope is allotted a unique, specialized scope, as well as access to the environmental scope.
 * Names in a specialized scope override names in environmental scopes.
 * 
 * Joseph Rock
 * Jan. 21, 2013
 * Version 2.0
 */

import scala.collection.mutable.HashMap

class DNAScope(codons: String, var variables: List[HashMap[String, DNADatum]]) {
  
  def this(codons: String) = this(codons, List())
  
  /**
   * Retrieves a DNADatum object from available scope or returns null.
   * It uses the name of the variable defined in DNAScript code.
   * If the name of the variable uses the "AAA" particle,
   * it will be treated as a DNADatum literal.
   */
  def find(key: String): DNADatum = {
    def findHelper(name: String, vars: List[HashMap[String, DNADatum]]): DNADatum = {
      if(vars isEmpty) new DNADatum(name)
      else vars.head get name.substring(3) match {
        case Some(datum) => datum
        case None => findHelper(name, vars.tail)
      }
    }
    
    key match {
      case t if t equals "" => null
      case s if s startsWith "AAA" => new DNADatum(key)
      case _ => findHelper(key, variables)
    }
  }
  
  /**
   * Adds a key-value pair of String -> DNADatum to the current lexical scope 
   */
  def add(key:String, datum: DNADatum) {
    variables.head += (key.substring(3)  -> datum)
  }
  
  /**
   * Cleans the passed instructions and parses them.
   */
  def start() {
    variables = (new HashMap[String, DNADatum]) :: variables
    val cleanCodons = DNAScope.removeNonCodons(codons)
    parse(cleanCodons)
    val m = ""
  }
  
  
  /**
   * Takes valid codon instructions and evaluates them dynamically.
   * To understand the operations here, one must consult the
   * DNAScript language specification
   */
  def parse(instructions: String) {
    if(instructions.length < 18) () //if no valid instructions remain, return Unit
    else
    instructions.substring(0,3) match {
      //Integer instantiation
      case "TAA" => val (grabbed, remaining) = DNAScope.nextTokens(instructions, 2)
    		  		if(grabbed == "") ()
    		  	    assignDatum(grabbed.substring(0,18), grabbed.substring(18))
    		  		parse(remaining)
    		  		
      //String instantiation
      case "TGA" => val (grabbed, remaining) = DNAScope.nextTokensUntil(instructions, "GGA")
      				if(grabbed == "") ()
      			    assignDatum(grabbed.substring(0,18), grabbed.substring(18))
      				parse(remaining)
      				
      //Boolean instantiation
      case "TCA" => val (grabbed, remaining) = DNAScope.nextTokens(instructions, 3)
      				if(grabbed == "") ()
      			    assignDatum(grabbed.substring(0, 18), grabbed.substring(18))
      				parse(remaining)
      				
      //Addition operation
      case "CAA" => val (grabbed, remaining) = DNAScope.nextTokens(instructions, 3)
      				if(grabbed == "") ()
      			    add(grabbed.substring(0, 18),
      				    grabbed.substring(18, 36),
      				    grabbed.substring(36))
      				parse(remaining)
      				
      //Subtraction operation
      case "CAC" => val (grabbed, remaining) = DNAScope.nextTokens(instructions, 3)
      				if(grabbed == "") ()
      				sub(grabbed.substring(0, 18),
      				    grabbed.substring(18, 36),
      				    grabbed.substring(36))
      				parse(remaining)
      				
      //Multiplication operation
      case "CAT" => val (grabbed, remaining) = DNAScope.nextTokens(instructions, 3)
      				if(grabbed == "") ()
      				mult(grabbed.substring(0, 18),
      					 grabbed.substring(18, 36),
      					 grabbed.substring(36))
      				parse(remaining)
      				
      //Integer division operation
      case "CAG" => val (grabbed, remaining) = DNAScope.nextTokens(instructions, 3)
      				if(grabbed == "") ()
      				div(grabbed.substring(0, 18),
      					grabbed.substring(18, 36),
      					grabbed.substring(36))
      				parse(remaining)
      				
      //For loop mechanism
      case "ACC" => var (loopVals, remaining) = DNAScope.nextTokens(instructions, 2)
      			    val (garbage, restOfCode) = DNAScope.nextTokensUntil(remaining, "TAC")
      				val (forLoopCodons, rest) = DNAScope.nextTokensInScope(restOfCode)
      				if(loopVals == "" || forLoopCodons == "") ()
      				//ATT prefixed codon is garbage, so we drop it before passing to for loop
      				instantiateForLoop(forLoopCodons.dropRight(18), loopVals)
      				parse(rest)
      				
      //If branch mechanism
      case "AGG" => val (boolean, remaining) = DNAScope.nextToken(instructions)
      				val (garbage, restOfCode) = DNAScope.nextTokensUntil(remaining, "TAC")
      				val (ifLoopCodons, rest) = DNAScope.nextTokensInScope(restOfCode)
      				if(boolean == "" || ifLoopCodons == "") ()
      				instantiateIfBranch(ifLoopCodons.dropRight(18), boolean)
      				parse(rest)
      				
      //While loop mechanism
      case "AAC" => val (boolean, remaining) = DNAScope.nextToken(instructions)
      				val (garbage, restOfCode) = DNAScope.nextTokensUntil(remaining, "TAC")
      				val (whileLoopCodons, rest) = DNAScope.nextTokensInScope(restOfCode)
      				if(boolean == "" || whileLoopCodons == "") ()
      				instantiateWhileLoop(whileLoopCodons.dropRight(18), boolean)
      				parse(rest)
      				
      //Print variable as integer
      case "TTA" => val (grabbed, remaining) = DNAScope.nextToken(instructions)
      				if(grabbed == "") ()
      				tryIntPrint(grabbed)
      				parse(remaining)
      
	  //Print variable as String
      case "TTC" => val (grabbed, remaining) = DNAScope.nextToken(instructions)
      				if(grabbed == "") ()
      				tryStringPrint(grabbed)
      				parse(remaining)
      
      //Print variable as Boolean
      case "TTG" => val (grabbed, remaining) = DNAScope.nextToken(instructions)
      				if(grabbed == "") ()
      				tryBooleanPrint(grabbed)
      				parse(remaining)
      
      //Print variable as Int/String
      case "TTT" => val (grabbed, remaining) = DNAScope.nextToken(instructions)
      				if(grabbed == "") ()
      				tryIntStringPrint(grabbed)
      				parse(remaining)		
      				
      case _ => ()
    }
  }
  
  //These are the methods the parser uses
  
  /** 
   * AssignDatum will take a name and value for a new DNADatum
   * It will then add it to the current scope
   */
  def assignDatum(name: String, value: String){
    add(name, new DNADatum(find(value).getValue))
  }
  
  /**
   * Add will add two DNADatums and store the result in a third
   * It takes the names of the three DNADatums as Strings
   * sumName = adder1 + adder2
   */
  def add(sumName: String, adder1: String, adder2: String){
    val sum = find(sumName)
    val add1 = find(adder1)
    val add2 = find(adder2)
    if(sum != null && add1 != null && add2 != null)
    	sum set (DNADatum.int2Block(DNADatum.block2Int(add1.value)
    								 + DNADatum.block2Int(add2.value)))
    val t = ()
  }
  
  /**
   * Sub will subtract two DNADatums and store the result in a third
   * It takes the names of the three DNADatums as Strings
   * difName = subter1 - subter2
   */
  def sub(difName: String, subter1: String, subter2: String){
    val difference = find(difName)
    val dif1 = find(subter1)
    val dif2 = find(subter2)
    if(difference != null && dif1 != null && dif2 != null)
    	difference set (DNADatum.int2Block(DNADatum.block2Int(dif1.value)
    							 			- DNADatum.block2Int(dif2.value)))
  }
  
  /**
   * Mult works like add and sub except it multiplies the numbers
   * prodName = factor1*factor2
   */
  def mult(prodName: String, factor1: String, factor2: String){
    val product = find(prodName)
    val fac1 = find(factor1)
    val fac2 = find(factor2)
    if(product != null && fac1 != null && fac2 != null)
    	product set (DNADatum.int2Block(DNADatum.block2Int(fac1.value)
    							 		 * DNADatum.block2Int(fac2.value)))
  }
  
  /**
   * Div works like mult, except it performs integer division
   * quotName = dividend/divisor
   */
  def div(quotName: String, dividend: String, divisor: String){
	val quotient = find(quotName)
	val dend = find(dividend)
	val visor = find(divisor)
	if(quotient != null && dend != null && visor != null)
		quotient set(DNADatum.int2Block(DNADatum.block2Int(dend.value)
										  / DNADatum.block2Int(visor.value)))
	val t = ()
  }
  
  /**
   * Creates a for loop and runs instructions.
   * Will execute instructions until the iterating value
   * has surpassed the end value, after starting from
   * the initial value.
   */
  def instantiateForLoop(instructions: String, loopVals:String){
    val startingDatum = find(loopVals.substring(0,18))
    val endingDatum = find(loopVals.substring(18))
    if(startingDatum == null || endingDatum == null) ()
    val forLoop = new ForScope(instructions, variables,
    						   startingDatum.asInt(), endingDatum.asInt())
    forLoop.start()
  }
  
  /**
   * Creates an if branch and runs instructions.
   * Will execute instructions once if the boolean evaluates to true
   * Takes String instructions and the name of the DNADatum 
   * that contains the desired boolean value. If that DNADatum
   * does not exist, no branch will be created.
   */
  def instantiateIfBranch(instructions: String, boolean:String){
    val boolDatum = find(boolean)
    if(boolDatum == null) ()
    val ifBranch = new IfScope(instructions, variables, boolDatum)
    ifBranch.start()
  }
  
  /**
   * Creates a while loop and runs instructions.
   * Will execute instructions so long as the boolean evaluates to true
   * Takes String instructions and the name of the DNADatum 
   * that contains the desired boolean value. If that DNADatum
   * does not exist, no loop will be created.
   */
  def instantiateWhileLoop(instructions: String, boolean:String){
    val boolDatum = find(boolean)
    if(boolDatum == null) ()
    val whileLoop = new WhileScope(instructions, variables, boolDatum)
    whileLoop.start()
  }
  
  /**
   * Will attempt to call print() on the integer form of a DNADatum
   * Takes the name of the desired DNADatum
   */
  def tryIntPrint(name: String){
    val datum = find(name)
    if(datum != null)
      println(datum.asInt())
  }
  
  /**
   * Will attempt to call print() on the String form of a DNADatum
   * Takes the name of the desired DNADatum
   */
  def tryStringPrint(name: String){
    val datum = find(name)
    if(datum != null)
      println(datum.asString())
  }
  
  /**
   * Will attempt to call print() on the Boolean form of a DNADatum
   * Takes the name of the desired DNADatum.
   */
  def tryBooleanPrint(name: String){
    val datum = find(name)
    if(datum != null) {
      val (name1, name2) = datum.getInvolvedData
	  val (datum1, datum2) = (find(name1), find(name2))
      val evaluableBoolean = datum.asBoolean(datum1, datum2)
      println(evaluableBoolean)
    }
  }
  
  /**
   * Will attempt to print the toString of the DNADatum
   * Takes the name of the desired DNADatum
   * The toString method is of the form:
   * datum.asInt() + "/" + datum.asString()
   */
  def tryIntStringPrint(name: String){
    val datum = find(name)
    if(datum != null)
      println(datum.toString)
  }
}

object DNAScope {
  
  /**
   * Strips any non A, G, T, or C's from the code (String). Returns result (String)
   */
  def removeNonCodons(instruction: String) = instruction.replaceAll("[^AGTC]", "")
  
  /**
   * Takes a String of valid instructions and splits the results into a (String, String) tuple
   * The first component contains the next "howMany" valid instructions, the second contains the remaining
   * instructions. If the instructions are too short for a valid instruction, empty Strings are returned
   * Precondition: Any non-codons have been removed from "instructions"
   */
  def nextTokens(instructions: String, howMany: Int): (String, String) = {
    if(instructions.length < 18*howMany) ("", "")
    else instructions.splitAt(18*howMany)
  }
  
  /**
   * Uses the nextTokens method, but only pulls one valid instruction.
   * For more information, see the comment above "nextTokens(String, Int)"
   */
  def nextToken(instructions: String) = nextTokens(instructions, 1)
  
  /**
   * Will grab instructions until after it grabs a valid instruction beginning with
   * the prefix "untilPrefix" If it does not find a valid instruction that begins
   * with "untilPrefix" by the time it runs out of "instructions", empty Strings are returned
   * Precondition: Any non-codons have been pruned from "instructions"
   */
  def nextTokensUntil(instructions: String, untilPrefix: String): (String, String) = {
    def takeMore(next: String, remaining: String): (String, String) = {      
      if(remaining.length < 18) ("", "")
      
      val (first, tail) = nextToken(remaining)
      
      if(remaining startsWith untilPrefix)
        (next + first, tail)
      else
        takeMore(next + first, tail)
    }
    takeMore("", instructions)
  }
  
  /**
   * nextTokensInScope considers the case that there can be nested scopes.
   * It tries to grab tokens such that there are a proper order/number of
   * Scope opening and closing tokens ("TAC" and "ATT" respectively)
   * Returns ("","") if the remainder of the code is improperly typed
   * Precondition: The calling function will pass the codons that
   *   follow the first "TAC" block, thus the "TAC" count starts at 1
   */
  def nextTokensInScope(instructions: String): (String, String) = {
    var (tacCount, attCount) = (1,0)
    var (grabbed, remaining) = ("", instructions)
    while(tacCount > attCount){
      val (next, leftOver) = nextToken(remaining)
      grabbed += next
      remaining = leftOver
      
      next match {
        case s:String if s startsWith "TAC" => tacCount += 1
        case t:String if t startsWith "ATT" => attCount += 1
        case e:String if e equals "" => grabbed = ""
                                        remaining = ""
                                        tacCount = 0
                                        attCount = 1
        case _ => ()
      }
    }
    (grabbed, remaining)   
   } 
}