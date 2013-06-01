

object DNARunner {
  def main(args: Array[String]){
	  
    if(args.length == 1){
    	val runner = new DNAScope(args(0))
    	try{
    	  runner.start()
    	}
    	catch{
    	  case e:Throwable => println("Exception: " + e)
    	}
    }
  }
}