package bimbo.data

trait ItemDS {
    def getAllItems(): Seq[Item] 
    
    def getDSFile():String
}