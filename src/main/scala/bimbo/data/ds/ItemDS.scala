package bimbo.data.ds

import bimbo.data.Item

trait ItemDS {
    def getAllItems(): Seq[Item] 
    
    def getDSFile():String
}