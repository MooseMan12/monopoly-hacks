import scala.util.Random
import scala.collection.mutable.Buffer

val BOARD_SIZE = 36
val PRISON_LOC = 26
val GO_TO_PRISON_LOC = 9
val TCH_LOC = 6
val CHANCE_LOC = 14
val P_GO_TO_PRISON = 0.25
val boardPositionCount = Buffer[Int]()
var boardPosition = 0

def rollDie:Int  = Random.nextInt(6)+1
def rollDice:Int = rollDie+rollDie

def rollDiceMany( times:Int ) = {
    Range(0,times).map( x=>rollDice ).groupBy(x=>x)
        .map(p=>p._1->p._2.length).toSeq.sortBy(p=>p._1)
        .foreach( p=>println(s"${p._1}\t${p._2}") )
}

def advance():Unit = {
    val amount = rollDice
    val landLocation = (boardPosition + amount)%BOARD_SIZE

    boardPosition = landLocation match {
        case GO_TO_PRISON_LOC => PRISON_LOC
        case TCH_LOC | CHANCE_LOC => if (Random.nextDouble() < P_GO_TO_PRISON ) PRISON_LOC else landLocation
        case _ => landLocation
    }

    boardPositionCount(boardPosition) = boardPositionCount(boardPosition)+1
}

def printCounts = {
    boardPositionCount.zipWithIndex.foreach( p=>println(s"${p._2}\t${p._1}") )
}

// init landing counts
boardPositionCount.addAll(Range(0,BOARD_SIZE).map(x=>0))