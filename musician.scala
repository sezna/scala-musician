import scala.util.Random
import org.jfugue.player.Player
//globals
val wholeProbability     = 1
val halfProbability      = 1 
val quarterProbability   = 1
val eighthProbability    = 1
val sixteenthProbability = 1
val dottedProbability    = 1
val timeSignature        = 32 //number of 32nd notes
val cutOff               = 8
val featureProbability   = 1//out of 100
val songForm             = "AABA"
val melodyVariation      = .01 // chance that the melody wil vary when the part repeats
val sectionLength        = 2
//var potentialNotes       = Array("a", "b")

val test2 = getMelody(10)
val player = new Player
//player.play(test2)
val toPlay = makeSection
val string = "V0 " + toPlay._1 + " V2 " + toPlay._2 
player.play(string)



// Makesong function that will make a few melodies, attach them to different parts of the song, and repeat things.

// Function that makes one section
def makeSection():(String, String) = {
  val melody = getMelody(sectionLength)
  val harmony = getHarmony(sectionLength)
  (melody, harmony)

}

def getHarmony(numOfMeasures:Int):String = {
  var toReturn = ""
  var rhythms:Array[Array[String]] = Array()
  for (i <- 0 until numOfMeasures) {
    rhythms = rhythms :+ getRhythm
  }
  for (i <- 0 until rhythms.length) {
    for (j <- 0 until rhythms(i).length) {
      rhythms(i)(j) = "Gmaj" + rhythms(i)(j)
    }
  }
  var toReturnString = ""
  for (i <- 0 until rhythms.length) {
    for (j <- 0 until rhythms(i).length) {
      toReturnString = toReturnString + " " + rhythms(i)(j)
    }
    toReturnString = toReturnString + " |"
  }
  println(toReturnString)
  toReturnString
}

//rate a melody's cohesion
def melodyRate(melody:String):Double = {
  val melodyNotes = melody.filter(_ != "|").split(" ")  
  

  0.0
}
//getHarmony function that takes in the key and stuff and alters the probabilities of what getnote will return


//make melody function that uses getnote and getrhythm a lot
def getMelody(numOfMeasures:Int):String = {
  var noteList:Array[String] = Array()
  var measureList:Array[Array[String]] = Array()
  for (i <- 0 until numOfMeasures) {
    val measure = getRhythm
    measureList = measureList :+ measure
  }
  for (i <- 0 until measureList.length) {
    for (j <- 0 until measureList(i).length) {
      measureList(i)(j) = getNote(Array("a", "b", "c")) + measureList(i)(j)
    }
  }
  var measureArray:Array[String] = Array()
  for (i <- 0 until measureList.length) {
    for (j <- 0 until measureList(i).length) {
      measureArray = measureArray :+ (measureList(i)(j) + " ")
    }
  }
  var measureString = measureArray(0) + " "
  for (i <- 1 until measureArray.length) {
    measureString = measureString + "| " + measureArray(i)
  }
  measureString
}


//getrhythm that looks at the global properties and returns a full measure of rhythm - need to implement features
def getRhythm():Array[String] = {
  var timeLeft = timeSignature
  var toReturn:Array[String] = Array()
  if (Random.nextDouble() > featureProbability) {
    while (timeLeft > 0) {                                   // Builds a measure out of the features list
      val currentFeature = getRhythmFeature;                 
      if (timeLeft - currentFeature.map(_._2).sum >= 0) { 
        timeLeft = timeLeft - currentFeature.map(_._2).sum
        toReturn = toReturn ++ currentFeature.map(_._1) 
      }
      else {
        if (timeLeft < cutOff) {
          timeLeft = timeSignature
          toReturn = Array()
        }
      }
    }
  }
  while (timeLeft > 0) {
    val currentNote = getRandRhythm
    if (timeLeft - currentNote._2 >= 0) {
      timeLeft = timeLeft - currentNote._2
      toReturn = toReturn :+ currentNote._1
    }
    else {
      if (timeLeft < cutOff) {
        timeLeft = timeSignature
        toReturn = Array()
      }
    }
  }
  toReturn
}
//getRhythmFeature returns a feature from a list of rhythmic features
def getRhythmFeature():Array[(String, Int)] = {
  val rhythmFeatures:Array[Array[(String, Int)]] = Array(Array(("s", 2), ("s", 2), ("s", 2), ("s", 2)), Array(("q.", 12), ("s", 2)))
  rhythmFeatures(Random.nextInt(rhythmFeatures.size))
}

//getRandRhythm returns a random rhythm note based on global properties and a number that represents how many thirty second  notes it occupies
def getRandRhythm():(String, Int) = {
  val rhythmTypes = Array(("w", 32), ("h", 16), ("q", 8), ("i", 4), ("s", 2))
  var toPickFrom:Array[(String, Int)] = Array()
  for (i <- 0 until wholeProbability) {
    toPickFrom = toPickFrom :+ ("w", 32)
  }
  for (i <- 0 until halfProbability) {
    toPickFrom = toPickFrom :+ ("h", 16)
  }
  for (i <- 0 until quarterProbability) {
    toPickFrom = toPickFrom :+ ("q", 8)
  }
  for (i <- 0 until eighthProbability) {
    toPickFrom = toPickFrom :+ ("i", 4)
  }
  for (i <- 0 until sixteenthProbability) {
    toPickFrom = toPickFrom :+ ("s", 2)
  }
  var toReturn = toPickFrom(Random.nextInt(toPickFrom.length))
  if (Random.nextDouble() > dottedProbability) {
    toReturn = ((toReturn._1 + "."), (toReturn._2 + toReturn._2 / 2))
  }
  toReturn
}

//getNote function that will take in an array of potential notes and randomly select one.
def getNote(potentialNotes:Array[String]):String = {
  if (potentialNotes.size > 0) {
    potentialNotes(Random.nextInt(potentialNotes.size))
  }
  else "c"
}

