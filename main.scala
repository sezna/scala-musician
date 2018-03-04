

/**
 * @author awlecks
 * to add: basslines
 *         preferably add some sort of cadencing
 *         dynamics
 *         maybe a dissonance remover
 *         midi export
 *         structure specifier 
 *         GUI
 */
import org.jfugue.theory.ChordProgression;
import org.jfugue.pattern.Pattern;
import org.jfugue.player.Player;
import org.jfugue.rhythm.Rhythm;
// object music4 extends App {
  /*STRUCTURE*/
    val sections = 6 //number of sections
    val meter = 4 //number of beats per measure
    val groupings = 4 //how many bars per section, this is sometimes squared
    val grouping_chance = 1.0 //double value, shouldn't be greater than 1.0, should be more than double grouping chance
    val double_grouping_chance = 0.4 //chance of double grouping. should always be less than grouping_chance
    val song_doubled = false //double the song?
    val tempo = "120" 
  /*HARMONIC PROPERTIES AND FREQUENCIES*/
    val chord_progressions = List[String]("Amin Fmaj Cmaj Gmaj")
 
  /*PROBABILITIES - HARMONY*/
    var whole_frequency = 1
    var half_frequency = 4
    var quarter_frequency = 10
    var eighth_frequency = 4
    var sixteenth_frequency = 1
    var rest_frequency = .1
    var dotted_frequency = .4
    val harmony_durations = ("w" * whole_frequency) + ("h" * half_frequency) + ("q" * quarter_frequency) + ("i" * eighth_frequency) + ("s" * sixteenth_frequency)
    var to_play_chords = makeFinalHarmony()
  /*PROBABILITIES - MELODY*/
    var whole_probability = 1
    var half_probability = 2
    var quarter_probability = 5
    var eighth_probability = 4
    var sixteenth_probability = 6
    var rest_probability = .1
    var dotted_probability = .2
    var same_probability = 3
    var step_probability = 5
    var skip_probability = 1
    var leap_probability = 1
    var octave_up_probability = .1
    var octave_down_probability = .1
    var chordal_probability= 4
    var atonal_probability = 0 //best coupled with a high sharp probability, because this only adds tonal notes to the bank. It does add all of them, though.
    var sharp_probability = 0.01
    var rhythm_same_as_chords_probability = 0
    var attack = 64
    var decay = 127
  //  var flat_probability = 1
  /*MELODY PROPERTIES*/
    var melody_lower_range = 4
    var melody_upper_range = 7
    var melody_reset_point = 5
    val melody_durations = ("w" * whole_probability) + ("h" * half_probability) + ("q" * quarter_probability) + ("i" * eighth_probability) + ("s" * sixteenth_probability)
    var to_play_melody = makeMelody(to_play_chords, melody_durations)
    /*PROBABILITIES - BASS */
        attack = 64
        decay = 60
        sharp_probability = 0
        whole_probability = 0
        half_probability = 2
        quarter_probability = 20
        eighth_probability = 3
        sixteenth_probability = 15
        rest_probability = .2
        dotted_probability = 0
        same_probability = 1
        step_probability = 1
        skip_probability = 1
        leap_probability = 1
        chordal_probability = 300
        atonal_probability = 0
        melody_lower_range = 2
        melody_upper_range = 4
        melody_reset_point = 3
        rhythm_same_as_chords_probability = 10
    val bass_durations = ("w" * whole_probability) + ("h" * half_probability) + ("q" * quarter_probability) + ("i" * eighth_probability) + ("s" * sixteenth_probability)  
    var to_play_bass = makeMelody(to_play_chords, bass_durations)

  println("to play: " + to_play_chords)
  /*actually play the music*/
    if (song_doubled) {
      to_play_chords = to_play_chords * 2
      to_play_melody = to_play_melody * 2
      to_play_bass = to_play_bass * 2
    }
   
    val musicString = "V0 I[Square] T" + tempo + " " + to_play_melody + " V1 I[Piano] T" + tempo + " " + to_play_chords + " V2 I[Piano] T" + tempo + " " + to_play_bass
    println(musicString)
    val p1 = new Pattern(musicString)
    var rhythm = new Rhythm()
        rhythm.addLayer("`" * meter)
        rhythm.addLayer("+" + ("." * (meter - 1)))
    val metro = new Pattern("T" + tempo + " " + rhythm.getPattern().repeat(to_play_chords.filter(x => x == '|').length))
    val player = new Player();
//    player.play(p1, p2, metro)
    player.play(p1)
    println("Would you like to save this session? y/n")
    var yesno = scala.io.StdIn.readLine
    if (yesno == "y") {
        try {
          val format = new java.text.SimpleDateFormat("d-M-y-hh-mm-ss")
          val file_name = (format.format(java.util.Calendar.getInstance().getTime()))
          org.jfugue.midi.MidiFileManager.savePatternToMidi(p1, new java.io.File(file_name + "_melody.mid"));
//          org.jfugue.midi.MidiFileManager.savePatternToMidi(p2, new java.io.File(file_name + "_harmony.mid"));
       }catch{ case e: Exception => println("something happened") }
    // Handle the exception
   
    }
   
   
   var note_index = 0 
   var curr_octave = melody_reset_point
  
  
 def octaveMath() {
   var random = scala.util.Random.nextDouble
   if (random <= octave_up_probability) {
  //   note_index = note_index % 11
     curr_octave = curr_octave + 1
   }
   else if (random <= octave_down_probability) {
     //note_index = math.abs(note_index) % 11
     curr_octave = curr_octave - 1
   }
   if (curr_octave < melody_lower_range || curr_octave > melody_upper_range) {
     curr_octave = melody_reset_point
   }
 }
   
 case class HarmonyPair(beat:Double, to_be_boosted:List[String])
 def makeMelody(harmony:String, melody_durations:String):String = {
   var harmony_boosts = List[HarmonyPair]()
   var curr_beat = 0.0
   val chromatic_list = List[String]("A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#")
   val tonal_list = List[String]("A", "B", "C", "D", "E", "F", "G")
   var tmp = List[String]()
   var harmony_list = harmony.split(" ").toList
   for (i <- 0 until harmony_list.length) {
     if (harmony_list(i).contains("maj")) {
//       println("harmony list contains a major chord at index " + i + " " + harmony_list(i))
       var tmp2 = (harmony_list(i).filter(x => ("ABCDEFG#".contains(x))))
       tmp = tmp.+:((chromatic_list((chromatic_list.indexOf(tmp2)+4) % 12)).toString)
       tmp = tmp.+:((chromatic_list((chromatic_list.indexOf(tmp2)+7) % 12)).toString)
       tmp = tmp.+:(tmp2)
//       println("to be boosted: " + tmp)
       harmony_boosts = harmony_boosts.+:(new HarmonyPair(curr_beat, tmp))
       tmp = List[String]()
     }
     else if (harmony_list(i).contains("min")) {
      //println("harmony list contains a minor chord at index " + i + " " + harmony_list(i))
       var tmp2 = (harmony_list(i).filter(x => ("ABCDEFG#".contains(x))))
       tmp = tmp.+:((chromatic_list((chromatic_list.indexOf(tmp2)+3) % 12)).toString)
       tmp = tmp.+:((chromatic_list((chromatic_list.indexOf(tmp2)+7) % 12)).toString)
       tmp = tmp.+:(tmp2)
//       println("to be boosted: " + tmp)
       harmony_boosts = harmony_boosts.+:(new HarmonyPair(curr_beat, tmp))
       tmp = List[String]()
     }
     else if (harmony_list(i).contains('R')) {
//       println("harmony list contains a rest at index " + i + " " + harmony_list(i))
     }
     if (harmony_list(i).contains('w') || harmony_list(i).contains('h') || harmony_list(i).contains('q') || harmony_list(i).contains('i') || harmony_list(i).contains('s') || harmony_list(i).contains('.')) {
       var tmp4 = ""
       if (harmony_list(i).contains('.')) tmp4 = harmony_list(i)(harmony_list(i).length - 1).toString + harmony_list(i)(harmony_list(i).length - 2).toString
       else tmp4 = harmony_list(i)(harmony_list(i).length - 1).toString
//       println("The rhythm is: " + tmp4.filter(x => "whqis.".contains(x)))
//       println("The current beat is: " + curr_beat)
//       println("The current measure is: " + ((curr_beat / meter).toInt +1))
       curr_beat = beatMath(curr_beat, tmp4.filter(x => "whqis.".contains(x)).toString, "+")
      
     }
   }
   val melody_rhythms = generateMeasures(melody_durations, harmony.filter(x => x == '|').length)
 //  println(harmony.filter(x => x == '|').length)
   val melody_list = melody_rhythms.split(" ")
   var tmp3 = curr_beat
   var detected = ""
   var to_return = ""
   var boosting = List[String]()
   curr_beat = 0.0
   var curr_note = "A"
   note_index = chromatic_list.indexOf(curr_note)
   for (i <- 0 until melody_list.length) {

     boosting = List[String]()
     if (melody_list(i).contains('w') || melody_list(i).contains('h') || melody_list(i).contains('q') || melody_list(i).contains('i') || melody_list(i).contains('s') || melody_list(i).contains('.'))  {
       detected = melody_list(i)
     //  println("Rhythm detected. " + detected)
       if (curr_beat <= harmony_boosts(0).beat) {
         for (j <- 0 until chordal_probability)
         boosting = boosting.++(harmony_boosts(0).to_be_boosted)
         }
       else {
         harmony_boosts = harmony_boosts.tail
       }
       for (j <- 0 until step_probability){
         boosting = boosting.+:(tonal_list((tonal_list.indexOf(curr_note) + 1) % tonal_list.length))
         boosting = boosting.+:(tonal_list(math.abs(tonal_list.indexOf(curr_note) - 1) % tonal_list.length))
       }
       for (j <- 0 until skip_probability) {
         boosting = boosting.+:(tonal_list((tonal_list.indexOf(curr_note) + 2) % tonal_list.length))
         boosting = boosting.+:(tonal_list(math.abs(tonal_list.indexOf(curr_note) - 2) % tonal_list.length))
       }
       for (j <- 0 until leap_probability) {
         boosting = boosting.+:(tonal_list((tonal_list.indexOf(curr_note) + 3) % tonal_list.length))
         boosting = boosting.+:(tonal_list(math.abs(tonal_list.indexOf(curr_note) - 3) % tonal_list.length))
       }
       for (j <- 0 until atonal_probability) {
         boosting = boosting.++(tonal_list)
       }
       for (j <- 0 until same_probability) {
         boosting = boosting.+:(curr_note)
       }
      
       curr_beat = beatMath(curr_beat, detected, "+")
  //     println("The current beat is: " + curr_beat)
       curr_note = boosting(scala.util.Random.nextInt(boosting.length))
       if (scala.util.Random.nextDouble() <= sharp_probability){
         curr_note = curr_note + "#"
       }
       octaveMath()
       to_return = to_return + curr_note + curr_octave + melody_list(i) + "A" + attack + "D" + decay + " "
     }
     else
     to_return = to_return + melody_list(i) + " "
   }
//   println("melody beat: " + curr_beat + " rhythm beat: " + tmp3)
//   println("melody rhythm: " + melody_rhythms)
//   println("harmony rhythm: " + harmony)
//   println(to_return)
   to_return
 }
 
   
 def makeFinalHarmony(): String = {
  var to_play_chords = ""
  var section_chords = List[List[String]]()
  var harmonic_rhythm_list = generateMeasures(harmony_durations, sections)
  //println(sections + " sections")
  for (i <- 0 until sections) {
    section_chords = section_chords.+:(chord_progressions(scala.util.Random.nextInt(chord_progressions.length)).split(" ").toList)
  }
   var chord_index = 0
   var measure_index = 0
   var prev_was_R = false
   for (i <- 0 until harmonic_rhythm_list.length) {
     if (prev_was_R) {
       to_play_chords = to_play_chords + harmonic_rhythm_list(i)
       prev_was_R = false
     }
     else if (harmonic_rhythm_list(i) == '.') {
          to_play_chords = to_play_chords + '.'
        }
     else if (harmonic_rhythm_list(i) == ' ' || harmonic_rhythm_list(i) == '|') {
       to_play_chords = to_play_chords + harmonic_rhythm_list(i)
       if (harmonic_rhythm_list(i) == '|') {
         measure_index = measure_index + 1
         chord_index = 0
       }
     }
     else if (harmonic_rhythm_list(i) == 'R') {
       to_play_chords = to_play_chords + harmonic_rhythm_list(i)
       prev_was_R = true
     }
     else {
       to_play_chords = to_play_chords + section_chords(measure_index)(chord_index) + harmonic_rhythm_list(i)
       chord_index = chord_index + 1
       if (chord_index >= section_chords(measure_index).length) {
         chord_index = 0
       }
     }    
    }
   var to_return = ""
   var grouping_tmp = to_play_chords.split('|')
   for (i <- 0 until grouping_tmp.length) {
     var tmp = scala.util.Random.nextDouble()
     if (grouping_tmp(i) == " ") {
       to_return = to_return + ' '
     }
     else {
       if (tmp < grouping_chance) {
         to_return = to_return + (grouping_tmp(i)  + " | " )* groupings
       }
       if (tmp < double_grouping_chance) {
         to_return = to_return + (grouping_tmp(i)  + " | ")* groupings
       }
     }
   }
/*   var tmp = 0.0
   for (i <- 0 until to_return.length) {
     if ("whqis.".contains(to_return(i))) {
       tmp = beatMath(tmp, to_return(i),)
     }
   }*/
   to_return
  }  
   
  def songStructure():List[Int] = {
      var toReturn = List[Int]()
      for (i <- 0 until sections) {
        var random = scala.util.Random.nextInt(10)
        if (random == 0) {
          toReturn = toReturn.+:((meter * 1.5).toInt)
        }
        else if (random >0 && random <= 8) {
          toReturn = toReturn.+:(meter)
        }
        else {
          toReturn = toReturn.+:(meter * 2)
        }
      }
      toReturn
    }
  
  def generateMeasures(dur_freq:String, number_of_measures:Int):String = {
      var fail_count = 0
      var measure = List[String]()
      var toReturn = List[List[String]]()
      var currNote = "q"
      var prevNote = ""
      var tmp = ""
      var beats_left:Double = meter.toDouble
      var is_measure_full = false
      for (i <- 0 until number_of_measures) {
        while(!is_measure_full){
        /*  if(prevNote.contains(".")) {
            tmp = currNote
            currNote = rhythmMath(prevNote, "-", 1)
            prevNote = tmp
            beats_left = beatMath(beats_left, currNote)
          }
          else */if (scala.util.Random.nextInt(10) >= 5) {
            currNote = currNote
            beats_left = beatMath(beats_left, currNote, "-")
          }
          else {
            prevNote = currNote
            currNote = dur_freq(scala.util.Random.nextInt(dur_freq.length - 1)).toString
            if (scala.util.Random.nextDouble() <= dotted_probability) {
              currNote = currNote + "."
            }
            else if (scala.util.Random.nextDouble() <= rest_frequency) {
              currNote = "R" + currNote
            }
            beats_left = beatMath(beats_left, currNote, "-")
          }
          measure = measure.+:(currNote)
          if (beats_left == 0) is_measure_full = true;
          else if (beats_left < 0) {beats_left = meter.toDouble;
          measure = List[String]();
          fail_count = fail_count + 1}
        }
        toReturn = toReturn.+:(measure)
        measure = List[String]()
        is_measure_full = false
      }
      var toReturnReal = ""
      for (i <- 0 until toReturn.length) {
        for (j <- 0 until toReturn(i).length) {
          toReturnReal = toReturnReal + toReturn(i)(j) + " "
        }
        toReturnReal = toReturnReal + "| "
      }
     // println("string for merging with chords: " + toReturnReal)
     // println("that took " + fail_count + " tries")
      toReturnReal
    }
   
  def beatMath(beats_left:Double, note:String, op:String):Double = {
      var return_value:Double = 0.0
      if (note.contains("q")) return_value =  1
      else if (note.contains("i")) return_value =  .5
      else if (note.contains("s")) return_value =  .25
      else if (note.contains("h")) return_value =  2
      else if (note.contains("w")) return_value =  4
      if (note.contains(".")) return_value = return_value * 1.5
      if (op == "-") return beats_left - return_value
      else return beats_left + return_value
    }
   
  def rhythmMath(note:String, operation:String, value:Int):String = {
        val rhythmArray = Array[String]("w", "h", "q", "i", "s")
        if (operation == "+") {
          return rhythmArray(math.abs(rhythmArray.indexOf(note) - value))
        }
        else if (operation == "-") {
          return rhythmArray((rhythmArray.indexOf(note) + value) % 5)
        }
        else if (operation == "/") {
          return rhythmArray(rhythmArray.indexOf(note) / value)
        }
        else
          println("Error in rhythmMath function. Returning same value")
          return note
      }
   
    ////////////END//////////////
  //}
