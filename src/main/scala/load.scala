import scala.io.Source

package fptp {

  package object load {

    def constituencyWeights(filename: String) : Map[String, Int] = {
      val csvFile : Iterator[String] = Source.fromResource(filename).getLines
      val line1 = csvFile.slice(0, 1).toList(0)
      val line2 = csvFile.slice(0, 1).toList(0)
      val constituencies = line1.split(',').map(_.trim).toList
      val weights = line2.split(',').map(_.trim).map(_.toInt).toList
      (constituencies.tail zip weights.tail).toMap
    }

    def parseParty(line: String) : String =
      line.split(',').map(_.trim).toList(0)

    def parseVotes(
      line: String,
      constituencies: List[String]
    ) : Map[String, Double] = {
      val votes = line.split(',').map(_.trim).toList
      (constituencies zip votes.tail.map(_.toDouble)).toMap
    }

    def countTotal(
      constituency: String,
      constituencyVotes: Map[String, Map[String, Double]]
    ) : Double = {
      val parties = constituencyVotes.keys.toList
      val partyVotes = parties.map(constituencyVotes(_)(constituency))
      partyVotes.sum
    }
    
    def constituencyVotes(filename: String) : Map[String, Map[String, Double]] = {
      val csvFile : Iterator[String] =
        Source.fromResource(filename).getLines
      val csvFile2 : Iterator[String] =
        Source.fromResource(filename).getLines      
      val line1 = csvFile.slice(0, 1).toList(0)
      val constituencies = line1.split(',').map(_.trim).toList.tail
      val parties =
        csvFile2.drop(2).map(parseParty).toList
      val partyVotes =
        csvFile.drop(1).map(parseVotes(_, constituencies)).toList
      val constituencyVotes = (parties zip partyVotes).toMap
      val totalVotes = constituencies.map(countTotal(_, constituencyVotes))
      val totalConstituencyVotes =
        (constituencies zip totalVotes).toMap
      constituencyVotes + ("TOT" -> totalConstituencyVotes)
    }

  }

}
