import fptp._

object Main {
  def main(args: Array[String]) : Unit = {
    val electoralCollege = fptp.load.constituencyWeights("uspres_2016.csv")
    val partyConstituencyVotes =
      List(fptp.load.constituencyVotes("uspres_2016.csv"))
    val parties = partyConstituencyVotes(0).keys.toList
    val voteStrength = fptp.compute.voteStrength(partyConstituencyVotes(0))
    val constituencyVotes = partyConstituencyVotes ++ voteStrength
    val partyConstituencyVotesAdjustedList =
      parties.map(
        party =>
        (partyConstituencyVotes(0)(party) - "ME") - "NE"
      )
    val partyConstituencyVotesAdjusted =
      (parties zip partyConstituencyVotesAdjustedList).toMap
    val voteStrengthAdjusted0List =
      parties.filter(_ != "TOT").map(
        party =>
        (voteStrength(0)(party) - "ME") - "NE"
      )
    val voteStrengthAdjusted0 =
      (parties zip voteStrengthAdjusted0List).toMap
    val voteStrengthAdjusted1List =
      parties.filter(_ != "TOT").map(
        party =>
        (voteStrength(1)(party) - "ME") - "NE"
      )
    val voteStrengthAdjusted1 =
      (parties zip voteStrengthAdjusted1List).toMap
    val constituencyVotesAdjusted =
      List(partyConstituencyVotesAdjusted, voteStrengthAdjusted0, voteStrengthAdjusted1)
    val totalVotes = fptp.compute.partyVotes(constituencyVotesAdjusted)
    val partyVotes = fptp.compute.partyFilteredPercents(totalVotes)
    val projectedVotes =
      Map(
        "DEM" -> 0.48,
        "REP" -> 0.46,
        "LIB" -> 0.03,
        "GRE" -> 0.01,
        "OTH" -> 0.01
      )
        
    val voteRetention = fptp.project.voteRetention(partyVotes, projectedVotes)
    val projection = fptp.project.constituencyVotes(constituencyVotes, voteRetention)
    val win = fptp.project.constituencyWinners(projection)
    val seats = fptp.project.weightedSeats(win, electoralCollege)
    }
}

