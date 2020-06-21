package fptp {

  package object compute {

    def weak(
      partyVotes : Double,
      totalVotes : Double,
      maxWeak : Double
    ) : Double = (partyVotes.toDouble / totalVotes > maxWeak) match {
      case true => maxWeak * totalVotes
      case false => partyVotes
    }

    def partyWeak(
      party: String,
      constituencyVotes: Map[String, Map[String, Double]],
      maxWeak: Double
    ) : Map[String, Double] = {
      val constituencies = constituencyVotes("TOT").keys.toList
      val weakVotes =
        constituencies.map(
          constituency =>
          weak(
            constituencyVotes(party)(constituency),
            constituencyVotes("TOT")(constituency),
            maxWeak
          )
        )
      (constituencies zip weakVotes).toMap
    }

    def partyStrong(
      partyWeakVotes: Map[String, Double],
      partyConstituencyVotes: Map[String, Double]
    ) : Map[String, Double] = {
      val constituencies = partyConstituencyVotes.keys.toList
      val strongVotes =
        constituencies.map(
          constituency =>
          partyConstituencyVotes(constituency) - partyWeakVotes(constituency)
        )
      (constituencies zip strongVotes).toMap
    }

    def voteStrength(
      constituencyVotes: Map[String, Map[String, Double]],
      maxWeak : Double = 0.2
    ) : List[Map[String, Map[String, Double]]] = {
      val parties = (constituencyVotes - "TOT").keys.toList
      val weakVotesList =
        parties.map(
          party =>
          partyWeak(party, constituencyVotes, maxWeak)
        )
      val weakVotes = (parties zip weakVotesList).toMap
      val strongVotesList =
        parties.map(
          party =>
          partyStrong(weakVotes(party), constituencyVotes(party))
        )
      val strongVotes = (parties zip strongVotesList).toMap
      List(weakVotes, strongVotes)
    }

    def totalVotes(
      constituencyVotes: Map[String, Map[String, Double]],
    ) : Map[String, Double] = {
      val parties = constituencyVotes.keys.toList
      val partyVotes = parties.map(constituencyVotes(_).values.sum)
      (parties zip partyVotes).toMap
    }

    def partyVotes(
      constituencyVotes: List[Map[String, Map[String, Double]]]
    ) : List[Map[String, Double]] = {
      constituencyVotes.map(totalVotes(_))
    }

    def partyPercents(
      partyVotes: Map[String, Double],
      total: Double
    ) : Map[String, Double] = {
      val parties = partyVotes.keys.toList
      val pcts = parties.map(party => partyVotes(party) / total)
      (parties zip pcts).toMap
    }

    def partyFilteredPercents(
      partyVotes: List[Map[String, Double]]
    ) : List[Map[String, Double]] = {
      val total = partyVotes(0)("TOT")
      partyVotes.map(partyPercents(_, total))
    }

  }

}
