package fptp {

  package object project {

    def partyVoteRetention(
      basePercents: List[Map[String, Double]],
      percentChange: Map[String, Double],
      party: String
    ) : List[Double] = {
      val weak = (percentChange(party) <= -1 * basePercents(1)(party)) match {
        case true => 0
        case false => 1 + percentChange(party) / basePercents(1)(party)
      }
      val strong = (percentChange(party) <= -1 * basePercents(1)(party)) match {
        case false => 1
        case true =>
          (basePercents(2)(party) <= 0.0) match {
            case true => 0
            case false =>
              1 + ((basePercents(1)(party) + percentChange(party)) /
                basePercents(2)(party))
          }
      }
      List(weak, strong)
    }

    def voteRetention(
      basePercents: List[Map[String, Double]],
      projectedPercents: Map[String, Double]
    ) : Map[String, List[Double]] = {
      val parties = projectedPercents.keys.toList
      val percentChangeList =
        parties.map(
          party =>
          projectedPercents(party) - basePercents(0)(party)
        )
      val percentChange = (parties zip percentChangeList).toMap
      val partyVoteRetentions =
        parties.map(
          party =>
          partyVoteRetention(basePercents, percentChange, party)
        )
      (parties zip partyVoteRetentions).toMap
    }

    def partyConstituencyVotes(
      baseConstituencyVotes: List[Map[String, Map[String, Double]]],
      voteRetention: List[Double],
      party: String
    ) : Map[String, Double] = {
      val constituencies = baseConstituencyVotes(0)(party).keys.toList
      val weakVotes = baseConstituencyVotes(1)(party)
      val strongVotes = baseConstituencyVotes(2)(party)
      val projectedConstituencyVotes =
        constituencies.map(
          constituency =>
          weakVotes(constituency) * voteRetention(0) +
            strongVotes(constituency) * voteRetention(1)
        )
          (constituencies zip projectedConstituencyVotes).toMap
    }

    def constituencyVotes(
      baseConstituencyVotes: List[Map[String, Map[String, Double]]],
      voteRetention: Map[String, List[Double]]
    ) : Map[String, Map[String, Double]] = {
      val parties = voteRetention.keys.toList
      val constituencyVotesList =
        parties.map(
          party =>
          partyConstituencyVotes(
            baseConstituencyVotes,
            voteRetention(party),
            party
          )
        )
      (parties zip constituencyVotesList).toMap
    }

    def constituencyWinner(
      constituencyVotes: Map[String, Map[String, Double]],
      constituency: String
    ) : String = {
      val parties = constituencyVotes.keys.toList
      val partyVotes =
        parties.map(party => constituencyVotes(party)(constituency))
      val winner =
        parties.filter(
          party =>
          constituencyVotes(party)(constituency) ==
            partyVotes.max
        )
      winner(0)
    }

    def constituencyRunnerUp(
      constituencyVotes: Map[String, Map[String, Double]],
      constituency: String
    ) : String = {
      constituencyWinner(
        constituencyVotes - constituencyWinner(constituencyVotes, constituency),
        constituency
      )
    }

    def constituencyWinners(
      constituencyVotes: Map[String, Map[String, Double]]
    ) : Map[String, String] = {
      val partyOne = constituencyVotes.keys.toList(0)
      val constituencies = constituencyVotes(partyOne).keys.toList
      val winners = constituencies.map(constituencyWinner(constituencyVotes, _))
        (constituencies zip winners).toMap
    }

    def weightedSeats(
      constituencyWinners: Map[String, String],
      constituencyWeights: Map[String, Int]
    ) : Map[String, Int] = {
      val parties = constituencyWinners.values.toSet
      val partyWinsList =
        parties.map(
          party =>
          constituencyWinners.keys.toList.filter(
            constituency =>
            constituencyWinners(constituency) == party
          )
        ).toList
      val partyWins = (parties zip partyWinsList).toMap
      val partySeatsList =
        parties.map(party => partyWins(party).map(constituencyWeights(_)))
      val partySeatsWeighted = (parties zip partySeatsList).toMap
      val partySeats = parties.map(partySeatsWeighted(_).sum)
      (parties zip partySeats).toMap
    }
    //TODO: add function to calculate margins in each constituency
    /*def constituencyMargins(
      constituencyVotes: Map[String, Map[String, Double]]
    ) : Map[String, List[String, Double]] = { //Constituency, List[Party, Lead]

    }*/

  }

}
