object UserAgentCalculator extends Factory {
    for {
      userAgent <- userAgent
      findResult = ieMatch.find if findResult
    } yield ver
}
