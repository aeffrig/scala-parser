object CyclicDependencyException {
    def str(info: ResolutionInfo) =
      s"${info.resourceName} from: ${info.origins.mkString(", ")}"
}
