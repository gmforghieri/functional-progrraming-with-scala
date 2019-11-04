package dataset
import java.text.SimpleDateFormat
import java.util.SimpleTimeZone
import dataset.util.Commit.Commit

/**
  * Use your knowledge of functional programming to complete the following functions.
  * You are recommended to use library functions when possible.
  *
  * The data is provided as a list of `Commit`s. This case class can be found in util/Commit.scala.
  * When asked for dates, use the `commit.commit.committer.date` field.
  *
  * This part is worth 40 points.
  */
object Dataset {
    val format = new SimpleDateFormat("H")
    format.setTimeZone(new SimpleTimeZone(SimpleTimeZone.UTC_TIME, "UTC"))

    /** Q16 (5p)
      * For the commits that are accompanied with stats data, compute the average of their additions.
      * You can assume a positive amount of usable commits is present in the data.
      * @param input the list of commits to process.
      * @return the average amount of additions in the commits that have stats data.
      */
    def avgAdditions(input: List[Commit]): Int = {
        val list = input.filter(_.stats.isDefined)
        list.map(commit => commit.stats.get.additions).sum / list.length
    }

    /** Q17 (8p)
      * Find the hour of day (in 24h notation, UTC time) during which the most javascript (.js) files are changed in commits.
      * The hour 00:00-00:59 is hour 0, 14:00-14:59 is hour 14, etc.
      * Hint: for the time, use `SimpleDateFormat` and `SimpleTimeZone`.
      * @param input list of commits to process.
      * @return the hour and the amount of files changed during this hour.
      */
    def jsTime(input: List[Commit]): (Int, Int) = {
        input.map(commit => (format.format(commit.commit.committer.date).toInt,
          commit.files.filter(commit => commit.filename.get.matches(".*\\.js$"))))
          .map(commit => (commit._1, commit._2.size)).filter(commit => commit._2 != 0).groupBy(commit => commit._1)
          .mapValues(_.map(commit => commit._2).sum).toList.maxBy(_._2)
    }

    /** Q18 (9p)
      * For a given repository, output the name and amount of commits for the person
      * with the most commits to this repository.
      * For the name, use `commit.commit.author.name`.
      * @param input the list of commits to process.
      * @param repo the repository name to consider.
      * @return the name and amount of commits for the top committer.
      */
    def topCommitter(input: List[Commit], repo: String): (String, Int) = {
        input.filter(_.url.contains(repo)).groupBy(_.commit.author.name).mapValues(_.size).maxBy(_._2)
    }

    /** Q19 (9p)
      * For each repository, output the name and the amount of commits that were made to this repository in 2019 only.
      * Leave out all repositories that had no activity this year.
      * @param input the list of commits to process.
      * @return a map that maps the repo name to the amount of commits.
      *
      * Example output:
      * Map("KosDP1987/students" -> 1, "giahh263/HQWord" -> 2)
      */
    def commitsPerRepo(input: List[Commit]): Map[String, Int] = ???

    /** Q20 (9p)
      * Derive the 5 file types that appear most frequent in the commit logs.
      * @param input the list of commits to process.
      * @return 5 tuples containing the file extension and frequency of the most frequently appeared file types, ordered descendingly.
      */
    def topFileFormats(input: List[Commit]): List[(String, Int)] = ???
}
