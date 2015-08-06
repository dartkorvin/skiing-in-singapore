import org.scalatest.{Matchers, FlatSpec}
import su.moi.lerrox.jgraphTest.MountainGuide

/**
 * Created by lerrox on 06.08.15.
 */
class MountainGuideSpec  extends FlatSpec with Matchers{

  "MountainGuide" should "find longest path with maximum drop" in {

    val mapString = "4 4 \n4 8 7 3 \n2 5 9 3 \n6 3 2 5 \n4 4 1 6"
    val guide = new MountainGuide(mapString)
    
    val bestPath = guide.longestPathWithMaxDrop()

    bestPath shouldNot be(None)
    bestPath.get.drop should be(8)
    bestPath.get.length should be(5)

  }

}
