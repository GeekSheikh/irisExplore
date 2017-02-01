//GitExample - https://github.com/JakeGreene/iris-ml/

//Load Functions at bottom first.

import org.apache.spark.sql.{SQLContext,Row,DataFrame}
import org.apache.spark.sql.types._
import org.apache.spark.ml.feature.StringIndexer
import org.apache.spark.ml.clustering.KMeans
import org.apache.spark.ml.Pipeline
import org.apache.spark.ml.feature.VectorIndexer
import org.apache.spark.ml.feature.VectorAssembler
import org.apache.spark.ml.linalg.Vectors
val sqlContext = new SQLContext(sc)
import sqlContext.implicits._
  
val irisFeatureColumn = "iris-features"
val irisTypeColumn = "iris-type"

//Take a look at the data with the schema.
val irisDF = loadIris("/user/sense/dsw/iris/data/iris.data")
  
irisDF.show(20,false)
irisDF.printSchema
  
val (trainingData, testData) = {
    // Experiment with adjusting the size of the training set vs the test set
val split = irisDF.randomSplit(Array(0.40, 0.60))
  (split(0), split(1))
  }

val kmeans = new KMeans()
  .setK(6)
  .setFeaturesCol(irisFeatureColumn)
  
trainingData.cache()
  
val model = kmeans.fit(irisDF)
  
testData.cache()
val predictions = model.transform(irisDF)

val predsAndTypes = predictions.select("prediction", "iris-type").collect().toList
predsAndTypes.foreach { case Row(prediction: Int, irisType: String) =>
    println(s"Assigned Cluster: $prediction\tIris Type: $irisType")
  }

val setosaAccuracy = accuracyOf("Iris-setosa", predsAndTypes)
println(s"Accuracy of iris setosa is ${setosaAccuracy * 100}")
val versicolorAccuracy = accuracyOf("Iris-versicolor", predsAndTypes)
println(s"Accuracy of iris versicolor is ${versicolorAccuracy * 100}")
val virginicasAccuracy = accuracyOf("Iris-virginica", predsAndTypes)
println(s"Accuracy of iris virginicas is ${virginicasAccuracy * 100}")

  
def accuracyOf(irisType: String, predsAndTypes: List[Row]): Double = {
  val clusters = predsAndTypes.collect {
    case Row(prediction: Int, iris: String) if iris == irisType => prediction
  }
  val cluster = mostCommon(clusters)
  clusters.filter(_ == cluster).size / clusters.size.toDouble
}

def mostCommon[A](l: List[A]): A = {
  l.groupBy(identity).mapValues(_.size).maxBy(_._2)._1
}


def loadIris(filePath: String): DataFrame = {
  val irisData = sqlContext.sparkContext.textFile(filePath).flatMap { text =>
    text.split("\n").toList.map(_.split(",")).collect {
      case Array(sepalLength, sepalWidth, petalLength, petalWidth, irisType) =>
        (Vectors.dense(sepalLength.toDouble, sepalWidth.toDouble, petalLength.toDouble, petalWidth.toDouble), irisType)
    }
  }
  sqlContext.createDataFrame(irisData).toDF(irisFeatureColumn, irisTypeColumn)
}

  
  
  
  
  
  
  
  
  
