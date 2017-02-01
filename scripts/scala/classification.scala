import scala.reflect.runtime.universe
import org.apache.spark.SparkConf
import org.apache.spark.SparkContext
import org.apache.spark.ml.Pipeline
import org.apache.spark.ml.classification.RandomForestClassifier
import org.apache.spark.ml.feature.StringIndexer
import org.apache.spark.ml.tuning.ParamGridBuilder
import org.apache.spark.mllib.evaluation.MulticlassMetrics
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator
import org.apache.spark.ml.linalg.Vectors
import org.apache.spark.sql.Row
import org.apache.spark.sql.SQLContext
import org.apache.spark.ml.tuning.TrainValidationSplit
  
val sqlContext = new SQLContext(sc)
import sqlContext.implicits._

val irisDataFrame = loadIris("/user/sense/dsw/iris/data/iris.data")
val (trainingData, testData) = {
  // Experiment with adjusting the size of the training set vs the test set
  val split = irisDataFrame.randomSplit(Array(0.8, 0.2))
  (split(0), split(1))
}
    
val indexer = new StringIndexer()
  .setInputCol(irisTypeColumn)
  .setOutputCol("label")
val classifier = new RandomForestClassifier()
  .setFeaturesCol(irisFeatureColumn)
val pipeline = new Pipeline()
  .setStages(Array(indexer, classifier))
    
val paramGrid = new ParamGridBuilder()
  .addGrid(classifier.maxDepth, Array(2, 5, 10))
  .addGrid(classifier.numTrees, Array(10, 20, 40))
  .addGrid(classifier.impurity, Array("gini", "entropy"))
  .build()

// Use 80% of the data to train and 20% to validate
val trainValidationSplit = new TrainValidationSplit()
  .setEstimator(pipeline)
  .setEvaluator(new MulticlassClassificationEvaluator())
  .setEstimatorParamMaps(paramGrid)
  .setTrainRatio(0.8)

// Create our model with the training-set of data
trainingData.cache()
val model = trainValidationSplit.fit(trainingData)

// Use the model with our test-set of data
testData.cache()
val testResults = model.transform(testData)
    
val predAndLabels = testResults
    .select("prediction", "label")
    .map { case Row(prediction: Double, label: Double) => 
      (prediction, label)
    }
val metrics = new MulticlassMetrics(predAndLabels.rdd)
println(s"Precision ${metrics.precision}")
println(s"Recall ${metrics.recall}")
println(s"F1 Score ${metrics.fMeasure}")

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