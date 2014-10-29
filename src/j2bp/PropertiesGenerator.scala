package j2bp

import scala.collection.immutable.List
import scala.collection.mutable.Map
import scala.collection.mutable.HashMap
import scala.collection.JavaConversions._

import common.Constants
import common.Property
import common.BinaryPredicate
import common.ExpressionUtils


abstract class PropertiesGenerator
{
	def inferProperties(progClasses : List[String]) : List[Property]
	
}
