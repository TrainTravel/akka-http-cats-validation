package com.example

import akka.http.scaladsl.server.Directives
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import spray.json._
import cats._
import cats.data._
import cats.implicits._
import com.example.Validation.OrderJsonSupport

object Validation {
  abstract class ValidationFailure(val message: String)

  type ValidationResult[A] = ValidatedNel[ValidationFailure, A]

  trait Validatable[A] {
    def validate: ValidationResult[A]
  }

  val somea: Some[String] = Some("a")
  val somea2: Option[String] = "a".some
  val v1 = "error".invalid[Option[String]]

  case object ItemsIsEmpty extends ValidationFailure("Item is empty")

  case object SkuIsEmpty extends ValidationFailure("SKU is empty")

  case object CountIsInvalid extends ValidationFailure("Count must be positive")

  case class Item(sku: String, count: Int) extends Validatable[Item] {
    override def validate: ValidationResult[Item] = (
      validateSku,
      validateCount
      ).mapN(Item)

    private def validateSku: ValidationResult[String] =
      if (sku.nonEmpty) sku.validNel else SkuIsEmpty.invalidNel

    private def validateCount: ValidationResult[Int] =
      if (count > 0) count.validNel else CountIsInvalid.invalidNel

  }

  case class Order(items: List[Item]) extends Validatable[Order] {
    def validate: ValidationResult[Order] = validateItems.map(Order)

    private def validateItems: ValidationResult[List[Item]] = {
      if (items.nonEmpty) items.traverse(_.validate) else ItemsIsEmpty.invalidNel // Same code for each entity validation

    }
  }

  trait OrderJsonSupport extends SprayJsonSupport with DefaultJsonProtocol {
    implicit val itemFormat: JsonFormat[Item] = jsonFormat2(Item)
    implicit val orderFormat: RootJsonFormat[Order] = jsonFormat1(Order)
  }

}

class OrderApi extends Directives with OrderJsonSupport {

  val route =
    post {
      entity(as[Order]) { order =>
        complete(s"Ordered $order")
      }
    }
}