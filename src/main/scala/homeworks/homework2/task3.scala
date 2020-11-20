package homeworks.homework2

import cats.Semigroup
import cats.data.NonEmptyList

object task3 extends App {
  // Semigroup[A] - тайпкласс, который определяет ассоциативную операцию combine(x: A, y: A): A для типа A
  // Более слабый, чем Monoid - не определяет нейтральный элемент

  // Реализуйте функцию combineErrors,
  // которая принимает Either от непустого списка ошибок типа E или успеха типа A
  // и возвращает Either от ошибки типа E, полученной комбинацией ошибок из списка в начальном порядке,
  // или успеха типа A
  //
  // Большинство методов NonEmptyList работают так же, как у обычного List,
  // но некоторые операции, например, head более безопасны, так как не бросают исключений
  def combineErrors[E, A](a: Either[NonEmptyList[E], A])(implicit semigroupE: Semigroup[E]): Either[E, A] =
    a match {
      case Right(value)     => Right(value)
      case Left(errorsList) => Left(errorsList.reduceLeft(semigroupE.combine))
    }

  implicit object semigroupE extends Semigroup[Throwable] {
    override def combine(x: Throwable, y: Throwable): Throwable = new Exception(s"${x.getMessage}, ${y.getMessage}")
  }


  println(combineErrors(Right(123)))
  // Right(123)
  println(combineErrors(Left(
    NonEmptyList.of(
      new IllegalArgumentException("Oops!"),
      new IllegalStateException("Oh no!"),
      new IllegalAccessError("Yikes!")
    )))
  )
  // Left(java.lang.Exception: Oops!, Oh no!, Yikes!)


  // Реализуйте функцию mergeMaps, которая объединяет два ассоциативных массива по следующим правилам:
  // * если ключ есть в обоих массивах, то в результирующем массиве у этого ключа будет значение,
  //   полученное комбинацией двух значений для этого ключа из исходных массивов
  //   (причем значение из первого массива будет первым аргументом функции combine, а значение из второго массива будет вторым)
  // * если ключ есть только в одном из массивов, то в результирующем массиве будет этот ключ c таким же значением
  // * если ключа нет ни в одном массиве, то в результирующем массиве его тоже не будет
  def mergeMaps[K, V](left: Map[K, V], right: Map[K, V])(implicit semigroupV: Semigroup[V]): Map[K, V] = {
    val leftKeys = left.keys.toList
    val rightKeys = right.keys.toList
    val keys = (leftKeys ::: rightKeys).distinct
    keys.foldRight[List[(K, V)]](Nil){
      (key, items) => (left.get(key): @unchecked, right.get(key): @unchecked) match {
        case (Some(leftItem), Some(rightItem)) => key -> semigroupV.combine(leftItem, rightItem) :: items
        case (Some(leftItem), None)            => key -> leftItem :: items
        case (None, Some(rightItem))           => key -> rightItem :: items
      }
    }.toMap
  }

  implicit object semigroupV extends Semigroup[String] {
    override def combine(x: String, y: String): String = x + y
  }

  println(
    mergeMaps(
      Map(1 -> "a", 2 -> "b", 3 -> "c"),
      Map(1 -> "d", 3 -> "e", 4 -> "f")
    )
  )
  // Map(1 -> ad, 3 -> ce, 4 -> f, 2 -> b)
}
