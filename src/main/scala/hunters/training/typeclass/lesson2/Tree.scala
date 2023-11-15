package hunters.training.typeclass.lesson2

case class Tree[T](value: T, children: List[Tree[T]])
