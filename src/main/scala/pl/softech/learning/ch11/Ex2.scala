package pl.softech.learning.ch11

import pl.softech.learning.ch6.State

object Ex2 {

  trait MonadInstances {

    implicit def stateMonadInstance[S]: Monad[({type λ[A] = State[S, A]})#λ] = new Monad[({type λ[A] = State[S, A]})#λ] {
      override def flatMap[A, B](fa: State[S, A])(f: A => State[S, B]): State[S, B] = fa.flatMap(f)

      override def pure[A](a: A): State[S, A] = State.unit(a)
    }

  }

  type Registers = List[Int]

  type StackState[A] = State[(Registers, List[String]), A]

  def main(args: Array[String]): Unit = {

    import MonadInstances._

    Monad[StackState]

    def ret: StackState[Int] = State { case (regs, stack) =>
      (if (stack.isEmpty) regs.headOption.getOrElse(0) else stack.head.toInt, (regs, stack))
    }

    def continue(program: StackState[Int]): StackState[Int] = for {
      stack <- State.get[(Registers, List[String])].map(_._2)
      res <- if (stack.size == 0) ret else program
    } yield res

    def opF(f: Registers => (Int, Registers)): StackState[Int] = for {
      state <- State.get[(Registers, List[String])]
      (regs, stack) = state
      (res, regsN) = f(regs)
      _ <- State.set((regsN, res.toString :: stack.drop(1)))
    } yield res

    def op(f: Int => Int): StackState[Int] = opF { case a :: tail =>
      (f(a), tail)
    }

    def biOp(f: (Int, Int) => Int): StackState[Int] = opF { case a :: b :: tail =>
      (f(a, b), tail)
    }

    val numberOp: StackState[Int] = for {
      state <- State.get[(Registers, List[String])]
      (regs, stack) = state
      res = stack.head.toInt
      _ <- State.set((res :: regs, stack.drop(1)))
    } yield res

    lazy val program: StackState[Int] = for {
      state <- State.get[(Registers, List[String])]
      _ <- state match {
        case (_, h :: _) => h match {
          case "+" => biOp(_ + _)
          case "*" => biOp(_ * _)
          case "-" => op(x => -x)
          case _ => numberOp
        }
        case _ => ret
      }
      res <- continue(program)
    } yield res

    println(program.run((List.empty, List.empty)))
    println(program.run((List.empty, List("1", "2"))))
    println(program.run((List.empty, List("1", "2", "+"))))
    println(program.run((List.empty, List("1", "2", "+", "1", "+"))))
    println(program.run((List.empty, List("3", "1", "2", "+", "*"))))
    println(program.run((List.empty, List("3", "1", "2", "+", "*", "-"))))
    println(program.run((List.empty, List("3", "1", "2", "+", "-", "*"))))

  }


}
