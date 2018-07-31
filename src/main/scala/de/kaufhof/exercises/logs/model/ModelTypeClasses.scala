/**
  * implemented by Alexander Ray (info@alexray.me)
  *
  * model type classes
  */
package de.kaufhof.exercises.logs.model

object ModelTypeClasses {

  /**
    * printer type class
    * used for print a log representation to a string or json or some another format
    * for now only string represantion is implemented [[de.kaufhof.exercises.logs.model.ModelStringPrinterImplicits]]
    * @tparam T - print source
    * @tparam R - print format
    */
  trait Printer[T, R] {
    def print(x: T): R
  }

  /**
    * extends all of the classes that supports a Printer type class with a x.print method
    */
  implicit class PrinterOps[T, R](val a: T) extends AnyVal {
    def print(implicit p: Printer[T, R]): R = p.print(a)
  }

}
