package pl.softech.learning.ch13

import java.nio.channels.AsynchronousFileChannel

import pl.softech.learning.ch7.parallelism.NonBlocking.Par

object Ex5 {

  // TODO We’re not going to work through a full-fledged implementation of a non-blocking I/O library here, but you may be interested to explore this on your own by building off the java.nio library ( API at http://mng.bz/uojM). As a start, try implementing an asynchronous read from an AsynchronousFileChannel ( API at http://mng.bz/X30L)

  def read(file: AsynchronousFileChannel, fromPosition: Long, numBytes: Int): Par[Either[Throwable, Array[Byte]]] = ???

  def main(args: Array[String]): Unit = {

  }

}
