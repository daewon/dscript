package org.apache.s2graph.core.parsers

import org.apache.s2graph.core.parsers.Exp.ExpTree.Exp

package object Exp {
  type ExpResult = Either[List[String], Exp]

}
