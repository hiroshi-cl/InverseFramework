package inverse_macros.mixin

trait InverseMacrosEngine extends inverse_macros.UseGlobal {
  import global._
  var inverseMacrosEngine: Tree = q"new inverse_macros.inverseMacroEngine()"
  val mockInverseMacrosEngine: Tree = q"new debug.inverseMacroEngine()"
}
