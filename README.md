# Inverse Macros

This framework is an extension of Scala 2.11.7 / 2.12.0-M3, which enables inverse macros.
This repository includes the compiler plugin, the rewriting engine, sample inverse macros,
and benchmarks.

***This framework is developed only for academic studies.***
***We want you to re-implement for industrial uses.***

## What is inverse macros?

The inverse macro system is a new kind of typed syntactic macro systems.
It has three characteristics:

1. When expanding macros, the inverse macro system captures not only trees of names and arguments, but also its ***continuation*** trees.
2. To embed macros, the inverse macro system uses **type annotations** instead of syntactic elements.
3. The inverse macro system is a **less destructive** extension.
   For example, its rewriting is limited to a block and the syntax and the type system are almost the same to the pure Scala.

### Expressiveness

The inverse macro system enables many side-effectful DSLs.

1. lazy
2. shift/reset
3. monads
4. applicatives
5. many potential applications (staging?, inlining?, ...)

Since inverse macros are macros, providing variants is also much easier than developing compiler plugin.

In addition, if you use runtime exception as a side-channel system,
you can also implement inter-procedural variants of the above.
The inverse macro system also enables implementing shift/reset.
Almost all source codes using CPS plugin are also compilable using shift/reset implemented with an inverse macro only with a small modification.[^1]
The power of inverse macros is quite limited, but it has enough expressiveness.

[^1]: As mentioned in the section of limitations, shift/reset implemented with an inverse macro is worse than CPS plugin in points of type and performance.

### Limitations

1. The inverse macro system does not provide a side-channel system itself, but you can use runtime exceptions instead.
   However, runtime exception is a slow language construct on popular VMs.
   The implementation strategy, which is using runtime exception heavily, severely affects the performance.
   Inverse macros have many potential applications, but most of those are not effective.

2. The inverse macro system does not provide a view from outside of the methods.
   This limitation makes type checking / inference system poorer.
   For example, shift/reset implemented with an inverse macro check and infer types less precisely than that of CPS plugin.

3. In general, macros are very dangerous. Inverse macros are also very dangerous!

## How to use inverse macros?

If you use SBT, you can enables the inverse macro system easily.

### If you want to develop a library using inverse macros

```
scalaVersion := "2.11.7"     // for Scala 2.11.7
// scalaVersion := "2.12.0-M3"  // if you use Scala 2.12.0-M3
resolvers += "hiroshi-cl" at "https://hiroshi-cl.github.io/sbt-repos/",
addCompilerPlugin("jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" %% "core_plugin" % "3.0.0-SNAPSHOT")
libraryDependencies += "jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" %% "core_engine" % "3.0.0-SNAPSHOT"
```

### If you want to use a library using inverse macros / If you want to try sample inverse macros

```
scalaVersion := "2.11.7"     // for Scala 2.11.7
// scalaVersion := "2.12.0-M3"  // if you use Scala 2.12.0-M3
resolvers += "hiroshi-cl" at "https://hiroshi-cl.github.io/sbt-repos/",
addCompilerPlugin("jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" %% "core_plugin" % "3.0.0-SNAPSHOT")
libraryDependencies += "jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" %% "core_engine" % "3.0.0-SNAPSHOT"
libraryDependencies += <a library you want to use>
```

### Sample DSLs

You can also try easily sample DSLs implemented as inverse macros using SBT.
You can see how to use these DSLs in this repository.

#### General-purpose DSLs

1. shift/reset: `"jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" %% "dsl_continuations" % "3.0.0-SNAPSHOT"`
2. lazy: `"jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" %% "dsl_lazys" % "3.0.0-SNAPSHOT"`
3. monad in direct style (like Do notaion): `"jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" %% "dsl_monads" % "3.0.0-SNAPSHOT"`
4. concurrent DSLs (CPS + lazy, like MulitLisp's future): `"jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" %% "dsl_concurrent" % "3.0.0-SNAPSHOT"`
5. applicative in direct style (like applicative Do notation): `"jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" %% "dsl_applicatives" % "3.0.0-SNAPSHOT"`

#### Ad-hoc DSLs (Toy DSLs)

1. auto resource management: `"jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" %% "dsl_arm" % "3.0.0-SNAPSHOT"`
2. forking in while expression: `"jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" %% "dsl_forkwhile" % "3.0.0-SNAPSHOT"`
3. skipping operator: `"jp.ac.u_tokyo.i.ci.csg.hiroshi_yamaguchi" %% "dsl_skipto" % "3.0.0-SNAPSHOT"`

## Publications

1. Hiroshi Yamaguchi and Shigeru Chiba. 2015. Inverse Macro in Scala. In *Proceedings of the 2015 ACM SIGPLAN International Conference on Generative Programming: Concept and Experiences*, pages 85--94.
   * Full paper; Acceptance ratio (including short paper): 21 (full 15 + short 6) / 53 = 39.62%
   * Source code snapshot:
     [compiler plugin](https://github.com/hiroshi-cl/InverseMacros-plugin/),
     [rewrting engine, sample inverse macros, experiment](https://github.com/hiroshi-cl/InverseMacros/tree/gpce2015)
2. (In progress; not included in this public repository)

## Old Repositories

* Compiler plugin: https://github.com/hiroshi-cl/InverseMacros-plugin
* Rewriting engine, sample inverse macros, benchmarks: https://github.com/hiroshi-cl/InverseMacros