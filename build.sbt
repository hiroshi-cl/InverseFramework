
lazy val root = project.aggregated(".")(core, test, dsl).settings(name := "inverse framework")

lazy val core = project.aggregated("core")(core_plugin, core_engine)

lazy val test = project.aggregated("test")(
  test_engine,
  test_paradise,
  test_sandbox,
  test_owner,
  test_wild
)
lazy val test_owner = project.aggregated("test/owner")(
  test_owner_applicatives,
  test_owner_concurrent,
  test_owner_continuations,
  test_owner_lazys,
  test_owner_monads
)
lazy val test_engine              = project.imEngineTest("engine")
lazy val test_sandbox             = project.imEngineTest("sandbox")
lazy val test_owner_applicatives  = project.imEngineTest("owner_applicatives")
lazy val test_owner_concurrent    = project.imEngineTest("owner_concurrent")
lazy val test_owner_continuations = project.imEngineTest("owner_continuations")
lazy val test_owner_lazys         = project.imEngineTest("owner_lazys")
lazy val test_owner_monads        = project.imEngineTest("owner_monads")
lazy val test_wild                = project.imEngineTest("wild")
lazy val test_paradise            = project.paradiseTest("paradise")
lazy val test_paradise_mock       = project.paradiseTest("paradise_mock").shareWith(test_paradise).addOption("m")
lazy val test_paradise_none       = project.paradiseTest("paradise_none").shareWith(test_paradise).addOption("E")

lazy val dsl = project.aggregated("dsl")(
  dsl_applicatives,
  dsl_arm,
  dsl_concurrent,
  dsl_continuations,
  dsl_forkwhile,
  dsl_lazys,
  dsl_monads,
  dsl_skipto
)
lazy val dsl_applicatives  = project.dsl("applicatives")
lazy val dsl_arm           = project.dsl("arm")
lazy val dsl_concurrent    = project.dsl("concurrent")
lazy val dsl_continuations = project.dsl("continuations")
lazy val dsl_forkwhile     = project.dsl("forkwhile")
lazy val dsl_lazys         = project.dsl("lazys")
lazy val dsl_monads        = project.dsl("monads")
lazy val dsl_skipto        = project.dsl("skipto")

lazy val experiments = project.aggregated("experiments")(
  experiments_delimcc_continuations,
  experiments_delimcc_scala_util,
  experiments_monads_monads,
  experiments_monads_continuations,
  experiments_monads_scala_util,
  experiments_concurrent_applicatives,
  experiments_concurrent_concurrent
)
lazy val experiments_delimcc_scala_util = project.contExperiment("delimcc")
lazy val experiments_monads_scala_util  = project.contExperiment("monads")
lazy val experiments_delimcc_continuations   = project.imExperiment("delimcc", "continuations").dependsOn(dsl_continuations)
lazy val experiments_monads_monads           = project.imExperiment("monads", "monads").dependsOn(dsl_monads)
lazy val experiments_monads_continuations    = project.imExperiment("monads", "continuations").dependsOn(dsl_continuations)
lazy val experiments_concurrent_applicatives = project.imExperiment("concurrent", "applicatives").dependsOn(dsl_applicatives)
lazy val experiments_concurrent_concurrent   = project.imExperiment("concurrent", "concurrent").dependsOn(dsl_concurrent)