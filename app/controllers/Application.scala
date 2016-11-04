package controllers

import java.nio.file._
import javax.inject.Inject

import it.unich.jandom._
import models._
import play.api.Environment
import play.api.data.Form
import play.api.data.Forms._
import play.api.i18n.MessagesApi
import play.api.mvc._

class Application @Inject() (implicit val messages: MessagesApi, env: Environment)  extends Controller  {

  val menuItemCONST = Map[String, Int] ("INDEX"       -> 0,
                                        "CREDITS"     -> 1,
                                        "SINTAX_R"    -> 2,
                                        "TUTORIAL"    -> 3,
                                        "ANALYZE_R"   -> 4,
                                        "ANALIZE_FAST"-> 5,
                                        "ANALYZE_BC"  -> 6)

  val domain = ui.OutputInterface.getNumericalDomains.zipWithIndex.toSeq
  val objDom = ui.OutputInterface.getObjectDomains.zipWithIndex
  val widSco = ui.OutputInterface.getWideningStrategies.zipWithIndex
  val narStr = ui.OutputInterface.getNarrowingStrategies.zipWithIndex

  def index = Action {
    Ok(views.html.home("Jandom Homepage", "Description", "", menuItemCONST("INDEX")))
  }

  def credits = Action {
    Ok(views.html.credits(menuItemCONST("CREDITS")))
  }

  def syntaxR = Action {
    Ok(views.html.syntaxR(menuItemCONST("SINTAX_R")))
  }

  def tutorial = Action {
    Ok(views.html.tutorial(menuItemCONST("TUTORIAL")))
  }

  val parametersForm = Form {
    mapping(
      "code" -> text,
      "parData.Domain" -> number,
      "parData.Object Domain" -> number,
      "parData.Widening Scope" -> number,
      "parData.Narrowing Strategy" -> number,
      "parData.Delay" -> number
    )(ParameterData.apply)(ParameterData.unapply)
  }

  def analyzeR = Action {
    Ok(views.html.showFormRandom("", parametersForm, domain, objDom, widSco, narStr)(menuItemCONST("ANALYZE_R")))

  }

  def resultR = Action { implicit request =>
    parametersForm.bindFromRequest.fold(
      formWithErrors => {
        // binding failure, you retrieve the form containing errors:
        BadRequest(views.html.index("error", formWithErrors.toString))
      },
      randomData => {
        /* binding success, you get the actual value. */
        val nR = models.ParameterData(randomData.code, randomData.domain, randomData.objDomain, randomData.widScope, randomData.narrStrategy, randomData.delay)
        val debug = false
        val res = ui.OutputInterface.analyzeRandomStr(nR.code, nR.domain, nR.widScope, nR.narrStrategy, nR.delay, debug)
        Ok(views.html.resultR(nR.code, res)(menuItemCONST("ANALYZE_R")))
      }
    )
  }

  def analyzeFast = Action {
    Ok(views.html.showFormFast("", parametersForm, domain, objDom, widSco, narStr)(menuItemCONST("ANALIZE_FAST")))
  }

  def resultFast = Action { implicit request =>
    parametersForm.bindFromRequest.fold(
      formWithErrors => {
        // binding failure, you retrieve the form containing errors:
        BadRequest(views.html.index("error", formWithErrors.toString))
      },
      fastData => {
        /* binding success, you get the actual value. */
        val nR = models.ParameterData(fastData.code, fastData.domain, fastData.objDomain, fastData.widScope, fastData.narrStrategy, fastData.delay)
        val debug = false
        val (grafo, res) = ui.OutputInterface.analyzeFastModelStr(nR.code, nR.domain, nR.widScope, nR.narrStrategy, nR.delay, debug)
        //val grafo = "digraph { q1 -> q2 [label = t1]; q2 -> q3 [label = t2]; q2 -> q3 [label = t3]; q3 -> q1 [label = t4]; q1 -> q4 [label = t5]; }"
        Ok(views.html.resultFast(nR.code, res.toString, grafo.toString)(menuItemCONST("ANALIZE_FAST")))
      }
    )
  }

  val parametersBytecode = Form {
    mapping(
      "classes"      -> text,
      "method"       -> text,
      "irType"       -> number,
      "analysisType" -> number,
      "parData"      -> mapping(
        "code"               -> text,
        "Domain"             -> number,
        "Object Domain"      -> number,
        "Widening Scope"     -> number,
        "Narrowing Strategy" -> number,
        "Delay"              -> number
      )(ParameterData.apply)(ParameterData.unapply)
    )(ParameterBytecodeData.apply)(ParameterBytecodeData.unapply)
  }

  val exampleDir = env.rootPath.toPath.resolve("public").resolve("example").resolve("java")
  var jarFile = exampleDir
  var filename = ""
  var classSoot = Seq[String]()
  var methodsSoot = Seq[Seq[String]]()
  val irType = Seq ("Baf", "Jimple")
  val analysisType = Seq("Numerical", "Object")

  def getKlassAndMethods (dir: Path) : (Seq[String], Seq[Seq[String]]) = {
    var methods = new scala.collection.mutable.ListBuffer[Seq[String]]()
    val scene = soot.Scene.v()
    val classes = ui.OutputInterface.getClasses(dir)
    for (klass <- (classes.zipWithIndex)){
      methods += (ui.OutputInterface.getMethods(dir, klass._2))
    }
    (classes, methods.toSeq)
  }


  def analyzeBytecode = Action {
    Ok(views.html.showFormBytecode("", parametersForm, domain, objDom, widSco, narStr)(menuItemCONST("ANALYZE_BC")))
  }

  def updateFileSoot = Action(parse.multipartFormData) { request =>
    val uploadedJar = request.body.file("fileJar") filter ( _.ref.file.length() != 0 )
    // Play 2.5.x has a bug which requires a convoluted approach for checking
    // if a file has been uploaded: https://github.com/playframework/playframework/issues/6203
    uploadedJar.map { uploadedJar =>
      filename = uploadedJar.filename
      jarFile = uploadedJar.ref.file.toPath
      val (classSoot1, methodsSoot1) = getKlassAndMethods(jarFile)
      classSoot = classSoot1
      methodsSoot = methodsSoot1

      val methodsSeq = methodsSoot.map(a => a.zipWithIndex).zipWithIndex

      Ok(views.html.showFormBytecodeKM(filename, parametersForm, domain, objDom, widSco, narStr, parametersBytecode,
        classSoot.zipWithIndex, methodsSeq, irType.zipWithIndex, analysisType.zipWithIndex)(menuItemCONST("ANALYZE_BC")))

    }.getOrElse {
      filename = request.body.asFormUrlEncoded.get("isExample").head(0)
      if (filename != "-1"){
        Ok(views.html.home(filename, "Description", "", menuItemCONST("INDEX")))
        jarFile = exampleDir.resolve(filename)
        val (classSoot1, methodsSoot1) = getKlassAndMethods(jarFile)
        classSoot = classSoot1
        methodsSoot = methodsSoot1
        val methodsSeq = methodsSoot.map(a => a.zipWithIndex).zipWithIndex
        Ok(views.html.showFormBytecodeKM(filename, parametersForm, domain, objDom, widSco, narStr, parametersBytecode,
          classSoot.zipWithIndex, methodsSeq, irType.zipWithIndex, analysisType.zipWithIndex)(menuItemCONST("ANALYZE_BC")))
      }
      else {
        Redirect(routes.Application.index).flashing("error" -> "Missing file")
      }
    }
  }

  def showBytecode = Action { implicit request =>
    parametersBytecode.bindFromRequest.fold(
      formWithErrors => {
        // binding failure, you retrieve the form containing errors:
        BadRequest(views.html.index("error1", formWithErrors.toString))
      },
      bcData => {
        /* binding success, you get the actual value. */
        val nB = models.ParameterBytecodeData (bcData.classes, bcData.method, bcData.irType, bcData.analysisType, bcData.parData)
        val isBaf = if (nB.irType == 0) true else false
        val methodsSeq = methodsSoot.map(a => a.zipWithIndex).zipWithIndex
        val textCode = ui.OutputInterface.getSootAbstraction(jarFile, nB.classes.toInt, nB.method.toInt, isBaf)
        Ok(views.html.showFormBytecodeCode(filename, parametersForm, domain, objDom, widSco, narStr, parametersBytecode,
          classSoot.zipWithIndex, methodsSeq, irType.zipWithIndex, analysisType.zipWithIndex, textCode, nB)(menuItemCONST("ANALYZE_BC")))
      }
    )
  }

  def resultBytecode = Action { implicit request =>
    parametersBytecode.bindFromRequest.fold(
      formWithErrors => {
        // binding failure, you retrieve the form containing errors:
        BadRequest(views.html.index("error1", formWithErrors.toString))
      },
      success = bcData => {
        /* binding success, you get the actual value. */
        val nB = models.ParameterBytecodeData(bcData.classes, bcData.method, bcData.irType, bcData.analysisType, bcData.parData)
        val klass = nB.classes.toInt
        val method = nB.method.toInt
        val isBaf = if (nB.irType == 0) true else false
        val isNum = if (nB.analysisType == 0) true else false
        val debug = false
        val methodsSeq = methodsSoot.map(a => a.zipWithIndex).zipWithIndex
        val textCode = ui.OutputInterface.getSootAbstraction(jarFile, nB.classes.toInt, nB.method.toInt, isBaf)

        val origine = request.body.asFormUrlEncoded.get("origine")
        if (origine.head == "0"){
          val res = ui.OutputInterface.analyze(jarFile, klass, method, isNum, isBaf, nB.parData.domain,
            nB.parData.widScope, nB.parData.narrStrategy, nB.parData.delay, debug)

          Ok(views.html.resultBytecode(nB.parData.code, res)(menuItemCONST("ANALYZE_BC")))
        }
        else {
          Ok(views.html.showFormBytecodeCode(filename, parametersForm, domain, objDom, widSco, narStr, parametersBytecode,
            classSoot.zipWithIndex, methodsSeq, irType.zipWithIndex, analysisType.zipWithIndex, textCode, nB)(menuItemCONST("ANALYZE_BC")))
        }
      }
    )
  }
}
