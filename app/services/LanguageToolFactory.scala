package services

import java.io.File

import model.{CheckQuery, PatternRule, RuleMatch}
import org.languagetool.{ResultCache, UserConfig, Language, Languages, JLanguageTool}
import org.languagetool.rules.patterns.{PatternRule => LTPatternRule}
import org.languagetool.rules.spelling.morfologik.suggestions_ordering.SuggestionsOrdererConfig

import collection.JavaConverters._

trait IValidator {
  def check(query: CheckQuery): List[RuleMatch]
  def getRules: List[PatternRule]
}

trait IValidatorFactory {
  def createInstance(): IValidator
  def getName(): String
}

class LanguageToolFactory(name: String, maybeLanguageModelDir: Option[File] = None) extends IValidatorFactory {
  val cache: ResultCache = new ResultCache(10000)
  val userConfig: UserConfig = new UserConfig()

  def createInstance(): IValidator = {
    val language: Language = Languages.getLanguageForShortCode("en-GB")

    maybeLanguageModelDir.foreach { languageModel =>
      SuggestionsOrdererConfig.setNgramsPath(languageModel.toString)
    }

    val instance = new JLanguageTool(language, cache, userConfig)
    maybeLanguageModelDir.foreach(instance.activateLanguageModelRules)
    new LanguageTool(instance)
  }

  def getName = name
}

class LanguageTool(underlying: JLanguageTool) extends IValidator {
  def check(query: CheckQuery): List[RuleMatch] = {
    println(s"Running check on thread -- #${Thread.currentThread().getId} ${Thread.currentThread().getName}")
    underlying.check(query.text).asScala.toList.map(RuleMatch.fromLT)
  }

  def getRules: List[PatternRule] = {
    underlying.getAllActiveRules.asScala.toList.flatMap(_ match {
      case patternRule: LTPatternRule => Some(PatternRule.fromLT(patternRule))
      case _ => None
    })
  }
}