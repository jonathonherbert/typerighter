package services

import java.io.File

import model.RuleMatch
import org.languagetool._
import org.languagetool.rules.spelling.morfologik.suggestions_ordering.SuggestionsOrdererConfig

import collection.JavaConverters._

object LanguageTool {
  def createInstance(maybeLanguageModelDir: Option[File]): LanguageTool = {
    val language: Language = Languages.getLanguageForShortCode("en-GB")
    val cache: ResultCache = new ResultCache(10000)
    val userConfig: UserConfig = new UserConfig()

    maybeLanguageModelDir.foreach { languageModel =>
      SuggestionsOrdererConfig.setNgramsPath(languageModel.toString)
    }

    val instance = new JLanguageTool(language, cache, userConfig)
    maybeLanguageModelDir.foreach(instance.activateLanguageModelRules)
    new LanguageTool(instance)
  }
}

class LanguageTool(underlying: JLanguageTool) {
  def check(text: String): Seq[RuleMatch] = {
    underlying.check(text).asScala.map(RuleMatch.fromLT)
  }
}