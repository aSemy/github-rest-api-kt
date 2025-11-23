import dev.adamko.githubclient.lib.utils.StringBlockBuilder
import dev.adamko.githubclient.lib.utils.buildStringBlock
import io.swagger.v3.oas.models.Operation
import io.swagger.v3.oas.models.PathItem
import io.swagger.v3.oas.models.media.*
import io.swagger.v3.parser.OpenAPIV3Parser
import io.swagger.v3.parser.core.models.SwaggerParseResult
import java.nio.file.Path
import java.nio.file.StandardOpenOption
import kotlin.io.path.*


fun main(args: Array<String>) {

  fun getArg(name: String): String {
    return args.firstOrNull { it.startsWith("$name=") }
      ?.substringAfter("$name=")
      ?: error("missing required argument '$name'")
  }

  val specFile = Path(getArg("specFile"))
  val outputDir = Path(getArg("outputDir"))

  val openApi = OpenAPIV3Parser().readContents(
    specFile.readText(),
  )

  generate(openApi, outputDir)
}

private fun generate(
  openApi: SwaggerParseResult,
  outputFile: Path
) {
  with(GenScope(outputFile, openApi)) {

    defaultHttpClient()

    buildPaths()

    buildTypes()
  }
}


private class GenScope(
  val outputDir: Path,
  val apiSpec: SwaggerParseResult,
) {

  private fun getFile(name: String): Path {
    val outputFile = outputDir.resolve(name)
    require(outputFile.absolute().normalize().parent == outputDir.absolute().normalize()) {
      "file $outputFile must be a child of the output directory: $outputFile"
    }
    outputFile.apply {
      if (!exists()) {
        writeText(
          buildStringBlock {
            line("package dev.adamko.github")
            line("")
            line("import io.ktor.client.*")
            line("import io.ktor.client.engine.cio.*")
            line("import io.ktor.client.plugins.logging.*")
            line("import io.ktor.client.plugins.resources.*")
            line("import io.ktor.client.request.*")
            line("import io.ktor.client.statement.*")
            line("import io.ktor.http.*")
            line("import io.ktor.resources.*")
            line("import kotlin.time.Instant")
            line("import kotlinx.serialization.*")
            line("import kotlinx.serialization.json.*")
            line("")
          }
        )
      }
    }
    return outputFile
  }

  fun build(
    file: String,
    content: context(GenScope) StringBlockBuilder.() -> Unit,
  ) {
    val outputFile = getFile(file)

    outputFile.bufferedWriter(options = arrayOf(StandardOpenOption.APPEND)).use { writer ->
      writer.appendLine(
        buildStringBlock {
          line()
          content()
        }
      )
    }
  }
}


context(g: GenScope)
private fun buildPaths() {

  val containerToSubContainers = mutableMapOf<String, MutableSet<String>>()
  val containerToFns = mutableMapOf<String, MutableList<String>>()
  val containerToRoutes = mutableMapOf<String, MutableMap<String, MutableList<String>>>()
  val containerToPostRequestBody = mutableMapOf<String, MutableMap<String, MutableList<String>>>()
  val containerToResponseBody = mutableMapOf<String, MutableMap<String, MutableList<String>>>()

  g.apiSpec.openAPI.paths.forEach { (path, data) ->
    data.readOperationsMap().forEach { (method, operation) ->

      val opContainer = operation.operationId.substringBeforeLast("/")

      val opContainerParent = opContainer.substringBeforeLast("/", "")
        .ifEmpty { "GitHubClient" }
      containerToSubContainers.getOrPut(opContainerParent) { mutableSetOf() }.add(opContainer)

      val opName = operation.operationId.substringAfterLast("/").toCamelCase().lowercaseFirstChar()

      containerToFns.getOrPut(opContainer) { mutableListOf() }.add(
        buildFn(
          opName = opName,
          method = MethodData(
            operation = operation,
            method = method,
            path = path,
          ),
          container = opContainer,
        )
      )

      containerToRoutes.getOrPut(opContainer) { mutableMapOf() }
        .getOrPut(opName) { mutableListOf() }
        .add(
          buildStringBlock {
            line("""@Resource("$path")""")
            block("class Route internal constructor(", ")") {
              operation.parameters?.filter { it.`in` == "path" }?.forEach { parameter ->
                val parameterName = parameter.name.toCamelCase().lowercaseFirstChar()
                if (parameterName != parameter.name) {
                  line("""@SerialName("${parameter.name}")""")
                }
                line("""val ${parameterName}: ${parameter.schema.kotlinType},""")
              }
            }
          }
        )

      val requestBody = createRequestBody(operation)
      if (requestBody != null) {
        containerToPostRequestBody.getOrPut(opContainer) { mutableMapOf() }
          .getOrPut(opName) { mutableListOf() }
          .add(requestBody)
      }
      val responseBody = createResponseBody(operation)
      if (responseBody != null) {
        containerToResponseBody.getOrPut(opContainer) { mutableMapOf() }
          .getOrPut(opName) { mutableListOf() }
          .add(responseBody)
      }
    }
  }

  fun StringBlockBuilder.addContainer(container: String) {
    val subContainers = containerToSubContainers[container]?.sorted().orEmpty()
    val routesMap = containerToRoutes[container].orEmpty()
    val requestBodies = containerToPostRequestBody[container].orEmpty()
    val responseBodies = containerToResponseBody[container].orEmpty()

    val signature = buildStringBlock {
      block("class ${container.toCamelCase()} internal constructor(", ")") {
        if (container == "GitHubClient")
          line("internal val httpClient: HttpClient = defaultHttpClient,")
        else
          line("internal val httpClient: HttpClient,")
      }
    }.trimEnd()

    if (subContainers.isEmpty() && routesMap.isEmpty()) {
      line(signature)
    } else {
      block("$signature {", "}") {

        subContainers.forEach { subContainer ->
          line("")
          line("val ${subContainer.toCamelCase().lowercaseFirstChar()}: ${subContainer.toCamelCase()} =")
          line("  ${subContainer.toCamelCase()}(httpClient)")
        }

        (routesMap.keys + requestBodies.keys + responseBodies.keys).sorted().forEach { name ->
          val rs = routesMap[name].orEmpty()
          val req = requestBodies[name].orEmpty()
          val res = responseBodies[name].orEmpty()
          if (rs.isNotEmpty() || req.isNotEmpty() || res.isNotEmpty()) {
            val objName = if (name.lowercase() == "list") {
              "Listing"
            } else {
              name.uppercaseFirstChar()
            }
            block("data object $objName {", "}") {
              rs.forEach { route ->
                line(route)
              }
              req.forEach { requestBody ->
                line(requestBody)
              }
              res.forEach { response ->
                line(response)
              }
            }
          }
        }

        subContainers.forEach {
          line("")
          addContainer(it)
        }
      }
    }
  }

  g.build("GitHubClient.kt") {
    containerToSubContainers.forEach { (container, _) ->
      addContainer(container)
    }
  }

  containerToFns.forEach { (container, fns) ->
    g.build("GitHubClient.${container.toCamelCase()}.kt") {
      fns.sorted()
        .forEach { fn ->
          line("")
          line(fn)
        }
    }
  }
}

context(g: GenScope)
private fun defaultHttpClient() {
  g.build("defaultHttpClient.kt") {
    block("internal val defaultHttpClient: HttpClient by lazy {", "}") {
      block("HttpClient(CIO) {", "}") {
        line("expectSuccess = false")
      }
    }
  }
}


private fun buildKdoc(operation: Operation): String {
  return buildString {
    if (operation.summary != null) {
      appendLine("### ${operation.summary.trim()}")
      appendLine()
    }
    if (operation.description != null) {
      appendLine(
        operation.description
          .replace(". ", ".\n")
          .replace("see \"[", "see \n\"[")
      )
      appendLine()
    }
    appendLine("`${operation.operationId}`")
  }.trimEnd()
    .lines()
    .joinToString(prefix = "/**\n", separator = "\n", postfix = "\n */") { " * $it".trimEnd() }
}


private fun String.toCamelCase(): String {
  return split { !it.isLetterOrDigit() }
    .joinToString("") { it.uppercaseFirstChar() }
}

private fun String.uppercaseFirstChar(): String =
  replaceFirstChar { it.titlecaseChar() }

private fun String.lowercaseFirstChar(): String =
  replaceFirstChar { it.lowercaseChar() }

private fun String.split(predicate: (c: Char) -> Boolean): List<String> {
  return fold(mutableListOf(StringBuilder())) { acc, c ->
    if (predicate(c)) {
      acc.add(StringBuilder())
    } else {
      acc.last().append(c)
    }
    acc
  }.map { it.toString() }
}

context(g: GenScope)
private val Schema<*>.kotlinType: String
  get() {
    return when (this) {
      is ArbitrarySchema -> "Arbitrary"
      is ArraySchema     -> "List<${items.kotlinType}>"
      is BinarySchema    -> "Binary"
      is BooleanSchema   -> "Boolean"
      is ByteArraySchema -> "ByteArray"
      is ComposedSchema  -> {
        anyOf?.firstNotNullOfOrNull { it.kotlinType }
          ?: oneOf?.firstNotNullOfOrNull { it.kotlinType }
          ?: "Composed"
      }

      is DateSchema      -> {
        "Date"
      }

      is DateTimeSchema  -> "Instant"
      is EmailSchema     -> "Email"
      is FileSchema      -> "File"
      is IntegerSchema   ->
        when (format) {
          "int64" -> "Long"
          else    -> "Int"
        }

      is JsonSchema      -> "Json"
      is MapSchema       -> {
        "JsonObject"
//        "Map<*, *>"
      }

      is NumberSchema    ->
        when (format) {
          "float" -> "Float"
          else    -> "Double"
        }

      is ObjectSchema    -> {
        "Object"
      }

      is PasswordSchema  -> "Password"
      is StringSchema    -> "String"
      is UUIDSchema      -> "Uuid"
      else               -> {
        val ref = resolveRefDeep()
        if (this == ref) {
          println("unknown type: $this")
          "Nothing??"
        } else {
          ref.kotlinType
        }
      }
    }
  }

context(g: GenScope)
private fun Schema<*>.resolveRefDeep(
  visited: MutableSet<String> = mutableSetOf()
): Schema<*> {
  val ref = this.`$ref` ?: return this
  val refName = ref.substringAfterLast('/')

  if (!visited.add(refName)) return this // avoid cycles

  val resolved = g.apiSpec.openAPI.components?.schemas?.get(refName) ?: return this
  return if (resolved.`$ref` != null) {
    resolved.resolveRefDeep(visited)
  } else {
    resolved
  }
}

context(g: GenScope)
private fun buildFn(
  container: String,
  opName: String,
  method: MethodData,
//  operation: Operation,
): String {

  val parameters =
    method.operation.parameters.orEmpty().map { parameter ->
      val parameterName = run {
        val base = parameter.name.toCamelCase().lowercaseFirstChar()
        when (base) {
          "in"      -> "input"
          "package" -> {
            if ("comma-separated list of package names" in parameter.description.orEmpty()) {
              "packages"
            } else {
              "`package`"
            }
          }

          else      -> base
        }
      }

      if (parameter.required) {
        "${parameterName}: ${parameter.schema.kotlinType},"
      } else {
        "${parameterName}: ${parameter.schema.kotlinType}? = null,"
      }
    }

  val response = method.operation.responses.orEmpty()["200"]
  val content = response?.content?.get("application/json")
  val responseType = if (content != null) {
    ": ${content.schema.kotlinType}"
  } else {
    ""
  }


  return buildStringBlock {
    line(buildKdoc(method.operation))
    block("suspend fun GitHubClient.${container.toCamelCase()}.${opName}(", ")$responseType {") {
      parameters.forEach {
        line(it)
      }
    }
    block("", "}") {
      line(buildFnBody(method))
    }
  }
}


context(g: GenScope)
private fun buildFnBody(
  method: MethodData,
): String {
  return buildStringBlock {

    val resName = method.operation.operationId.run {
      val n = substringAfterLast("/").toCamelCase()
      val objName = if (n.lowercase() == "list") {
        "Listing"
      } else {
        n.uppercaseFirstChar()
      }

      "GitHubClient." +
          substringBeforeLast("/").toCamelCase() + "." +
          objName + ".Route"
    }
    block("val resource = $resName(", ")") {
      method.operation.parameters.orEmpty().filter { it.`in` == "path" }.forEach { parameter ->
        val pn = parameter.name.toCamelCase().lowercaseFirstChar()
        line("$pn = $pn,")
      }
    }

    when (method.method) {
      PathItem.HttpMethod.POST    -> {
        block("val result = httpClient.post(resource = resource) {", "}") {
        }
        line("TODO()")
      }

      PathItem.HttpMethod.GET     -> {
        block("val result = httpClient.get(resource = resource) {", "}") {
        }
        line("TODO()")
      }

      PathItem.HttpMethod.PUT     -> {
        block("val result = httpClient.put(resource = resource) {", "}") {
        }
        line("TODO()")
      }

      PathItem.HttpMethod.PATCH   -> {
        block("val result = httpClient.patch(resource = resource) {", "}") {
        }
        line("TODO()")
      }

      PathItem.HttpMethod.DELETE  -> {
        block("val result = httpClient.delete(resource = resource) {", "}") {
        }
        line("TODO()")
      }

      PathItem.HttpMethod.HEAD    -> {
        line("TODO()")
      }

      PathItem.HttpMethod.OPTIONS -> {
        line("TODO()")
      }

      PathItem.HttpMethod.TRACE   -> {
        line("TODO()")
      }
    }
  }
}

private data class MethodData(
  val method: PathItem.HttpMethod,
  val operation: Operation,
  val path: String,
)


context(g: GenScope)
private fun createRequestBody(
  operation: Operation,
): String? {
  val requestBody = operation.requestBody ?: return null

  val content = requestBody.content?.get("application/json") ?: return null

  val clsName = "RequestBody"

  return generateKotlinDataClass(content.schema, clsName)
}

context(g: GenScope)
private fun createResponseBody(
  operation: Operation,
): String? {
  val response = operation.responses.orEmpty()["200"] ?: return null
  val content = response.content?.get("application/json") ?: return null


//  val requestBody = operation.responses ?: return null
//
//  val content = requestBody.content?.get("application/json") ?: return null

  val clsName = "ResponseBody"

  return generateKotlinDataClass(content.schema, clsName)
}

context(g: GenScope)
private fun generateKotlinDataClass(
  schema: Schema<*>,
  className: String,
): String? {
  val schema = schema.resolveRefDeep()
  if (schema !is ObjectSchema) return null

  val properties = schema.properties ?: return null
  val requiredProperties = schema.required.orEmpty()

  if (properties.isEmpty()) return null

  return buildStringBlock {
    line("@Serializable")
    block("data class $className(", ")") {

      for ((propertyName, propertySchema) in properties) {
        // Determine the Kotlin type for the property
        val kotlinType = propertySchema.kotlinType

        // Add the property to the class definition
        val isNullable = propertyName !in requiredProperties
        val valName = propertyName.toCamelCase().lowercaseFirstChar().let {
          when {
            it == "object" -> "obj"
            else           -> it
          }
        }
        if (valName != propertyName) {
          line("@SerialName(\"$propertyName\")")
        }
        line("val $valName: $kotlinType${if (isNullable) "?" else ""},")
      }
    }
  }
}

context(g: GenScope)
private fun buildTypes() {
  g.build("Types.kt") {
    line("typealias Composed = String")
    line("")
    line("typealias Object = String")
    line("")
    line("typealias Email = String")
//    line("")
//    line("typealias Date = String")
    line("")
    line("@Serializable")
    line("@JvmInline")
    line("value class Date(val date: String)")
  }
}
