module CheckDoc

using Base.Docs: Binding, DocStr, MultiDoc, getdoc, resolve
using StyledStrings
using StyledStrings: @styled_str as @S_str, Face, addface!, AnnotatedString

export checkdocs

include("utils.jl")
include("checkapi.jl")
include("checks.jl")
include("run.jl")
include("report.jl")

const SEVERITY_CODES =
    Dict(:error => 1,
         :warning => 2,
         :suggestion => 3,
         :tip => 4)

const SEVERITIES =
    Dict(1 => (:checkdoc_error, "Error"),
         2 => (:checkdoc_warning, "Warning"),
         3 => (:checkdoc_suggestion, "Suggestion"),
         4 => (:checkdoc_tip, "Tip"))

const CHECKDOC_FACES = (
    :checkdoc_error => Face(inherit = :error),
    :checkdoc_warning => Face(inherit = :warning),
    :checkdoc_suggestion => Face(foreground = :bright_blue),
    :checkdoc_tip => Face(inherit = :tip)
)

__init__() = foreach(addface!, CHECKDOC_FACES)

const LEVEL1 = [
    Checks.NonEmpty(),
    Checks.PublicHaveDoc(),
    Checks.MacrosHaveDoc(),
    Checks.FunctionsHaveDoc(),
    Checks.IncludesAllArgs(),
    Checks.IncludesAllKwargs(),
    Checks.SignatureMatches(),
    Checks.MentionsPotentialErrors(),
]

const LEVEL2 = append!([
    Checks.ArgsInOrder(),
    Checks.ModulesHaveDoc(),
    Checks.TypesHaveDoc(),
    Checks.VariablesHaveDoc(),
    Checks.RogueSpaces(),
], LEVEL1)

const LEVEL3 = append!([
    Checks.VerbVoice(),
    Checks.ProperNouns(),
    Checks.SentencePunctuation(),
    Checks.SectionCapitalization(),
    Checks.SummaryLine(),
    Checks.SummaryBrief(),
    Checks.SummarySentence(),
], LEVEL2)

const DEFAULT_CHECKS = LEVEL3

end
