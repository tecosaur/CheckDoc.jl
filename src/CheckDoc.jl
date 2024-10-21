module CheckDoc

using Base.Docs: Binding, DocStr, MultiDoc, getdoc, resolve
using StyledStrings
using StyledStrings: @styled_str as @S_str, Face, addface!, AnnotatedString

export checkdocs

@static if VERSION >= v"1.11"
    eval(Expr(:public, :LineNumber, :AbstractCheck, :DocContext, :ShortIssue,
              :check, :applicable, :priority, :terminal, :label, :explanation,
              :LEVEL1, :LEVEL2, :LEVEL3, :LEVEL4, :LEVEL5))
end

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

"""
    LEVEL1

The most basic level of documentation: mere existence.

It's a crying shame if there these checks fail. Please fix them and move onto
`LEVEL2`.

See also: `LEVEL2`, `LEVEL3`, `LEVEL4`, `LEVEL5`.
"""
const LEVEL1 = AbstractCheck[
    Checks.Structure.NonEmpty,
    Checks.HasDoc.Exports,
    Checks.HasDoc.Public,
]

"""
    LEVEL2

Documentation exists, and contains a signature and some textual description.

Documentation passing these qualities matches the approximate structure of a
standard Julia docstring, but is of unknown quality. It's a small step from here
to `LEVEL3`

See also: `LEVEL1`, `LEVEL3`, `LEVEL4`, `LEVEL5`.
"""
const LEVEL2 = append!(AbstractCheck[
    Checks.Structure.SignatureFirst,
    Checks.Structure.SignatureMatches,
    Checks.Structure.HasSummary,
    Checks.Style.Text.Termination,
    Checks.Style.Text.Capitalization,
    Checks.Structure.MaxLineLength,
    Checks.Structure.RogueSpaces,
], LEVEL1)

"""
    LEVEL3

Documentation that merely conforms to the [recommendations of the Julia
manual](https://docs.julialang.org/en/v1/manual/documentation/#Writing-Documentation).

Documentation passing these checks may be helpful, but could also be missing key
information.

See also: `LEVEL1`, `LEVEL2`, `LEVEL4`, `LEVEL5`.
"""
const LEVEL3 = append!(AbstractCheck[
    Checks.Style.Summary.IsBrief,
    Checks.Style.Summary.ImperativeOpening,
    Checks.Style.Summary.ImperativeMood,
], LEVEL2)

"""
    LEVEL4

Documentation exists for all major components of the module, and covers all the
basics.

See also: `LEVEL1`, `LEVEL2`, `LEVEL3`, `LEVEL5`.
"""
const LEVEL4 = append!(AbstractCheck[
    Checks.HasDoc.Macros,
    Checks.HasDoc.Functions,
    Checks.Structure.IncludesAllArgs,
    Checks.Structure.IncludesAllKwargs,
    Checks.Structure.MentionsPotentialErrors,
], LEVEL3)

"""
    LEVEL5

Thorough and well-structured documentation across the entire codebase.

See also: `LEVEL1`, `LEVEL2`, `LEVEL3`, `LEVEL4`.
"""
const LEVEL5 = append!(AbstractCheck[
    Checks.HasDoc.Types,
    Checks.HasDoc.Variables,
    Checks.Style.ArgsInOrder,
    Checks.Style.Text.ProperNouns,
    Checks.Style.Section.Capitalization,
], LEVEL4)

const DEFAULT_CHECKS = LEVEL5

end
