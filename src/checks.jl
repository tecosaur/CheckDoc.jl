module Checks

module Internals

using Markdown
using InteractiveUtils
using CodeTracking
using StyledStrings: @styled_str as @S_str, AnnotatedString

using ...CheckDoc
using CheckDoc: AbstractCheck, DocContext, ShortIssue, mdfindall, extract_signature, docfunction, astsearch
import CheckDoc: check, applicable, priority, terminal, label, explanation

abstract type ExistenceCheck <: AbstractCheck end

struct AllHaveDoc <: ExistenceCheck end
struct ExportsHaveDoc <: ExistenceCheck end
struct PublicHaveDoc <: ExistenceCheck end
struct ModulesHaveDoc <: ExistenceCheck end
struct FunctionsHaveDoc <: ExistenceCheck end
struct MacrosHaveDoc <: ExistenceCheck end
struct TypesHaveDoc <: ExistenceCheck end
struct VariablesHaveDoc <: ExistenceCheck end

applicable(::AllHaveDoc, ::Symbol) = true
applicable(::ExportsHaveDoc, ::Symbol) = true
applicable(::PublicHaveDoc, ::Symbol) = true
applicable(::ModulesHaveDoc, kind::Symbol) = kind === :module
applicable(::FunctionsHaveDoc, kind::Symbol) = kind === :function
applicable(::MacrosHaveDoc, kind::Symbol) = kind === :macro
applicable(::TypesHaveDoc, kind::Symbol) = kind === :type
applicable(::VariablesHaveDoc, kind::Symbol) = kind === :variable

function check(::ExistenceCheck, doc::DocContext)
    if isnothing(doc.parsed)
        ShortIssue(:error, "Should be documented")
    end
end
terminal(::ExistenceCheck) = true

function check(::ExportsHaveDoc, doc::DocContext)
    isdefined(doc.binding.mod, doc.binding.var) || return
    getglobal(doc.binding.mod, doc.binding.var) isa Module && return
    if isnothing(doc.parsed) && Base.isexported(doc.binding.mod, doc.binding.var) &&
        !(!isnothing(doc.alias) && Base.isexported(doc.alias.mod, doc.alias.var))
        ShortIssue(:error, "Should be documented")
    end
end

@static if VERSION >= v"1.11"
    function check(::PublicHaveDoc, doc::DocContext)
        strictpublic(mod::Module, var::Symbol) =
            Base.ispublic(mod, var) && !Base.isexported(mod, var)
        isdefined(doc.binding.mod, doc.binding.var) || return
        getglobal(doc.binding.mod, doc.binding.var) isa Module && return
        if isnothing(doc.parsed) && strictpublic(doc.binding.mod, doc.binding.var) &&
            !(!isnothing(doc.alias) && strictpublic(doc.alias.mod, doc.alias.var))
            ShortIssue(:error, "Should be documented")
        end
    end
end

priority(::ExportsHaveDoc) = -96
priority(::PublicHaveDoc) = -95
priority(::FunctionsHaveDoc) = -94
priority(::MacrosHaveDoc) = -93
priority(::TypesHaveDoc) = -92
priority(::VariablesHaveDoc) = -91
priority(::ExistenceCheck) = -90

explanation(::ExportsHaveDoc) =
    S"Exported functions, types, and variables should have documentation strings. \
      This helps users understand how to use them and what they do."
explanation(::PublicHaveDoc) =
    S"Public functions, types, and variables should have documentation strings. \
      This helps users understand how to use them and what they do."
explanation(::ExistenceCheck) =
    S"Docstrings explaining purpose and usage are helpful regardless of whether \
      they're user-facing or internal."

abstract type ContentsCheck <: AbstractCheck end

"""
    NonEmpty <: ContentsCheck

Check that the documentation string actually has content.
"""
struct NonEmpty <: ContentsCheck end

function check(::NonEmpty, doc::DocContext)
    if !isnothing(doc.parsed) && all(isspace, doc.raw)
        ShortIssue(:error, "The documentation string should not be empty")
    end
end

priority(::NonEmpty) = -80
terminal(::NonEmpty) = true

struct MentionsPotentialErrors <: ContentsCheck end

applicable(::MentionsPotentialErrors, kind::Symbol) =
    kind === :function

function check(::MentionsPotentialErrors, doc::DocContext{Markdown.MD})
    dmethod = docfunction(doc.data)
    isnothing(dmethod) && return
    ast = definition(dmethod)
    isnothing(ast) && return
    thrown = astsearch(e -> Meta.isexpr(e, :call, 2) && first(e.args) == :throw, ast)
    isempty(thrown) && return
    errstrings = []
    for expr in thrown
        if Meta.isexpr(expr.args[2], :call, 2)
            err = first(expr.args[2].args)
            if Meta.isexpr(err, :curly)
                push!(errstrings, String(first(err.args)))
            elseif err isa Symbol
                push!(errstrings, String(err))
            end
        end
    end
    allcode = map(c -> c.code,
                  filter(c -> isempty(c.language),
                         mdfindall(Markdown.Code, doc.parsed)))
    issues = ShortIssue[]
    for errtype in unique(errstrings)
        if !any(c -> startswith(errtype, c), allcode)
            push!(issues, ShortIssue(
                :warning, S"Should mention that {warning:$errtype} may be thrown"))
        end
    end
    issues
end

struct IncludesAllArgs <: ContentsCheck end

applicable(::IncludesAllArgs, kind::Symbol) =
    kind === :function

function check(::IncludesAllArgs, doc::DocContext{Markdown.MD})
    siginfo = extract_signature(doc.parsed)
    argnames = if !isnothing(siginfo)
        map(String, map(first, siginfo.args) |> Iterators.flatten)
    else
        dmethod = docfunction(doc.data)
        !isnothing(dmethod) || return
        _, decls, _, _ = Base.arg_decl_parts(dmethod)
        map(d -> if !isempty(first(d))
                first(d)
            elseif startswith(last(d), "Type{")
                last(d)[ncodeunits("Type{")+1:end-1]
            else
                ""
            end,
            decls[2:end])
    end
    allcode = map(c -> c.code,
                  filter(c -> isempty(c.language),
                         mdfindall(Markdown.Code, doc.parsed)))
    issues = ShortIssue[]
    for arg in argnames
        if arg ∉ allcode
            push!(issues, ShortIssue(:warning, S"The argument {code,emphasis:$arg} should be mentioned"))
        end
    end
    issues
end

explanation(::IncludesAllArgs) =
    S"What's obvious naming to one person can be opaque to another. \
      It's often worth explicitly mentioning all arguments in the docstring."

struct IncludesAllKwargs <: ContentsCheck end

applicable(::IncludesAllKwargs, kind::Symbol) =
    kind === :function

function check(::IncludesAllKwargs, doc::DocContext{Markdown.MD})
    siginfo = extract_signature(doc.parsed)
    kwargnames = if !isnothing(siginfo)
        map(String, map(first, siginfo.kwargs))
    else
        dmethod = docfunction(doc.data)
        !isnothing(dmethod) || return
        map(String, Base.kwarg_decl(dmethod))
    end
    allcode = map(c -> c.code,
                  filter(c -> isempty(c.language),
                         mdfindall(Markdown.Code, doc.parsed)))
    issues = ShortIssue[]
    for kwarg in kwargnames
        if kwarg ∉ allcode
            push!(issues, ShortIssue(:warning, S"The keyword argument {code,emphasis:$kwarg} should be mentioned"))
        end
    end
    issues
end

explanation(::IncludesAllKwargs) =
    S"What's obvious naming to one person can be opaque to another. \
      It's worth explicitly mentioning all keyword arguments in the docstring."

struct SignatureFirst <: ContentsCheck end

function check(::SignatureFirst, doc::DocContext{Markdown.MD})
    isempty(doc.parsed.content) && return
    first(doc.parsed.content) isa Markdown.Code && return
    nested = first(doc.parsed.content)
    nested isa Markdown.MD && !isempty(nested.content) &&
        first(nested.content) isa Markdown.Code && return
    ShortIssue(:error, "The call signature should be given at the start")
end

explanation(::SignatureFirst) =
    S"Putting the call signature at the start of the docstring is a \
      firmly entrenched convention in Julia. It makes it easier to \
      quickly understand what a function does and how to use it."

struct SignatureMatches <: ContentsCheck end

applicable(::SignatureMatches, kind::Symbol) =
    kind ∈ (:function, :macro)

function check(::SignatureMatches, doc::DocContext{Markdown.MD})
    firstcode = if isempty(doc.parsed.content)
    elseif first(doc.parsed.content) isa Markdown.Code
        first(doc.parsed.content).code
    elseif (nested = first(doc.parsed.content)) isa Markdown.MD &&
        !isempty(nested.content) && first(nested.content) isa Markdown.Code
        first(nested.content).code
    end
    isnothing(firstcode) && return
    fname = String(doc.binding.var)
    fprefix = ifelse(doc.kind === :function, fname * '(', fname)
    startswith(firstcode, fprefix) && return
    startswith(chopprefix(firstcode, String(nameof(doc.binding.mod)) * '.'), fprefix) && return
    signame = first(split(firstcode, ('(', ' ')))
    ShortIssue(:warning, S"The initial call signature appears to be about {code:$signame} not {code:$fname}")
end

explanation(::SignatureMatches) =
    S"The documented name of a function should match its name in the documented signature.
      Often due to refactoring drift, a function's name can end up out of sync with its docstring."

struct RogueSpaces <: ContentsCheck end

function check(::RogueSpaces, doc::DocContext)
    for line in eachsplit(doc.raw, '\n')
        if endswith(line, ' ')
            return ShortIssue(:warning, "Trailing spaces should be removed")
        end
    end
end

abstract type WritingStyleCheck <: AbstractCheck end

struct SummaryStyleCheck{F} <: WritingStyleCheck
    label::String
    checkfn::F
    kinds::Union{Nothing, Vector{Symbol}}
    priority::Int
    explanation::Union{Nothing, String, AnnotatedString{String}}
end

function check(ssc::SummaryStyleCheck, doc::DocContext)
    paras = mdfindall(Markdown.Paragraph, doc.parsed)
    isempty(paras) && return
    content = strip(sprint(print, Markdown.MD([first(paras)])))
    isempty(content) && return
    ssc.checkfn(content)
end

application(::SummaryStyleCheck, kind::Symbol) = isnothing(scc.kinds) || kind in ssc.kinds
label(ssc::SummaryStyleCheck) = ssc.label
priority(ssc::SummaryStyleCheck) = ssc.priority
explanation(ssc::SummaryStyleCheck) = ssc.explanation

const SUMMARY_MAX_LENGTH = 92

function brief_summary(content::AbstractString)
    if textwidth(content) > SUMMARY_MAX_LENGTH
        ShortIssue(:suggestion, S"The summary is currently {warning:$(textwidth(content))} {bold:>} {success:$SUMMARY_MAX_LENGTH} characters long")
    end
end

const SummaryBrief =
    SummaryStyleCheck("SummaryBrief", brief_summary, nothing, 5,
                      S"It is good to have a short overview of the purpose of a function/variable/etc.")

const COMMON_NONIMPERATIVE_OPENINGS = Set([
    "a", "an", "basic", "common", "current", "custom", "data",
    "default", "does", "example", "function", "generic", "handler",
    "helper", "here", "hook", "internal", "it", "main", "method",
    "module", "new", "number", "optional", "placeholder", "reference",
    "result", "same", "setup", "should", "simple", "some", "special",
    "static", "string", "that", "these", "this", "unique", "unit",
    "utility", "what", "wrapper"
])

function first_word_nonimperative(line::AbstractString)
    word = first(eachsplit(line))
    if lowercase(word) ∈ COMMON_NONIMPERATIVE_OPENINGS
        ShortIssue(:suggestion, S"The summary should not start with the non-imperative opening {emphasis:$word}")
    end
end

const SummaryStartsImperatively =
    SummaryStyleCheck("SummaryStartsImperatively", first_word_nonimperative, nothing, 11, nothing)

const COMMON_IMPERATIVE_MOOD_TRANSFORMS = Dict(
    "adds" => "add",
    "allows" => "allow",
    "appends" => "append",
    "applies" => "apply",
    "arranges" => "arrange",
    "brings" => "bring",
    "calls" => "call",
    "catches" => "catch",
    "changes" => "change",
    "checks" => "check",
    "contains" => "contain",
    "converts" => "convert",
    "creates" => "create",
    "defines" => "define",
    "destroys" => "destroy",
    "determines" => "determine",
    "disables" => "disable",
    "echoes" => "echo",
    "executes" => "execute",
    "extends" => "extend",
    "evals" => "evaluate",
    "evaluates" => "evaluate",
    "finds" => "find",
    "forces" => "force",
    "gathers" => "gather",
    "generates" => "generate",
    "goes" => "go",
    "guesses" => "guess",
    "highlights" => "highlight",
    "holds" => "hold",
    "ignores" => "ignore",
    "indents" => "indent",
    "initializes" => "initialize",
    "inserts" => "insert",
    "installs" => "install",
    "investigates" => "investigate",
    "keeps" => "keep",
    "kills" => "kill",
    "leaves" => "leave",
    "lets" => "let",
    "loads" => "load",
    "looks" => "look",
    "makes" => "make",
    "marks" => "mark",
    "moves" => "move",
    "notifies" => "notify",
    "offers" => "offer",
    "parses" => "parse",
    "performs" => "perform",
    "prepares" => "prepare",
    "prepends" => "prepend",
    "prompts" => "prompt",
    "reads" => "read",
    "raises" => "raise",
    "removes" => "remove",
    "replaces" => "replace",
    "resets" => "reset",
    "restores" => "restore",
    "returns" => "return",
    "runs" => "run",
    "saves" => "save",
    "says" => "say",
    "searches" => "search",
    "selects" => "select",
    "sets" => "set",
    "shows" => "show",
    "signifies" => "signify",
    "sorts" => "sort",
    "starts" => "start",
    "steps" => "step",
    "stores" => "store",
    "switches" => "switch",
    "tells" => "tell",
    "tests" => "test",
    "toggles" => "toggle",
    "tries" => "try",
    "turns" => "turn",
    "undoes" => "undo",
    "unloads" => "unload",
    "updates" => "update",
    "uses" => "use",
    "yanks" => "yank",
)

function all_imperative_words(line::AbstractString)
    issues = ShortIssue[]
    for word in split(line)
        wlower = lowercase(word)
        if haskey(COMMON_IMPERATIVE_MOOD_TRANSFORMS, wlower)
            push!(issues, ShortIssue(:suggestion, S"The verb {emphasis:$word} should be voiced as the imperative {emphasis:$(COMMON_IMPERATIVE_MOOD_TRANSFORMS[wlower])}"))
        end
    end
    issues
end

const SummaryImperativeMood =
    SummaryStyleCheck(
        "SummaryImperativeMood", all_imperative_words, [:function], 10,
        S"The imperative mood is recommended by the {(underline=blue),link={https://docs.julialang.org/en/v1/manual/documentation/#Writing-Documentation}:Julia manual} \
          over the indicative mood. Writing in the imperative mood forms a grammatically \
          {italic:complete} sentence with no dangling antecedent, and is often more concise and direct. \
          Think of it as describing the action of the function as a command the user gives to \
          the computer by calling the method.")

struct ParagraphCheck{F} <: WritingStyleCheck
    label::String
    checkfn::F
    kinds::Union{Nothing, Vector{Symbol}}
    priority::Int
    explanation::Union{Nothing, String, AnnotatedString{String}}
end

function check(ssc::ParagraphCheck, doc::DocContext)
    paras = mdfindall(Markdown.Paragraph, doc.parsed)
    isempty(paras) && return
    issues = ShortIssue[]
    for para in paras
        content = strip(sprint(print, Markdown.MD([para])))
        all(isspace, content) && continue
        result = ssc.checkfn(content)
        if result isa ShortIssue
            push!(issues, result)
        elseif result isa Vector{ShortIssue}
            append!(issues, result)
        end
    end
    issues
end

application(::ParagraphCheck, kind::Symbol) = isnothing(scc.kinds) || kind in ssc.kinds
label(ssc::ParagraphCheck) = ssc.label
priority(ssc::ParagraphCheck) = ssc.priority
explanation(ssc::ParagraphCheck) = ssc.explanation

const PROPER_NOUNS_LOWER = Set(["julia"])

function capitalized_proper_nouns(para::AbstractString)
    issues = ShortIssue[]
    for word in split(para)
        if word in PROPER_NOUNS_LOWER
            push!(issues, ShortIssue(:suggestion, S"The proper noun {emphasis:$word} should be capitalised"))
        end
    end
    issues
end

const ParagraphProperNouns =
    ParagraphCheck("ParagraphProperNouns", capitalized_proper_nouns, nothing, 20, nothing)

const TERMINATING_PUNCTUATION = ('.', '!', '?', ':')

function punctuation_terminated(content::AbstractString)
    if last(content) ∉ TERMINATING_PUNCTUATION
        tail = if textwidth(content) < 15
            S"{green:$content}"
        else
            S"{shadow:…}{green:$(content[thisind(content, end-14):end])}"
        end
        punctoptions = join(map(c -> S"{emphasis:$c}", TERMINATING_PUNCTUATION), ", ", ", or ")
        ShortIssue(:suggestion, S"Paragraph \"$tail\" should be terminated with a punctuation mark ($punctoptions)")
    end
end

const ParagraphTermination =
    ParagraphCheck("ParagraphTermination", punctuation_terminated, nothing, 18, nothing)

function sentence_capitalized(content::AbstractString)
    issues = ShortIssue[]
    skipnext = false
    for sentence in eachsplit(content, ". ")
        if skipnext
            skipnext = false
        elseif islowercase(first(sentence))
            head = if textwidth(sentence) < 15
                sentence
            else
                S"{green:$(sentence[1:thisind(sentence, 15)])}{shadow:…}"
            end
            push!(issues, ShortIssue(:suggestion, S"Sentence \"$head\" does not start with a capital letter"))
        end
        lastboundary = something(findlast(c -> isspace(c) || (c != '.' && ispunct(c)), sentence), 0)
        lastword = sentence[nextind(sentence, lastboundary):end]
        skipnext = length(lastword) == 1 ||
            lowercase(lastword) ∈ ("e.g", "i.e", "etc", "et al", "cf", "vs", "a.k.a", "n.b", "misc", "resp", "inc", "univ", "..")
    end
    issues
end

const SentenceCapitalization =
    ParagraphCheck("SentenceCapitalization", sentence_capitalized, nothing, 15, nothing)

struct SectionCapitalization <: WritingStyleCheck end

function check(::SectionCapitalization, doc::DocContext{Markdown.MD})
    secs = mdfindall(Markdown.Header, doc.parsed)
    issues = ShortIssue[]
    for sec in secs
        content = strip(sprint(print, Markdown.MD(sec.text)))
        if islowercase(first(content))
            head = if textwidth(content) < 15
                S"{green:$content}"
            else
                S"{green:$(content[1:thisind(content, 15)])}{shadow:…}"
            end
            push!(issues, ShortIssue(:suggestion, S"Section \"$head\" does not start with a capital letter"))
        end
    end
    issues
end

abstract type StructureCheck <: AbstractCheck end

struct HasSummary <: StructureCheck end

function check(::HasSummary, doc::DocContext{Markdown.MD})
    paras = mdfindall(Markdown.Paragraph, doc.parsed)
    if isempty(paras)
        ShortIssue(:warning, "Should have a summary line")
    end
end

priority(::HasSummary) = -20

struct QuotedSymbols <: StructureCheck end

struct ArgsInOrder <: StructureCheck end

applicable(::ArgsInOrder, kind::Symbol) =
    kind === :function

function check(::ArgsInOrder, doc::DocContext{Markdown.MD})
    sigargs = extract_signature(doc.parsed)
    !isnothing(sigargs) || return
    argnames = map(String, sigargs.argnames)
    allcode = map(c -> c.code,
                  filter(c -> isempty(c.language),
                         mdfindall(Markdown.Code, doc.parsed)))
    argorder = filter(!isnothing, indexin(argnames, allcode))
    if !issorted(argorder)
        given = join(map(a -> S"{code,emphasis,success:$a}", argnames), ", ")
        mentioned = join(map(a -> S"{code,emphasis,warning:$a}", allcode[sort(argorder)]), ", ")
        ShortIssue(:tip, S"It is nice to describe the arguments in the same order they are listed in the signature ($given rather than $mentioned).")
    end
end

priority(::ArgsInOrder) = 50

struct HasReturnType <: StructureCheck end

applicable(::HasReturnType, kind::Symbol) =
    kind === :function

function check(::HasReturnType, doc::DocContext{Markdown.MD})
    sig = extract_signature(doc.parsed)
    !isnothing(sig) || return
    if isnothing(sig.returntype)
        ShortIssue(:warning, "Should mention the return type")
    end
end

struct MaximumLineLength <: StructureCheck
    length::Int
end

function check(mll::MaximumLineLength, doc::DocContext{Markdown.MD})
    issues = ShortIssue[]
    for line in eachsplit(doc.raw, '\n')
        if textwidth(line) > mll.length
            truncline = if textwidth(line) < 20
                S"{green:$line}"
            else
                S"{green:$(line[1:thisind(line, 9)]){shadow:…}$(line[thisind(line, end-9):end])}"
            end
            push!(issues, ShortIssue(:suggestion, S"Line \"$truncline\" is {warning:$(textwidth(line))} characters long"))
        end
    end
    issues
end

explanation(mll::MaximumLineLength) =
    S"Keeping lines to a reasonable length in the docstring source makes it easier \
      to read and maintain, as we use the same programs as for code. Try to keep lines \
      under {emphasis:$(mll.length)} characters long."

priority(::MaximumLineLength) = 30

const MAX_LINE_LENGTH = 92

const MaxLineLength = MaximumLineLength(MAX_LINE_LENGTH)

end

# The public bits

@static if VERSION >= v"1.11"
    eval(Expr(:public, :HasDoc, :Style, :Structure))
end

const HasDoc =
    (All = Internals.AllHaveDoc(),
     Exports = Internals.ExportsHaveDoc(),
     Public = Internals.PublicHaveDoc(),
     Modules = Internals.ModulesHaveDoc(),
     Functions = Internals.FunctionsHaveDoc(),
     Macros = Internals.MacrosHaveDoc(),
     Types = Internals.TypesHaveDoc(),
     Variables = Internals.VariablesHaveDoc())

const Style =
    (ArgsInOrder = Internals.ArgsInOrder(),
     Summary = (IsBrief = Internals.SummaryBrief,
                ImperativeOpening = Internals.SummaryStartsImperatively,
                ImperativeMood = Internals.SummaryImperativeMood),
     Text = (ProperNouns = Internals.ParagraphProperNouns,
             Termination = Internals.ParagraphTermination,
             Capitalization = Internals.SentenceCapitalization),
     Section = (; Capitalization = Internals.SectionCapitalization()))

const Structure =
    (NonEmpty = Internals.NonEmpty(),
     MentionsPotentialErrors = Internals.MentionsPotentialErrors(),
     IncludesAllArgs = Internals.IncludesAllArgs(),
     IncludesAllKwargs = Internals.IncludesAllKwargs(),
     SignatureFirst = Internals.SignatureFirst(),
     SignatureMatches = Internals.SignatureMatches(),
     HasSummary = Internals.HasSummary(),
     HasReturnType = Internals.HasReturnType(),
     RogueSpaces = Internals.RogueSpaces(),
     MaxLineLength = Internals.MaxLineLength)

end
