module Checks

using Markdown
using InteractiveUtils
using CodeTracking
using StyledStrings: @styled_str as @S_str

using ..CheckDoc
using CheckDoc: AbstractCheck, DocContext, ShortIssue, mdfindall, extract_signature, docfunction, astsearch
import CheckDoc: check, applicable, priority, terminal

abstract type ExistenceCheck <: AbstractCheck end

struct HasDoc <: ExistenceCheck end
struct PublicHaveDoc <: ExistenceCheck end
struct ModulesHaveDoc <: ExistenceCheck end
struct FunctionsHaveDoc <: ExistenceCheck end
struct MacrosHaveDoc <: ExistenceCheck end
struct TypesHaveDoc <: ExistenceCheck end
struct VariablesHaveDoc <: ExistenceCheck end

applicable(::HasDoc, ::Symbol) = true
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

@static if VERSION >= v"1.11"
    function check(::PublicHaveDoc, doc::DocContext)
        isdefined(doc.binding.mod, doc.binding.var) || return
        if isnothing(doc.parsed) && Base.ispublic(doc.binding.mod, doc.binding.var) &&
            !(!isnothing(doc.alias) && Base.ispublic(doc.alias.mod, doc.alias.var))
            ShortIssue(:error, "Should be documented")
        end
    end
end

priority(::Type{PublicHaveDoc}) = -96
priority(::Type{FunctionsHaveDoc}) = -95
priority(::Type{MacrosHaveDoc}) = -94
priority(::Type{TypesHaveDoc}) = -93
priority(::Type{VariablesHaveDoc}) = -92
priority(::Type{<:ExistenceCheck}) = -90

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

priority(::Type{NonEmpty}) = -80
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

struct SignatureGiven <: ContentsCheck end

function check(::SignatureGiven, doc::DocContext{Markdown.MD})
    isempty(doc.parsed.content) && return
    first(doc.parsed.content) isa Markdown.Code && return
    nested = first(doc.parsed.content)
    nested isa Markdown.MD && !isempty(nested.content) &&
        first(nested.content) isa Markdown.Code && return
    ShortIssue(:error, "The call signature should be given at the start")
end

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

struct RogueSpaces <: ContentsCheck end

function check(::RogueSpaces, doc::DocContext)
    for line in eachsplit(doc.raw, '\n')
        if endswith(line, ' ')
            return ShortIssue(:warning, "Trailing spaces should be removed")
        end
    end
end

abstract type WritingStyleCheck <: AbstractCheck end

struct SummaryVerbVoice <: WritingStyleCheck end

const COMMON_WRONG_VERB_VOICES = Dict(
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

function check(::SummaryVerbVoice, doc::DocContext{Markdown.MD})
    paras = mdfindall(Markdown.Paragraph, doc.parsed)
    isempty(paras) && return
    issues = ShortIssue[]
    content = strip(sprint(print, Markdown.MD([first(paras)])))
    for word in split(content)
        wlower = lowercase(word)
        if haskey(COMMON_WRONG_VERB_VOICES, wlower)
            push!(issues, ShortIssue(:suggestion, S"The verb {emphasis:$word} should be voiced as {emphasis:$(COMMON_WRONG_VERB_VOICES[wlower])}"))
        end
    end
    issues
end

struct ProperNouns <: WritingStyleCheck end

const PROPER_NOUNS = Set(["julia"])

function check(::ProperNouns, doc::DocContext{Markdown.MD})
    paras = mdfindall(Markdown.Paragraph, doc.parsed)
    issues = ShortIssue[]
    for para in paras
        content = strip(sprint(print, Markdown.MD([para])))
        for word in split(content)
            if word in PROPER_NOUNS
                push!(issues, ShortIssue(:suggestion, S"The proper noun {emphasis:$word} should be capitalised"))
            end
        end
    end
    issues
end

struct ParagraphTermination <: WritingStyleCheck end

function check(::ParagraphTermination, doc::DocContext{Markdown.MD})
    paras = mdfindall(Markdown.Paragraph, doc.parsed)
    issues = ShortIssue[]
    for para in paras
        content = strip(sprint(print, Markdown.MD([para])))
        if last(content) ∉ ('.', '!', '?', ':')
            tail = if textwidth(content) < 15
                S"{green:$content}"
            else
                S"{shadow:…}{green:$(content[thisind(content, end-14):end])}"
            end
            push!(issues, ShortIssue(:suggestion, S"Paragraph \"$tail\" should end with a punctuation mark"))
        end
    end
    issues
end

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

struct SentenceCapitalization <: WritingStyleCheck end

function check(::SentenceCapitalization, doc::DocContext{Markdown.MD})
    paras = mdfindall(Markdown.Paragraph, doc.parsed)
    issues = ShortIssue[]
    for para in paras
        content = strip(sprint(print, Markdown.MD([para])))
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

struct SummaryBrief <: StructureCheck end

function check(::SummaryBrief, doc::DocContext{Markdown.MD})
    paras = mdfindall(Markdown.Paragraph, doc.parsed)
    isempty(paras) && return
    firstpara = first(paras)
    paralen = textwidth(strip(sprint(print, Markdown.MD([first(paras)]))))
    if paralen > 96
        ShortIssue(:suggestion, S"The summary line should be brief, but is currently {warning:$paralen} {bold:>} {success:96} characters long")
    end
end

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

end
