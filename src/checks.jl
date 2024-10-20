module Checks

using Markdown
using InteractiveUtils
using CodeTracking

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
priority(::ExistenceCheck) = -95

@static if VERSION >= v"1.11"
    function check(::PublicHaveDoc, doc::DocContext)
        isdefined(doc.binding.mod, doc.binding.var) || return
        if isnothing(doc.parsed) && Base.ispublic(doc.binding.mod, doc.binding.var)
            ShortIssue(:error, "Should be documented")
        end
    end
end

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

priority(::NonEmpty) = -90
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
                :warning, "Should mention that '$errtype' may be thrown"))
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
        map(first, decls[2:end])
    end
    allcode = map(c -> c.code,
                  filter(c -> isempty(c.language),
                         mdfindall(Markdown.Code, doc.parsed)))
    issues = ShortIssue[]
    for arg in argnames
        if arg ∉ allcode
            push!(issues, ShortIssue(:warning, "The argument '$arg' should be mentioned"))
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
            push!(issues, ShortIssue(:warning, "The keyword argument '$kwarg' should be mentioned"))
        end
    end
    issues
end

struct SignatureMatches <: ContentsCheck end

applicable(::SignatureMatches, kind::Symbol) =
    kind ∈ (:function, :macro)

function check(::SignatureMatches, doc::DocContext{Markdown.MD})
    haskey(doc.data, :binding) || return
    fname = String(doc.data[:binding].var)
    firstcode = if isempty(doc.parsed.content)
    elseif first(doc.parsed.content) isa Markdown.Code
        first(doc.parsed.content).code
    elseif (nested = first(doc.parsed.content)) isa Markdown.MD &&
        !isempty(nested.content) && first(nested.content) isa Markdown.Code
        first(nested.content).code
    end
    if isnothing(firstcode)
        ShortIssue(:error, "The call signature should be given at the start")
    elseif !startswith(firstcode, ifelse(doc.kind === :function, fname * '(', fname))
        Core.eval(Main, :(parsed = $(doc.parsed)))
        ShortIssue(:warning, "The initial call signature appears to be about '$(first(split(firstcode)))'")
    end
end

abstract type WritingStyleCheck <: AbstractCheck end

struct VerbVoice <: WritingStyleCheck end

struct RogueSpaces <: WritingStyleCheck end

struct ProperNouns <: WritingStyleCheck end

struct SentencePunctuation <: WritingStyleCheck end

struct SectionCapitalization <: WritingStyleCheck end

abstract type StructureCheck <: AbstractCheck end

struct SummaryLine <: StructureCheck end

struct SummaryBrief <: StructureCheck end

struct SummarySentence <: StructureCheck end

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
        ShortIssue(:suggestion, "It is nice to document the arguments in the order they are given.")
    end
end

end
