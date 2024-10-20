struct LineNumber
    file::String
    line::Int
end

abstract type AbstractCheck end

struct DocContext{T}
    kind::Symbol
    binding::Binding
    source::Union{LineNumber, Nothing}
    data::Dict{Symbol, Any}
    raw::String
    parsed::T
end

struct ShortIssue
    severity::Int
    message::AnnotatedString{String}
end

ShortIssue(severity::Symbol, issue::AnnotatedString{String}) =
    ShortIssue(get(SEVERITY_CODES, severity, 0), issue)

ShortIssue(severity::Symbol, issue::AnnotatedString) =
    ShortIssue(get(SEVERITY_CODES, severity, 0),
               AnnotatedString(String(issue), annotations(issue)))

ShortIssue(severity::Symbol, issue::AbstractString) =
    ShortIssue(get(SEVERITY_CODES, severity, 0), AnnotatedString(String(issue)))

"""
    check(condition::AbstractCheck, doc::DocContext{Content}) ->
        Union{Nothing, ShortIssue, Vector{ShortIssue}}

Check whether `condition` is satisfied for `doc`. Returns `nothing`
if so, one or more `ShortIssue` complaints otherwise.
"""
function check end

check(::AbstractCheck, ::DocContext) = nothing

"""
    applicable(check::AbstractCheck, kind::Symbol)

Whether `check` is relevant to a particular `kind` of object.

`kind` should be one the following symbols:
- `:module`
- `:macro`
- `:function`
- `:type`
- `:variable`

The default implementation of `applicable` assumes that `check` applies to all
kinds.
"""
function applicable end

applicable(::AbstractCheck, ::Symbol) = true

"""
    priority(check::Type{<:AbstractCheck}) -> Integer
    priority(check::AbstractCheck) -> Integer

How important it is that `check` is satisfied.

`check` can be either a type or an instance of a check. The generic
instance-based method forwards to the type-based method.

Lower values are considered more important, and the default is `0`.

This affects the order in which checks are run, with more important checks
run first. In turn, this affects when a [`terminal`](@ref) result may be hit,
and the order in which issues are displayed.
"""
function priority end

priority(::Type{<:AbstractCheck}) = 0
priority(c::AbstractCheck) = priority(typeof(c))

"""
    terminal(check::AbstractCheck, doc::DocContext) -> Bool

Whether failure of `check` should prevent further checks from being run.
"""
function terminal end

terminal(c::AbstractCheck, ::DocContext) = terminal(c)
terminal(c::AbstractCheck) = false

"""
    label(check::AbstractCheck) -> String

A human-readable label for `check`.

The generic implementation returns the name of the type.
"""
label(c::AbstractCheck) = String(nameof(typeof(c)))
