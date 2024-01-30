struct LineNumber
    file::String
    line::Int
end

struct DocIssue
    label::Symbol
    severity::Int
    binding::Binding
    source::Union{LineNumber, Nothing}
    issue::AbstractString
end

struct ShortIssue
    severity::Int
    issue::AbstractString
end

const SEVERITY_CODES =
    Dict(:error => 1,
         :warning => 2,
         :suggestion => 3,
         :tip => 4)

DocIssue(label::Symbol, severity::Symbol, binding::Binding,
         source::Union{LineNumber, Nothing}, issue::AbstractString) =
             DocIssue(label, get(SEVERITY_CODES, severity, 0),
                      binding, source, issue)

ShortIssue(severity::Symbol, issue::AbstractString) =
    ShortIssue(get(SEVERITY_CODES, severity, 0), issue)

struct DocsReport
    mod::Module
    checked::Vector{Symbol}
    issues::Vector{Pair{Symbol, Vector{DocIssue}}}
    maxlevel::Union{Int, Nothing}
end
