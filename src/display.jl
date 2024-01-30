const SEVERITY_COLORS =
    Dict(1 => :red,        # Error
         2 => :yellow,     # Warning
         3 => :light_blue, # Suggestion
         4 => :green)      # Tip

const SEVERITY_LABELS =
    Dict(1 => "Error",
         2 => "Warning",
         3 => "Suggestion",
         4 => "Tip")

function Base.show(io::IO, ::MIME"text/plain", report::DocsReport)
    printstyled(io, " ◀▶ Report on ", bold=true)
    printstyled(io, length(report.checked), bold=true, color=:light_green)
    printstyled(io, ifelse(length(report.checked) == 1,
                       " symbol from ",
                       " symbols from "), bold=true)
    printstyled(io, string(report.mod), bold=true, color=:light_blue)
    printstyled(io, " ━ found ", bold=true)
    let totalissues = mapreduce(length ∘ last, +, report.issues, init=0)
        printstyled(io, totalissues, bold=true, color=:light_red)
        printstyled(io, " possible", ifelse(totalissues == 1, " issue", " issues"), bold=true)
    end
    print(io, '\n', '═'^70)
    if !isnothing(report.maxlevel)
        printstyled(io, " (severity ≥ $(report.maxlevel))", color=:light_black)
    end
    print('\n')
    for (label, issues) in report.issues
        print(io, ifelse(isempty(issues), '╶', '╭'), "─● ")
        printstyled(io, String(label), bold=true, color=:light_cyan)
        print(io, '\n')
        for (i, issue) in enumerate(sort(issues, by=i->i.severity))
            color = get(SEVERITY_COLORS, issue.severity, :light_black)
            label = get(SEVERITY_LABELS, issue.severity, "Unknown")
            print(io, "│ ")
            printstyled(io, "┌ "; color)
            printstyled(io, "$i $label: "; color, bold=true)
            printstyled(io, String(issue.label); color)
            print(io, "\n│ ")
            printstyled(io, "│ "; color)
            print(io, issue.issue)
            print(io, ifelse(i == length(issues), "\n╰ ", "\n│ "))
            printstyled(io, "└"; color)
            if !isnothing(issue.source)
                printstyled(" @ $(issue.source.file):$(issue.source.line)",
                            color=:light_black)
            else
                printstyled(" @ unknown", color=:light_black)
            end
            print(io, '\n')
        end
    end
end
