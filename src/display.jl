using StyledStrings

const SEVERITY_FACES =
    Dict(1 => :red,         # Error
         2 => :yellow,      # Warning
         3 => :bright_blue, # Suggestion
         4 => :green)       # Tip

const SEVERITY_LABELS =
    Dict(1 => "Error",
         2 => "Warning",
         3 => "Suggestion",
         4 => "Tip")

function Base.show(io::IO, ::MIME"text/plain", report::DocsReport)
    totalissues = mapreduce(length ∘ last, +, report.issues, init=0)
    headerline =
        styled" {bold:◀▶ Report on {bright_green:$(length(report.checked))}\
               $(ifelse(length(report.checked) == 1, \" symbol from \", \" symbols from \"))\
               {bright_blue:$(report.mod)} ━ found {bright_red:$totalissues} possible\
               $(ifelse(totalissues == 1, \" issue\", \" issues\"))}"
    print(io, headerline, '\n', '═'^textwidth(headerline))
    if !isnothing(report.maxlevel)
        print(io, styled" {grey:(severity ≥ $(report.maxlevel))}")
    end
    for (label, issues) in report.issues
        print(io, '\n', ifelse(isempty(issues), '╶', '╭'), "─● ", styled"{bold,bright_cyan:$label}")
        for (i, issue) in enumerate(sort(issues, by=i->i.severity))
            face = get(SEVERITY_FACES, issue.severity, :light_black)
            label = get(SEVERITY_LABELS, issue.severity, "Unknown")
            srcloc = if isnothing(issue.source) "unknown" else
                string(issue.source.file, ':', issue.source.line) end
            print(io, '\n',
                  styled"│ {$face:┌ {bold:$i $label\:} $(issue.label)}\n\
                         │ {$face:│} $(issue.issue)\n$(ifelse(i == length(issues), '╰', '│')) \
                           {$face:└} {grey:@ $srcloc}")
        end
    end
end
