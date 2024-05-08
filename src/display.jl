using StyledStrings
using StyledStrings: Face, addface!

const CHECKDOC_FACES = (
    :checkdoc_error => Face(inherit = :error),
    :checkdoc_warning => Face(inherit = :warning),
    :checkdoc_suggestion => Face(foreground = :bright_blue),
    :checkdoc_tip => Face(inherit = :tip)
)

__init__() = foreach(addface!, CHECKDOC_FACES)

const SEVERITIES =
    Dict(1 => (:checkdoc_error, "Error"),
         2 => (:checkdoc_warning, "Warning"),
         3 => (:checkdoc_suggestion, "Suggestion"),
         4 => (:checkdoc_tip, "Tip"))

function Base.show(io::IO, ::MIME"text/plain", report::DocsReport)
    totalissues = mapreduce(length ∘ last, +, report.issues, init=0)
    headerline =
        styled" {bold:{grey:◀▶} Report on {bright_green:$(length(report.checked))}\
               $(ifelse(length(report.checked) == 1, \" symbol from \", \" symbols from \"))\
               {bright_blue:$(report.mod)} ━ found {checkdoc_error:$totalissues} possible\
               $(ifelse(totalissues == 1, \" issue\", \" issues\"))}"
    print(io, headerline, '\n', styled"{grey:$('═'^textwidth(headerline))}")
    if !isnothing(report.maxlevel)
        print(io, styled" {grey:(severity ≥ $(report.maxlevel))}")
    end
    for (i, (label, issues)) in enumerate(report.issues)
        print(io, '\n', ifelse(isempty(issues), '╶', '╭'), styled"─● {bold,code:$label}")
        for (ii, issue) in enumerate(sort(issues, by=i->i.severity))
            face,label = get(SEVERITIES, issue.severity, (:bright_black, "Unknown"))
            srcloc = if isnothing(issue.source) styled"{italic:unknown}" else
                string(issue.source.file, ':', issue.source.line) end
            # TODO reflow the text of `issue.issue`.
            print(io, '\n',
                  styled"│ {$face:┌ {bold:$i.$ii $label:} $(issue.label)}\n\
                         │ {$face:│} $(issue.issue)\n$(ifelse(ii == length(issues), '╰', '│')) \
                           {$face:└} {grey:@ $srcloc}")
        end
    end
end
