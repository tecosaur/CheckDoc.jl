struct DocsReport{G1, G2}
    mod::Module
    checked::Vector{Binding}
    rules::Vector{AbstractCheck}
    issues::Vector{Pair{G1, Vector{Pair{G2, Vector{DocIssue}}}}}
    maxlevel::Union{Int, Nothing}
end

function Base.getindex(rep::DocsReport, i::Int)
    sofar = 0
    for (group, issues) in rep.issues
        if sofar + length(issues) >= i
            return issues[i - sofar]
        end
        sofar += length(issues)
    end
    throw(BoundsError(rep, i))
end

Base.firstindex(rep::DocsReport) = 1
Base.lastindex(rep::DocsReport) = sum((sum(length ∘ last, group, init=0) for (_, group) in rep.issues), init=0)
Base.length(rep::DocsReport) = lastindex(rep)

function checkdocs(mod::Module, checks::Vector{AbstractCheck} = DEFAULT_CHECKS; recurse::Bool=true, kwargs...)
    binds = bindings(mod; recurse)
    issues = runchecks(mod, binds, checks)
    igroups, maxlevel = aggregateissues(issues; kwargs...)
    DocsReport(mod, binds, checks, igroups, maxlevel)
end

function aggregateissues(issues::Vector{DocIssue}; by::Union{Symbol, Tuple{Symbol, Symbol}} = :severity, maxlevel::Union{Int, Symbol, Nothing}=nothing)
    if maxlevel isa Symbol
        maxlevel = get(SEVERITY_CODES, maxlevel, 0)
    end
    if !isnothing(maxlevel)
        filter!(i -> i.severity ≤ maxlevel, issues)
    end
    by1, by2 = if by isa Tuple{Symbol, Symbol}
        by
    else # `by isa Symbol`
        by, if by ∈ (:severity, :level)
            :issue
        elseif by ∈ (:issue, :kind, :check)
            :name
        else
            :severity
        end
    end
    groupissues(issues, groupkind(by1), groupkind(by2)), maxlevel
end

function checkdocs(binding::Binding, checks::Vector{AbstractCheck} = DEFAULT_CHECKS; kwargs...)
    issues = runchecks(binding.mod, binding, checks)
    igroups, maxlevel = aggregateissues(issues; kwargs...)
    DocsReport(binding.mod, [binding], checks, igroups, maxlevel)
end

checkdocs(mod::Module, name::Symbol, checks::Vector{AbstractCheck} = DEFAULT_CHECKS; kwargs...) =
    checkdocs(Binding(mod, name), checks; kwargs...)

checkdocs(name::Symbol, checks::Vector{AbstractCheck} = DEFAULT_CHECKS; kwargs...) =
    checkdocs(Base.binding_module(Main, name), name, checks; kwargs...)

function checkdocs(f::Function, checks::Vector{AbstractCheck} = DEFAULT_CHECKS; kwargs...)
    # This is just a guess, but it's the best that I'm aware of
    mod = argmin(m -> m.primary_world, methods(f)).module
    checkdocs(Binding(mod, nameof(f)), checks; kwargs...)
end

checkdocs(thing::Union{Binding, Symbol, Function}, checks::Vector{<:AbstractCheck}; kwargs...) =
    checkdocs(thing, Vector{AbstractCheck}(checks); kwargs...)

checkdocs(mod::Module, name::Symbol, checks::Vector{<:AbstractCheck}; kwargs...) =
    checkdocs(mod, name, Vector{AbstractCheck}(checks); kwargs...)

function groupkind(name::Symbol)
    if name ∈ (:severity, :level)
        Int
    elseif name ∈ (:name, :entity)
        Binding
    elseif name ∈ (:issue, :kind, :check)
        AbstractCheck
    else
        throw(ArgumentError("Unknown grouping key: $name"))
    end
end

function groupissues(issues::Vector{DocIssue}, gkind::Type)::Vector{Pair{gkind, Vector{DocIssue}}}
    groups = Dict{gkind, Vector{DocIssue}}()
    grouporder = Vector{gkind}()
    for issue in issues
        gkey = if gkind === Int
            issue.severity
        elseif gkind === Binding
            issue.binding
        else
            issue.check
        end
        if haskey(groups, gkey)
            push!(groups[gkey], issue)
        else
            groups[gkey] = [issue]
            push!(grouporder, gkey)
        end
    end
    if gkind === Int
        sort!(grouporder)
    elseif gkind === Binding
        sort!(grouporder, by = b -> (nameof(b.mod), b.var))
    else
        sort!(grouporder, by = priority)
    end
    [grp => groups[grp] for grp in grouporder]
end

function groupissues(issues::Vector{DocIssue}, gkind::Type, gskind::Type)::Vector{Pair{gkind, Vector{Pair{gskind, Vector{DocIssue}}}}}
    groups = groupissues(issues, gkind)
    [grp => groupissues(gissues, gskind) for (grp, gissues) in groups]
end

function Base.show(io::IO, ::MIME"text/plain", issue::DocIssue)
    sface, slabel = get(SEVERITIES, issue.severity, (:bright_black, "Unknown"))
    show(io, DocIssue)
    print(io, styled"""({$sface,bold:$slabel}): \
                         {yellow:$(label(issue.check))}({green:"$(issue.message)"})\n  \
                         binding: {code:$(issue.binding)}\n  \
                         source:  $(srcloc(issue))""")
end

function srcloc(issue::DocIssue)
    if isnothing(issue.source)
        dir = pkgdir(issue.binding.mod)
        if isnothing(dir)
            S"{shadow:{italic:@} unknown}"
        else
            S"{shadow,light:within $(contractuser(dir))}"
        end
    else
        dir = joinpath(contractuser(dirname(issue.source.file)), "")
        file = basename(issue.source.file)
        S"{shadow:{italic:@} {light:$dir}{underline:$file}:{italic:$(issue.source.line)}}"
    end
end

function Base.show(io::IO, ::MIME"text/plain", report::DocsReport{G1, G2}) where {G1, G2}
    totalissues = length(report)
    headerline =
        styled" {bold:{grey:◀▶} Report on {bright_green:$(length(report.checked))} \
               $(ifelse(length(report.checked) == 1, \"entity\", \"entities\")) \
               in {bright_blue:$(report.mod)} ━ found {checkdoc_error:$totalissues} possible\
               $(ifelse(totalissues == 1, \" issue\", \" issues\"))}"
    print(io, headerline, '\n', styled"{grey:$('═'^textwidth(headerline))}")
    if !isnothing(report.maxlevel)
        print(io, styled" {grey:(severity ≥ $(report.maxlevel))}")
    end
    icount = 0
    wrapwidth = min(96, last(displaysize(io)))
    gstyle(severity::Int, issues::Vector{DocIssue}) =
        get(SEVERITIES, severity, (:bright_black, "Unknown"))
    gstyle(bind::Binding, issues::Vector{DocIssue}) =
        :code, chopprefix(string(bind), "$(report.mod).")
    gstyle(check::CheckDoc.AbstractCheck, issues::Vector{DocIssue}) =
        (if allequal(i -> i.severity, issues)
             get(SEVERITIES, first(issues).severity, (:bright_black, "")) |> first
         else
             :default
         end,
         label(check))
    seen_explanations = Set{AbstractCheck}()
    for (group, sgroup) in report.issues
        (isempty(sgroup) || all(isempty ∘ last, sgroup)) && continue
        groupissues = collect(Iterators.map(last, sgroup) |> Iterators.flatten)
        gface, glabel = gstyle(group, groupissues)
        print(io, styled"\n{$gface:╭─● {bold:$glabel} ($(length(groupissues)))}")
        if G1 == AbstractCheck && group ∉ seen_explanations
            push!(seen_explanations, group)
            explain = explanation(subgroup)
            if !isnothing(explain)
                for line in wraplines(explain, wrapwidth - 2)
                    print(io, styled"\n{$gface:│} {italic,light:$line}")
                end
            end
        end
        for (subgroup, issues) in sgroup
            sgface, sglabel = gstyle(subgroup, issues)
            print(io, styled"\n{$gface:│} {$sgface:┌ $sglabel ($(length(issues)))}")
            gutterprefix_cc = styled"\n{$gface:│} {$sgface:┆} "
            gutterprefix_ce = styled"\n{$gface:│} {$sgface:└} "
            gutterprefix_ee = styled"\n{$gface:╰} {$sgface:└} "
            samemessage = allequal(i -> i.message, issues)
            if samemessage
                lines = wraplines(first(issues).message,
                                  wrapwidth - textwidth(gutterprefix_cc) - 2,
                                  textwidth(sglabel) + 3 + ndigits(length(issues)))
                for (i, line) in enumerate(lines)
                    if i == 1
                        print(io, styled"{$sgface::} ", line)
                    else
                        print(io, gutterprefix_cc, ' '^2, line)
                    end
                end
            end
            if G2 == AbstractCheck && group ∉ seen_explanations
                push!(seen_explanations, subgroup)
                explain = explanation(subgroup)
                if !isnothing(explain)
                    for line in wraplines(explain, wrapwidth - textwidth(gutterprefix_cc))
                        print(io, gutterprefix_cc, styled"{italic,light:$line}")
                    end
                end
            end
            for issue in issues
                icount += 1
                ssgroup = if G1 ∈ (Int, Binding) && G2 ∈ (Int, Binding)
                    issue.check
                elseif G1 === Int
                    issue.binding
                else
                    issue.severity
                end
                iface, ilabel = gstyle(ssgroup, [issue])
                print(io, if !samemessage
                          gutterprefix_cc
                      elseif issue == last(issues) && subgroup == first(last(sgroup))
                          gutterprefix_ee
                      elseif issue == last(issues)
                          gutterprefix_ce
                      else
                          gutterprefix_cc
                      end,
                      styled"{bold:$(lpad(icount, ndigits(totalissues))).} {$iface:$ilabel} $(srcloc(issue))")
                if !samemessage
                    lines = wraplines(issue.message, wrapwidth - textwidth(gutterprefix_cc) - 2 - ndigits(totalissues))
                    for (i, line) in enumerate(lines)
                        gprefix = if issue != last(issues) || i < lastindex(lines)
                            gutterprefix_cc
                        elseif subgroup == first(last(sgroup))
                            gutterprefix_ee
                        else
                            gutterprefix_ce
                        end
                        print(io, gprefix, ' '^(2+ndigits(totalissues)), line)
                    end
                end
            end
        end
    end
end

"""
    wraplines(content::AnnotatedString, width::Integer = 80, column::Integer = 0)

Wrap `content` into a vector of lines of at most `width` (according to
`textwidth`), with the first line starting at `column`.
"""
function wraplines(content::Union{Annot, SubString{<:Annot}}, width::Integer = 80, column::Integer = 0) where { Annot <: AnnotatedString}
    s, lines = String(content), SubString{Annot}[]
    i, lastwrap, slen = firstindex(s), 0, ncodeunits(s)
    most_recent_break_opportunity = 1
    while i < slen
        if isspace(s[i]) && s[i] != '\n'
            most_recent_break_opportunity = i
        elseif s[i] == '\n'
            push!(lines, content[nextind(s, lastwrap):prevind(s, i)])
            lastwrap = i
            column = 0
        elseif column >= width && most_recent_break_opportunity > 1
            if lastwrap == most_recent_break_opportunity
                nextbreak = findfirst(isspace, @view s[nextind(s, lastwrap):end])
                if isnothing(nextbreak)
                    break
                else
                    most_recent_break_opportunity = lastwrap + nextbreak
                end
                i = most_recent_break_opportunity
            else
                i = nextind(s, most_recent_break_opportunity)
            end
            push!(lines, content[nextind(s, lastwrap):prevind(s, most_recent_break_opportunity)])
            lastwrap = most_recent_break_opportunity
            column = 0
        end
        column += textwidth(s[i])
        i = nextind(s, i)
    end
    if lastwrap < slen
        push!(lines, content[nextind(s, lastwrap):end])
    end
    lines
end
