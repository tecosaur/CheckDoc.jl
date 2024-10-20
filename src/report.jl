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

function checkdocs(mod::Module, checks::Vector{AbstractCheck} = DEFAULT_CHECKS; kwargs...)
    binds = bindings(mod)
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
            :issue
        end
    end
    groupissues(issues, by1, by2), maxlevel
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

function groupissues(issues::Vector{DocIssue}, groupby::Symbol, subgroup::Union{Symbol, Nothing}=nothing)
    gkind = if groupby ∈ (:severity, :level)
        Int
    elseif groupby ∈ (:name, :entity)
        Binding
    elseif groupby ∈ (:issue, :kind, :check)
        Type{<:AbstractCheck}
    else
        throw(ArgumentError("Unknown grouping key: $groupby"))
    end
    groups = Dict{gkind, Vector{DocIssue}}()
    grouporder = Vector{gkind}()
    for issue in issues
        gkey = if gkind === Int
            issue.severity
        elseif gkind === Binding
            issue.binding
        else
            typeof(issue.check)
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
    end
    if isnothing(subgroup)
        [grp => groups[grp] for grp in grouporder]
    else
        [grp => groupissues(groups[grp], subgroup, nothing) for grp in grouporder]
    end
end

function Base.show(io::IO, ::MIME"text/plain", issue::DocIssue)
    sface, slabel = get(SEVERITIES, issue.severity, (:bright_black, "Unknown"))
    srcloc = if isnothing(issue.source) styled"{italic,shadow:unknown:unknown} (within {underline:$(pkgdir(issue.binding.mod))})" else
        styled"{underline:$(issue.source.file){magenta::$(issue.source.line)}}" end
    show(io, DocIssue)
    print(io, styled"""({$sface,bold:$slabel}): \
                         {yellow:$(label(issue.check))}({green:"$(issue.message)"})\n  \
                         binding: {code:$(issue.binding)}\n  \
                         source:  $srcloc""")
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
    gstyle(group::Type{Int}, issues::Vector{DocIssue}) =
        get(SEVERITIES, first(issues).severity, (:bright_black, "Unknown"))
    gstyle(group::Type{Binding}, issues::Vector{DocIssue}) =
        :code, chopprefix(string(first(issues).binding), "$(report.mod).")
    gstyle(group::Type{DataType}, issues::Vector{DocIssue}) =
        (if allequal(i -> i.severity, issues)
             get(SEVERITIES, first(issues).severity, (:bright_black, "")) |> first
         else
             :default
         end,
         label(first(issues).check))
    G3 = first(setdiff((Int, Binding, DataType), (G1, G2)))
    for (group, sgroup) in report.issues
        (isempty(sgroup) || all(isempty ∘ last, sgroup)) && continue
        groupissues = collect(Iterators.map(last, sgroup) |> Iterators.flatten)
        gface, glabel = gstyle(G1, groupissues)
        print(io, styled"\n{$gface:╭─● {bold:$glabel}}")
        for (subgroup, issues) in sgroup
            sgface, sglabel = gstyle(G2, issues)
            print(io, styled"\n{$gface:│} {$sgface:┌ $sglabel}")
            samemessage = allequal(i -> i.message, issues)
            samemessage &&
                print(io, styled"{$sgface::} ", first(issues).message)
            for issue in issues
                icount += 1
                iface, ilabel = gstyle(G3, [issue])
                gutterprefix = styled"\n{$gface:$(ifelse(issue == last(issues) && subgroup == first(last(sgroup)), '╰', '│'))} \
                                 {$sgface:$(ifelse(issue == last(issues), '└', '│'))}"
                print(io, gutterprefix, styled" {bold:$(lpad(icount, ndigits(totalissues))).} {$iface:$ilabel}",
                      if isnothing(issue.source)
                          styled" {shadow:{bold,italic:@} unknown {light:(within $(pkgdir(issue.binding.mod)))}}"
                      else
                          styled" {shadow:{bold,italic:@} $(issue.source.file):$(issue.source.line)}"
                      end)
                if !samemessage
                    print(io, gutterprefix, ' '^(2+ndigits(totalissues)), ' ', issue.message)
                end
            end
        end
    end
end
