module CheckDoc

using InteractiveUtils
using CodeTracking
using Base.Docs: Binding, DocStr, MultiDoc, getdoc, resolve

include("types.jl")
include("display.jl")
include("utils.jl")
include("checks.jl")

__init__() = foreach(addface!, CHECKDOC_FACES)

CHECKS = Pair{Symbol, Function}[
    # Specially handled checks
    :macro => Returns(nothing),
    :function => Returns(nothing),
    :variable => Returns(nothing),
    :type => Returns(nothing),
    :undocumented => Returns(nothing),
    :macro_hasdoc => Returns(nothing),
    :function_hasdoc => Returns(nothing),
    :variable_hasdoc => Returns(nothing),
    :type_hasdoc => Returns(nothing),
    # Actual check functions
    :non_empty => check_non_empty,
    :missing_args => check_missing_args,
    :missing_kwargs => check_missing_kwargs,
    :args_in_order => check_args_in_order,
    :mentions_errors => check_mentions_errors,
    :signature => check_signature,
]

export checkdocs

"""
    checkdocs(mod::Module; kwargs...)

# Options

- `all`: Check unexported symbols
- `only`: Check whitelist (active when non-empty)
- `skip`: Check blacklist
- `maxlevel`: Omit issues above this level
- `viral`: check docstrings in other packages
"""
function checkdocs(mod::Module; all::Bool=true,
                   only::Vector{Symbol}=Symbol[], skip::Vector{Symbol}=Symbol[],
                   maxlevel::Union{Int, Nothing}=nothing, viral::Bool=true)
    issues = Vector{Pair{Symbol, Vector{DocIssue}}}()
    checks = getchecks(only, skip)
    relevant_names = filter(
        name -> !startswith(String(name), '#'),
        setdiff(names(mod; all), (:eval, :include, nameof(mod))))
    bindings = Binding.(Ref(mod), relevant_names) .|> Docs.aliasof |> unique
    alldocs = recursivedocs(mod)
    for bind in bindings
        nissues = runchecks(alldocs, bind, checks)
        if nissues isa Vector{DocIssue}
            if !isnothing(maxlevel)
                filter!(i -> i.level <= maxlevel, nissues)
            end
            if !viral
                if isnothing(pathof(mod))
                    filter!(i -> isnothing(i.source), nissues)
                else
                    moddir = dirname(pathof(mod))
                    filter!(i -> !isnothing(i.source) && startswith(i.source.file, moddir),
                            nissues)
                end
            end
            !isempty(nissues) && push!(issues, bind.var => nissues)
        end
    end
    DocsReport(mod, relevant_names, issues, maxlevel)
end

function recursivedocs(mod::Module)
    alldocs = Docs.meta(mod, autoinit=false) |> copy
    function adddocs!(to::IdDict, submod::Module, alreadyseen::Set{Module})
        push!(alreadyseen, submod)
        subdocs = Docs.meta(submod, autoinit=false)
        if !isnothing(subdocs)
            for (binding, docs) in subdocs
                if haskey(to, binding)
                    mergedoc = MultiDoc()
                    mergedoc.order = vcat(to[binding].order, docs.order)
                    mergedoc.docs = IdDict(merge(to[binding].docs, docs.docs))
                    to[binding] = mergedoc
                else
                    to[binding] = docs
                end
            end
        end
        for name in names(submod, all=true)
            if (subsub = getfield(submod, name)) isa Module && subsub ∉ alreadyseen
                adddocs!(to, subsub, alreadyseen)
            end
        end
    end
    for name in names(mod, all=true)
        if getfield(mod, name) isa Module
            adddocs!(alldocs, getfield(mod, name), Set{Module}([mod]))
        end
    end
    alldocs
end

function checkdocs(mod::Module, bind::Docs.Binding;
                   only::Vector{Symbol}=Symbol[], skip::Vector{Symbol}=Symbol[],
                   maxlevel::Union{Int, Nothing}=nothing)
    issues = runchecks(Docs.meta(mod, autoinit=false), bind, getchecks(only, skip))
    isnothing(maxlevel) || filter!(i -> i.level <= maxlevel, issues)
    DocsReport(mod, [bind.var], [bind.var => something(issues, DocIssue[])], maxlevel)
end

checkdocs(mod::Module, name::Symbol; only::Vector{Symbol}=Symbol[], skip::Vector{Symbol}=Symbol[]) =
    checkdocs(mod, Binding(mod, name); only, skip)

checkdocs(name::Symbol; only::Vector{Symbol}=Symbol[], skip::Vector{Symbol}=Symbol[]) =
    checkdocs(Base.binding_module(Main, name), name; only, skip)

function checkdocs(f::Function; only::Vector{Symbol}=Symbol[], skip::Vector{Symbol}=Symbol[])
    # This is just a guess, but it's the best that I'm aware of
    mod = argmin(m -> m.primary_world, methods(f)).module
    checkdocs(mod, Binding(mod, nameof(f)); only, skip)
end

function getchecks(only::Vector{Symbol}=Symbol[], skip::Vector{Symbol}=Symbol[])
    if isempty(only) && isempty(skip)
        CHECKS
    elseif !isempty(only)
        filter(((c, _),) -> c ∈ only, CHECKS)
    else # !isempty(skip)
        filter(((c, _),) -> c ∉ skip, CHECKS)
    end
end

const MINIMUM_AST_NODES = Ref(10)

function runchecks(alldocs::IdDict, bind::Docs.Binding, checks::Vector{Pair{Symbol, Function}})
    source = if (func = resolve(bind)) isa Union{Function, Type}
        fmethods = filter(m -> m.module == bind.mod, methods(func))
        !isempty(fmethods) || return nothing
        oldest = argmin(m -> m.primary_world, fmethods)
        file, line = Base.updated_methodloc(oldest)
        LineNumber(file, Int(line))
    end
    thing = getfield(bind.mod, bind.var)
    category = if thing isa Function
        if startswith(String(bind.var), '@')
            :macro
        else
            :function
        end
    elseif thing isa Type
        :type
    else
        :variable
    end
    # Early return conditions
    category ∈ first.(checks) || return nothing
    if !haskey(alldocs, bind)
        :undocumented ∈ first.(checks) || return nothing
        thing_plural, needsdoc = if thing isa Function
            "functions", :function_hasdoc ∈ first.(checks) && bind.var !== :__init__
        elseif thing isa Type
            "types", :type_hasdoc ∈ first.(checks)
        else # thing is a Variable
            "variables", :variable_hasdoc ∈ first.(checks)
        end
        return if needsdoc
            [DocIssue(:undocumented, :error, bind, source,
                      "All $thing_plural might as well have a documentation string")]
        end
    elseif thing isa Function
        nontrivial = false
        for method in methods(thing)
            if method.module == bind.mod &&
                ast_nodes(definition(method)) >= MINIMUM_AST_NODES[]
                nontrivial = true
                break
            end
        end
        nontrivial || return nothing
    end
    ensurevec(v::Vector) = v
    ensurevec(x::Any) = [x]
    # Do the checks
    issues = DocIssue[]
    for doc in alldocs[bind].docs |> values
        parsed = Docs.parsedoc(doc)
        raw = join(doc.text)
        docsource = if category === :function && haskey(doc.data, :typesig)
            try
                dmethods = methods(func, maxunion(doc.data[:typesig]))
                if !isempty(dmethods)
                    file, line = Base.updated_methodloc(first(dmethods))
                    LineNumber(file, Int(line))
                end
            catch err
                Core.eval(Main, :(dtype = $(doc.data[:typesig])))
                @error "Oh no!" err
            end
        elseif haskey(doc.data, :path) && haskey(doc.data, :linenumber)
            LineNumber(doc.data[:path], doc.data[:linenumber])
        end
        for (label, check) in checks
            abort = false
            results = #try
                check(category, doc.data, raw, parsed)
            # catch err
            #     @warn "Could not call $check" err
            # end
            !isnothing(results) || continue
            if results isa Tuple{<:Any, Bool}
                results, abort = results[1], results[2]
            end
            results = ensurevec(results)
            if results isa Vector{ShortIssue}
                append!(issues,
                        [DocIssue(label, i.severity, bind, docsource, i.issue)
                         for i in results])
            else
                @warn "Unsupported check results format: $(typeof(results))"
            end
            abort && return issues
        end
    end
    issues
end

function ast_nodes(ast::Expr)
    if ast.head == :(::)
        ast_nodes(first(ast.args))
    elseif ast.head == :function
        ast_nodes(last(ast.args))
    else
        mapreduce(ast_nodes, +, ast.args, init=0)
    end
end
ast_nodes(::LineNumberNode) = 0
ast_nodes(::Any) = 1

end
