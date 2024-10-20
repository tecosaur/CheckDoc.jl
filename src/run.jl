const MINIMUM_AST_NODES = Ref(10)

struct DocIssue
    check::AbstractCheck
    severity::Int
    binding::Binding
    source::Union{LineNumber, Nothing}
    message::AnnotatedString{String}
end

DocIssue(check::AbstractCheck, severity::Symbol, binding::Binding,
         source::Union{LineNumber, Nothing}, issue::AnnotatedString{String}) =
             DocIssue(check, get(SEVERITY_CODES, severity, 0),
                      binding, source, issue)

DocIssue(check::AbstractCheck, binding::Binding, source::Union{LineNumber, Nothing}, short::ShortIssue) =
    DocIssue(check, short.severity, binding, source, short.message)

function runchecks(mod::Module, checks::Vector{AbstractCheck})
    runchecks(mod, bindings(mod), checks)
end

function runchecks(mod::Module, names::Vector{Symbol}, checks::Vector{AbstractCheck})
    bindings = [Docs.aliasof(Binding(mod, name)) for name in names] |> unique
    runchecks(mod, bindings, checks)
end

function runchecks(mod::Module, binding::Binding, checks::Vector{AbstractCheck})
    runchecks(Docs.meta(mod, autoinit=false), binding, checks)
end

function runchecks(mod::Module, name::Symbol, checks::Vector{AbstractCheck})
    runchecks(mod, Docs.aliasof(Binding(mod, name)), checks)
end

function runchecks(mod::Module, bindings::Vector{Docs.Binding}, checks::Vector{AbstractCheck})
    alldocs = recursivedocs(mod)
    runchecks(alldocs, bindings, checks)
end

function bindings(mod::Module)
    allbinds = Binding[]
    modseen = Set{Module}()
    function addbinds!(to::Vector{Binding}, seen::Set{Module}, submod::Module)
        push!(seen, submod)
        for name in names(submod, all=true)
            name ∈ (:eval, :include, :__init__) && continue
            isdefined(submod, name) || continue
            startswith(String(name), '#') && continue
            val = getglobal(submod, name)
            if val isa Module && parentmodule(val) === submod
                val ∉ seen && addbinds!(to, seen, val)
            elseif val isa Function || val isa Type
                parentmodule(val) === submod &&
                    push!(to, Binding(submod, name))
            else # Variable
                push!(to, Binding(submod, name))
            end
        end
    end
    addbinds!(allbinds, modseen, mod)
    map(Docs.aliasof, allbinds) |> unique
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
            isdefined(submod, name) || continue
            subsub = getglobal(submod, name)
            if subsub isa Module && subsub ∉ alreadyseen
                adddocs!(to, subsub, alreadyseen)
            end
        end
    end
    for name in names(mod, all=true)
        isdefined(mod, name) || continue
        if getglobal(mod, name) isa Module
            adddocs!(alldocs, getglobal(mod, name), Set{Module}([mod]))
        end
    end
    alldocs
end

function runchecks(alldocs::IdDict, bindings::Vector{Docs.Binding}, checks::Vector{AbstractCheck})
    issues = DocIssue[]
    checks = sort(checks, by=priority)
    for bind in bindings
        bissues = runchecks(alldocs, bind, checks)
        !isempty(bissues) && append!(issues, bissues)
    end
    issues
end

function runchecks(alldocs::IdDict, bind::Docs.Binding, checks::Vector{AbstractCheck})
    issues = DocIssue[]
    checks = sort(checks, by=priority)
    for doc in doccontexts(alldocs, bind)
        for chk in checks
            applicable(chk, doc.kind) || continue
            results = check(chk, doc)
            isnothing(results) && continue
            if results isa ShortIssue
                push!(issues, DocIssue(chk, bind, doc.source, results))
            elseif results isa Vector{ShortIssue}
                isempty(results) && continue
                for result in results
                    push!(issues, DocIssue(chk, bind, doc.source, result))
                end
            else
                @warn "Unsupported $(typeof(chk)) check results format: $(typeof(results))"
            end
            terminal(chk) && break
        end
    end
    issues
end

function runchecks(mod::Module, checks::Vector{<:AbstractCheck})
    @nospecialize checks
    runchecks(mod, Vector{AbstractCheck}(checks))
end

function runchecks(source::Union{Module, IdDict},
                   target::Union{Symbol, Vector{Symbol}, Binding, Vector{Binding}},
                   checks::Vector{<:AbstractCheck})
    @nospecialize checks
    runchecks(source, target, Vector{AbstractCheck}(checks))
end

function doccontexts(alldocs::IdDict, bind::Docs.Binding)
    res = if isdefined(bind.mod, bind.var)
        resolve(bind)
    end
    category = bindkind(bind)
    if haskey(alldocs, bind)
        Iterators.map(alldocs[bind].docs |> values) do doc
            raw = join(doc.text)
            parsed = Docs.parsedoc(doc)
            docsource = @something(
                if category === :function && haskey(doc.data, :typesig) && !isnothing(res)
                    try
                        dmethods = methods(res, maxunion(doc.data[:typesig]))
                        if !isempty(dmethods)
                            file, line = Base.updated_methodloc(first(dmethods))
                            LineNumber(file, Int(line))
                        end
                    catch err
                        Core.eval(Main, :(dtype = $(doc.data[:typesig])))
                        @error "Oh no!" err
                    end
                end,
                if haskey(doc.data, :path) && haskey(doc.data, :linenumber)
                    LineNumber(doc.data[:path], doc.data[:linenumber])
                end)
            DocContext(category, bind, docsource, doc.data, raw, parsed)
        end
    else
        [DocContext(category, bind, nothing, Dict{Symbol, Any}(), "", nothing)]
    end
end

function bindkind(bind::Docs.Binding)
    isdefined(bind.mod, bind.var) || return :undefined
    thing = getglobal(bind.mod, bind.var)
    if thing isa Function
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
