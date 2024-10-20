using Markdown

mdfindall(::Type{T}, ::Any, sofar::Vector{T}=T[]) where {T} = sofar

function mdfindall(::Type{T}, md::Union{Markdown.MD, Markdown.Paragraph,
                                        Markdown.BlockQuote, Markdown.Admonition},
                   sofar::Vector{T}=T[]) where {T}
    for thing in md.content
        if thing isa T
            push!(sofar, thing)
        else
            mdfindall(T, thing, sofar)
        end
    end
    sofar
end

function mdfindall(::Type{T}, textual::Union{Markdown.Header, Markdown.Footnote,
                                             Markdown.Italic, Markdown.Bold, Markdown.Link},
                   sofar::Vector{T}) where {T}
    isnothing(textual.text) && return sofar
    for thing in textual.text
        if thing isa T
            push!(sofar, thing)
        else
            mdfindall(T, thing, sofar)
        end
    end
    sofar
end

function mdfindall(::Type{T}, list::Markdown.List, sofar::Vector{T}) where {T}
    for item in list.items
        if item isa T
            push!(sofar, item)
        else
            mdfindall(T, item, sofar)
        end
    end
    sofar
end

function mdfindall(::Type{T}, vec::Vector, sofar::Vector{T}) where {T}
    for elt in vec
        mdfindall(T, elt, sofar)
    end
    sofar
end

# ---

"""
    extract_signature(docstr::Markdown.MD)

Attempt to interpret the first element of `docstr` as a
function signature, and from it extract the function name,
arguments, and keword arguments.
"""
function extract_signature(docstr::Markdown.MD)
    firstcode = if isempty(docstr.content)
    elseif first(docstr.content) isa Markdown.Code
        first(docstr.content).code
    elseif (nested = first(docstr.content)) isa Markdown.MD &&
        !isempty(nested.content) && first(nested.content) isa Markdown.Code
        first(nested.content).code
    end
    if !isnothing(firstcode)
        parse_signature(firstcode)
    end
end

"""
    parse_signature(sig::String)

Perform a best-effort conversion of a signature `sig` to a `NamedTuple` giving
the function, arguments, and keyword arguments specified in `sig`:
- `name`: The name of the function being called
- `args`: A vector of argument forms, each form consists of a three-value tuple,
  the first value being a list of symbols mentioned in the argument, the second
  being the type of the argument (as a `Symbol` or `Expr`), and the third a `Bool`
  indicating whether the argument has a default value.
- `kwargs`: A vector of keyword-argument forms, also a three-value tuple, but with just
  a single `Symbol` as the first value.
- `argnames`: A vector of all `args` and `kwargs` names, for convenience.

If multiple signatures are present in `sig`, the first is used.  If the
signature could not be determined, `nothing` is returned.
"""
function parse_signature(sig::String)
    # TODO extract /all/ signatures
    exprs = Meta.parseall(sig)
    expr = first(filter(e -> !(e isa LineNumberNode), exprs.args))
    if Meta.isexpr(expr, :(=), 2)
        expr = last(expr.args)
    end
    if Meta.isexpr(expr, :(::), 2) || Meta.isexpr(expr, :->, 2)
        expr = first(expr.args)
    end
    function interpretarg(arg; kw::Bool=false)
        hasdefault = false
        if Meta.isexpr(arg, :kw, 2)
            arg = arg.args[1]
            hasdefault = true
        end
        if Meta.isexpr(arg, :call, 3) && first(arg.args) == :(=>)
            a = interpretarg(arg.args[2])
            b = interpretarg(arg.args[3])
            (vcat(first(a), first(b)),
             Expr(:curly, :Pair, last(a), last(b)),
             hasdefault)
        elseif Meta.isexpr(arg, :tuple)
            parts = interpretarg.(arg.args)
            (vcat(first.(parts)...),
             Expr(:curly, :Tuple, last.(parts)...),
             hasdefault)
        elseif Meta.isexpr(arg, :(...), 1)
            vars, type = interpretarg(arg.args[1])
            if kw
                ([Symbol(string(first(vars), "..."))],
                 :NamedTuple, false) # Not quite, but effectively...
            else
                (vars, Expr(:curly, :Vararg, type), false)
            end
        elseif Meta.isexpr(arg, :(::), 2)
            ([arg.args[1]], arg.args[2], hasdefault)
        elseif Meta.isexpr(arg, :(::), 1) && Meta.isexpr(arg.args[1], :curly) && arg.args[1].args[1] === :Type
            ([arg.args[1].args[2]], :Type, hasdefault)
        elseif arg isa Symbol
            ([arg], :Any, hasdefault)
        end
    end
    function interpretarg!(acc::Vector, arg; kw::Bool=false)
        if Meta.isexpr(arg, :vect)
            for varg in arg.args
                # Create a varg=true form to recognise the indication that
                # this has a default value, whether or not varg is a = expr.
                varg = Expr(:kw, if Meta.isexpr(varg, :(=), 2) first(varg.args) else varg end, true)
                interpretarg!(acc, varg; kw)
            end
        elseif (parsed = interpretarg(arg; kw)) |> !isnothing
            push!(acc, parsed)
        end
        acc
    end
    if Meta.isexpr(expr, :call)
        func = first(expr.args)
        args = Tuple{Vector{Symbol}, Union{Symbol, Expr}, Bool}[]
        kwargs = Tuple{Symbol, Union{Symbol, Expr}, Bool}[]
        argnames = Symbol[]
        length(expr.args) == 1 && return (; func, args, kwargs, argnames)
        if Meta.isexpr(expr.args[2], :parameters)
            kws = Tuple{Vector{Symbol}, Union{Symbol, Expr}, Bool}[]
            for kw in expr.args[2].args
                interpretarg!(kws, kw; kw=true)
            end
            append!(kwargs, (first(a), b, c) for (a, b, c) in kws)
        end
        for arg in expr.args[2+!isempty(kwargs):end]
            interpretarg!(args, arg)
        end
        for (a, _, _) in args
            append!(argnames, a)
        end
        append!(argnames, map(first, kwargs))
        (; func, args, kwargs, argnames)
    end
end

function asexpr(T::Type)
    # FIXME: This is awful, but ... it seems to work
    Meta.parse(sprint(show, T))
end

function match_signture(sig::NamedTuple{(:name, :args, :kwargs)}, method::Method)
    mname, margs... = collect(method.sig.parameters)
    mkwargs = Base.kwarg_decl(method)
    length(sig.args) == length(margs) || return :arg_len_mismatch
    sort(first.(sig.kwargs)) == sort(mkwargs) || return :kwarg_name_mismatch
end

# ---

function maxunion(T::Type)
    if T == Union{}
        Tuple{}
    elseif T isa Union
        ua = maxunion(T.a)
        ub = maxunion(T.b)
        ifelse(length(ua.parameters) < length(ub.parameters), ub, ua)
    elseif T isa UnionAll
        maxunion(Base.unwrap_unionall(T))
    else
        T
    end
end

function _astsearch(predicate::Function, ast::Expr, sofar::Vector{Expr})
    if predicate(ast)
        push!(sofar, ast)
    else
        for arg in ast.args
            _astsearch(predicate, arg, sofar)
        end
    end
    sofar
end

_astsearch(::Function, ::Any, sofar::Vector{Expr}) = sofar

astsearch(predicate::Function, ast::Expr) = _astsearch(predicate, ast, Expr[])

function docfunction(data::Dict)
    if haskey(data, :binding) && haskey(data, :typesig) &&
        (dmethods = methods(resolve(data[:binding]), maxunion(data[:typesig]))) |> length == 1
        first(dmethods)
    end
end
