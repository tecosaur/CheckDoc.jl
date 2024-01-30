function check_non_empty(::Symbol, ::Dict, raw::String, ::Any)
    if all(isspace, raw)
        ShortIssue(:error, "The documentation string should not be empty"), true
    end
end

function check_mentions_errors(category::Symbol, data::Dict, _, parsed::Markdown.MD)
    dmethod = docfunction(data)
    isnothing(dmethod) && return
    ast = definition(dmethod)
    isnothing(ast) && return
    thrown = astsearch(e -> Meta.isexpr(e, :call, 2) && first(e.args) == :throw, ast)
    isempty(thrown) && return
    errstrings = []
    for expr in thrown
        if Meta.isexpr(expr.args[2], :call, 2)
            err = first(expr.args[2].args)
            if Meta.isexpr(err, :curly)
                push!(errstrings, String(first(err.args)))
            elseif err isa Symbol
                push!(errstrings, String(err))
            end
        end
    end
    allcode = map(c -> c.code,
                  filter(c -> isempty(c.language),
                         mdfindall(Markdown.Code, parsed)))
    issues = ShortIssue[]
    for errtype in unique(errstrings)
        if !any(c -> startswith(errtype, c), allcode)
            push!(issues, ShortIssue(
                :warning, "Should mention that '$errtype' may be thrown"))
        end
    end
    issues
end

function check_missing_args(category::Symbol, data::Dict, _, parsed::Markdown.MD)
    if (dmethod = docfunction(data)) |> !isnothing
        _, decls, _, _ = Base.arg_decl_parts(dmethod)
        allcode = map(c -> c.code,
                      filter(c -> isempty(c.language),
                             mdfindall(Markdown.Code, parsed)))
        argnames = first.(decls[2:end])
        issues = ShortIssue[]
        for arg in argnames
            if arg ∉ allcode
                push!(issues, ShortIssue(:warning, "The argument '$arg' should be mentioned"))
            end
        end
        issues
    end
end

function check_missing_kwargs(category::Symbol, data::Dict, _, parsed::Markdown.MD)
    if (dmethod = docfunction(data)) |> !isnothing
        kwargnames = String.(Base.kwarg_decl(dmethod))
        allcode = map(c -> c.code,
                      filter(c -> isempty(c.language),
                             mdfindall(Markdown.Code, parsed)))
        issues = ShortIssue[]
        for kwarg in kwargnames
            if kwarg ∉ allcode
                push!(issues, ShortIssue(:warning, "The keyword argument '$kwarg' should be mentioned"))
            end
        end
        issues
    end
end

function check_args_in_order(category::Symbol, data::Dict, _, parsed::Markdown.MD)
    if (dmethod = docfunction(data)) |> !isnothing
        sigargs = extract_signature(parsed)
        argnames = String.(Base.method_argnames(dmethod)[2:end])
        # @info "Args" data[:binding] sigargs.args argnames
        allcode = map(c -> c.code,
                      filter(c -> isempty(c.language),
                             mdfindall(Markdown.Code, parsed)))
        argorder = filter(!isnothing, indexin(argnames, allcode))
        if !issorted(argorder)
            ShortIssue(:suggestion, "It is nice to document the arguments in the order they are given.")
        end
    end
end

function check_signature(category::Symbol, data::Dict, _, parsed::Markdown.MD)
    if category ∈ (:function, :macro) && haskey(data, :binding)
        fname = String(data[:binding].var)
        firstcode = if isempty(parsed.content)
        elseif first(parsed.content) isa Markdown.Code
            first(parsed.content).code
        elseif (nested = first(parsed.content)) isa Markdown.MD &&
            !isempty(nested.content) && first(nested.content) isa Markdown.Code
            first(nested.content).code
        end
        if isnothing(firstcode)
            ShortIssue(:error, "The call signature should be given at the start")
        elseif !startswith(firstcode, ifelse(category === :function, fname * '(', fname))
            Core.eval(Main, :(parsed = $parsed))
            ShortIssue(:warning, "The initial call signature appears to be about '$(first(split(firstcode)))'")
        end
    end
end

function check_verb_voice end

function check_rogue_spaces end

function check_proper_nouns end

function check_sentence_punctuation end

function check_sentence_capitalized end

function check_section_capitalized end

function check_summary_line end

function check_summary_brief end

function check_summary_sentence end

function check_quoted_symbols end
