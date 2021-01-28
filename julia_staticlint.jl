using StaticLint, SymbolServer
using Printf

SL = StaticLint
SS = SymbolServer

function parse_file(rootfile)
    # ..:: Setup the linting server ::..

    # >> Julia configuration <<
    depot = first(SS.Pkg.depots())
    cache = joinpath(dirname(pathof(SS)), "..", "store")
    env = dirname(SS.Pkg.Types.Context().env.project_file)

    # >> Server <<
    server = SL.FileServer()
    ssi = SymbolServerInstance(depot, cache)
    _, server.symbolserver = SS.getstore(ssi, env)
    server.symbol_extends  = SS.collect_extended_methods(server.symbolserver)


    # Loads and parses the file
    f = SL.loadfile(server, rootfile)

    # SL's main run- finding variables, scopes, etc.
    SL.semantic_pass(f)

    # Run lint checks.
    hints = Dict()
    slopts = SL.LintOptions(:)

    for (path, file) in server.files
        SL.check_all(file.cst, slopts, server)
        hints[path] = SL.collect_hints(file.cst, server)
    end

    for (p, hs) in hints
        for (offset, x) in hs
            if (SL.haserror(x) &&
                SL.errorof(x) isa SL.LintCodes)
                error_discription = SL.LintCodeDescriptions[SL.errorof(x)]
                @printf "%s:%d: error: %s\n" p offset error_discription
            else
                # missing reference
                error_discription = string("Missing reference for ",
                                           SL.CSTParser.valof(x))
                @printf "%s:%d: error: %s\n" p offset error_discription
            end
        end
    end

end

const root_filepath = Base.ARGS[1]

parse_file(root_filepath)
