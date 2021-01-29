using StaticLint
using StaticLint: quoted, unquoted, should_mark_missing_getfield_ref, hasref,
    parentof, is_in_fexpr, errorof, haserror

using CSTParser
using CSTParser: EXPR, headof, isidentifier, isnonstdid, valof, iscall

using SymbolServer
using Printf
using Sockets

SL = StaticLint
SS = SymbolServer
T_Error = Tuple{Tuple{Int,Int},EXPR}

# Julia configuration
depot = first(SS.Pkg.depots())
cache = joinpath(dirname(pathof(SS)), "..", "store")
env = dirname(SS.Pkg.Types.Context().env.project_file)

# Setup server
server = SL.FileServer()
ssi = SymbolServerInstance(depot, cache)
_, server.symbolserver = SS.getstore(ssi, env)
server.symbol_extends  = SS.collect_extended_methods(server.symbolserver)

"""
Reduce the recorded span and fullspan of the expression by CSTParser, when the
expression contains unicode characters. The change is done on the original
struct.

Args:
    e: the original expression.
"""
function fix_string_span_unicode!(e::EXPR)
    val = CSTParser.valof(e)
    if typeof(val)==String && !isascii(val)
        # Hacky length fix
        for c in val
            if !isascii(c)
                e.span -= 1
                e.fullspan -= 1
            end
        end
    end
end

"""
Collect linter output about the file.
This is basically copied from the StaticLint.collect_hints function, however I
updated it slightly to output the (start, end) character positions of the
error, and incorporated the feedback from
https://github.com/julia-vscode/CSTParser.jl/issues/238 on how to make the
character positions match correctly.

Note: the second output is used internally by the function. You don't need to
look at it when using the function to collect the array of errors (the first
output).
"""
function lint_collect_hints(x::EXPR,
                            server,
                            missingrefs=:all,
                            isquoted=false,
                            errs=T_Error[],
                            pos=1)::Tuple{Array{T_Error,1},Integer}
    if isquoted && unquoted(x)
        isquoted = false
    elseif quoted(x)
        isquoted = true
    end

    if headof(x) === :errortoken
        # collect parse errors
        push!(errs, ((pos, pos+x.span), x))
    elseif !isquoted
        if missingrefs != :none &&
            isidentifier(x) &&
            !hasref(x) &&
            !(valof(x) == "var" &&
              parentof(x) isa EXPR &&
              isnonstdid(parentof(x))) &&
              !((valof(x) == "stdcall" ||
                 valof(x) == "cdecl" ||
                 valof(x) == "cdecl" ||
                 valof(x) == "fastcall" ||
                 valof(x) == "thiscall" ||
                 valof(x) == "llvmcall") &&
                is_in_fexpr(x, x -> (iscall(x) &&
                                     isidentifier(x.args[1]) &&
                                     valof(x.args[1]) == "ccall")))
            push!(errs, ((pos, pos+x.span), x))
        elseif haserror(x) && errorof(x) isa SL.LintCodes
            # collect lint hints
            push!(errs, ((pos, pos+x.span), x))
        end
    elseif isquoted && missingrefs == :all && should_mark_missing_getfield_ref(x, server)
        push!(errs, ((pos, pos+x.span), x))
    end

    for a in x
        if a.args === nothing
            fix_string_span_unicode!(a)
            # println(pos, ",", pos+a.span, ",", valof(a))
            lint_collect_hints(a, server, missingrefs, isquoted, errs, pos)
            pos += a.fullspan
        else
            _,pos = lint_collect_hints(a, server, missingrefs, isquoted, errs, pos)
        end
    end

    return errs, pos
end

"""
Print out the error.

Args:
    p: the file path.
    pos: the (start,end) character positions.
    desc: the error description

Returns:
    msg: the error message in the agreed format.
"""
function format_error(p::String, pos::Tuple{Int,Int}, desc::String)::String
    msg = @sprintf "%s:%d:%d: error: %s\n" p pos... desc
    return msg
end

"""
Static lint a file.

Args:
    rootfile: the file to be linted.
    server: the StaticLint server.
    conn: the TCP connection to send errors back over.
"""
function lint_file(rootfile::String,
                   server::SL.FileServer,
                   conn::TCPSocket)::Nothing
    empty!(server.files)

    # Load and parse the file
    f = SL.loadfile(server, rootfile)

    # SL's main run- finding variables, scopes, etc.
    SL.semantic_pass(f)

    # Run lint checks.
    hints = Dict()
    slopts = SL.LintOptions(:)
    for (path, file) in server.files
        SL.check_all(file.cst, slopts, server)
        hints[path],_ = lint_collect_hints(file.cst, server)
    end

    # Send errors back to client
    for (p, hs) in hints
        for (offset, x) in hs
            if (SL.haserror(x) &&
                SL.errorof(x) isa SL.LintCodes)
                error_description = SL.LintCodeDescriptions[SL.errorof(x)]
                write(conn, format_error(p, offset, error_description))
            else
                # missing reference
                error_description = string("Missing reference for ",
                                           SL.CSTParser.valof(x))
                write(conn, format_error(p, offset, error_description))
            end
        end
    end

    return nothing
end

@printf "Started server\n"

tcpserver = listen(1111) # Start the server
while true
    # Wait for client connection
    conn = accept(tcpserver)
    # Read the file to parse
    rootfile = readline(conn)
    # Stop the server?
    if rootfile == "stop"
        close(conn)
        break
    end
    # Line the file
    try
        lint_file(rootfile, server, conn)
    catch err
    end
    close(conn)
end

close(tcpserver)

@printf "Shutdown server\n"
