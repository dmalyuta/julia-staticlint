using StaticLint
using StaticLint: quoted, unquoted, should_mark_missing_getfield_ref, hasref,
    parentof, is_in_fexpr, errorof, haserror

using CSTParser
using CSTParser: EXPR, headof, isidentifier, isnonstdid, valof, iscall

using SymbolServer
using Printf
using Sockets

# ..:: Variables and data structures ::..

mutable struct ErrorSpan
    beg_char::Integer
    end_char::Integer
end

SL = StaticLint
SS = SymbolServer
T_Error = Tuple{ErrorSpan,EXPR}

error = "error"
warn = "warn"
info = "info"

const LintCodeToErrorLevel = Dict{SL.LintCodes,String}(
    SL.IncorrectCallArgs => error,
    SL.IncorrectIterSpec => error,
    SL.NothingEquality => error,
    SL.NothingNotEq => error,
    SL.ConstIfCondition => info,
    SL.EqInIfConditional => error,
    SL.PointlessOR => info,
    SL.PointlessAND => info,
    SL.UnusedBinding => info,
    SL.InvalidTypeDeclaration => error,
    SL.UnusedTypeParameter => info,
    SL.IncludeLoop => warn,
    SL.MissingFile => error,
    SL.InvalidModuleName => error,
    SL.TypePiracy => warn,
    SL.UnusedFunctionArgument => warn,
    SL.CannotDeclareConst => error,
    SL.InvalidRedefofConst => error,
    SL.NotEqDef => warn,
    SL.KwDefaultMismatch => error,
    SL.InappropriateUseOfLiteral => warn,
    SL.ShouldBeInALoop => warn,
    SL.TypeDeclOnGlobalVariable => error,
    SL.UnsupportedConstLocalVariable => warn,
    SL.UnassignedKeywordArgument => error,
    SL.CannotDefineFuncAlreadyHasValue => error,
    SL.DuplicateFuncArgName => error)

# ..:: Functions ::..

#= Print out the error.

Args:
    p: the file path.
    pos: the (start,end) character positions.
    desc: the error description
    lvl: the error level (error? warning? info?)

Returns:
    msg: the error message in the agreed format. =#
function format_error(p::String, pos::ErrorSpan, desc::String, lvl::String)::String
    msg = @sprintf "%s:%d:%d: %s: %s\n" p pos.beg_char pos.end_char lvl desc
    return msg
end

#= Collect linter output about the file.

This is basically copied from the StaticLint.collect_hints function, however I
updated it slightly to output the (start, end) character positions of the
error, and incorporated the feedback from
https://github.com/julia-vscode/CSTParser.jl/issues/238 on how to make the
character positions match correctly.

Note: the second output is used internally by the function. You don't need to
look at it when using the function to collect the array of errors (the first
output). =#
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
        push!(errs, (ErrorSpan(pos, pos+x.span), x))
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
            push!(errs, (ErrorSpan(pos, pos+x.span), x))
        elseif haserror(x) && errorof(x) isa SL.LintCodes
            # collect lint hints
            push!(errs, (ErrorSpan(pos, pos+x.span), x))
        end
    elseif (isquoted &&
            missingrefs == :all &&
            should_mark_missing_getfield_ref(x, server))
        push!(errs, (ErrorSpan(pos, pos+x.span), x))
    end

    for a in x
        if a.args === nothing
            lint_collect_hints(a, server, missingrefs, isquoted, errs, pos)
            pos += a.fullspan
        else
            _,pos = lint_collect_hints(a, server, missingrefs, isquoted, errs, pos)
        end
    end

    return errs, pos
end

#= Convert byte to character count.

Convert error start and end character locations from a byte measurement to a
strict character count measurement.

Args:
    src: the source code (as a string) which this error is for.
    offset: the error location in bytes. Modified in place. =#
function convert_pos_byte_to_char!(src::String, offset::ErrorSpan)
    offset.beg_char = length(src, 1, offset.beg_char)
    offset.end_char = length(src, 1, offset.end_char)
end

#= Static lint a file.

Args:
    rootfile: the file to be linted.
    server: the StaticLint server.
    conn: the TCP connection to send errors back over. =#
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
    files_src = Dict(f.path=>f.source)
    for (p, hs) in hints
        for (offset, x) in hs
            if !haskey(files_src, p)
                files_src[p] = read(p, String)
            end

            if (SL.haserror(x) &&
                SL.errorof(x) isa SL.LintCodes)
                description = SL.LintCodeDescriptions[SL.errorof(x)]
                level = LintCodeToErrorLevel[SL.errorof(x)]
                convert_pos_byte_to_char!(files_src[p], offset)
                write(conn, format_error(p, offset, description, level))
            else
                # Use of undeclared variable
                error_description = string("Missing reference for ",
                                           SL.CSTParser.valof(x))
                level = error
                convert_pos_byte_to_char!(files_src[p], offset)
                write(conn, format_error(p, offset, description, level))
            end
        end
    end

    return nothing
end

# ..:: Start the server loop ::..

@printf "Initializing server\n"

# Julia configuration
depot = first(SS.Pkg.depots())
cache = joinpath(dirname(pathof(SS)), "..", "store")
env = dirname(SS.Pkg.Types.Context().env.project_file)

# Setup server
server = SL.FileServer()
ssi = SymbolServerInstance(depot, cache)
_, server.symbolserver = SS.getstore(ssi, env)
server.symbol_extends  = SS.collect_extended_methods(server.symbolserver)

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
        bt = catch_backtrace()
        msg = sprint(showerror, err, bt)
        println("***Experienced the following error:***\n", msg)
    end
    close(conn)
end

close(tcpserver)

@printf "Shutdown server\n"
