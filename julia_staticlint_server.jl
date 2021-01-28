using StaticLint, SymbolServer
using Printf
using Sockets

SL = StaticLint
SS = SymbolServer

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
        hints[path] = SL.collect_hints(file.cst, server)
    end

    # Send errors back to client
    for (p, hs) in hints
        for (offset, x) in hs
            if (SL.haserror(x) &&
                SL.errorof(x) isa SL.LintCodes)
                error_discription = SL.LintCodeDescriptions[SL.errorof(x)]
                msg = @sprintf "%s:%d: error: %s\n" p offset error_discription
                write(conn, msg)
            else
                # missing reference
                error_discription = string("Missing reference for ",
                                           SL.CSTParser.valof(x))
                msg = @sprintf "%s:%d: error: %s\n" p offset error_discription
                write(conn, msg)
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
