using Sockets
using Printf

const file_to_lint = @sprintf "%s\n" Base.ARGS[1]
const TIMEOUT = 60 # Number of seconds to wait before closing

"""
    read_timeout(tcp)

Read line over TCP with a timeout. Copied from
`https://github.com/JuliaLang/julia/issues/36217#issuecomment-823344326`.
"""
function read_timeout(tcp::TCPSocket)
    # create timeout timer
    status_before = tcp.status
    tm = Timer(TIMEOUT) do
        lock(tcp.cond)
        tcp.status = Base.StatusEOF
        notify(tcp.cond)
        unlock(tcp.cond)
    end
    # read synchronously
    out = readline(tcp)
    # check timer
    if isopen(tm) # readline worked
        close(tm)
        return out
    else # got EOF because of timer
        # Read timed out!
        tcp.status = status_before
        return nothing
    end
end

# Set to server
try
    # Send a file path to server for linting
    conn = connect(1111)
    write(conn, file_to_lint)
    # Receive error messages back
    while true
        response = read_timeout(conn)
        if isnothing(response) || response == "<<end_lint>>"
            break
        end
        @printf "%s\n" response
    end
catch err
    # No server
end
