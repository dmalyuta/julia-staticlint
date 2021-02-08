using Sockets
using Printf

file_to_lint = @sprintf "%s\n" Base.ARGS[1]

# Set to server
try
    # Send a file path to server for linting
    conn = connect(1111)
    write(conn, file_to_lint)
    # Receive error messages back
    while true
        response = readline(conn)
        if response == "<<end_lint>>"
            break
        end
        @printf "%s\n" response
    end
catch err
    # No server
end
