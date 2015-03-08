module Lidr2Idr

lidr2idr : File -> IO ()
lidr2idr file = do eof <- (feof file)
                   if eof then
                     return ()
                   else
                     do line <- (fread file)
                        if (isPrefixOf ">" line) then
                          putStr $ strTail $ line
                        else if (length (trim line)) == 0 then
                          putStr $ line
                        else
                          putStr $ ("--" ++ line)
                        lidr2idr $ file
