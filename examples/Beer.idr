module Beer

beer : Nat -> IO ()
beer Z = return ()
beer (S k) = do let a = " bottles of beer on the wall, "
                let b = " bottles of beer. Take one down, pass it around, "
                let c = " bottles of beer on the wall..."
                putStrLn (show (S k) <+> a <+> show (S k) <+> b <+> show k <+> c)
                beer k

main : IO ()
main = beer 99



