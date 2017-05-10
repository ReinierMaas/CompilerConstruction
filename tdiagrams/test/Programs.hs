module Programs (
    correctPrograms,
    typeIncorrectPrograms,
    incorrectPrograms
    ) where

correctPrograms :: [String]
correctPrograms = [
    "program hello in Haskell",
    "platform x64-windows",
    "platform x86",
    "interpreter hugs for Haskell in i686-windows",
    "compiler uuagc from UUAG to Haskell in i686-windows",
    "execute program hello in Haskell on interpreter hugs for Haskell in x86-windows end",
    "execute interpreter hugs for Haskell in x86-windows on platform x86-windows end",
    "execute program hello in Haskell on interpreter hugs for Haskell in i686-windows end",
    "compile program hello in UUAG with compiler uuagc from UUAG to Haskell in i686-windows end",
    "execute compile program hello in UUAG with compiler uuagc from UUAG to Haskell in i686-windows end on interpreter hugs for Haskell in i686-windows end",
    "execute execute compile program hello in UUAG with compiler uuagc from UUAG to Haskell in i686-windows end on platform i686-windows end on interpreter hugs for Haskell in i686-windows end"
    ]

typeIncorrectPrograms :: [String]
typeIncorrectPrograms = [
    "execute platform x86 on platform ARM end", -- 1
    "execute platform x86 on platform x86 end", -- 1
    "execute program hello in Haskell on program world in Haskell end", -- 2
    "execute interpreter hugs for Haskell in x86-windows on compiler hugs from Haskell to x86-windows in x86-windows end", -- 2
    "execute compile program hello in UUAG with compiler uuagc from UUAG to Haskell in i686-windows end on interpreter x64-on-arm for x64 in arm end", -- 3
    "compile platform x64 with compiler x64-to-x86 from x64 to x86 in x64 end", -- 4
    "compile program hello in x64 with platform x64 end", -- 5
    "compile program hello in Haskell with compiler uuagc from UUAG to Haskell in i686-windows end" -- 6
    ]

incorrectPrograms :: [String]
incorrectPrograms = [
    "program hello",
    "platform"
    ]
