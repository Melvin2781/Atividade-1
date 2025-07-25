import Data.Char (toLower) -- Importação da função toLower

-- Função para normalizar o input
normalizar :: Char -> Char
normalizar c
    -- A com acento.
    | c `elem` "ÁÀÂÃÄáàâãä" = 'a'
    -- E com acento.
    | c `elem` "ÉÈÊËéèêë" = 'e'
    -- I com acento.
    | c `elem` "ÍÌÎÏíìîï" = 'i'
    -- O com acento.
    | c `elem` "ÓÒÔÕÖóòôõö" = 'o'
    -- U com acento.
    | c `elem` "ÚÙÛÜúùûü" = 'u'
    -- Ç para C
    | c `elem` "Çç" = 'c'
    -- Letras maiúsculas sem acento para letras minúsculas sem acento.
    | c `elem` "ABCDEFGHIJKLMNOPQRSTUVWXYZ" = toLower c
    -- Outros caracteres
    | otherwise = c

-- Função para realizar a substituição em um caractere normalizado.
cifra :: [Char] -> Char -> Char
cifra chave c
    | c >= 'a' && c <= 'z' = head [y| (x, y) <- zip ['a', 'b'.. 'z'] chave, c == x]
    | otherwise = c

-- Função de criptografia em String.
criptografar :: [Char] -> String -> String
criptografar chave xs = [cifra chave (normalizar(x))| x <- xs]

-- Função para realizar a substituição contrária em um único caractere.
cifraCont :: [Char] -> Char -> Char
cifraCont chave c
    | c >= 'A' && c <= 'Z' = head [y| (x, y) <- zip chave ['a', 'b' .. 'z'], x == c]
    | otherwise = c

-- Função para descriptografar a string.
descriptografar :: [Char] -> String -> String
descriptografar chave xs = [cifraCont chave x| x <- xs]

-- exemplo da utilização da criptografia.
exemplo1 = criptografar ['Z', 'Y', 'N', 'G', 'W', 'Q', 'A', 'M', 'X', 'P', 'K', 'V', 'U', 'L', 'C', 'E', 'F', 'R', 'I', 'B', 'S', 'J', 'D', 'O', 'T', 'H'] "OLA"
-- Exemplo da utilização da descriptografia.
exemplo2 = descriptografar ['Z', 'Y', 'N', 'G', 'W', 'Q', 'A', 'M', 'X', 'P', 'K', 'V', 'U', 'L', 'C', 'E', 'F', 'R', 'I', 'B', 'S', 'J', 'D', 'O', 'T', 'H'] "CVZ"