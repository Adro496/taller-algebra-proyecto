-- DATOS Y SHOW

data Modificacion = Insertar Integer Char | Borrar Integer | Substituir Integer Char deriving (Show, Eq)

type PaqueteModificaciones = [Modificacion]

data Archivo = ArchivoVacio | NuevaVersion PaqueteModificaciones Archivo
instance Show Archivo where
    show ArchivoVacio = "Archivo vacio"
    show file = "Archivo: " ++ obtenerUltimaVersion file

data SCV = NuevoSCV | AgregarArchivo Archivo SCV
instance Show SCV where
    show NuevoSCV = "SCV vacio"
    show scv = verArchivos scv

verArchivos :: SCV -> String
verArchivos NuevoSCV = ""
verArchivos (AgregarArchivo file scv) = "- " ++ (show file) ++ "\n" ++ (verArchivos scv)

-- EJERCICIOS

-- Ejercicio 1/8
aplicarModificacion :: String -> Modificacion -> String
aplicarModificacion str (Insertar 0 z) = z : str
aplicarModificacion (x:xl) (Insertar n z) = x : aplicarModificacion xl (Insertar (n - 1) z)
aplicarModificacion (x:xl) (Borrar 1) = xl
aplicarModificacion (x:xl) (Borrar n) = x : aplicarModificacion xl (Borrar (n - 1))
aplicarModificacion str (Substituir n z) = aplicarModificacion (aplicarModificacion str (Insertar n z)) (Borrar n)

-- Ejemplos:
-- Main> aplicarModificacion "d" (Insertar 1 'a')
-- "da"
-- Main> aplicarModificacion "d" (Insertar 0 'a')
-- "ad"
-- Main> aplicarModificacion "dato" (Borrar 1)
-- "ato"

-- Ejercicio 2/8
aplicarPaqueteModificaciones :: String -> PaqueteModificaciones -> String
aplicarPaqueteModificaciones str [] = str
aplicarPaqueteModificaciones str (x:xl) = aplicarPaqueteModificaciones (aplicarModificacion str x) xl

-- Ejemplos:
-- Main> aplicarPaqueteModificaciones "dato" [Substituir 1 'p', Insertar 4 's']
-- "patos"

-- Ejercicio 3/8
obtenerUltimaVersion :: Archivo -> String
obtenerUltimaVersion ArchivoVacio = ""
obtenerUltimaVersion (NuevaVersion xlm file) = aplicarPaqueteModificaciones (obtenerUltimaVersion file) xlm

-- Ejemplos: (ver def. archivo1 y archivo2 abajo)
-- Main> obtenerUltimaVersion archivo1
-- "dato"
-- Main> obtenerUltimaVersion archivo2
-- "ddato"

-- Ejercicio 4/8
cantVersiones :: Archivo -> Integer
cantVersiones ArchivoVacio = 0
cantVersiones (NuevaVersion _ file) = 1 + cantVersiones file

-- Ejemplos:
-- Main> cantVersiones archivo1
-- 1
-- Main> cantVersiones archivo2
-- 2

-- Ejercicio 5/8
obtenerVersion :: Integer -> Archivo -> String
obtenerVersion 0 _ = ""
obtenerVersion n (NuevaVersion xlm f0)  | n == cantVersiones f1 = obtenerUltimaVersion f1
                                        | otherwise = obtenerVersion n f0
                                        where f1 = NuevaVersion xlm f0

-- Ejemplos:
-- Main> obtenerVersion 1 archivo2
-- "dato"

-- Ejercicio 6/8
levenshtein :: String -> String -> Integer --PaqueteModificaciones
levenshtein str1 str2   | min (length str1) (length str2) == 0 = max (len str1) (len str2)
                        | last str1 == last str2 = levenshtein (init str1) (init str2)
                        | otherwise = minimum [1 + levenshtein (init str1) str2,
                                               1 + levenshtein str1 (init str2),
                                               1 + levenshtein (init str1) (init str2)]

-- Ejemplos:
-- Main> levenshtein "auto" "automata"
-- 4

-- Ejercicio 7/8
levenshtein2 :: String -> String -> PaqueteModificaciones
levenshtein2 str1 str2  | (length str1, length str2) == (0, 0) = []
                        | length str1 == 0 = Insertar 0 (last str2) : levenshtein2 str1 (init str2)
                        | length str2 == 0 = Borrar (len str1) : levenshtein2 (init str1) str2
                        | last str1 == last str2 = levenshtein2 (init str1) (init str2)
                        | otherwise = minLength [Borrar (len str1) : levenshtein2 (init str1) str2,
                                                 Insertar (len str1) (last str2) : levenshtein2 str1 (init str2),
                                                 Substituir (len str1) (last str2) : levenshtein2 (init str1) (init str2)]

minLength :: [PaqueteModificaciones] -> PaqueteModificaciones
minLength (x:xl) | xl == [] = x
                 | length x <= length (minLength xl) = x
                 | otherwise = minLength xl

-- Ejemplos:
-- Main> levenshtein2 "auto" "automata"
-- [Insertar 4 'a',Insertar 4 't',Insertar 4 'a',Insertar 4 'm']

-- Ejercicio 8/8
agregarVersion :: String -> Archivo -> Archivo
agregarVersion "" ArchivoVacio = ArchivoVacio
agregarVersion str file = NuevaVersion (levenshtein2 (obtenerUltimaVersion file) str) file

-- Ejemplos:
-- Main> agregarVersion "dato" archivo2
-- Archivo: dato

-- Funciones provistas por la cátedra

len :: [a] -> Integer
len xs = fromIntegral (length xs)

-- Archivos

archivo1 = NuevaVersion [Insertar 0 'd', Insertar 1 'a', Insertar 2 't',Insertar 3 'o'] ArchivoVacio

archivo2 = NuevaVersion [Insertar 0 'd'] archivo1

archivo3 = NuevaVersion [Substituir 1 'p', Insertar 4 's'] archivo2

archivo4 = NuevaVersion [Insertar 4 'a',Insertar 4 't',Insertar 4 'a',Insertar 4 'm'] archivo3