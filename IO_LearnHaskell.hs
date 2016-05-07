-- La función . es composición.
-- La función words agarra un string y lo convierte a lista.
-- La función unwords agarra una lista y la pasa a string


-- Con IO hay que compilar el programa. Para ello hacemos:
-- 			ghc --make <nombre_archivo>
-- Luego se generará un ejecutable y para poder ejecutarlo: ./<nombre_archivo>

-- También puedo usar runhaskell <nombre_archivo>.hs y con esto se compila y ejecuta
-- automáticamente.

-- ..............................................................................

-- Esto se puede descomentar:
--main = putStrLn "Hello World!" -- Se definió la función main. Dentro de ella se 
          						 -- llama la función putStrLn con el parámetro String.
-- ..............................................................................


-- La función putStrLn toma una cadena y devuelve una acción IO que devuelve un
-- tipo () (es decir, la tupla vacía, también conocida como unidad).

-- Una acción IO se ejecutará cuando le damos el nombre main y se ejecuta el programa.

-- ..............................................................................

-- Esto se puede descomentar:
-- main = do 
--	 putStrLn "Hello, what's your name?"
--	 name <- getLine  -- Me permite pedir por consola.
--	 putStrLn ("Hey " ++ name ++ ", you rock!")
-- ..............................................................................

-- En un bloque do, la última acción no puede ser ligada.

-- Para obtener el valor contenido dentro de una acción IO, tiene que estar ligado
-- a un nombre con <- dentro de otra acción IO.

-- putStr: parecido putStrLn ya que toma una cadena y realiza una accn PERO no hace
--  	   un salto de línea.
-- putChar: toma un carácter y devuelve una acción IO que lo imprimirá por la terminal.


