-- *****************************************************************************

-- Universidad Simón Bolívar.
-- Departamento de Computación y Tecnología de la Información.
-- Laboratorio de Lenguajes de Programación.
-- Abril - Julio 2016.
-- Alumnos:
-- 			Cinthya Ramos.    09-11237.
-- 			Edward Fernández. 10-11121.

-- Descripción:
-- 		Archivo en el que se definirán las diferentes funciones a utilizar en la 
--      implementación del asistente de pruebas.

-- *****************************************************************************

import ValidTerms -- Se importan los términos válidos

main = do
	--putStrLn "Probando..."
	--putChar 'A'
	let pruebaImpresion =  show (a \/ b) -- Se almacena la versión "String" en pruebaImpresion.
	putStrLn (pruebaImpresion ++ " ")    -- Se ejecuta la acción correspondiente.
