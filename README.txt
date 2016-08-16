Smaug utiliza Haskell y requiere de la plataforma Haskell
https://www.haskell.org/
https://www.haskell.org/downloads
Descargar haskell platform. La versión minima y la de stack no son las que usé yo al crear esto.

Ir en la línea de comandos a la carpeta del compilador una vez hecho y usar
cabal install --only-dependencies
(Esto "deberia" funcionar sin este comando, en todo caso, las dependencias vienen incluidas con haskell platform según entiendo.)

Luego ejecutar
cabal build

Utilizar
cabal run
Y se entrará en un modo en el que se construyen árboles de statements
(llamar funciones, asignar, etc)

El ejecutable se encuentra en dist/build/smaug
Y si se hace correr como
smaug.exe -c "archivo de entrada" "archivo de salida"

va a compilar el lenguaje a JavaScript.
out.js es una salida de ejemplo para test.sg, ejecutable en ej.html.

Sobre las reglas semánticas, usando a op b op c donde op es un operador binario de comparación no es posible, como generalización de a < b < c de mile.
Revisa si a la izquierda o derecha de un operador + el tipo corresponde a un literal string
Revisa si los simbolos usados en una expresión fueron declarados en el ámbito local o en uno más global.
Revisa que los símbolos declarados no se repitan en el mismo ámbito.

Los "let" requieren de una inicialización válida.
Los for poseen dos formas, una donde se declara un límite superior, y otra donde se declara una expresión.
El parser no requiere que el cuerpo de un statement se encuentre dentro de un par de llaves [] o {} para considerarse válido.
El uso de operaciónes matemáticas se hace a través de la librería Math de javascript, como se ve en test.sg.
Así mismo, no existe print, pero sí document.writeln (como en javascript) y prompt.
test.sg muestra el cálculo aproximado del número de euler junto con statements de flujo de control for, while

No hay manejo de comentarios ni de linea simple y multiple, con el fin de no requerir aún mas lookahead que el que uso para validar statements

El punto de entrada se encuentra en Main.hs
y todos los archivos están bajo src/

La implementación del lenguaje Smaug requiere un entendimiento del patrón de "Monads" de Haskell
Utilizé los siguientes recursos para entender el uso de Parsec:
http://dev.stephendiehl.com/fun/002_parsers.html
http://book.realworldhaskell.org/read/using-parsec.html
y para el lenguaje en sí
http://learnyouahaskell.com/chapters
y para el paradigma que utiliza
https://drboolean.gitbooks.io/mostly-adequate-guide/content/
me ayudó a entender el lenguaje a un nivel más profundo.

El lenguaje se basa únicamente en un arbol binario y usa asociatividad por la izquierda por defecto.
El parser y EBNF se encuentran en Parser.hs y utiliza una tecnica llamada "Parser Combinators" utilizando Parsec
El ADT del lenguaje se encuentra en LangADT
El TreeParser que ejecuta las reglas semánticas se encuentra en TreeParser.hs.
Las reglas de símbolos y ámbito se encuentran en SemanticRules/SymbolsAndScope.hs