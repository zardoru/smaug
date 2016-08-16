Smaug utiliza Haskell y requiere de la plataforma Haskell
https://www.haskell.org/
https://www.haskell.org/downloads
Descargar haskell platform. La versi�n minima y la de stack no son las que us� yo al crear esto.

Ir en la l�nea de comandos a la carpeta del compilador una vez hecho y usar
cabal install --only-dependencies
(Esto "deberia" funcionar sin este comando, en todo caso, las dependencias vienen incluidas con haskell platform seg�n entiendo.)

Luego ejecutar
cabal build

Utilizar
cabal run
Y se entrar� en un modo en el que se construyen �rboles de statements
(llamar funciones, asignar, etc)

El ejecutable se encuentra en dist/build/smaug
Y si se hace correr como
smaug.exe -c "archivo de entrada" "archivo de salida"

va a compilar el lenguaje a JavaScript.
out.js es una salida de ejemplo para test.sg, ejecutable en ej.html.

Sobre las reglas sem�nticas, usando a op b op c donde op es un operador binario de comparaci�n no es posible, como generalizaci�n de a < b < c de mile.
Revisa si a la izquierda o derecha de un operador + el tipo corresponde a un literal string
Revisa si los simbolos usados en una expresi�n fueron declarados en el �mbito local o en uno m�s global.
Revisa que los s�mbolos declarados no se repitan en el mismo �mbito.

Los "let" requieren de una inicializaci�n v�lida.
Los for poseen dos formas, una donde se declara un l�mite superior, y otra donde se declara una expresi�n.
El parser no requiere que el cuerpo de un statement se encuentre dentro de un par de llaves [] o {} para considerarse v�lido.
El uso de operaci�nes matem�ticas se hace a trav�s de la librer�a Math de javascript, como se ve en test.sg.
As� mismo, no existe print, pero s� document.writeln (como en javascript) y prompt.
test.sg muestra el c�lculo aproximado del n�mero de euler junto con statements de flujo de control for, while

No hay manejo de comentarios ni de linea simple y multiple, con el fin de no requerir a�n mas lookahead que el que uso para validar statements

El punto de entrada se encuentra en Main.hs
y todos los archivos est�n bajo src/

La implementaci�n del lenguaje Smaug requiere un entendimiento del patr�n de "Monads" de Haskell
Utiliz� los siguientes recursos para entender el uso de Parsec:
http://dev.stephendiehl.com/fun/002_parsers.html
http://book.realworldhaskell.org/read/using-parsec.html
y para el lenguaje en s�
http://learnyouahaskell.com/chapters
y para el paradigma que utiliza
https://drboolean.gitbooks.io/mostly-adequate-guide/content/
me ayud� a entender el lenguaje a un nivel m�s profundo.

El lenguaje se basa �nicamente en un arbol binario y usa asociatividad por la izquierda por defecto.
El parser y EBNF se encuentran en Parser.hs y utiliza una tecnica llamada "Parser Combinators" utilizando Parsec
El ADT del lenguaje se encuentra en LangADT
El TreeParser que ejecuta las reglas sem�nticas se encuentra en TreeParser.hs.
Las reglas de s�mbolos y �mbito se encuentran en SemanticRules/SymbolsAndScope.hs