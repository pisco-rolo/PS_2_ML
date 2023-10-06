# Problem Set 2
El repositorio contiene las siguientes carpetas:

- `document`: contiene el archivo final en `.pdf` subido a Bloque Neón, pero adicionalmente los `.tex`. 
- `references`: contiene el enunciado y algunos documentos adicionales consultados por el equipo.
- `scripts`: contiene todos los scripts.
- `stores`: contiene la base de datos extraída de la página _web_ de Ignacio Sarmiento. 
- `views`: contiene las figuras y tablas exportadas al documento.

Para ejecutar el código, basta con abrir el archivo `predicting_prices.Rproj` ubicado en la raíz del repositorio. Una vez abierto, la librería `here` permite emplear direcciones relativas independiente de quién esté ejecutando el código. Así, el siguiente paso es abrir el archivo denominado `main.R` en la carpeta `scripts`, y este archivo 'invoca' a _scripts_ auxiliares que generarán los resultados presentados en el documento `.pdf`. 
* _Nota_. El _web scrapping_ es demorado, por lo cual hay un parámetro denominado `primeraVez` que definimos como `FALSE`. Si desea volver a realizar el _web scrapping_, basta con definirlo como `TRUE`. Sin embargo, al definirse como `FALSE`, el código va a cargar la base de datos almacenada en `stores`.
