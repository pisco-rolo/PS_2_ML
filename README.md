# Problem Set 2
El repositorio contiene las siguientes carpetas:

- `document`: contiene el archivo final en `.pdf` subido a Bloque Neón, pero adicionalmente los `.tex`. 
- `references`: contiene el enunciado y algunos documentos adicionales consultados por el equipo.
- `scripts`: contiene todos los scripts.
- `stores`: contiene la base de datos extraída de la página _web_ de Ignacio Sarmiento. 
- `views`: contiene las figuras y tablas exportadas al documento.

Para ejecutar el código, basta con abrir el archivo `predicting_prices.Rproj` ubicado en la raíz del repositorio. Una vez abierto, la librería `here` permite emplear direcciones relativas independiente de quién esté ejecutando el código. Así, el siguiente paso es abrir el archivo denominado `main.R` en la carpeta `scripts`, y este archivo 'invoca' a _scripts_ auxiliares que generarán los resultados presentados en el documento `.pdf`. 
* *Nota*. En las bases de datos contamos con dos especialmente pesadas. Estas se relacionan con el número de valiacionespor pasajero en TransMilenio, y con el censo de hogares de 2018. Para replicar el código es necesario descargar los datos de los siguientes enlaces de DropBox y adjuntarlos en la carpeta `stores`.
    * https://www.dropbox.com/t/vOs4aIlWuo44VrQJ
    * https://www.dropbox.com/t/L8b7ahb0oc1kz3Kg