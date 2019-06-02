# Group 4 Assignment - Data Driven Security

Group Assignment base repository for the Data Driven Security subject of the [CyberSecurity Management Msc](https://www.talent.upc.edu/ing/professionals/presentacio/codi/221101/cybersecurity-management/).

Group 4 of 9th editon composed by:
  @dero1982   Horacio
  @igna89     Ignasi

## Analisis de Botnet mediante el uso de la información proporcionada por Feodo

### Requerimientos

  - Dataset de las IPs bloquedas y disponibles en el siguiente fichero: https://feodotracker.abuse.ch/downloads/ipblocklist.csv.
  - Uso de Maxmind para asociar rangos de IP con Latitud Longitud para poder "pintar" los mapas.
  
  
### Descripción del proyecto

Se ha creado un paquete con diferentes funciones, utilizando el contenido de las clases e investigación propia, para intentar dar forma a los datos originales.

Las funciones más importantes creadas en el paquete son las siguientes:

analysis.df

maxmindg4.df

Mediante estas dos funciones (hay más que se utilizan internamente), se ha obtenido el dataframe y ajustado de manera de obtener datos relevantes para analizar.

### Objetivo
La intención del proyecto es prever cuándo y que tipo de malware se utilizara para el ataque en base a la información obtenida.

### Obtención de información
Como se ha comentado anteriormente, con los datos de Feodo y Maxmind, se han unido para poder realizar un análisis y relacion de los diferentes mawlares, ubicaciones y espacios temporales, en los que se produjeron, de manera de intentar preveer futuros ataques.

Cabe destacar que los datos de Feodo, se actualizan cada 5' minutos.

### Liempieza y transformación
Como primera tarea, todo y que se partia de un dataset bastante pulido, se ha tenido que realizar una limpieza del csv para poder obviar la cabecera y última fila, que contenian valores irrelevantes.

Una vez obtenido el dataset inicial, se han tenido que unir con la base de datos de MaxMind, para poder obtener información de la ubicación de los diferentes ataques. 

### Analisis de datos
Una vez analizados los datos se ha visto que:
  El malware más utilizado en los ataques es Heodo.
  Los días más frecuentes para este tipo de ataques se concentra entre semana, siendo los más destacados martes y jueves. Nos hubiese gustado poder profundizar un poco más sobre este punto, para intentar ver no solo el día, sino si se podía hacer alguna inferencia más con la fecha en concreto en la que se produjeron los ataques (Ej: Fechas especiales, Noticias, etc)
  Analizando un poco más los datos y al disponer de los países, se ha podido ver que la distribución de Malware, parece estar relacionada también con el destino. Por ejemplo Heodo, se utilizó mayoritariamente en los ataques realizados en Sudamerica, Norte America y Asia, mientras que TrickBot se utilizó en su mayoría en Europa.

### Results / Conclusions.
Los resultados se pueden ver en el fichero markdown que se genera.
