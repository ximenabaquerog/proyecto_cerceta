
Este repositorio ha sido creado para el trabajo final de la asignatura de Ecoinformática, en el Máster en  Conservación, 
Gestión y Restauración de la Biodiversidad de la Universidad de Granada. 

Autores:
- Ximena Baquero González
- Eva Bautista Herruzo
- Carlos Guzón García
- Rynn Kerkhove

El repositorio es también un proyecto de RStudio y tiene activado control de versiones con git. La estructura de 
carpetas y archivos es la siguiente: 

- 01_importar_limpiar_datos.Rmd
Documento RMarkdown donde se importan y depuran los datos de censos para producir la tabla final con abundancias por 
año, especie y localidad para los meses invernales. 

- data/ 

	> 2005_2026_series/
Datos mensuales de censos aéreos y terrestres de aves acuáticas; descompresión del .zip descargado de la web del ICTS 
de la EBD para la serie 2005-2026.

	> functional_groups.xlsx
Libro de Excel con la lista de especies estudiadas y su clasificación en grupos funcionales (Almeida et al., 2020;
Hernandez-Possémé et al. enviado). Consta de 2 hojas:  

- "unbalanced_class": clasificación que solo considera "wader_mid" a las especies que forrajean a mayor profundidad
y tienen mayor Body Size Index de las estudidas por Hernandez-Possémé et al., (agrupadas juntas en su figura 2). 
- "balanced_class": clasificación que amplía el grupo "wader_mid" a todas las especies que no aparecen agrupadas en 
en los valores mínimos de BSI-profundidad, equilibrando el tamaño de "wader_mid" y "wader_shall" en caso de que sea
relevante para los análisis.
	
	> functional_groups_unbalanced.csv
Tabla derivada de la hoja "unbalanced_class", para trabajar en el script "add.functional_groups.R".

- outputs/
	
	> species_abundance_yearly.csv
Tabla de abundancias por año, especie y localidad. 

- scripts/
	
	> add_functional_groups.R
Script donde se añade la información de grupos funcionales a la tabla de abundancias.


