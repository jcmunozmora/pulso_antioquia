********************************************************************************
************************************** AUX *************************************
***************************** SIVIGILA - 2023 **********************************
********************************************************************************

clear all 
set more off

*** Directories

** Laura

if "`c(username)'" == "user" {
    
	//cd "C:/Users/brian/OneDrive - Universidad EAFIT/Documentos/"
	global path "C:/Users/user/OneDrive - Universidad EAFIT/Antioquias"
	global rawdata "$path/00_Inputs"
	global scripts "$path/02_Scripts"
	global data "$path/01_Processed_Data"
}

preserve
* Codigos Dane
import excel "$rawdata/subregiones.xls", firstrow clear
replace NOMBRE_DEPTO = NOMBRE_DEPTO[_n-1] if missing(NOMBRE_DEPTO)
drop PROVINCIA Total Nombre
drop if regexm(NOMBRE_DEPTO, "^Total")
keep if NOMBRE_DEPTO == "ANTIOQUIA"
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "Á", "A", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "É", "E", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "Í", "I", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "Ó", "O", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "Ú", "U", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "Ñ","N", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "Ü","U", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO,"a","A", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO,"i","I", .)
replace NOMBRE_MPIO = "EL CARMEN DE VIBORAL" if NOMBRE_MPIO == "CARMEN DE VIBORAL"
replace NOMBRE_MPIO = "EL PENOL" if NOMBRE_MPIO == "PENOL"
replace NOMBRE_MPIO = "EL RETIRO" if NOMBRE_MPIO == "RETIRO"
replace NOMBRE_MPIO = "EL SANTUARIO" if NOMBRE_MPIO == "SANTUARIO"
replace NOMBRE_MPIO = "DONMATIAS" if NOMBRE_MPIO == "DON MATIAS"
replace NOMBRE_MPIO = "SAN ANDRES DE CUERQUIA" if NOMBRE_MPIO == "SAN ANDRES"
replace NOMBRE_MPIO = "SAN PEDRO DE LOS MILAGROS" if NOMBRE_MPIO == "SAN PEDRO"
replace NOMBRE_MPIO = "SANTA FE DE ANTIOQUIA" if NOMBRE_MPIO == "SANTAFE DE ANTIOQUIA"
replace NOMBRE_MPIO = "SANTA ROSA DE OSOS" if NOMBRE_MPIO == "SANTA ROSA de osos" 

save "$data/codigos_dane.dta",replace  
restore

* Data: Poblacion

import excel "$rawdata/POBLACION2023.xls", sheet("Zona-Sexo") clear
drop if B == ""
drop E F G H I J
replace B = "Total" in 2
replace C = "Total_Hombres" in 2
replace D = "Total_mujeres" in 2
replace A = "Departamento" in 1
drop if C == ""
drop in 2
rename A NOMBRE_MPIO
rename B Total
rename C Total_Hombres
rename D Total_Mujeres
drop in 1
drop if inlist(NOMBRE_MPIO, "MAGDALENA MEDIO","BAJO CAUCA","URABA","NORDESTE","OCCIDENTE","NORTE","ORIENTE","SUROESTE","VALLE DE ABURRA")
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "á", "a", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "é", "e", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "í", "i", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ó", "o", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ú", "u", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ñ", "n", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ü", "u", .)

replace NOMBRE_MPIO = upper(NOMBRE_MPIO)
* Merge (Data = Coodigos dane + poblacion)
mmerge NOMBRE_MPIO using "$data/codigos_dane.dta"
drop _merge
order NOMBRE_DEPTO NOMBRE_MPIO CODIGO_MUNICIPIO Total Total_Hombres Total_Mujeres

* Guardar base de poblacion total 
save "$data/poblacion_total_genero.dta", replace

********************************************************************************
********************************************************************************
***************************DATOS POR EDAD***************************************
********************************************************************************
********************************************************************************

clear all 
set more off
import excel "$rawdata/POBLACION2023.xls", sheet("GrupoEdadTotal") clear
drop in 1
drop if A == "TOTAL DEPARTAMENTO"
drop if inlist(A, "MAGDALENA MEDIO","BAJO CAUCA","URABA","NORDESTE","OCCIDENTE","NORTE","ORIENTE","SUROESTE","VALLE DE ABURRA")
drop if B== ""
replace A = "NOMBRE_MPIO" in 1

local nombres "A B C D E F G H I J K L M N O P Q R S"
local nuevos_nombres "NOMBRE_MPIO Total edad_0_4 edad_5_9 edad_10_14 edad_15_19 edad_20_24 edad_25_29 edad_30_34 edad_35_39 edad_40_44 edad_45_49 edad_50_54 edad_55_59 edad_60_64 edad_65_69 edad_70_74 edad_75_79 edad_80_mas"

local i = 1
foreach old of local nombres {
    local new : word `i' of `nuevos_nombres'
    rename `old' `new'
    local i = `i' + 1
}

drop in 1

replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "á", "a", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "é", "e", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "í", "i", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ó", "o", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ú", "u", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ñ", "n", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ü", "u", .)

replace NOMBRE_MPIO = upper(NOMBRE_MPIO)
* Merge (Data = Coodigos dane + poblacion por edad)
mmerge NOMBRE_MPIO using "$data/codigos_dane.dta"
drop _merge
order NOMBRE_DEPTO NOMBRE_MPIO CODIGO_MUNICIPIO Total

* Guardar base de poblacion total 
save "$data/poblacion_total_edad.dta", replace

********************************************************************************
********************************************************************************
*********************DATOS POR EDAD - Mujeres***********************************
********************************************************************************
********************************************************************************

clear all 
set more off
import excel "$rawdata/POBLACION2023.xls", sheet("GrupoEdadMujeres") clear
drop in 1
drop if A == "TOTAL DEPARTAMENTO"
drop if inlist(A, "MAGDALENA MEDIO","BAJO CAUCA","URABA","NORDESTE","OCCIDENTE","NORTE","ORIENTE","SUROESTE","VALLE DE ABURRA")
drop if B== ""
replace A = "NOMBRE_MPIO" in 1

local nombres "A B C D E F G H I J K L M N O P Q R S"
local nuevos_nombres "NOMBRE_MPIO Total edad_0_4 edad_5_9 edad_10_14 edad_15_19 edad_20_24 edad_25_29 edad_30_34 edad_35_39 edad_40_44 edad_45_49 edad_50_54 edad_55_59 edad_60_64 edad_65_69 edad_70_74 edad_75_79 edad_80_mas"

local i = 1
foreach old of local nombres {
    local new : word `i' of `nuevos_nombres'
    rename `old' `new'
    local i = `i' + 1
}

drop in 1

replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "á", "a", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "é", "e", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "í", "i", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ó", "o", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ú", "u", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ñ", "n", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ü", "u", .)

replace NOMBRE_MPIO = upper(NOMBRE_MPIO)
* Merge (Data = Coodigos dane + poblacion por edad)
mmerge NOMBRE_MPIO using "$data/codigos_dane.dta"
drop _merge
order NOMBRE_DEPTO NOMBRE_MPIO CODIGO_MUNICIPIO Total

* Guardar base de poblacion total 
save "$data/poblacion_total_edad_mujeres.dta", replace

********************************************************************************
********************************************************************************
*********************DATOS POR EDAD - Hombres***********************************
********************************************************************************
********************************************************************************
clear all 
set more off
import excel "$rawdata/POBLACION2023.xls", sheet("GrupoEdadHombres") clear
drop in 1
drop if A == "TOTAL DEPARTAMENTO"
drop if inlist(A, "MAGDALENA MEDIO","BAJO CAUCA","URABA","NORDESTE","OCCIDENTE","NORTE","ORIENTE","SUROESTE","VALLE DE ABURRA")
drop if B== ""
replace A = "NOMBRE_MPIO" in 1

local nombres "A B C D E F G H I J K L M N O P Q R S"
local nuevos_nombres "NOMBRE_MPIO Total edad_0_4 edad_5_9 edad_10_14 edad_15_19 edad_20_24 edad_25_29 edad_30_34 edad_35_39 edad_40_44 edad_45_49 edad_50_54 edad_55_59 edad_60_64 edad_65_69 edad_70_74 edad_75_79 edad_80_mas"

local i = 1
foreach old of local nombres {
    local new : word `i' of `nuevos_nombres'
    rename `old' `new'
    local i = `i' + 1
}

drop in 1

replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "á", "a", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "é", "e", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "í", "i", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ó", "o", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ú", "u", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ñ", "n", .)
replace NOMBRE_MPIO = subinstr(NOMBRE_MPIO, "ü", "u", .)

replace NOMBRE_MPIO = upper(NOMBRE_MPIO)
* Merge (Data = Coodigos dane + poblacion por edad)
mmerge NOMBRE_MPIO using "$data/codigos_dane.dta"
drop _merge
order NOMBRE_DEPTO NOMBRE_MPIO CODIGO_MUNICIPIO Total

* Guardar base de poblacion total 
save "$data/poblacion_total_edad_hombres.dta", replace






