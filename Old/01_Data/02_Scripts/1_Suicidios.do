********************************************************************************
******************* Do File (1)  Suicidios: Intentos de suicidio****************
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

* Data
import excel "$rawdata/Datos_2023_356.xlsx", firstrow clear 
keep if (Pais_ocurrencia == "COLOMBIA" & COD_DPTO_O == "05")
keep ANO EDAD SEXO COD_DPTO_O COD_MUN_O Municipio_ocurrencia
gen CODIGO_MUNICIPIO = COD_DPTO_O +  COD_MUN_O

* Merge 
mmerge CODIGO_MUNICIPIO using "$data/codigos_dane.dta"
drop if _merge != 3
drop _merge Municipio_ocurrencia
gen casos = 1


* # de intentos de suicidio por municipio
preserve
* Total
collapse (sum) casos, by(CODIGO_MUNICIPIO NOMBRE_MPIO)
gen id_data = 1
gen variable = "saldo"
gen id_nivel = "municipal"
gen id_time = "1"
gen time = "2023"
gen ct_type = "ninguno"
export delimited using "$data/intento_suicidio_2023_saldo", replace
restore


* % de intentos de suicidio por municipio
preserve
destring EDAD, replace
keep if (EDAD > 15 & EDAD <=64)
collapse (sum) casos, by(CODIGO_MUNICIPIO NOMBRE_MPIO)
mmerge CODIGO_MUNICIPIO using "$data/poblacion_total_edad.dta" 
destring edad_*, replace
gen edad_15_64 = edad_15_19 + edad_20_24 + edad_25_29 + edad_30_34 + edad_35_39 + edad_40_44 + edad_45_49 + edad_50_54 + edad_55_59 + edad_60_64 
keep CODIGO_MUNICIPIO NOMBRE_MPIO casos NOMBRE_DEPTO edad_15_64 _merge
replace casos = 0 if casos == .
drop _merge
gen porcentaje = (casos/edad_15_64)*100
rename porcentaje porcentaje_suicidio_15_64 
drop edad_15_64 casos
export delimited using "$data/intento_suicidio_porcentaje_1564", replace
restore

preserve
destring EDAD, replace
collapse (sum) casos, by(CODIGO_MUNICIPIO NOMBRE_MPIO)
mmerge CODIGO_MUNICIPIO using "$data/poblacion_total_edad.dta" 
destring Total*, replace
keep CODIGO_MUNICIPIO NOMBRE_MPIO casos NOMBRE_DEPTO Total _merge
replace casos = 0 if casos == .
drop _merge
gen porcentaje_suicidio_total = (casos/Total)*100
drop Total
export delimited using "$data/intento_suicidio_2023_porcentaje_total", replace
restore


* menores de 5 - 14
preserve
destring EDAD, replace
keep if EDAD < = 14
collapse (sum) casos, by(CODIGO_MUNICIPIO NOMBRE_MPIO)
mmerge CODIGO_MUNICIPIO using "$data/poblacion_total_edad.dta" 
destring edad_*, replace
gen edad_514 = edad_5_9 +edad_10_14
keep CODIGO_MUNICIPIO NOMBRE_MPIO casos NOMBRE_DEPTO edad_514 _merge
replace casos = 0 if casos == .
drop _merge
gen porcentaje_suicidio_514 = (casos/edad_514)*100
drop edad_514
export delimited using "$data/intento_suicidio_2023_porcentaje_514", replace
restore

*15-29
preserve
destring EDAD, replace
keep if (EDAD >14 & EDAD <=29)
collapse (sum) casos, by(CODIGO_MUNICIPIO NOMBRE_MPIO)
mmerge CODIGO_MUNICIPIO using "$data/poblacion_total_edad.dta" 
destring edad_*, replace
gen edad_1529 = edad_15_19 +edad_20_24 + edad_25_29
keep CODIGO_MUNICIPIO NOMBRE_MPIO casos NOMBRE_DEPTO edad_1529 
replace casos = 0 if casos == .
gen porcentaje_suicidio_1529 = (casos/edad_1529)*100
export delimited using "$data/intento_suicidio_2023_porcentaje_1529", replace
restore

* 30-59
preserve
destring EDAD, replace
keep if (EDAD >29 & EDAD <=59)
collapse (sum) casos, by(CODIGO_MUNICIPIO NOMBRE_MPIO)
mmerge CODIGO_MUNICIPIO using "$data/poblacion_total_edad.dta" 
destring edad_*, replace
gen edad_3059 = edad_30_34 + edad_35_39 + edad_40_44 + edad_45_49 + edad_50_54 + edad_55_59
keep CODIGO_MUNICIPIO NOMBRE_MPIO casos NOMBRE_DEPTO edad_3059
replace casos = 0 if casos == .
gen porcentaje_suicidio_3059 = (casos/edad_3059)*100
sort porcentaje_suicidio_3059
export delimited using "$data/intento_suicidio_2023_porcentaje_3059", replace
restore

*60 
preserve
destring EDAD, replace
keep if (EDAD >= 60)
collapse (sum) casos, by(CODIGO_MUNICIPIO NOMBRE_MPIO)
mmerge CODIGO_MUNICIPIO using "$data/poblacion_total_edad.dta" 
destring edad_*, replace
gen edad_60 = edad_60_64 + edad_65_69 + edad_70_74 + edad_75_79 + edad_80_mas
keep CODIGO_MUNICIPIO NOMBRE_MPIO casos NOMBRE_DEPTO edad_60
replace casos = 0 if casos == .
gen porcentaje_suicidio_60 = (casos/edad_60)*100
sort porcentaje_suicidio_60
export delimited using "$data/intento_suicidio_2023_porcentaje_60", replace
restore

* Género 
* Mujeres
preserve
keep if SEXO == "F"
collapse (sum) casos, by(CODIGO_MUNICIPIO NOMBRE_MPIO)
mmerge CODIGO_MUNICIPIO using "$data/poblacion_total_genero.dta" 
destring Total*, replace
keep CODIGO_MUNICIPIO NOMBRE_MPIO casos NOMBRE_DEPTO Total_Mujeres
replace casos = 0 if casos == .
gen porcentaje_suicidio_mujeres = (casos/Total_Mujeres)*100
sort porcentaje_suicidio_mujeres
drop Total_Mujeres
export delimited using "$data/intento_suicidio_2023_porcentaje_mujeres", replace
restore

* Género 
preserve
keep if SEXO == "M"
collapse (sum) casos, by(CODIGO_MUNICIPIO NOMBRE_MPIO)
mmerge CODIGO_MUNICIPIO using "$data/poblacion_total_genero.dta" 
destring Total*, replace
keep CODIGO_MUNICIPIO NOMBRE_MPIO casos NOMBRE_DEPTO Total_Hombres
replace casos = 0 if casos == .
gen porcentaje_suicidio_hombres = (casos/Total_Hombres)*100
sort porcentaje_suicidio_hombres
drop Total_Hombres
export delimited using "$data/intento_suicidio_2023_porcentaje_hombres", replace
restore








