* ANTIOQUIAS - 00
* BUILDING FINAL  DATA
* DIRECTORIES
* PLEASE CHANGE THE DIREC


* LAURA
if "`c(username)'" == "user" {
    
	global path "C:/Users/user/OneDrive - Universidad EAFIT/Antioquias"
	global rawdata "$path/01_Data/00_Inputs"
	global scripts "$path/02_Code"
	global data "$path/01_Data/01_Derived"
	global output "$path/03_Outputs"
} 

cd "$data"
ls

local direccion "20250505_SEGURIDAD_SALUD_DEFICT_VIVIENDA 20250506_DEyC 20250506_ECV 20250506_ECV_ADICIONALES 20250506_EDUCACION i_datalake_ges_pub_dicc infraestructura_municipios_dicc IRCA_decreto_y_resolucion_dicc IRCA_dicc natalidad_dicc poblacion_municipal_total_2023_dicc suicidios_e_intentos_medias_dicc"

foreach x in `direccion' {
    import excel using "`x'.xlsx", sheet("Data") firstrow allstring clear
    save "`x'.dta", replace
}

use "20250506_ECV.dta", clear
gen codigo_mod = "0" + ind_mpio
drop ind_mpio
rename codigo_mod ind_mpio 
save "20250506_ECV.dta", replace
use "20250506_ECV_ADICIONALES.dta",clear
gen codigo_mod = "0" + ind_mpio
drop ind_mpio
rename codigo_mod ind_mpio 
save "20250506_ECV_ADICIONALES.dta",replace
use "i_datalake_ges_pub_dicc.dta", clear
gen codigo_mod = "0" + ind_mpio
drop ind_mpio
rename codigo_mod ind_mpio 
save "i_datalake_ges_pub_dicc.dta",replace
use "infraestructura_municipios_dicc.dta", clear
gen codigo_mod = "0" + ind_mpio
drop ind_mpio
rename codigo_mod ind_mpio 
save "infraestructura_municipios_dicc.dta",replace
use "IRCA_decreto_y_resolucion_dicc.dta", clear
gen codigo_mod = "0" + ind_mpio
drop ind_mpio
rename codigo_mod ind_mpio 
save "IRCA_decreto_y_resolucion_dicc.dta",replace
use "IRCA_dicc.dta", replace
gen codigo_mod = "0" + ind_mpio
drop ind_mpio
rename codigo_mod ind_mpio 
save "IRCA_dicc.dta",replace
use "natalidad_dicc.dta", replace
gen codigo_mod = "0" + ind_mpio
drop ind_mpio
rename codigo_mod ind_mpio 
save "natalidad_dicc.dta",replace
use "poblacion_municipal_total_2023_dicc.dta", replace
gen codigo_mod = "0" + ind_mpio
drop ind_mpio
rename codigo_mod ind_mpio 
save "poblacion_municipal_total_2023_dicc.dta",replace
* Merge 
use "20250505_SEGURIDAD_SALUD_DEFICT_VIVIENDA.dta", replace
merge 1:1 ind_mpio nvl_label using "20250505_SEGURIDAD_SALUD_DEFICT_VIVIENDA.dta"
drop _merge
merge 1:1 ind_mpio using "20250506_DEyC.dta"
drop _merge
merge 1:1 ind_mpio using "20250506_ECV.dta"
drop _merge
merge 1:1 ind_mpio using "20250506_ECV_ADICIONALES.dta"
drop _merge
merge 1:1 ind_mpio using "20250506_EDUCACION.dta"
drop _merge
merge 1:1 ind_mpio using "i_datalake_ges_pub_dicc.dta"
drop _merge
merge 1:1 ind_mpio using "infraestructura_municipios_dicc.dta"
drop _merge
merge 1:1 ind_mpio using "IRCA_decreto_y_resolucion_dicc.dta"
drop _merge
merge 1:1 ind_mpio using "IRCA_dicc.dta"
drop _merge
merge 1:1 ind_mpio using "natalidad_dicc.dta"
drop _merge
merge 1:1 ind_mpio using "poblacion_municipal_total_2023_dicc.dta"
drop _merge
merge 1:1 ind_mpio using "suicidios_e_intentos_medias_dicc.dta"


drop  _merge
replace TS_total = "NA" if TS_total == ""
replace TS_15_29 = "NA" if TS_15_29 == ""
replace TS_30_59 = "NA" if TS_30_59 == ""
replace TS_15_64 = "NA" if TS_15_64 == ""
replace TS_hombres = "NA" if TS_hombres == ""
replace TS_mujeres = "NA" if TS_mujeres == ""
replace TIS_total = "NA" if TIS_total == ""
replace TIS_15_29 = "NA" if TIS_15_29 == ""
replace TIS_30_59 = "NA" if TIS_30_59 == ""
replace TIS_15_64 = "NA" if TIS_15_64 == ""
replace TIS_hombres = "NA" if TIS_hombres == ""	
replace TIS_mujeres = "NA" if TIS_mujeres == ""
* Save Final Ind data
save "$data/01_final_data.dta"




