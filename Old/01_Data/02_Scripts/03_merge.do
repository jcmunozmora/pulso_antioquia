********************************************************************************
******************* Do File (1)  Suicidios: Intentos de suicidio****************
***************************** SIVIGILA - 2023 **********************************
********************************************************************************

clear all 
set more off

*** Directories

** Laura

cd "C:/Users/user/OneDrive - Universidad EAFIT/Antioquias/03_Indicadores"
local direccion "20250506_ECV_ADICIONALES 20250506_ECV 20250506_EDUCACION"


foreach x in `direccion'{
   import excel  "`x'.xlsx", firstrow allstring clear
	save "`x'.dta", replace 
	}

	
		
*local direccion "ACLED_events_municipal_2023_total ACLED_fatilities_municipal_2023_total"
*local i=1	
*	foreach x in `direccion'{
*		import delimited "`x'.csv", stringcols(_all) clear 
*		replace base_datos=base_datos+"_"+"`i'"
*		save "`x'.dta", replace 
*		local i=`i'+1
*	}

use "20250506_ECV_ADICIONALES.dta", clear
merge 1:1 ind_mpio using "20250506_ECV.dta"	
drop _merge
preserve
use "20250506_EDUCACION.dta", clear
gen aux  = substr(ind_mpio, 2, 4)
drop ind_mpio
rename aux ind_mpio
save "20250506_EDUCACION.dta",replace
restore
merge 1:1 ind_mpio using "20250506_EDUCACION.dta"	
drop _merge

preserve
import excel "i_datalake_ges_pub_dicc", describe
import excel "i_datalake_ges_pub_dicc.xlsx", sheet("Data") firstrow clear
tostring ind_mpio, replace
save "i_datalake_ges_pub_dicc.dta",replace
restore
merge 1:1 ind_mpio using "i_datalake_ges_pub_dicc.dta"	
drop _merge

preserve
import excel "IRCA_decreto_y_resolucion_dicc" , describe
import excel "IRCA_decreto_y_resolucion_dicc", sheet("Data") firstrow clear
tostring ind_mpio, replace
save "IRCA_decreto_y_resolucion_dicc.dta", replace
restore
merge 1:1 ind_mpio using "IRCA_decreto_y_resolucion_dicc.dta"	
drop _merge

preserve
import excel "IRCA_dicc", describe
import excel "IRCA_dicc", sheet("Data") firstrow clear
tostring ind_mpio, replace
save "IRCA_dicc.dta", replace
restore

merge 1:1 ind_mpio using "IRCA_dicc.dta"	
drop _merge
drop time_*

preserve
import excel "poblacion_municipal_total_2025_dicc", describe
import excel "poblacion_municipal_total_2025_dicc", sheet("Data") firstrow clear
tostring ind_mpio, replace
save "poblacion_municipal_total_2025_dicc.dta", replace
restore
merge 1:1 ind_mpio using "poblacion_municipal_total_2025_dicc.dta"	
drop _merge

export excel using "datos_consolidados.xlsx", firstrow(variables) replace






