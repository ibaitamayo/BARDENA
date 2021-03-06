Este paquete por ahora hace varias cosas:
1.Utiliza los códigos CIE9, CIE10 de los ingresos del conjunto básico mínimo de datos y los códigos CIAP2 de los diagnósticos de primaria y computa el índice Charlson y Elixhauser devolviendo además la primera fecha en la que se introdujo un código relacionado con cada grupo de patologías.


Para este punto utiliza los siguientes códigos, extraidos del paquete comorbidity:
Pretendemos realizar y actualizar los datos del  primer punto mediante los históricos de pacientes codificados en CIAP2.


Nuestros códigos específicos para el cálculo el charlson:
ICD-9-CM codes
The ICD-9-CM codes used by comorbidity to compute the Charlson comorbidity index are:
o	Myocardial infarction: 410.x, 412.x
o	Congestive heart failure: 398.91, 402.01, 402.11, 402.91, 404.01, 404.03, 404.11, 404.13, 404.91, 404.93, 425.4 - 425.9, 428.x
o	Peripheral vascular disease: 093.0, 437.3, 440.x, 441.x, 443.1 - 443.9, 447.1, 557.1, 557.9, V43.4
o	Cerebrovascular disease: 362.34, 430.x - 438.x
o	Dementia: 290.x, 294.1, 331.2
o	Chronic pulmonary disease: 416.8, 416.9, 490.x - 505.x, 506.4, 508.1, 508.8
o	Rheumatic disease: 446.5, 710.0 - 710.4, 714.0 - 714.2, 714.8, 725.x
o	Peptic ulcer disease: 531.x - 534.x
o	Mild liver disease: 070.22, 070.23, 070.32, 070.33, 070.44, 070.54, 070.6, 070.9, 570.x, 571.x, 573.3, 573.4, 573.8, 573.9, V42.7
o	Diabetes without chronic complication: 250.0 - 250.3, 250.8, 250.9
o	Diabetes with chronic complication: 250.4 - 250.7
o	Hemiplegia or paraplegia: 334.1, 342.x, 343.x, 344.0 - 344.6, 344.9
o	Renal disease: 403.01, 403.11, 403.91, 404.02, 404.03, 404.12, 404.13, 404.92, 404.93, 582.x, 583.0 - 583.7, 585.x, 586.x, 588.0, V42.0, V45.1, V56.x
o	Any malignancy, including lymphoma and leukaemia, except malignant neoplasm of skin: 140.x - 172.x, 174.x - 195.8, 200.x - 208.x, 238.6
o	Moderate or severe liver disease: 456.0 - 456.2, 572.2- 572.8
o	Metastatic solid tumour: 196.x - 199.x
o	AIDS/HIV: 042.x - 044.x
There is a difference between codes reported above for peripheral vascular disease and the paper by Quan et al.; the code 47.1 reported in the paper is replaced by 447.1, as it is likely a typo. See here and here for more details.
ICD-10 codes
The ICD-10 codes used by comorbidity to compute the Charlson comorbidity index are:
o	Myocardial infarction: I21.x, I22.x, I25.2
o	Congestive heart failure: I09.9, I11.0, I13.0, I13.2, I25.5, I42.0, I42.5 - I42.9, I43.x, I50.x, P29.0
o	Peripheral vascular disease: I70.x, I71.x, I73.1, I73.8, I73.9, I77.1, I79.0, I79.2, K55.1, K55.8, K55.9, Z95.8, Z95.9
o	Cerebrovascular disease: G45.x, G46.x, H34.0, I60.x - I69.x
o	Dementia: F00.x - F03.x, F05.1, G30.x, G31.1
o	Chronic pulmonary disease: I27.8, I27.9, J40.x - J47.x, J60.x - J67.x, J68.4, J70.1, J70.3
o	Rheumatic disease: M05.x, M06.x, M31.5, M32.x - M34.x, M35.1, M35.3, M36.0
o	Peptic ulcer disease: K25.x - K28.x
o	Mild liver disease: B18.x, K70.0 - K70.3, K70.9, K71.3 - K71.5, K71.7, K73.x, K74.x, K76.0, K76.2 - K76.4, K76.8, K76.9, Z94.4
o	Diabetes without chronic complication: E10.0, E10.1, E10.6, E10.8, E10.9, E11.0, E11.1, E11.6, E11.8, E11.9, E12.0, E12.1, E12.6, E12.8, E12.9, E13.0, E13.1, E13.6, E13.8, E13.9, E14.0, E14.1, E14.6, E14.8, E14.9
o	Diabetes with chronic complication: E10.2 - E10.5, E10.7, E11.2 - E11.5, E11.7, E12.2 - E12.5, E12.7, E13.2 - E13.5, E13.7, E14.2 - E14.5, E14.7
o	Hemiplegia or paraplegia: G04.1, G11.4, G80.1, G80.2, G81.x, G82.x, G83.0 - G83.4, G83.9
o	Renal disease: I12.0, I13.1, N03.2 - N03.7, N05.2 - N05.7, N18.x, N19.x, N25.0, Z49.0 - Z49.2, Z94.0, Z99.2
o	Any malignancy, including lymphoma and leukaemia, except malignant neoplasm of skin: C00.x - C26.x, C30.x - C34.x, C37.x - C41.x, C43.x, C45.x - C58.x, C60.x - C76.x, C81.x - C85.x, C88.x, C90.x - C97.x
o	Moderate or severe liver disease: I85.0, I85.9, I86.4, I98.2, K70.4, K71.1, K72.1, K72.9, K76.5, K76.6, K76.7
o	Metastatic solid tumour: C77.x - C80.x
o	AIDS/HIV: B20.x - B22.x, B24.x

Weights
Each condition from the Charlson score is assigned a score when computing the weighted Charlson index, irrespectively of the coding system utilised. In particular, diabetes with complications, hemiplegia/paraplegia, renal disease, and malignancies are assigned a score of 2; moderate/severe liver disease is assigned a score of 3; metastatic solid tumour and AIDS/HIV are assigned a score of 6; the remaining comorbidities are assigned a score of 1. comorbidityallows the option of applying a hierarchy of comorbidities should a more severe version be present: by choosing to do so (and that is the default behaviour of comorbidity) a type of comorbidity is never computed more than once for a given patient.

Los códigos CIAP2 son los siguientes:
grupo CIAP2
ami K75
ami K76
chf K76
chf K77
pvd K92
pvd K99
cevd K89
cevd K90
cevd K91
dementia P70
copd R95
rheumd_ch L88
pud_ch D86
pud_ch D85
mld D72
mld D97
diabwoc T89
diabwoc T90
diabwic 
hp N18
rend K87
rend U88
rend U99.01
rend U99
canc D77
canc R85
canc A79
canc D74
canc D75
canc D76
canc R84
canc B74
canc L71
canc S77
canc N74
canc X76
canc X77
canc X75
canc W72
canc Y78
canc Y77
canc U75
canc U77
canc U76
canc F74
canc T71
canc T73
canc B72
canc B73
msld 
metacanc 
aids B90
carit K80
carit K79
carit K78
carit K04
carit K05
valv K83
valv K73
pcd K93
pcd K82
hypunc K86
hypc K87
para N18
ond N99
ond N87
ond N08
ond N86
ond N88
ond N19
ond N07
diabunc T89
diabunc T90
diabc 
hypothy T80
hypothy T86
rf U99.01
rf U99
ld D72
ld D97
pud_hx D86
pud_hx D85
lymph B72
lymph B74
solidtum D77
solidtum R85
solidtum A79
solidtum D74
solidtum D75
solidtum D76
solidtum R84
solidtum B74
solidtum L71
solidtum N74
solidtum X76
solidtum X77
solidtum X75
solidtum W72
solidtum Y78
solidtum Y77
solidtum U75
solidtum U77
solidtum U76
solidtum F74
solidtum T71
solidtum T73
solidtum S77
rheumd_hx L88
rheumd_hx K99
rheumd_hx B72
rheumd_hx L88.01
rheumd_hx L84
coag B83
obes T83
obes T82
wloss T91
wloss T08
fed T99
blane B80
dane B80
dane B81
alcohol P15
alcohol D87
alcohol D97
alcohol A86
drug P19
drug P18
psycho P72
psycho P98
psycho P73
depre P76
depre P82
depre P02




2.Define a los pacientes como fumador, no fumador y exfumador basándose en expresiones regulares de campos de texto libre del DGP tabaco de atención primaria.


