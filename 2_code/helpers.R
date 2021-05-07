

is_english = function(x, lim = .5) {
  if(!"eng" %in% ls(envir = .GlobalEnv)){
    tmp = readxl::read_excel('0_materials/SUBTLEX_US.xlsx') 
    .GlobalEnv$eng = tmp[order(tmp$FREQcount, decreasing = T),][['Word']][1:40000]
    }
  prop = numeric(length(x))
  for(i in 1:length(x)){
    if(i %% 100 == 0) cat(round(100*i / length(x),1),'%\n')
    ws = stringr::str_extract_all(stringr::str_to_lower(x[i]), '[:alpha:]+')[[1]]
    prop[i] = mean(ws %in% .GlobalEnv$eng)
    }
  tests = prop >= lim
  names(tests) = round(prop,3)
  tests
  }


# colors
unibas_cols = c(mint = "#A5D7D2",
                mint_hell = "#D2EBD9",
                red = "#D20537",
                anthra = "#2D373C",
                anthra_hell = "#46505A")
names(unibas_cols) = NULL

sdg_names = c(
  "01" = "No Poverty - 01",
  "02" = "Zero Hunger - 02",
  "03" = "Good Health and Well-being - 03",
  "04" = "Quality Education - 04",
  "05" = "Gender Equality - 05",
  "06" = "Clean Water and Sanitation - 06",
  "07" = "Affordable and Clean Energy - 07",
  "08" = "Decent Word and Economic Growth - 08",
  "09" = "Industry, Innovation and Infrastructure - 09",
  "10" = "Reduced Inequalities - 10",
  "11" = "Sustainable Cities and Communities - 11",
  "12" = "Responsible Consumption and Production - 12",
  "13" = "Climate Action - 13",
  "14" = "Life below Water - 14",
  "15" = "Life on Land - 15",
  "16" = "Peace, Justice and Strong Institutions - 16",
  "17" = "Partnership for the Goals - 17"
)

counts = c(11323, 93390,3349291,25550,35625, 46074,383354,89498,
           39114, 47227, 141331, 84127, 180102, 104532, 111202, 169330)
elsevier = tibble(item_type = "reference", 
                  name = names(sdg_names)[-17], 
                  count = counts, 
                  prop = counts / sum(counts))
