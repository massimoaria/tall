## code to prepare `df_languages` dataset goes here
known_models<- c(
  "english-ewt", "english-gum", "english-lines",
  "english-partut","italian-isdt",
  "italian-partut", "italian-postwita",
  "italian-twittiro", "italian-vit",
  "french-gsd", "french-partut", "french-sequoia",
  "french-spoken",
  "german-gsd", "german-hdt",
  "spanish-ancora", "spanish-gsd",
  "afrikaans-afribooms",
  "ancient_greek-perseus", "ancient_greek-proiel", "arabic-padt",
  "armenian-armtdp", "basque-bdt", "belarusian-hse", "bulgarian-btb",
  "catalan-ancora", "chinese-gsd", "chinese-gsdsimp", "classical_chinese-kyoto",
  "coptic-scriptorium", "croatian-set", "czech-cac", "czech-cltt",
  "czech-fictree", "czech-pdt", "danish-ddt", "dutch-alpino",
  "dutch-lassysmall", "estonian-edt", "estonian-ewt", "finnish-ftb",
  "finnish-tdt",  "galician-ctg", "galician-treegal",
  "gothic-proiel", "greek-gdt",
  "hebrew-htb", "hindi-hdtb", "hungarian-szeged", "indonesian-gsd",
  "irish-idt", "italian-isdt", "japanese-gsd", "korean-gsd",
  "korean-kaist", "latin-ittb", "latin-perseus", "latin-proiel",
  "latvian-lvtb", "lithuanian-alksnis", "lithuanian-hse",
  "maltese-mudt", "marathi-ufal", "north_sami-giella",
  "norwegian-bokmaal", "norwegian-nynorsk", "norwegian-nynorsklia",
  "old_church_slavonic-proiel", "old_french-srcmf", "old_russian-torot",
  "persian-seraji", "polish-lfg", "polish-pdb", "portuguese-bosque",
  "portuguese-gsd", "romanian-nonstandard", "romanian-rrt",
  "russian-gsd", "russian-syntagrus", "russian-taiga",
  "scottish_gaelic-arcosg", "serbian-set", "slovak-snk",
  "slovenian-ssj", "slovenian-sst",
  "swedish-lines", "swedish-talbanken", "tamil-ttb", "telugu-mtg",
  "turkish-imst", "ukrainian-iu", "urdu-udtb", "uyghur-udt",
  "vietnamese-vtb", "wolof-wtb")

default_models <-  gsub("-.*","",known_models)

chapter_vec <- c(
"CHAPTER","CAPITOLO","CHAPITRE","KAPITEL","CAPÍTULO","HOOFSTUK"  ,"ΚΕΦΑΛΑΙΟ","فصل","ԳԼՈՒԽ","KAPITULU","ГЛАВА","ГЛАВА",
"CAPÍTOL","章","章","ⲡⲏⲡⲟⲗⲁⲓⲟⲥ", "POGLAVLJE", "KAPITOLA",  "KAPITEL",   "HOOFDSTUK", "PEATÜKK",   "LUKU",      "CAPÍTULO",  "KAPITULS",
"ΚΕΦΑΛΑΙΟ",  "פרק",       "अध्याय",     "FEJEZET",   "BAB",       "CAIBIDIL",  "章"  ,      "장"  ,      "CAPITULUM", "NODAĻA",    "SKYRIUS",   "KAPITOLU",
"अध्याय",     "CAŊÁLAŠ",   "KAPITTEL",  "ГЛАВА" ,    "CHAPITRE",  "ГЛАВА" ,    "فصل"  ,     "ROZDZIAŁ",  "CAPÍTULO",  "CAPITOL",   "ГЛАВА"  ,   "CAIBIDIL" ,
"ГЛАВА" ,    "KAPITOLA" , "POGLAVJE"  ,"KAPITEL"  , "அதிர்வேகம்",  "అధ్యాయం"  ,   "BÖLÜM",     "ГЛАВА",     "باب",       "BAB",       "CHƯƠNG",    "KOW")

languages <- tibble(short=default_models, repo=known_models) %>%
  group_by(short) %>%
  mutate(repo=repo[1]) %>%
  distinct()

languages$chapter <- chapter_vec

usethis::use_data(languages, overwrite = TRUE)
