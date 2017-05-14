load(file.choose())
library(tm)
library(quanteda)
library(stringr)
library(qdap)
library(tidytext)
library(dplyr)
library(ggplot2)

##########
#create dict

id <- rownames(corpus_tidy)
corpus_tidy <- cbind(id=id, corpus_tidy) 

Mydic4 <- dictionary(list(
  Economy = c("jobs", "economy", "infrastructure", "unemployment", "spending",
            "taxes", "tax")))

Mydic5 <- dictionary(list(
  Healthcare = c("repeal", "replace", "obamacare", "insurance", "health",
               "healthcare", "medical")))

Mydic6 <- dictionary(list(
  ISIS = c("syria", "iraq", "ISIS", "bomb", "islam", "muslim", "terror",
         "terrorist")))

Mydic7 <- dictionary(list(
  Immigration_trade = c("mexico", "nafta", "trans", "pacific", "partnership", "china", "immigration", "build", "wall", "border")))

Mydic8 <- dictionary(list(
  Washington = c("drain", "swamp", "obama", "clinton", "hillary", "emails")))




########################### 
#Flesch.Kincaid score


corp2 <- corpus(corpus)

FRE_trump <- textstat_readability(corp2,
                                  measure=c('Flesch.Kincaid'))

fre_trump <- tidy(FRE_trump)
id <- rownames(fre_trump)
fre_trump <- cbind(id=id, fre_trump)
colnames(fre_trump)[3] <- "fre_score"
fre_trump$name <- NULL
fre_score2 <- right_join(corpus_tidy, fre_trump, by = "id")

ggplot(fre_score2, aes(y=fre_score, x=share_population_with_high_school_degree)) + geom_point() + geom_smooth(method = lm)
ggplot(fre_score2, aes(x=fre_score, y=median_household_income)) + geom_point() + geom_smooth(method = lm)
ggplot(fre_score2, aes(x=fre_score, y=share_non_citizen)) + geom_point() + geom_smooth(method = lm)
ggplot(fre_score2, aes(x=fre_score, y=share_non_white)) + geom_point() + geom_smooth(method = lm)
ggplot(fre_score2, aes(x=fre_score, y=share_white_poverty)) + geom_point() + geom_smooth(method = lm)
ggplot(fre_score2, aes(x=fre_score, y=gini_index)) + geom_point() + geom_smooth(method = lm)

#########################
#healthcare

corpus <- tm_map(corpus, content_transformer(tolower))    
withhealth <- dfm(corp2, dictionary = Mydic5)
withhealth <- tidy(withhealth)
id <- rownames(withhealth)
withhealth <- cbind(id=id, withhealth)
colnames(withhealth)[4] <- "healthcare_words"
withhealth$term <- NULL
withhealth$document <- NULL
withhealth2 <- right_join(fre_score2, withhealth, by = "id")




ggplot(withhealth2, aes(x=healthcare_words, y=Health.Index)) + geom_point() + geom_smooth(method = lm)


####################
#ISIS

ISIS <- dfm(corp2, dictionary = Mydic6)
ISIS <- tidy(ISIS)
id <- rownames(ISIS)
ISIS <- cbind(id=id, ISIS)
colnames(ISIS)[4] <- "ISIS_words"
ISIS$term <- NULL
ISIS$document <- NULL
ISIS2 <- right_join(withhealth2, ISIS, by = "id")

ggplot(ISIS2, aes(x=ISIS_words, y=hate_crimes_per_100k_splc)) + geom_point() + geom_smooth(method = lm)
ggplot(ISIS2, aes(x=ISIS_words, y=avg_hatecrimes_per_100k_fbi)) + geom_point() + geom_smooth(method = lm)


#######################
#Economy

econ <- dfm(corp2, dictionary = Mydic4)
econ <- tidy(econ)
id <- rownames(econ)
econ <- cbind(id=id, econ)
colnames(econ)[4] <- "economy_words"
econ$term <- NULL
econ$document <- NULL
econ2 <- right_join(ISIS2, econ, by = "id")
econ2$names <- NULL

ggplot(econ2, aes(y=economy_words, x=share_non_white)) + geom_point() + geom_smooth(method = lm)
ggplot(econ2, aes(y=economy_words, x=median_household_income)) + geom_point() + geom_smooth(method = lm)
ggplot(econ2, aes(y=economy_words, x=share_unemployed_seasonal)) + geom_point() + geom_smooth(method = lm)
ggplot(econ2, aes(y=economy_words, x=share_white_poverty)) + geom_point() + geom_smooth(method = lm)
ggplot(econ2, aes(y=economy_words, x=share_population_in_metro_areas)) + geom_point() + geom_smooth(method = lm)
ggplot(econ2, aes(y=economy_words, x=share_non_citizen)) + geom_point() + geom_smooth(method = lm)
ggplot(econ2, aes(y=economy_words, x=share_non_white)) + geom_point() + geom_smooth(method = lm)
ggplot(econ2, aes(y=economy_words, x=gini_index)) + geom_point() + geom_smooth(method = lm)


################################
# Immigration/Trade

imm <- dfm(corp2, dictionary = Mydic7)
imm <- tidy(imm)
id <- rownames(imm)
imm <- cbind(id=id, imm)
colnames(imm)[4] <- "Immigration_trade_words"
imm$term <- NULL
imm$document <- NULL
imm2 <- right_join(econ2, imm, by = "id")
imm2$names <- NULL

ggplot(imm2, aes(y=Immigration_trade_words, x=share_non_white)) + geom_point() + geom_smooth(method = lm)
ggplot(imm2, aes(y=Immigration_trade_words, x=median_household_income)) + geom_point() + geom_smooth(method = lm)
ggplot(imm2, aes(y=Immigration_trade_words, x=share_unemployed_seasonal)) + geom_point() + geom_smooth(method = lm)
ggplot(imm2, aes(y=Immigration_trade_words, x=share_white_poverty)) + geom_point() + geom_smooth(method = lm)
ggplot(imm2, aes(y=Immigration_trade_words, x=share_population_in_metro_areas)) + geom_point() + geom_smooth(method = lm)
ggplot(imm2, aes(y=Immigration_trade_words, x=share_non_citizen)) + geom_point() + geom_smooth(method = lm)
ggplot(imm2, aes(y=Immigration_trade_words, x=share_non_white)) + geom_point() + geom_smooth(method = lm)
ggplot(imm2, aes(y=Immigration_trade_words, x=gini_index)) + geom_point() + geom_smooth(method = lm)

###################################
#washington

wash <- dfm(corp2, dictionary = Mydic8)
wash <- tidy(wash)
id <- rownames(wash)
wash <- cbind(id=id, wash)
colnames(wash)[4] <- "Washington_words"
wash$term <- NULL
wash$document <- NULL
wash2 <- right_join(imm2, wash, by = "id")
wash2$names <- NULL

ggplot(wash2, aes(y=Washington_words, x=gini_index)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=Washington_words, x=share_population_in_metro_areas)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=Washington_words, x=share_unemployed_seasonal)) + geom_point() + geom_smooth(method = lm)
##########################################
#sentiment

sentiment <- function(words){
  require(quanteda)
  tok <- quanteda::tokenize(words)
  pos.count <- sum(tok[[1]]%in%pos[,1])
  cat("\n positive words:",tok[[1]][which(tok[[1]]%in%pos[,1])],"\n")
  neg.count <- sum(tok[[1]]%in%neg[,1])
  cat("\n negative words:",tok[[1]][which(tok[[1]]%in%neg[,1])],"\n")
  out <- (pos.count - neg.count)/(pos.count+neg.count)
  return(out)
}

wash2$text <- as.character(wash2$text)
wash2$sentiment <- NA
for (i in 1:nrow(wash2)){
  wash2[[i,25]] <- sentiment(wash2[[i,3]])
}

## plots for sentiment vs topics/states etc. 

#####################################################
#text related info

ggplot(wash2, aes(y=economy_words, x=fre_score)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=economy_words, x=healthcare_words)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=economy_words, x=Washington_words)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=economy_words, x=Immigration_trade_words)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=economy_words, x=ISIS_words)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=economy_words, x=sentiment)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=healthcare_words, x=fre_score)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=healthcare_words, x=sentiment)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=healthcare_words, x=Washington_words)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=healthcare_words, x=Immigration_trade_words)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=healthcare_words, x=ISIS_words)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=Washington_words, x=ISIS_words)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=Washington_words, x=Immigration_trade_words)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=Washington_words, x=sentiment)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=Washington_words, x=fre_score)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=Immigration_trade_words, x=fre_score)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=Immigration_trade_words, x=sentiment)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=Immigration_trade_words, x=ISIS_words)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=ISIS_words, x=fre_score)) + geom_point() + geom_smooth(method = lm)
ggplot(wash2, aes(y=ISIS_words, x=sentiment)) + geom_point() + geom_smooth(method = lm)
#####################################################

#total word counts and add to dataframe

dfm <- dfm(corp2)
rowSums(dfm)
word_count = c(7847, 2589, 3955, 3179, 3515, 2636, 5005, 5608, 12055, 2181, 3945, 8905, 9446, 5689, 3140, 3372, 4957, 2409, 5192, 
               2063, 7552, 1538, 1326, 1904, 2215, 2540, 2934, 2748, 7102, 1678, 1898, 1577, 1974, 1847, 3297, 1671, 1403, 1918,
               2783, 2281, 2367, 2157, 2256, 1947, 2664, 1019, 3308, 2935, 2828, 2272, 1212, 10040, 1719, 2839, 2167, 1791, 5821,
               2170, 1764, 1759, 5159, 1916, 2667, 4703, 6258,  824, 2213, 2596, 2788, 5779, 4538, 7489, 1942)

Word_count = data.frame(word_count)
id <- rownames(Word_count)
Word_count <- cbind(id=id, Word_count)
final <- right_join(wash2, Word_count, by = "id")

final$ISIS_prop <- final$ISIS_words / final$word_count
final$economy_prop <- final$economy_words / final$word_count
final$healthcare_prop <- final$healthcare_words / final$word_count
final$Immigration_trade_prop <- final$Immigration_trade_words / final$word_count
final$Washington_prop <- final$Washington_words / final$word_count

###############################
#ggplots with prop

#healthcare
ggplot(final, aes(x=healthcare_prop, y=Health.Index)) + geom_point() + geom_smooth(method = lm)

#ISIS
ggplot(final, aes(x=ISIS_prop, y=hate_crimes_per_100k_splc)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(x=ISIS_prop, y=avg_hatecrimes_per_100k_fbi)) + geom_point() + geom_smooth(method = lm)

#Economy
ggplot(final, aes(y=economy_prop, x=share_non_white)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=economy_prop, x=median_household_income)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=economy_prop, x=share_unemployed_seasonal)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=economy_prop, x=share_white_poverty)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=economy_prop, x=share_population_in_metro_areas)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=economy_prop, x=share_non_citizen)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=economy_prop, x=share_non_white)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=economy_prop, x=gini_index)) + geom_point() + geom_smooth(method = lm)

#Immigration/Trade
ggplot(final, aes(y=Immigration_trade_prop, x=share_non_white)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=Immigration_trade_prop, x=median_household_income)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=Immigration_trade_prop, x=share_unemployed_seasonal)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=Immigration_trade_prop, x=share_white_poverty)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=Immigration_trade_prop, x=share_population_in_metro_areas)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=Immigration_trade_prop, x=share_non_citizen)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=Immigration_trade_prop, x=share_non_white)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=Immigration_trade_prop, x=gini_index)) + geom_point() + geom_smooth(method = lm)

#Washington
ggplot(final, aes(y=Washington_prop, x=gini_index)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=Washington_prop, x=share_population_in_metro_areas)) + geom_point() + geom_smooth(method = lm)
ggplot(final, aes(y=Washington_prop, x=share_unemployed_seasonal)) + geom_point() + geom_smooth(method = lm)


################################
#extra BS

#Mydic <- dictionary(list(
 # health = c("health", "pollution", "cancer", "cardiovascular", "deaths", "chlamydia", "clinical", "care", "dentist", "diabetes",
  #           "drug", "excessive", "drink", "mental", "disorder", "distress", "physical", "immunization", "hpv", "tdap", "meningococcal",
   #          "morality", "infant", "disease", "infectious", "insurance", "birthweight", "obesity", "fatality", "fatalities","pertussis",
    #         "inactivity", "premature", "hospital", "hospitalization", "phyysicians", "salmonella", "smoking", "violence", "graduation",
     #        "binge", "cholesterol",'chronic', "colorectal", "fruits", "heart", "attack", "blood", "pressure", "high", "low", "injury", 
      #       "sleep", "stroke", "suicide", "vegetables", "water", "fluridation", "exercise")))

#mydic2 <- dictionary(list(
#  violence = c("acid", "aggressor", "agitator", "aim", "alert", "ambush", "ammunition", "anarchy", "anguish", "annihilate", "apartheid",
#               "arms", "arsenal", "artillery", "assassin", "assassinate", "assault", "atrocity", "attack", "authority", "automatic",
#               "barrage", "barricade", "battle", "battlefield", "belligerent", "betrayal", "blast", "blindside", "blood", "bloody",
#               "bomb", "bombardment", "breach", "break", "brutal", "brutality", "brute", "bullet", "bully", "burn", "cadaver",
#               "camouflage", "campaign", "captive", "capture", "careen", "carnage", "casualties", "cataclysm", "causes", "chaos", "charge",
#               "charred","checking", "clandestine", "clash", "coalition", "collapse", "combat", "commandos", "concentration", "concussion",
#               "conflagration", "conflict", "confrontation", "conquer", "consequences", "consolidate", "conspiracy", "conspire", "control",
#               "coordinates", "corpse", "counterattack", "Countermand", "crash", "crime", "crisis", "cross-hairs", "culpability", 
#               "damage", "danger", "dangerous", "dash", "dead", "deadly", "death", "debacle", "deception", "deliberate", "demolish", "demoralize",
#               "despot", "destroy", "destruction", "detect", "detection", "devastation", "device", "dictator", "dictatorship", "die", "disarmament",
#               "disaster", "disastrous", "discipline", "disease", "dispute", "disruption", "dissonance", "division", "domination", "doom", "downfall",
#               "drama", "dread", "encounter", "enemy", "enforce", "engagement", "epithet", "escalate", "excess", "execute", "execution", "expectations",
#               "explode", "exploitation", "explosion", "explosive", "expunge", "extremism", "faction", "fanatic", "fatal", "fear", "fearful", "felon",
#               "ferment", "ferocious", "feud", "fierce", "fiery", "fight", "fighter", "force", "forceful", "forces", "fray", "frenzy", "fuel", "fugitive",
#               "furtive", "gang", "gas", "genocide", "grave", "grenade", "grievous", "groans", "guard", "guerrillas", "guns", "gunship", "hammering",
#               "harass", "harsh", "hatch", "hate", "hatred", "hazard", "hiding", "hijack", "hijacker", "hit", "holocaust", "horror", "hostility", "howitzer",
#               "hurt", "ignite", "impact", "improvise", "incident", "incite", "incontrovertible", "infanticide", "infiltrate", "inflame", "informant", "injuries",
#               "inmate", "insurgent", "insurrection", "intense", "intercept", "interdiction", "international", "interrogation", "intervene", "intimidate",
#               "invasion", "investigate", "investigations", "involvement", "ire", "jail", "jeer", "jets", "join", "kamikaze", "keen", "kidnap", "killing",
#               "knife", "knock-out", "laser-activated", "launch", "launcher", "loathsome", "maim", "malevolent", "malicious", "maraud", "march", "massacre",
#               "mayhem", "megalomania", "menace", "militancy", "militant", "militaristic", "military", "militia", "mines", "missile", "mission", "mistreatment",
#               "mob", "mobile", "mobilization", "momentum", "mortars", "munitions", "murder", "muscle", "nationalist", "neutralize", "nightmare", "nitrate", "notorious", "nuclear",
#               "offensive", "officials", "onerous", "operation", "opposition", "order", "out", "outbreak", "overrun", "overthrow", "pacify", "partisan", "patrol", "penetrate",
#               "perform", "persecute", "petrify", "photos", "pilot", "pistol", "planes", "plunder", "position", "post-traumatic", "potent", "pound", "powder", "power",
#               "powerful", "preemptive", "premeditate", "prey", "prison", "prisoner", "proliferation", "provocation", "prowl", "pugnacious", "pulverize", "pushing",
#               "quail", "quarrel", "quell", "quiver", "radiation", "radical", "rage", "ravage", "ravish", "rebel", "rebellion", "reconnaissance", "recovery", "recruit",
#               "refugee", "regime", "regiment", "reinforcements", "relentless", "reparation", "reprisal", "reputation", "resistance", "retaliation", "retreat",
#               "retribution", "revenge", "revolution", "ricochet", "rifle", "rift", "riot", "rival", "rocket", "rot", "rounds", "rule", "ruthless", 
#               "sabotage", "sacrifice", "salvage", "sanction", "savage", "scare", "score", "scramble", "secrecy", "secret", "sedition", "seize", "seizure", "sensor",
#               "setback", "shelling", "shells", "shock", "shoot", "shot", "showdown", "siege", "skirmish", "slaughter", "smash", "smuggle", "soldier", "special-ops",
#               "specialized", "spy", "squad", "stalk", "stash", "stealth", "storm", "straggler", "strangle", "strategic", "strategist", "strategy", "strength", "strife",
#               "strike", "strip", "stronghold", "struggle", "subversive", "suffering", "superstition", "supplies", "support", "suppression", "surprise", "surrender", "survival",
#               "survivor", "suspect", "tactics", "tank", "target", "tension", "terror", "terrorism", "terrorist", "terrorize", "threaten", "thug", "thwart", "topple", "torch",
#               "tornado", "torpedo", "tourniquet", "tragic", "training", "trample", "trap", "trauma", "treachery", "trench", "trigger", "triumph", "tsunami", "turbulent",
#               "unbelievable", "unconventional", "unleash", "unruly", "uprising", "urgency", "vagrant", "vanguard", "vanish", "vehicle", "vehicular", "vendetta", "venomous",
#               "vicious", "victory", "vile", "vilify", "violation", "violence", "virulence", "vital", "vitriol", "vociferous", "void", "vow", "vulnerability",
#               "wage", "war", "warheads", "warplane", "warrant", "warrior", "watchdog", "watchful", "weapon", "weather", "well-trained", "wisdom", "worldwide", "wound",
#             "wreckage", "yearn", "yelling", "zeal", "zealot", "zigzag", "zone", "moron", "stupid", "tough", "bad", "lightweight"))) 


#mydic3 <- dictionary(list(
  #economy = c("immigration", "immigrant", "company", "companies", "money", "dollar", "dollars", "regulation", "billion", "billions", "china", "mexico", "poverty", "poor", "unemployment",
    #          "emplyment", "income", "job", "jobs", "wall", "appreciate", "bankrupt", "bankruptcy", "budget", "capital", "cash", "competition","consumer", "goods",  "cost", "crash", "credit",
   #           "currency", "debt", "deficit", "deposit", "depression", "economics", "economy",  "finance", "fiscal", "global", "inflation", "interest", "international", "invest", "investment",
  #            "loan", "loss", "market", "profit", "recession", "savings", "spending", "spent", "stock", "market", "tax", "trade", "value", "tax", "taxes", "oil", "build", "debt", "trillion",
 #             "hundreds", "nation", "national", "nafta", "tpp", "black", "african", "america", "american", "foreigner", "foreign")))

#violence <- dfm(corp2, dictionary = mydic2)
#violence <- tidy(violence)
#id <- rownames(violence)
#violence <- cbind(id=id, violence)
#colnames(violence)[4] <- "violence_words"
#violence$term <- NULL
#violence$document <- NULL
#violence2 <- right_join(withhealth2, violence, by = "id")

#ggplot(violence2, aes(x=violence_words, y=hate_crimes_per_100k_splc)) + geom_point()
#ggplot(violence2, aes(x=violence_words, y=avg_hatecrimes_per_100k_fbi)) + geom_point()
