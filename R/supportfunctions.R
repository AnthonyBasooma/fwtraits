
urltaxa <- function(){

  url <- "https://www.freshwaterecology.info/fweapi2/v1/query"

  taxalist <- list(list("Acipenseriformes","Anguilliformes", "Atheriniformes",
                        "Beloniformes","Clupeiformes","Cypriniformes","Cyprinodontiformes",
                        "Esociformes", "Gadiformes","Gasterosteiformes","Syngnathiformes",
                        "Siluriformes","Scorpaeniformes","Salmoniformes",
                        "Pleuronectiformes","Petromyzontiformes","Perciformes",
                        "Osmeriformes"), marcro = list("Bivalvia","Branchiobdellida", "Bryozoa", "Chironomidae",
                                                       "Coelenterata", "Coleoptera","Copepoda", "Crustacea",
                                                       "Diptera", "Ephemeroptera", "Gastropoda", "Heteroptera",
                                                       "Hirudinea", "Hydrachnidia", "Hymenoptera", "Lepidoptera",
                                                       "Megaloptera", "Nematomorpha","Nemertini", "Odonata",
                                                       "Oligochaeta", "Planipennia", "Plecoptera", "Polychaeta",
                                                     "Porifera", "Trichoptera", "Turbellaria"))
  ecoparm <- fwtraits::ecoparameters

  return(list(url, taxalist, ecoparm))
}

