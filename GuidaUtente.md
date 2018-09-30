# Guida Utente

In seguito è mostrata una guida utente volta a permettere di comprendere a pieno le funzionalità dell'interfaccia del simulatore.

All'avvio viene mostrato un launcher nel quale è possibile:
*	Caricare una simulazione esistente
*	Caricare e modificare una simulazione già esistente
*	Creare una nuova simulazione

Per il caricamento è sufficiente selezionare una simulazione completa, in formato yml, tramite file explorer.
La fase di creazione/modifica di una simulazione richiede invece una spiegazione dettagliata, mostrata in seguito

## View di Configurazione
Nella prima schermata è possibile aggiungere entità alla simulazione, è inoltre possibile salvare la simulazione, 
anche parzialmente completa.
Le entità che possono essere aggiunte sono:
*	**Piante**
*	**Animali**

Ogni volta che si presenta una schermata che contiene una lista di elementi, è possibile aggiungere un elemento cliccando sul tasto “Add”,
mentre è possibile modificare uno specifico elemento facendo doppio click su di esso.

### Schermata Creazione Pianta
Questa schermata è relativa alla creazione di una pianta, è necessario definire:
*	**Name:** nome della specie
*	**Height:** altezza
*	**Availability:** disponibilità
*	**Hardness:** durezza

Una volta compilati tutti i campi tale specie sarà aggiunta a quelle disponibili e verrà mostrata nella prima schermata assieme alle altre.

### Schermata Creazione Animale
Questa schermata è relativa al settaggio delle informazioni base di un animale. Tale configurazione risulta più complicata rispetto a 
quella della pianta poiché è richiesta la modellazione del genoma.
Occorre definire:
*	**Name:** nome della specie
*	**Gene length:** lunghezza del gene
*	**Alleles length:** lunghezza degli alleli
*	**Typology:** la dieta dell’animale (erbivoro, carnivoro)

Cliccando su “Confirm” si procede poi alla schermata relativa al cromosoma


### Schermata Cromosoma
In questa schermata si definisce il cromosoma dell’animale, che si divide in 3 parti, ognuna delle quali gestisce geni di un determinato tipo. 
Abbiamo infatti geni di tipi:
*	**Strutturale**
*	**Regolatore**
*	**Sessuale**

I cromosomi regolatori e sessuali sono accomunati dal fatto che possiedono proprietà già definite, per cui è necessario solo specificare come i geni 
andranno a modificarle.
A questo punto è possibile procedere con la modellazione dei geni.

### Schermata Gene
In questa schermata è possibile definire un gene, che come detto può essere di 3 tipi. I geni regolatori e sessuali però vengono gestiti allo stesso modo (default), 
che è differente da come avviene per i geni strutturali (custom).
Perciò, a seconda del tipo di gene in questione la schermata corrente può essere:
*	**Default:** per geni regolatori e sessuali
*	**Custom:** per geni strutturali

Procedendo poi con il tasto “Confirm” si raggiunge la schermata in cui vengono settati gli alleli relativi a tale gene.

### Schermata Alleli
Ogni gene possiede più forme alleliche, di cui però solo due saranno rilevanti e solamente una si manifesterà, in base alla dominanza.
In questa schermata è possibile gestire alleli per ogni gene: cliccando su “Add” o selezionandone uno si viene 
riportati ad una schermata che permette di specificare i parametri di un singolo allele.

### Schermata Allele
Questa schermata permette di specificare i valori di un allele, in particolare troviamo:
*	**Id**
*	**Dominance:** dominanza dell’allele
*	**Consume:**
*	**Probability:** probabilità dell’allele, legato alla riproduzione
*	**Effects:** effetti correlati a tale allele, specificabili tramite apposita schermata

### Schermata Effetti
In questa schermata è possibile specificare, per una proprietà, il valore associato. 
In questo modo viene impostata direttamente l’influenza di tale allele rispetto quella data proprietà. 

### Schermata Custom Gene
In tale schermata vengono specificati tutti gli aspetti del gene, devono essere infatti inseriti:
*	**Id:** identificatore univoco del gene
*	**Name:** nome del gene
*	**Properties:** proprietà relative a tale gene, specificabili tramite apposita schermata

### Schermata Proprietà
Questa schermata fa riferimento alla definizione di una nuova proprietà relativa al gene, 
ogni gene strutturale può essere infatti associato a più proprietà dell’animale.
Per una proprietà vanno specificati:
*	**Name:** nome della proprietà
*	**Conversion Maps:** mappe di conversione della proprietà, specificabili tramite apposita interfaccia

### Schermata Conversion Map
Ogni proprietà di un gene strutturale introdotta dall'utente possiede una o più conversion map, 
ovvero una mappa di conversione per specificare quale proprietà dell’animale viene influenzata da questa proprietà custom.
Ogni proprietà introdotta infatti deve essere associata in qualche modo a proprietà pre-esistenti, 
per cui il valore di una conversion map funge da moltiplicatore dell’effect dell’allele, 
permettendo quindi di ottenere una qualità dell’animale.
Detto in maniera più semplice ogni proprietà personalizzata dall’utente necessita di almeno una conversion map, 
la quale determina quale proprietà intrinseca dell’animale interessa per mezzo del valore specificato, che viene moltiplicato per l’effect dell’allele; 
più conversion map vengono specificate, più sono le qualità intrinseche che ne dipendono.
In questa schermata è perciò possibile specificare quale qualità intrinseca associare ed il relativo moltiplicatore.

### Schermata Default Gene
Questa schermata rappresenta una semplificazione della schermata custom.
In questo caso infatti occorre specificare l’id e la proprietà a cui fa riferimento il gene, 
scegliendo però tra un insieme di proprietà intrinseche. 
Non possono perciò essere specificate proprietà custom e quindi non vi è più il concetto di conversion map, 
poiché in questo caso il gene si riferisce ad una sola proprietà intrinseca dell’animale. 
In questo senso il gene default è un caso particolare del gene custom.

### Schermata di conferma
In questa schermata viene confermata la simulazione realizzata, definendo il numero di ciascuna entità all'interno del mondo.

## Configurazione diretta attraverso file YAML
Oltre che essere generato a seguito del salvataggio del setup della simulazione da interfaccia grafica, l'insieme dei file YAML è progettato per poter fungere anche da sistema di configurazione primario. Per poterlo utilizzare è però necessario scendere nel dettaglio di come questi file debbano essere compilati e divisi nella varie sottocartelle. Un esempio di simulazione già correttamente configurata è presente nella cartella demo del repository.

### File principale di configurazione
Deve contenere due diverse mappe:
* **animals**: avente come chiavi il percorso del file di configurazione di un animale e come chiave la quantità iniziale di individui di quella specie da inserire nella simulazione
* **animals**: avente come chiavi il percorso del file di configurazione di una pianta e come chiave la quantità iniziale di individui di quella specie da inserire nella simulazione

### File di configurazione di una pianta
Deve contenere le impostazioni relative ad una specie vegetale. I campi da inserire sono:
* **name**: nome custom
* **geneLength**: lunghezza degli identificatori dei geni
* **alleleLength**: lunghezza degli identificatori degli alleli
* **reign**: regno di appartenenza, in questo caso piante (P)
* **height**: altezza della pianta
* **nutritionalValue**: valore nutritivo
* **hardness**: durezza

### File di configurazione di un animale
Deve contenere le impostazioni relative ad una specie animale. Rispetto ad una pianta presenta una configurazione più complessa e strutturata, in quanto richiede di definire sia gli alleli dei geni di default, ossia predeterminati dal sistema, che i geni custom con relativi alleli. I campi da inserire sono:
* **name**: nome custom
* **geneLength**: lunghezza degli identificatori dei geni
* **alleleLength**: lunghezza degli identificatori degli alleli
* **reign**: regno di appartenenza, in questo caso animali (A)
* **typology**: tipologia, erbivoro (H) o carnivoro (C)
* **structuralChromosome**: path della cartella contenente i file di markup relativi ai geni strutturali dell'animale. È importante che la cartella contenga unicamente i file di markup relativi ai geni. Può però contenere uilteriori file non YAML e cartelle
* **regulationChromosome**: configurazione relativa al cromosoma regolatore, a sua volta deve contenere:
  - **allelesPath**: path della cartella contenente i file di markup degli alleli dei geni del cromosoma. È importante che la cartella contenga unicamente i file di markup relativi agli alleli
  - **names**: mappa contenente il nome dei geni regolatori come chiave ed il loro identificatore come valore. Il valore dell'identificatore è utilizzato per riconoscere gli alleli relativi nella cartella indicata in precedenza. I geni configurabili sono:
      - life
      - childhood
      - maturity
      - oldness
      - decline
* **sexualChromosome**: propietà analoga al regulationChromosome, i geni configurabili sono:
  - fertility
  - fecundity
  - pregnancyDuration
Gli alleli dei geni di default, il cui file di markup verrà descritto in seguito, dovranno avere effetto su di un'unica propietà, il cui nome corrisponde a quella del gene.

### File di configurazione di un gene
File utilizzato per configurare un gene custom, deve trovarsi nella cartella correta rispetto a quanto configurato nell'animale, insieme ai file di markup degli altri geni. I campi da inserire sono:
* **id**: id del gene
* **simpleName**: nome custom del gene
* **allelesPath**: cartella contenente gli alleli del gene. È importante che la cartella contenga unicamente i file di markup relativi agli alleli di quel gene
* **properties**: mappa contenente come chiave il nome della proprietà custom del gene e come valore la mappa di conversione. Questa ha come chiave la propietà base dell'animale nel quale convertire la propietà custom e come valore il fattore moltiplicativo di conversione. Ne deriva che un'unica propietà custom può essere convertita in proprietà multiple dell'animale. Le propietà di base dell'animale sul quale poter agire sono: 
  - fieldOfView
  - attractiveness
  - speed
  - height
  - rangeOfAction
  - resistenceToAttack
  - strength
  - nutritionalValue

È obbligatorio che nel complesso i diversi geni di un animale influenzino tutte le sue propietà base.

### File di configurazione di un allele
I campi da inserire sono:
* **gene**: id del gene al quale l'allele si riferisce
* **id**: id dell'allele
* **dominance**: livello di dominanza dell'allele, utilizzato per determinare quale dei due alleli posseduti da un individuo prevale, determinandone il fenotipo
* **consume**: consumo energetico determinato dall'allele
* **probability**: probabilità che l'allele sia posseduto da un individuo della specie. Utlizzata per la popolazione inizale del mondo. La somma delle probabilità degli alleli di un gene deve essere 1. Gli alleli con probabilità 0 sono mutanti ed appariranno casualmente nella popolazione con l'avanzare delle ere
* **effect**: mappa contenente gli effetti che l'allele ha sulle propietà definite dal suo gene. Contiene come chiavi il nome delle propietà e come valori il loro valore numerico

## View Principale
La view principale risulta invece molto più semplice e di immediata comprensione.
È divisa in sezioni, ciascuna delle quali occupa un tab e si occupa di una precisa funzionalità:
*	**World:** sezione principale, mostra il mondo contenente le entità, la storia esprimente gli eventi salienti e i dettagli di una certa entità selezionata. In questa schermata è possibile inoltre mettere in pausa o riprendere la simulazione e aggiungere entità e specie in modo dinamico
*	**Filters:** sezione che consente di filtrare le entità visualizzate nella sezione world in base ai valori specificati
*	**Statistics:** sezione relativa alle statistiche generali del mondo, è possibile inoltre visualizzare il replay della vita di un’entità selezionata
*	**Genome:** sezione che mostra il genoma di un animale selezionato; posizionando il cursore su un determinato gene è possibile visualizzarne tutte le informazioni
*	**Body parts:** sezione che mostra le componenti del corpo di un animale, specificandone lo stato


