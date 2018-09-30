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


## View Principale
La view principale risulta invece molto più semplice e di immediata comprensione.
È divisa in sezioni, ciascuna delle quali occupa un tab e si occupa di una precisa funzionalità:
*	**World:** sezione principale, mostra il mondo contenente le entità, la storia esprimente gli eventi salienti e i dettagli di una certa entità selezionata. In questa schermata è possibile inoltre mettere in pausa o riprendere la simulazione e aggiungere entità e specie in modo dinamico
*	**Filters:** sezione che consente di filtrare le entità visualizzate nella sezione world in base ai valori specificati
*	**Statistics:** sezione relativa alle statistiche generali del mondo, è possibile inoltre visualizzare il replay della vita di un’entità selezionata
*	**Genome:** sezione che mostra il genoma di un animale selezionato; posizionando il cursore su un determinato gene è possibile visualizzarne tutte le informazioni
*	**Body parts:** sezione che mostra le componenti del corpo di un animale, specificandone lo stato


