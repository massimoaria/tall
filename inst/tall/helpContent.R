helpContent <- function(){
  importmenu <- "
  <body>

  <h3><strong>Importing Data in TALL</strong></h3>

  <p>TALL provides a versatile and user-friendly interface for importing textual data from various sources, ensuring flexibility in data handling for diverse analytical needs. The platform supports multiple file formats and structures, allowing users to seamlessly prepare their datasets for analysis.</p>
  <hr>

  <h4><strong>Supported File Formats</strong></h4>

  <h4>1. Plain Text Files (.txt)</h4>
  <p>Plain text files can be imported in three different ways, depending on the structure of the data:</p>
  <ul>
    <li><strong>Single file containing a single document:</strong> Ideal for analyzing an individual document, such as a speech transcript, literary work, or report.</li>
    <li><strong>Single file with multiple documents separated by alphanumeric codes</strong> (e.g., 'Chapter', '0001', '****'):
      <ul>
        <li>TALL automatically detects these separators, enabling structured document segmentation.</li>
        <li>Users can further refine the segmentation using the <strong>Edit → Split</strong> menu.</li>
      </ul>
    </li>
    <li><strong>Multiple .txt files, where each file represents a separate document:</strong>
      <ul>
        <li>Users can either select individual files manually or import a compressed (.zip) folder containing multiple text files.</li>
        <li>Each document will be automatically assigned an ID based on its file name, ensuring clear organization.</li>
      </ul>
    </li>
  </ul>

  <h4>2. Tabular Data (.csv, .xlsx)</h4>
  <p>Tabular formats are useful for structured datasets, such as online reviews, survey responses, or social media posts.</p>
  <ul>
    <li>The text to be analyzed must be stored in a dedicated column named <strong>'text'</strong> to ensure proper identification.</li>
    <li>Each row in the dataset is treated as an individual document.</li>
    <li>Additional metadata (e.g., timestamps, user IDs, categories) can be retained for contextual analysis.</li>
  </ul>

  <h4>3. PDF Documents (.pdf)</h4>
  <p>TALL supports the import of PDF files, facilitating the analysis of academic papers, reports, books, and other document types.</p>
  <ul>
    <li>Text extraction occurs automatically, converting the content into a format suitable for processing.</li>
    <li><strong>Limitation:</strong> At the moment, TALL can only import and process PDFs that follow a single-column formatting. PDFs with multi-column layouts, footnotes, or complex page structures may not be correctly parsed, and additional preprocessing may be required.</li>
  </ul>

  <h4>4. Biblioshiny Export Files</h4>
  <p>TALL supports the import of files exported from <strong>Biblioshiny</strong>, the graphical user interface of the <strong>Bibliometrix</strong> R package. This feature allows users to directly analyze the textual content of bibliographic metadata extracted from bibliometric databases such as Scopus or Web of Science.</p>
  <ul>
    <li>The exported file (typically in <strong>.csv</strong> format) can be loaded into TALL.</li>
    <li>Users must specify which column (e.g., <strong>Abstract</strong>, <strong>Keywords</strong>, or <strong>Title</strong>) should be used as the main textual content for analysis.</li>
    <li>Other fields (e.g., authors, year, journal) can be imported and used as metadata for document grouping or filtering.</li>
  </ul>

  <hr>
  <h4><strong>TALL Structured Files (.tall)</strong></h4>
  <p>TALL allows users to save their analysis progress in a structured format, ensuring continuity across sessions.</p>
  <ul>
    <li><strong>Save Progress:</strong> Users can export their current session as a <strong>.tall</strong> file, preserving all imported data, configurations, and analytical steps.</li>
    <li><strong>Load Saved Sessions:</strong> Previously saved <strong>.tall</strong> files can be reloaded, allowing users to resume their work seamlessly without the need to re-import or preprocess data.</li>
  </ul>

  <p>By offering flexible and structured data import capabilities, TALL streamlines the initial steps of text analysis, enabling users to focus on extracting insights efficiently.</p>
  <hr>

  <div class='references'>
    <h4><strong>References</strong></h4>
    <p><strong>Aria, M., Cuccurullo, C., D’Aniello, L., Misuraca, M., & Spano, M. (2024).</strong> <i>Breaking Barriers with TALL: A Text Analysis Shiny app for ALL</i>. In A. Dister, D. Longrée (eds.), <i>Mots competes textes déchiffrés (JADT24)</i> Presses Universitaires De Louvain Vol.1 pp.39-48.</p>
    <p><strong>Aria, M., Cuccurullo, C., D’Aniello, L., Misuraca, M., & Spano, M. (2024).</strong> <i>TALL: A New Shiny App for Text Analysis</i>. In <i>Scientific Meeting of the Italian Statistical Society</i> (pp. 64-70). Cham: Springer Nature Switzerland.</p>
    <p><strong>Aria, M., Cuccurullo, C., D'Aniello, L., Misuraca, M., & Spano, M. (2023).</strong> <i>TALL: A New Shiny App of Text Analysis for All</i>. In <i>CLiC-it</i>.</p>
  </div>

</body>
"

  split <-  "<body>

    <h3><strong>Splitting the Corpus in TALL</strong></strong></h3>

    <p>TALL allows users to split textual data into smaller segments based on a specified sequence of characters. This feature is particularly useful when dealing with large documents containing multiple sections or structured content that needs to be analyzed separately.</p>
    <hr>
    <h4><strong>How It Works</strong></h4>
    <ul>
    <li>Users can define a <strong>delimiter</strong>, which is a sequence of characters used to segment the text.</li>
    <li>The delimiter must contain at least <strong>three characters</strong> to ensure accurate text splitting.</li>
    <li>The splitting process is <strong>case-sensitive</strong>, meaning that uppercase and lowercase variations are treated as distinct (e.g., <code>'CHAPTER'</code> is different from <code>'chapter'</code>).</li>
    </ul>
    <hr>
    <h4><strong>Example Use Cases</strong></h4>
    <ul>
    <li><strong>Books or Reports:</strong> Splitting a novel into chapters using <code>'CHAPTER '</code> as a delimiter.</li>
    <li><strong>Survey Responses:</strong> Separating responses when they are structured using a marker like <code>'###'</code> between answers.</li>
    <li><strong>Transcriptions:</strong> Dividing interview transcripts based on speaker labels (e.g., <code>'Speaker 1:'</code>).</li>
    </ul>

    <p>By offering a flexible splitting mechanism, TALL ensures that text segmentation aligns with the user's analytical needs, preserving the original structure for meaningful interpretation.</p>

</body>"

  random <- "<body>

    <h3><strong>Random Text Selection in TALL</strong></strong></h3>

    <p>TALL allows users to extract a random subset of imported texts for focused analysis. This feature is particularly useful when working with large corpora, enabling users to explore representative samples without processing the entire dataset.</p>
    <hr>
    <h4><strong>How It Works</strong></strong></h4>
    <ul>
    <li>The total number of imported texts is displayed, providing an overview of the dataset size.</li>
    <li>Users can define the <strong>sample size</strong> as a percentage (%) of the total corpus.</li>
    <li>The selection process is <strong>random</strong>, ensuring an unbiased representation of the dataset.</li>
    </ul>
    <hr>
    <h4><strong>Example Use Cases</strong></strong></h4>
    <ul>
    <li><strong>Analyzing Social Media Data:</strong> Selecting 10% of tweets from a large dataset to perform sentiment analysis.</li>
    <li><strong>Survey Research:</strong> Extracting a random subset of open-ended responses for qualitative coding.</li>
    <li><strong>Document Sampling:</strong> Reviewing a sample of reports or articles instead of analyzing the full collection.</li>
    </ul>

    <p>By enabling controlled sampling, TALL helps users balance efficiency and analytical depth, making text exploration more manageable and meaningful.</p>
    </body>"

  externalinfo <- "<body>

    <h3><strong>Importing External Information in TALL</strong></strong></h3>

    <p>TALL allows users to integrate additional information into their analysis by importing external datasets. This feature is particularly useful for enriching text data with metadata, annotations, or categorical variables, enabling a more comprehensive exploration of textual patterns.</p>
    <hr>
    <h4><strong>How to Import External Data</strong></strong></h4>
    <ul>
    <li>The external file must be in <strong>Excel format</strong> (<code>.xlsx</code>).</li>
    <li>The dataset must include a column labeled <strong>'doc_id'</strong>, which is used to match external information with the previously imported text data.</li>
    <li>The <strong>'doc_id'</strong> values must correspond exactly to the document identifiers assigned during text import to ensure proper alignment.</li>
    </ul>
    <hr>
    <h4><strong>Using External Information</strong></strong></h4>
    <ul>
    <li>Imported external data can be used to <strong>filter</strong> or <strong>group documents</strong> based on specific attributes (e.g., author, category, sentiment).</li>
    <li>This allows users to segment text collections efficiently, focusing on subsets relevant to their research questions.</li>
    </ul>
    <hr>
    <h4><strong>Download Document Identifiers</strong></strong></h4>
    <p>To facilitate the integration process, users can download a list of <strong>'doc_id'</strong> values associated with the imported text files below. This ensures that external data is formatted correctly before uploading.</p>

    <p>By supporting the import of structured external data, TALL enhances text analysis capabilities, allowing users to incorporate contextual information for richer insights.</p>

    </body>"

  tokenization <- "<body>

    <h3><strong>Tokenization, Lemmatization, and PoS Tagging in TALL</strong></strong></h3>

    <p>TALL provides robust Natural Language Processing (NLP) capabilities for preprocessing textual data, including <strong>tokenization, lemmatization, and Part-of-Speech (PoS) tagging</strong>. These steps are essential for transforming raw text into a structured format suitable for further analysis.</p>
    <hr>
    <h4><strong>Powered by UDPipe for NLP Preprocessing</strong></strong></h4>
    <p>TALL leverages the <strong>UDPipe</strong> library to perform tokenization, tagging, lemmatization, and dependency parsing. The <a href='https://cran.r-project.org/web/packages/udpipe/index.html' target='_blank'>udpipe R package</a> offers seamless access to pre-trained annotation models, supporting multiple languages.</p>

      <ul>
      <li><strong>Tokenization:</strong> Splits raw text into individual words or tokens.</li>
      <li><strong>Lemmatization:</strong> Converts words into their base or dictionary form (e.g., 'running' → 'run').</li>
      <li><strong>PoS Tagging:</strong> Assigns grammatical categories (e.g., noun, verb, adjective) to each word.</li>
      <li><strong>Dependency Parsing:</strong> Identifies syntactic relationships between words in a sentence.</li>
      </ul>
      <hr>
      <h4><strong>Updated Pre-trained Language Models</strong></strong></h4>
      <p>By default, UDPipe includes models based on <a href='https://universaldependencies.org/' target='_blank'>Universal Dependencies (UD)</a> version 2.5, but these had not been updated in some time. To enhance accuracy and ensure better linguistic processing, TALL now integrates updated <strong>pre-trained NLP language models</strong> from <strong>Universal Dependencies (UD) version 2.15</strong>.</p>

        <p>These models were trained using <strong>gold standard annotated corpora</strong> from the UD project, significantly improving the quality of text analysis in TALL. The updated pre-trained models used in TALL can be accessed through our <a href='https://github.com/massimoaria/tall.language.models' target='_blank'>GitHub repository</a>.</p>
      <hr>
          <h4><strong>Applications in NLP and Text Analysis</strong></strong></h4>
          <ul>
          <li><strong>Sentiment Analysis:</strong> Better understanding of word usage and context.</li>
          <li><strong>Topic Modeling:</strong> Improved preprocessing for cleaner topic extraction.</li>
          <li><strong>Corpus Exploration:</strong> Advanced filtering and segmentation of texts based on linguistic attributes.</li>
          </ul>

          <p>By integrating updated NLP models and leveraging powerful preprocessing techniques, TALL ensures high-quality text analysis, making it a valuable tool for researchers and practitioners in computational linguistics.</p>
         <hr>
          <div class='references'>
            <h4><strong>References</strong></strong></h4>
            <p><strong>TALL Pre-trained Models Repository:</strong> <a href='https://github.com/massimoaria/tall.language.models' target='_blank'>GitHub repository for pre-trained models</a></p>
              <p><strong>UDPipe R Package:</strong> <a href='https://cran.r-project.org/web/packages/udpipe/index.html' target='_blank'>CRAN link to UDPipe</a></p>
                <p><strong>Universal Dependencies Repository:</strong> <a href='https://universaldependencies.org/' target='_blank'>Universal Dependencies project</a></p>
                  </div>

                  </body>"

  specialentities <- "<body>

    <h3><strong>Tagging Special Entities in TALL</strong></strong></h3>

    <p>TALL automatically detects and tags <strong>special entities</strong> within texts, ensuring that key non-linguistic elements are properly identified and can be leveraged in further analysis.
    <br>Recognizing these entities helps improve text preprocessing, pattern recognition, and contextual analysis.</p>
    <hr>
    <h4><strong>Detected Special Entities</strong></strong></h4>
    <p>When processing textual data, TALL assigns specific tags to the following entities:</p>
    <ul>
    <li><strong>Email Addresses:</strong> Recognizes and tags email formats (e.g., <code>example@domain.com</code>).</li>
    <li><strong>URLs:</strong> Detects web links, ensuring they can be excluded or analyzed separately (e.g., <code>https://www.example.com/path</code>).</li>
    <li><strong>Emojis:</strong> Identifies and classifies emojis used in digital communication (e.g., 😊, 🚀, ❤️).</li>
    <li><strong>Hashtags:</strong> Extracts hashtags commonly used in social media and categorization (e.g., <code>#ExampleTag</code>).</li>
    <li><strong>IP Addresses:</strong> Detects standard IP address formats (e.g., <code>192.168.1.1</code>), which may be useful in network-related text analysis.</li>
    <li><strong>Mentions:</strong> Identifies references to usernames, particularly in social media or chat applications (e.g., <code>@username</code>).</li>
    </ul>
    <hr>
    <h4><strong>Why Special Entity Tagging Matters?</strong></strong></h4>
    <ul>
    <li><strong>Enhanced Text Cleaning:</strong> Filtering out or isolating elements that may not contribute to linguistic analysis.</li>
    <li><strong>Social Media and Web Analysis:</strong> Extracting meaningful patterns from hashtags, mentions, and URLs.</li>
    <li><strong>Sentiment and Emotion Studies:</strong> Analyzing the role of emojis in sentiment-based communication.</li>
    <li><strong>Cybersecurity and Digital Forensics:</strong> Identifying sensitive data points such as email addresses and IP addresses.</li>
    </ul>

    <p>By integrating special entity recognition, TALL enhances the preprocessing phase, ensuring that these elements are structured for more effective text analysis.</p>

    </body>"

  multiwordcreation <- "
  <body>

      <h3><strong>Algorithms for Automatic Multi-Word Extraction</strong></strong></h3>

      <p>The software <strong>TALL - Text Analysis for All</strong> employs four key algorithms to automatically generate multi-word sequences from a corpus of documents. These methods, widely recognized in computational linguistics and text mining, include <strong>Rapid Automatic Keyword Extraction (RAKE)</strong>, <strong>Pointwise Mutual Information (PMI)</strong>, <strong>Mutual Dependency (MD)</strong>, and <strong>Log-Frequency Biased Mutual Dependency (LF-MD)</strong>.</p>

      <br><h4><strong>- Rapid Automatic Keyword Extraction (RAKE)</strong></h4>
      <p>RAKE is a domain-independent keyword extraction algorithm that identifies key phrases by analyzing word co-occurrences within a document. It segments text into candidate keyword phrases based on stopword delimiters and then assigns scores based on word co-occurrence and frequency. Higher-scoring phrases are considered more relevant as multi-word expressions.</p>
      <p><strong>Reference:</strong><br>
      Rose, S., Engel, D., Cramer, N., &amp; Cowley, W. (2010). <em>Automatic keyword extraction from individual documents</em>. Text Mining: Applications and Theory, 1(1), 1-20.</p>

      <br><h4><strong>- Pointwise Mutual Information (PMI)</strong></h4>
      <p>PMI is a statistical measure used to assess the association strength between two words. It is defined as:</p>

      <p style='text-align: center;'>
          <em>PMI(w<sub>1</sub>, w<sub>2</sub>) = log ( P(w<sub>1</sub>, w<sub>2</sub>) / (P(w<sub>1</sub>) P(w<sub>2</sub>)) )</em>
      </p>

      <p>where P(w<sub>1</sub>, w<sub>2</sub>) is the probability of words w<sub>1</sub> and w<sub>2</sub> appearing together, and P(w<sub>1</sub>) and P(w<sub>2</sub>) are their individual probabilities. High PMI values indicate strong word associations, making the phrase a good multi-word candidate.</p>

      <p><strong>Reference:</strong><br>
      Church, K. W., &amp; Hanks, P. (1990). <em>Word association norms, mutual information, and lexicography</em>. Computational Linguistics, 16(1), 22-29.</p>

     <br><h4><strong>- Mutual Dependency (MD)</strong></h4>
      <p>Mutual Dependency extends PMI by considering the full context of a multi-word expression rather than just pairwise co-occurrence. It incorporates statistical dependency measures, ensuring that all words in a multi-word sequence contribute significantly to its overall meaning. This approach is particularly useful for identifying multi-word units beyond simple bigrams.</p>

      <p><strong>Reference:</strong><br>
      Thanopoulos, A., Fakotakis, N., &amp; Kokkinakis, G. (2002, May). <em>Comparative Evaluation of Collocation Extraction Metrics.</em> In LREC (Vol. 2, pp. 620-625).</p>

     <br><h4><strong>- Log-Frequency Biased Mutual Dependency (LF-MD)</strong></h4>
      <p>LF-MD refines the MD approach by incorporating word frequency into the dependency calculation. This method biases the selection of multi-word expressions toward frequent collocations while maintaining a balance between statistical significance and linguistic relevance. It is particularly useful in extracting meaningful multi-word expressions in large corpora where rare but statistically significant collocations might otherwise dominate.</p>

      <p><strong>Reference:</strong><br>
        Thanopoulos, A., Fakotakis, N., &amp; Kokkinakis, G. (2002, May). <em>Comparative Evaluation of Collocation Extraction Metrics.</em> In LREC (Vol. 2, pp. 620-625).</p>
  </body>
  "

  multiwordlist <- "<body>

    <h3><strong>Multi-Word Creation by a List in TALL</strong></strong></h3>

    <p>TALL allows users to define <strong>multi-word expressions (MWEs)</strong> by importing a predefined list of multi-word terms. This feature is particularly useful for ensuring that specific phrases or domain-specific expressions are treated as single units during text processing, improving linguistic analysis.</p>
    <hr>
    <h4><strong>How to Import a Multi-Word List</strong></strong></h4>
    <p>To integrate multi-word expressions into the analysis, users must provide a properly formatted list:</p>
    <ul>
    <li>The list must be in <strong>Excel (<code>.xlsx</code>) or CSV (<code>.csv</code>) format</strong>.</li>
    <li>The file should contain a <strong>single column</strong> where each row represents one multi-word expression.</li>
    <li><strong>Each term within a multi-word expression must be separated by a single whitespace<br></strong> (e.g., <code>machine learning</code>, <code>natural language processing</code>).</li>
    </ul>
    <hr>
    <h4><strong>Why Use Multi-Word Expressions?</strong></strong></h4>
    <ul>
    <li><strong>Preserving Meaningful Phrases:</strong> Ensuring that key terms (e.g., <code>artificial intelligence</code>) are not split into separate words.</li>
    <li><strong>Improving Text Preprocessing:</strong> Enhancing tokenization and lemmatization by treating phrases as cohesive units.</li>
    <li><strong>Enhancing Domain-Specific Analysis:</strong> Beneficial in specialized fields such as legal, medical, or technical texts, where multi-word terms have precise meanings.</li>
    </ul>

    <p>By supporting multi-word recognition, TALL provides users with greater flexibility in structuring their text analysis and ensures that critical expressions are accurately identified and processed.</p>

    </body>"

  customterm <- "<body>

    <h3><strong>Custom Term List in TALL</strong></strong></h3>

    <p>TALL allows users to define a <strong>Custom Term List</strong>, enabling more precise control over text processing and linguistic analysis. This feature allows users to manually assign custom tags to specific terms, overriding their default categorization by the language model.</p>
    <hr>
    <h4><strong>Why Use a Custom Term List?</strong></strong></h4>
    <ul>
    <li><strong>Highlighting Specific Concepts:</strong> Identifying key terms related to methodologies, specialized vocabulary, or domain-specific jargon.</li>
    <li><strong>Filtering Stop Words:</strong> Removing terms that are irrelevant to the analysis, ensuring a cleaner dataset.</li>
    <li><strong>Enhancing Named Entity Recognition (NER):</strong> Manually tagging specific words that the language model may misclassify.</li>
    <li><strong>Overriding Default PoS Assignments:</strong> Ensuring consistency in tagging across texts by defining a fixed categorization for certain terms.</li>
    </ul>
    <hr>
    <h4><strong>How to Import a Custom Term List</strong></strong></h4>
    <p>To integrate a custom list of terms, users must provide a properly formatted file:</p>
    <ul>
    <li>The list must be in <strong>Excel format (<code>.xlsx</code>)</strong>.</li>
    <li>The file should contain <strong>two columns</strong>:</li>
    <ul>
    <li><strong>First column:</strong> The list of terms to be tagged.</li>
    <li><strong>Second column:</strong> The corresponding Part-of-Speech (PoS) or user-defined category assigned to each term.</li>
    </ul>
    <li>The specified tags should align with standard linguistic categories (e.g., noun, verb, adjective) or custom categories for specific analysis needs.</li>
    </ul>
    <hr>
  <h4><strong>Example of Custom Term List Format</strong></h4>

  <table border='1' cellspacing='0' cellpadding='5'>
  <tr>
  <th> -------- Term ---------- </th>
  <th> ------ Custom Tag ------ </th>
  </tr>
  <tr>
  <td>artificial intelligence</td>
  <td>methodology</td>
  </tr>
  <tr>
  <td>deep learning</td>
  <td>methodology</td>
  </tr>
  <tr>
  <td>preprocess</td>
  <td>data_handling</td>
  </tr>
  <tr>
  <td>dataset</td>
  <td>data_handling</td>
  </tr>
  <tr>
  <td>remove</td>
  <td>Ignore</td>
  </tr>
  </table>
<br>
    <p>By allowing users to define and control term tagging, TALL provides enhanced flexibility for text analysis, making it a powerful tool for domain-specific research and refined linguistic processing.</p>

    </body>"

  posselection <- "<body>

    <h3><strong>PoS Tagging Selection in TALL</strong></strong></h3>

    <p>TALL provides users with the flexibility to <strong>select specific Part-of-Speech (PoS) tags</strong> to be used in subsequent analyses.
    This feature allows for greater control over the linguistic elements included in text processing,
    ensuring that only relevant grammatical categories are considered.</p>
    <hr>
    <h4><strong>Why Select PoS Tags?</strong></strong></h4>
    <ul>
    <li><strong>Filtering Out Unnecessary Elements:</strong> Excluding determiners, conjunctions, or punctuation that may not contribute to the analysis.</li>
    <li><strong>Focusing on Key Linguistic Features:</strong> Selecting only nouns and verbs for topic modeling, or adjectives and adverbs for sentiment analysis.</li>
    <li><strong>Improving Computational Efficiency:</strong> Reducing data size and processing time by analyzing only the most relevant word categories.</li>
    </ul>
    <hr>
    <h4><strong>How It Works in TALL</strong></strong></h4>
    <ul>
    <li>Users can manually <strong>select or deselect</strong> PoS categories from a predefined list.</li>
    <li>The available PoS tags follow the <strong>Universal Dependencies (UD) annotation scheme</strong>, ensuring consistency across different languages.</li>
    </ul>
    <hr>
    <h4><strong>Default Selected PoS Tags</strong></strong></h4>
    <p>By default, TALL selects the following PoS categories:</p>
    <ul>
    <li><strong>ADJ:</strong> Adjective – Descriptive words (e.g., 'beautiful', 'quick').</li>
    <li><strong>NOUN:</strong> Noun – Common nouns representing entities (e.g., 'dog', 'city').</li>
    <li><strong>PROPN:</strong> Proper Noun – Specific names of places, people, or organizations (e.g., 'London', 'NASA').</li>
    <li><strong>VERB:</strong> Verb – Action words representing processes (e.g., 'run', 'speak').</li>
    <li><strong>HAPAX:</strong> Words appearing only once in the text, useful for lexical richness analysis.</li>
    </ul>
    <hr>
    <h4><strong>Available PoS Categories in TALL</strong></strong></h4>
    <table border='1' cellspacing='0' cellpadding='5'>
    <tr>
    <th>PoS Tag</th>
    <th>Description</th>
    </tr>
    <tr><td>ADJ</td><td>Adjective</td></tr>
    <tr><td>ADP</td><td>Adposition</td></tr>
    <tr><td>ADV</td><td>Adverb</td></tr>
    <tr><td>AUX</td><td>Auxiliary</td></tr>
    <tr><td>CCONJ</td><td>Coordinating Conjunction</td></tr>
    <tr><td>DET</td><td>Determiner</td></tr>
    <tr><td>INTJ</td><td>Interjection</td></tr>
    <tr><td>NOUN</td><td>Noun</td></tr>
    <tr><td>NUM</td><td>Numeral</td></tr>
    <tr><td>PART</td><td>Particle</td></tr>
    <tr><td>PRON</td><td>Pronoun</td></tr>
    <tr><td>PROPN</td><td>Proper Noun</td></tr>
    <tr><td>PUNCT</td><td>Punctuation</td></tr>
    <tr><td>SCONJ</td><td>Subordinating Conjunction</td></tr>
    <tr><td>SYM</td><td>Symbol</td></tr>
    <tr><td>VERB</td><td>Verb</td></tr>
    <tr><td>X</td><td>Other</td></tr>
    <tr><td>Hapax</td><td>Words appearing only once in the corpus</td></tr>
    <tr><td>Single Character</td><td>Individual symbols or characters</td></tr>
    </table>
    <hr>
    <h4><strong>Custom Categories</strong></strong></h4>
    <p>In addition to predefined PoS categories, users may have also <strong>generated custom categories</strong> through the <strong>Custom List</strong> and <strong>Multi-Word</strong> menus.
    <br>These user-defined tags allow for specialized analysis by grouping specific terms under a unique classification system.</p>
    <hr>
    <h4><strong>Enhancing Analysis with PoS Selection</strong></strong></h4>
    <p>By allowing users to choose specific PoS categories, TALL ensures that the analysis is tailored to the user's research goals.
    <br>Whether performing <strong>keyword extraction, syntactic analysis, topic modeling, or sentiment analysis</strong>,
    the ability to refine PoS selection enhances the precision and interpretability of results.</p>

</body>"

overview <- "<body>
  <div class='container'>
    <h3><strong>Corpus Metrics in TALL</strong></h3>
    <p>These metrics provide a summary of the key textual characteristics of the analyzed corpus.</p>

    <h4><strong>📂 Corpus Size & Structure</strong></h4>
    <ul>
    <li><strong>Documents →</strong> The total number of documents in the corpus.</li>
    <li><strong>Sentences →</strong> The total number of sentences in the corpus.</li>
    <li><strong>Tokens →</strong> The total number of words or linguistic units, including punctuation marks.</li>
    <li><strong>Types →</strong> The number of unique words in the corpus, representing vocabulary richness.</li>
    <li><strong>Lemma →</strong> The number of unique lemmas, considering the base form of words.</li>
    </ul>

    <h4><strong>📏 Average Length Metrics</strong></h4>
    <ul>
    <li><strong>Doc Avg Length in Chars →</strong> The average number of characters per document.<br>
    <math xmlns='http://www.w3.org/1998/Math/MathML' style='font-size: 1.5em; display: block; text-align: center; margin-top: 4px; margin-bottom: 12px;'>
      <mrow>
      <mfrac>
      <mtext>Total Characters</mtext>
      <mtext>Number of Documents</mtext>
      </mfrac>
      </mrow>
      </math>
      </li>
      <li><strong>Doc Avg Length in Tokens →</strong> The average number of tokens per document.<br>
      <math xmlns='http://www.w3.org/1998/Math/MathML' style='font-size: 1.5em; display: block; text-align: center; margin-top: 4px; margin-bottom: 12px;'>
        <mrow>
        <mfrac>
        <mtext>Total Tokens</mtext>
        <mtext>Number of Documents</mtext>
        </mfrac>
        </mrow>
        </math>
        </li>
        <li><strong>Sent Avg Length in Chars →</strong> The average number of characters per sentence.<br>
        <math xmlns='http://www.w3.org/1998/Math/MathML' style='font-size: 1.5em; display: block; text-align: center; margin-top: 4px; margin-bottom: 12px;'>
          <mrow>
          <mfrac>
          <mtext>Total Characters</mtext>
          <mtext>Number of Sentences</mtext>
          </mfrac>
          </mrow>
          </math>
          </li>
          <li><strong>Sent Avg Length in Tokens →</strong> The average number of tokens per sentence.<br>
          <math xmlns='http://www.w3.org/1998/Math/MathML' style='font-size: 1.5em; display: block; text-align: center; margin-top: 4px; margin-bottom: 12px;'>
            <mrow>
            <mfrac>
            <mtext>Total Tokens</mtext>
            <mtext>Number of Sentences</mtext>
            </mfrac>
            </mrow>
            </math>
            </li>
            </ul>

            <h4><strong>📊 Lexical Metrics</strong></h4>
            <ul>
            <li><strong>Type-Token Ratio (TTR) →</strong> Ratio of unique words (types) to total words (tokens). Higher values indicate greater lexical diversity.<br>
            <math xmlns='http://www.w3.org/1998/Math/MathML' style='font-size: 1.5em; display: block; text-align: center; margin-top: 4px; margin-bottom: 12px;'>
              <mrow>
              <mtext>TTR = </mtext>
                <mfrac>
                <mtext>Types</mtext>
                <mtext>Tokens</mtext>
                </mfrac>
                </mrow>
                </math>
                </li>

                <li><strong>Hapax Legomena (%) →</strong> Percentage of words that appear only once in the corpus.<br>
                <math xmlns='http://www.w3.org/1998/Math/MathML' style='font-size: 1.5em; display: block; text-align: center; margin-top: 4px; margin-bottom: 12px;'>
                  <mrow>
                  <mtext>Hapax % = </mtext>
                    <mfrac>
                    <mtext>Hapax</mtext>
                    <mtext>Types</mtext>
                    </mfrac>
                    <mo>×</mo>
                    <mn>100</mn>
                    </mrow>
                    </math>
                    </li>

                    <li><strong>Guiraud Index →</strong> Measure of lexical richness correcting for text length.<br>
                    <math xmlns='http://www.w3.org/1998/Math/MathML' style='font-size: 1.5em; display: block; text-align: center; margin-top: 4px; margin-bottom: 12px;'>
                      <mrow>
                      <mtext>Guiraud = </mtext>
                        <mfrac>
                        <mtext>Types</mtext>
                        <msqrt>
                        <mtext>Tokens</mtext>
                        </msqrt>
                        </mfrac>
                        </mrow>
                        </math>
                        </li>
                        </ul>

                        <h4><strong>📊 Additional Lexical Measures</strong></h4>
                        <ul>
                        <li><strong>Lexical Density →</strong> Proportion of content words over total tokens.<br>
                        <math xmlns='http://www.w3.org/1998/Math/MathML' style='font-size: 1.5em; display: block; text-align: center; margin-top: 4px; margin-bottom: 12px;'>
                          <mrow>
                          <mtext>Lexical Density = </mtext>
                            <mfrac>
                            <mtext>Content Words</mtext>
                            <mtext>Total Tokens</mtext>
                            </mfrac>
                            </mrow>
                            </math>
                            </li>

                            <li><strong>Nominal Ratio →</strong> Ratio between nouns and verbs.<br>
                            <math xmlns='http://www.w3.org/1998/Math/MathML' style='font-size: 1.5em; display: block; text-align: center; margin-top: 4px; margin-bottom: 12px;'>
                              <mrow>
                              <mtext>Nominal Ratio = </mtext>
                                <mfrac>
                                <mtext>Number of Nouns</mtext>
                                <mtext>Number of Verbs</mtext>
                                </mfrac>
                                </mrow>
                                </math>
                                </li>

                                <li><strong>Gini Index →</strong> Measure of inequality in word frequency distribution. Calculated from the Lorenz curve of word frequencies.</li>

                                <li><strong>Yule’s K Index →</strong> Measure of lexical diversity based on word repetition.<br>
                                <math xmlns='http://www.w3.org/1998/Math/MathML' style='font-size: 1.5em; display: block; text-align: center; margin-top: 4px; margin-bottom: 12px;'>
                                  <mrow>
                                  <mtext>K = </mtext>
                                    <mn>10,000</mn>
                                    <mo>×</mo>
                                    <mfrac>
                                    <mrow>
                                    <mo>(</mo>
                                           <munderover>
                                           <mo>&#x2211;</mo>
                                           <mi>i</mi>
                                           <mi>n</mi>
                                           </munderover>
                                           <msup><mi>f</mi><mn>2</mn></msup>
                                           <mo>)</mo>
                                    <mo>-</mo>
                                    <mi>N</mi>
                                    </mrow>
                                    <msup><mi>N</mi><mn>2</mn></msup>
                                    </mfrac>
                                    </mrow>
                                    </math>
                                    </li>
                                    </ul>

                                    <div class='references'>
                                      <h4><strong>References</strong></h4>
                                      <p><strong>Baayen, R. H.</strong> <i>The effect of lexical specialization on the growth curve of vocabulary.</i> <strong>Computational Linguistics</strong>, 22(2), 1996.</p>
                                      <p><strong>Bentz, C., Alikaniotis, D., Cysouw, M., & Ferrer-i-Cancho, R.</strong> <i>The entropy of words—learnability and expressivity across more than 1000 languages.</i> <strong>Entropy</strong>, 19(6), 2017.</p>
                                      <p><strong>Biber, D.</strong> <i>Variation across speech and writing.</i> <strong>Cambridge University Press</strong>, 1988.</p>
                                      <p><strong>Guiraud, P.</strong> <i>Les caractères statistiques du vocabulaire.</i> <strong>Presse Universitaire de France</strong>, 1954.</p>
                                      <p><strong>Tweedie, F. J., & Baayen, R. H.</strong> <i>How variable may a constant be? Measures of lexical richness in perspective.</i> <strong>Computers and the Humanities</strong>, 32(5), 323–352, 1998.</p>
                                      <p><strong>Ure, J.</strong> <i>Lexical density and register differentiation. In G. Perren and J.L.M. Trim (eds).</i> <strong>Applications of Linguistics</strong>, Cambridge University Press, 443–452, 1971.</p>
                                      <p><strong>Yule, G. U.</strong> <i>The statistical study of literary vocabulary.</i> <strong>Cambridge University Press</strong>, 1944.</p>
                                      </div>
                                      </div>
                                      </body>"

wordincontext <- "<body>

    <h3><strong>Words in Context in TALL</strong></h3>

    <p>The <strong>Words in Context</strong> feature in TALL allows users to analyze how specific words appear in textual data, offering valuable insights into <strong>semantic usage, contextual meaning, and discourse structure</strong>. This tool is particularly useful for <strong>qualitative text analysis, linguistic research, and content exploration</strong> in diverse domains, such as <strong>social sciences, digital humanities, marketing, and legal studies</strong>.</p>
    <hr>
    <h4><strong>How Words in Context Works in TALL</strong></h4>

    <h4><strong>1. Concordance Analysis (Keyword in Context - KWIC)</strong></h4>
    <ul>
    <li>Displays a <strong>side-by-side view of words and their surrounding textual context</strong> (left and right neighbors).</li>
    <li>Helps in identifying <strong>common phrases, recurring structures, and usage variations</strong>.</li>
    <li>Useful for <strong>studying semantic shifts, idiomatic expressions, and collocations</strong>.</li>
    </ul>

    <div class='example'>
      📌 <strong>Example:</strong><br>
      If analyzing the term <strong>'sustainable'</strong> in a corpus of news articles, KWIC might show:<br>
      - 'sustainable <strong>development</strong> is a key focus of international policies'<br>
      - 'the company promotes <strong>sustainable</strong> and ethical supply chains'<br>
      - 'concerns over <strong>sustainable</strong> agricultural practices are increasing'<br>
      This helps in understanding <strong>how 'sustainable' is used in different thematic contexts</strong>.
    </div>

      <h4><strong>2. Context Window Customization</strong></h4>
      <ul>
      <li>Users can define the <strong>window size</strong> (number of words before and after the target term) to adjust the level of contextual information displayed.</li>
      <li>Shorter windows highlight <strong>immediate linguistic relationships</strong>, while larger windows help analyze <strong>broader semantic dependencies</strong>.</li>
      </ul>

      <div class='example'>
        📌 <strong>Example:</strong><br>
        When studying <strong>'risk'</strong> in financial reports, adjusting the window size allows users to see if it is used in association with:<br>
        - <strong>'risk management,' 'high-risk investments'</strong> (short window)<br>
        - <strong>'the recent economic downturn has increased financial risk for small businesses'</strong> (larger window)<br>
        </div>

        <h4><strong>3. Frequency and Distribution Insights</strong></h4>
        <ul>
        <li>Words appearing in <strong>multiple contexts</strong> can be analyzed for <strong>frequency trends</strong>, helping users identify <strong>dominant themes</strong> associated with a term.</li>
        <li>Examines whether a word is <strong>evenly distributed</strong> across the corpus or <strong>clustered</strong> in specific sections/documents.</li>
        </ul>

        <div class='example'>
          📌 <strong>Example:</strong><br>
          In a dataset of <strong>customer reviews</strong>, the word <strong>'expensive'</strong> might frequently co-occur with:<br>
          - <strong>'but worth it'</strong> in <strong>positive reviews</strong><br>
          - <strong>'not justified for the quality'</strong> in <strong>negative reviews</strong><br>
          This helps distinguish <strong>when 'expensive' has a neutral, positive, or negative connotation</strong>.
        </div>

          <p>By enabling <strong>customizable and interactive text exploration</strong>, the <strong>Words in Context</strong> tool in TALL provides users with <strong>a deeper understanding of language patterns</strong> in large textual datasets.</p>


          </body>"

  reinert <- "<body>

    <h3><strong>Reinert Clustering in TALL</strong></h3>

    <p>Reinert clustering is a <strong>hierarchical descending classification method</strong> used for <strong>textual data clustering</strong>. It identifies <strong>lexically homogeneous word clusters</strong> based on the <strong>co-occurrence of terms within textual contexts</strong>. Originally developed by <strong>Max Reinert (1983, 1990)</strong>, this approach has become a core method in <strong>corpus linguistics, sociolinguistics, and content analysis</strong>.</p>

    <p>Reinert’s method is particularly effective in <strong>structuring large textual datasets</strong>, making it a powerful tool for <strong>thematic segmentation, discourse analysis, and socio-linguistic research</strong>.</p>
    <hr>
    <h4><strong>How Reinert Clustering Works in TALL</strong></h4>

    <h4>1. Text Segmentation into Context Units</h4>
    <ul>
    <li>The text is divided into <strong>small context units (CUs)</strong>, typically <strong>paragraphs or fixed-length segments</strong>, to capture local lexical co-occurrence patterns.</li>
    <li>Each CU is treated as a <strong>vector</strong> of word frequencies.</li>
    </ul>

    <h4>2. Iterative Splitting of Clusters</h4>
    <ul>
    <li>The method starts with <strong>all CUs grouped together</strong>.</li>
    <li>A <strong>first split</strong> is performed, <strong>maximizing intra-cluster homogeneity</strong> while ensuring that <strong>word distributions</strong> differ between groups.</li>
    <li>This <strong>recursive process</strong> continues until no further meaningful lexical differentiation can be achieved.</li>
    </ul>

    <h4>3. Statistical Association of Words to Clusters</h4>
    <ul>
    <li>Words are <strong>assigned probabilistic weights</strong> based on their <strong>distribution within each cluster</strong>.</li>
    <li>The <strong>most characteristic words</strong> of each cluster are identified, forming the <strong>lexical profile</strong> of the topic.</li>
    </ul>

    <h4>4. Interpretation and Thematic Analysis</h4>
    <ul>
    <li>The final clusters represent <strong>coherent thematic units</strong>.</li>
    <li>Thematic interpretation is facilitated by <strong>analyzing the most significant words in each cluster</strong>.</li>
    </ul>
    <hr>
    <h4><strong>Reinert Clustering vs. Traditional Topic Modeling</strong></h4>
    <table border='1' cellspacing='0' cellpadding='5'>
      <tr>
      <th>Feature</th>
      <th>Reinert Clustering</th>
      <th>LDA Topic Modeling</th>
      </tr>
      <tr>
      <td><strong>Method</strong></td>
      <td>Hierarchical word clustering</td>
      <td>Probabilistic word-topic assignment</td>
      </tr>
      <tr>
      <td><strong>Output</strong></td>
      <td>Discrete word clusters with distinct themes</td>
      <td>Soft assignment of words to topics</td>
      </tr>
      <tr>
      <td><strong>Context Sensitivity</strong></td>
      <td>High – Uses local lexical co-occurrence</td>
      <td>Medium – Uses global probability distributions</td>
      </tr>
      <tr>
      <td><strong>Interpretability</strong></td>
      <td>Direct thematic segmentation</td>
      <td>Requires manual topic interpretation</td>
      </tr>
      <tr>
      <td><strong>Application</strong></td>
      <td>Text segmentation, discourse analysis</td>
      <td>Thematic classification, topic inference</td>
      </tr>
      </table>
    <hr>
      <h4><strong>Implementation of Reinert Clustering in TALL</strong></h4>

      <p>The implementation of <strong>Reinert clustering in TALL</strong> was <strong>inspired by the 'rainette' package</strong> (<strong>Barnier & Privé, 2023</strong>). The original routines have been <strong>adapted to work with the TALL data structure</strong>, which includes <strong>tokenized, lemmatized, and PoS-tagged corpora</strong>.</p>

      <p>This adaptation allows:</p>
      <ul>
      <li><strong>Customization of context unit size</strong> to fit different corpus structures.</li>
      <li><strong>Compatibility with pre-processed linguistic data</strong>, ensuring greater accuracy in lexical clustering.</li>
      <li><strong>Optimized performance</strong> for large-scale text analysis, leveraging <strong>TALL’s text processing pipeline</strong>.</li>
      <li><strong>Graphical visualization</strong> of thematic structures to facilitate <strong>interpretation and reporting</strong>.</li>
      </ul>

      <p>By adapting <strong>Reinert’s methodology to TALL’s specialized NLP framework</strong>, researchers can <strong>conduct advanced text clustering analyses</strong> while maintaining compatibility with <strong>state-of-the-art linguistic preprocessing techniques</strong>.</p>
    <hr>
      <div class='references'>
        <h4><strong>References</strong></h4>

        <p><strong>Reinert, M.</strong></p>
        <p><i>Une méthode de classification descendante hiérarchique : application à l'analyse lexicale par contexte.</i> <strong>Cahiers de l'analyse des données</strong>, 8(2), 1983.</p>

        <p><strong>Reinert, M.</strong></p>
        <p><i>Alceste: Une méthodologie d'analyse des données textuelles et une application: Aurelia De Gerard De Nerval.</i> <strong>Bulletin de Méthodologie Sociologique</strong>, 26(1), 1990. DOI: <a href='https://doi.org/10.1177/075910639002600103' target='_blank'>10.1177/075910639002600103</a></p>

        <p><strong>Barnier, J., & Privé, F.</strong></p>
        <p><i>rainette: The Reinert Method for Textual Data Clustering.</i> <strong>R CRAN Repository</strong>, 2023. DOI: <a href='https://doi.org/10.32614/CRAN.package.rainette' target='_blank'>10.32614/CRAN.package.rainette</a></p>
    </div>

</body>"

  correspondenceanalysis <- "<body>

    <h3><strong>Correspondence Analysis in TALL</strong></strong></h3>

    <p>Correspondence Analysis (<strong>CA</strong>) is a fundamental technique for exploring <strong>semantic relationships</strong> among words within a text collection (<strong>Benzécri, 1982; Lebart et al., 1997</strong>). By applying <strong>dimensionality reduction</strong>, CA represents the most relevant information in a low-rank vector space, uncovering <strong>latent structures</strong> within the data. These structures are then <strong>visualized on factorial maps</strong>, allowing users to detect associations between terms and documents effectively.</p>
    <hr>
    <h4><strong>Why Use Correspondence Analysis?</strong></h4>
    <ul>
    <li><strong>Revealing Hidden Patterns:</strong> CA captures relationships between words and documents that might not be immediately apparent.</li>
    <li><strong>Dimensionality Reduction:</strong> By projecting the data into a lower-dimensional space, CA simplifies complex text corpora while retaining key semantic information.</li>
    <li><strong>Visualization on Factorial Maps:</strong> The results are displayed on a <strong>graphical representation</strong>, enabling easy interpretation of term clusters and document similarities.</li>
    </ul>
    <hr>
    <h4><strong>Limitations of Correspondence Analysis</strong></h4>
    <p>One of the primary challenges of CA is that the <strong>new features</strong> generated through dimensionality reduction often lack <strong>direct interpretability</strong>. Since the transformation is data-driven, the factors extracted do not always correspond to clear linguistic or thematic constructs, making it more difficult to derive <strong>explicit meaning</strong> from the analysis.</p>
    <hr>
    <h4><strong>Enhancing Interpretability: The Tandem Approach</strong></h4>
    <p>To address this limitation, <strong>TALL integrates a tandem approach</strong>, which combines CA with <strong>clustering techniques</strong> to improve the interpretability of results (<strong>Misuraca & Spano, 2020</strong>). This approach follows a <strong>two-step process</strong>:</p>
    <ul>
    <li><strong>Dimensionality Reduction with CA:</strong> The text data is transformed into <strong>orthogonal and ordered features</strong>, preserving essential relationships while reducing complexity.</li>
    <li><strong>Hierarchical Clustering:</strong> Clustering is applied to the transformed data, allowing for <strong>multi-level aggregation</strong> of terms and documents. Unlike simple factor analysis, this method provides <strong>non-overlapping clusters</strong>, making the results easier to interpret.</li>
    </ul>
    <hr>
    <h4><strong>Applications of Correspondence Analysis in Text Mining</strong></h4>
    <ul>
    <li><strong>Exploring Co-occurrence Patterns:</strong> Identifying how frequently certain words appear together in a corpus.</li>
    <li><strong>Thematic Segmentation:</strong> Grouping documents based on their shared linguistic characteristics.</li>
    <li><strong>Semantic Mapping:</strong> Revealing <strong>latent structures</strong> within unstructured text data.</li>
    <li><strong>Lexical Field Analysis:</strong> Understanding how words are distributed and related within a text collection.</li>
    </ul>

    <p>By integrating <strong>Correspondence Analysis</strong> with <strong>clustering methods</strong>, TALL enhances the <strong>interpretability and usability</strong> of text mining workflows, offering a <strong>powerful framework</strong> for <strong>unsupervised exploration</strong> of large document collections.</p>
    <hr>
    <div class='references'>
      <h4><strong>References</strong></strong></h4>
      <p><strong>Benzécri, J. P.</strong> (1982). <i>Histoire et préhistoire de l’analyse des données.</i> Paris: Dunod.</p>
      <p><strong>Lebart, L., Salem, A., & Berry, L.</strong> (1997). <i>Exploring textual data.</i> Volume 4. Springer Science & Business Media.</p>
      <p><strong>Misuraca, M., & Spano, M.</strong> (2020). <i>Unsupervised Analytic Strategies to Explore Large Document Collections.</i> Heidelberg: Springer, 06, 17-28.</p>
      </div>

      </body>"

  cowordanalysis <- "<body>

    <h3><strong>Co-Word Analysis in TALL</strong></strong></h3>

    <p>Co-word analysis is a <strong>network-based text mining technique</strong> that examines <strong>co-occurrence patterns</strong> of words within a corpus, identifying <strong>semantic structures</strong> based on term relationships (<strong>Callon et al., 1983</strong>). This method is particularly valuable in <strong>detecting thematic clusters</strong> within large textual datasets, as it helps uncover <strong>conceptual linkages</strong> and <strong>emerging research topics</strong> in various fields.</p>
    <hr>
    <h4><strong>How Co-Word Analysis Works</strong></h4>
    <ul>
    <li><strong>Nodes represent words</strong> (terms extracted from the corpus).</li>
    <li><strong>Edges represent co-occurrence relationships</strong> (connections between words appearing together in the same context).</li>
    <li><strong>Edge weights reflect frequency</strong>, meaning stronger relationships are represented by thicker connections.</li>
    </ul>
    <hr>
    <h4><strong>Normalization Measures in Co-Word Analysis</strong></h4>
    <p>Raw co-occurrence frequencies can be <strong>biased by term frequency</strong> in the corpus, making normalization essential to provide meaningful co-word relationships. TALL allows users to apply different normalization measures to refine co-occurrence networks (<strong>Eck & Waltman, 2009</strong>):</p>

    <h4><em>Association Index</em></h4>
    <p>The <strong>Association Index (AI)</strong> normalizes co-occurrence counts relative to the <strong>expected frequency of terms</strong> in the corpus:</p>
    <p><code>AI<sub>ij</sub> = C<sub>ij</sub> / (C<sub>i</sub> × C<sub>j</sub>)</code></p>

      <h4><em>Cosine Similarity</em></h4>
      <p><strong>Cosine Similarity</strong> measures how similar two terms are based on their co-occurrence across different documents:</p>
      <p><code>cos(θ) = C<sub>ij</sub> / sqrt(C<sub>i</sub> × C<sub>j</sub>)</code></p>

        <h4><em>Jaccard Similarity</em></h4>
        <p>The <strong>Jaccard Similarity</strong> measures the co-occurrence strength relative to the total occurrences of both words:</p>
        <p><code>J<sub>ij</sub> = C<sub>ij</sub> / (C<sub>i</sub> + C<sub>j</sub> - C<sub>ij</sub>)</code></p>
    <hr>
          <h4><strong>Community Detection for Semantic Clustering</strong></h4>
          <p>To extract thematic clusters, TALL applies the <strong>Walktrap algorithm</strong> for community detection (<strong>Pons & Latapy, 2006</strong>):</p>
          <ul>
          <li>Uses <strong>random walks</strong> on the co-occurrence network to detect <strong>structurally cohesive word communities</strong>.</li>
          <li>Efficiently discovers <strong>hierarchical relationships</strong> among terms.</li>
          <li>Groups words into <strong>non-overlapping clusters</strong>, representing <strong>latent topics</strong> or <strong>conceptual domains</strong> within the corpus.</li>
          </ul>
    <hr>
          <h4><strong>Applications of Co-Word Analysis</strong></h4>
          <ul>
          <li><strong>Bibliometric and Scientometric Studies:</strong> Identifying research trends and thematic structures in academic literature.</li>
          <li><strong>Topic Detection in Large Text Collections:</strong> Extracting underlying themes from newspapers, reports, or social media content.</li>
          <li><strong>Keyword Network Exploration:</strong> Understanding <strong>how keywords interconnect</strong> and contribute to discourse formation.</li>
          <li><strong>Patent and Innovation Analysis:</strong> Revealing technological trends by examining term co-occurrence in patent databases.</li>
          <li><strong>Social Media and Sentiment Analysis:</strong> Discovering key discussion topics within online platforms.</li>
          </ul>
    <hr>
          <h4><strong>Advantages of Co-Word Analysis in TALL</strong></h4>
          <ul>
          <li><strong>Unsupervised Approach:</strong> Extracts thematic clusters <strong>without requiring predefined categories</strong>.</li>
          <li><strong>Graph-Based Representation:</strong> Provides an <strong>intuitive visualization</strong> of textual structures.</li>
          <li><strong>Scalable to Large Text Corpora:</strong> Efficiently handles extensive document collections.</li>
          <li><strong>Integration with Other Analytical Techniques:</strong> Can be combined with <strong>Correspondence Analysis</strong>, <strong>Topic Modeling</strong>, and <strong>Sentiment Analysis</strong> for richer insights.</li>
          </ul>
    <hr>
          <div class='references'>
            <h4><strong>References</strong></h4>
            <p><strong>Callon, M., Courtial, J.-P., Turner, W.A., & Bauin, S.</strong></p>
            <p><i>From translations to problematic networks: An introduction to co-word analysis.</i> <strong>Social Science Information</strong>, 22(2), 191-235.</p>

            <p><strong>Eck, N. J. V., & Waltman, L.</strong></p>
            <p><i>How to normalize co-occurrence data? An analysis of some well‐known similarity measures.</i> <strong>Journal of the American Society for Information Science and Technology</strong>, 60(8), 1635-1651.</p>

            <p><strong>Fortunato, S., & Hric, D.</strong></p>
            <p><i>Community detection in networks: A user guide.</i> <strong>Physics Reports</strong>, 659, 1-44.</p>

            <p><strong>Pons, P., & Latapy, M.</strong></p>
            <p><i>Computing communities in large networks using random walks.</i> Retrieved from <a href='https://arxiv.org/abs/physics/0512106' target='_blank'>arXiv:physics/0512106</a>.</p>
              </div>

              </body>"

  thematicmap <- "
  <body>

  <h3><strong>Thematic Map</strong></h3>

  <p>
  The <strong>Thematic Map</strong> feature in TALL enables users to explore the conceptual structure of a text corpus by visually mapping the most relevant topics. It is based on an unsupervised, network-based method designed to extract, cluster, and characterize groups of words representing distinct semantic areas within the analyzed texts. This approach has been successfully applied in bibliometric research and adapted in TALL for general-purpose text analysis.
</p>

  <hr>

  <h4><strong>Methodological Framework</strong></h4>

  <p>
  Thematic mapping starts with the construction of a <strong>co-occurrence matrix</strong> from the pre-processed text corpus. The association strength between terms is then calculated to normalize the raw co-occurrence frequencies:
  </p>

  <math xmlns=,http://www.w3.org/1998/Math/MathML, style=,font-size: 1.1em; display: block; text-align: center; margin: 10px 0;,>
    <mrow>
    <mtext>AS</mtext><msub><mi>jj'</mi></msub><mo>=</mo>
      <mfrac>
        <msub><mi>a</mi><mi>jj'</mi></msub>
    <mrow>
    <msub><mi>a</mi><mi>jj</mi></msub><mo>&#x22C5;</mo><msub><mi>a</mi><mi>j'j'</mi></msub>
    </mrow>
    </mfrac>
    </mrow>
    </math>

    <p>
    where <em>AS<sub>jj'</sub></em> is the association strength between terms <em>j</em> and <em>j'</em>, and <em>a<sub>jj'</sub></em> is their observed co-occurrence. This metric expresses the semantic relatedness of term pairs.
  </p>

  <p>
    A <strong>community detection algorithm</strong> (WalkTrap) is then applied to the normalized network to identify clusters of terms (i.e., topics). Each cluster is projected onto a two-dimensional plane using two dimensions:
  </p>

  <ul>
    <li><strong>Callon Centrality (CC):</strong> measures a topic’s interaction with others, indicating its <em>relevance</em> in the corpus.</li>
    <li><strong>Callon Density (CD):</strong> measures the internal cohesion of the topic, reflecting its <em>development</em>.</li>
  </ul>

  <p>Each topic is placed on a <strong>strategic diagram</strong> based on its centrality and density values:</p>

  <ul>
    <li><strong>Upper-right (Hot Topics):</strong> High centrality and high density – well-developed and important.</li>
    <li><strong>Lower-right (Basic Topics):</strong> High centrality and low density – important but still under development.</li>
    <li><strong>Upper-left (Niche Topics):</strong> Low centrality and high density – well developed but marginal.</li>
    <li><strong>Lower-left (Peripheral Topics):</strong> Low centrality and low density – weakly developed and marginal.</li>
  </ul>

  <hr>

  <h4><strong>Features in TALL</strong></h4>

  <ul>
    <li>Users can generate thematic maps from any textual dataset preprocessed and tokenized in TALL.</li>
    <li>The algorithm works automatically and does <strong>not require setting the number of topics</strong> in advance.</li>
    <li>Topics are labeled by the most frequent keywords within each cluster.</li>
    <li>Topic size (i.e., the size of the bubble) represents the number of terms in the cluster.</li>
    <li>The user can select specific time slices or metadata filters to perform <strong>comparative thematic analysis</strong> across groups or periods.</li>
  </ul>

  <p>
    Thematic maps offer a rich, interpretable representation of discourse structure and are particularly effective for exploratory text mining and culturomic studies.
  </p>

  <hr>

  <div class='references'>
    <h4><strong>References</strong></h4>
    <p><strong>Aria, M., Cuccurullo, C., D’Aniello, L., Misuraca, M., & Spano, M. (2022).</strong> <i>Thematic Analysis as a New Culturomic Tool: The Social Media Coverage on COVID-19 Pandemic in Italy.</i> <strong>Sustainability</strong>, 14(6), 3643. https://doi.org/10.3390/su14063643</p>
    <p><strong>Cobo, M.J., López-Herrera, A.G., Herrera-Viedma, E., & Herrera, F. (2011).</strong> <i>An approach for detecting, quantifying, and visualising the evolution of a research field: A practical application to the fuzzy sets theory field.</i> <strong>Journal of Informetrics</strong>, 5(1), 146–166.</p>
  </div>

</body>

  "

  tmkchoice <- "<body>

    <h3><strong>Topic Modeling in TALL: K Selection</strong></strong></h3>

    <p>Topic modeling is a fundamental technique in <strong>unsupervised text mining</strong>, allowing users to uncover <strong>latent themes</strong> within large collections of documents. One of the key challenges in <strong>Latent Dirichlet Allocation (LDA)</strong> and other topic modeling techniques is determining the <strong>optimal number of topics (K)</strong>.</p>

    <p>TALL estimates <strong>K automatically</strong> using four well-established statistical measures (<strong>Griffiths & Steyvers, 2004; Deveaud et al., 2014; Cao et al., 2009; Arun et al., 2010</strong>).
    <br>However, users can also <strong>manually adjust K</strong> and explore different solutions in the <strong>Model Estimation Menu</strong>, enabling greater flexibility based on the dataset and research objectives.</p>
    <hr>
    <h4><strong>Why is K Selection Important?</strong></h4>
    <ul>
    <li>A <strong>too small K</strong> may <strong>merge distinct topics</strong>, reducing the model's ability to separate different thematic structures.</li>
        <li>A <strong>too large K</strong> may <strong>fragment coherent topics</strong>, introducing unnecessary complexity and reducing interpretability.</li>
        <li>The <strong>correct K</strong> ensures that topics are <strong>coherent, interpretable, and representative</strong> of the dataset.</li>
    </ul>
    <hr>
    <h4><strong>Automatic K Estimation in TALL</strong></h4>
    <p>TALL integrates <strong>four standard measures</strong> for determining the optimal number of topics in LDA:</p>

    <h4><em>Griffiths & Steyvers (2004) – Log-Likelihood Maximization</em></h4>
    <p>- Based on <strong>Bayesian model selection</strong>, this method computes the <strong>log-likelihood</strong> of the dataset given the topic model.</p>
    <p>- The <strong>optimal K</strong> is the one that maximizes the likelihood function, meaning it best explains the data without overfitting.</p>

    <h4><em>Cao et al. (2009) – Topic Coherence Measure</em></h4>
    <p>- Computes the <strong>average pairwise similarity</strong> between topics based on word distributions.</p>
    <p>- The <strong>optimal K</strong> is found when inter-topic similarity is minimized, ensuring that topics are well-separated.</p>

    <h4><em>Arun et al. (2010) – KL Divergence-Based Measure</em></h4>
    <p>- Compares the <strong>word-topic distribution</strong> and <strong>document-topic distribution</strong> using <strong>Kullback-Leibler (KL) divergence</strong>.</p>
    <p>- The <strong>optimal K</strong> is identified as the point where KL divergence stabilizes, meaning topics balance between coherence and specificity.</p>

    <h4><em>Deveaud et al. (2014) – A Hybrid Approach</em></h4>
    <p>- A refinement of previous approaches that balances topic coherence and diversity.</p>
    <p>- The <strong>optimal K</strong> is chosen where <strong>topic distinctiveness</strong> is maximized while preserving thematic coverage.</p>
    <hr>
    <h4><strong>Manual K Adjustment for Customization</strong></h4>
    <p>While <strong>automatic estimation</strong> provides a strong baseline, users may need to adjust <strong>K manually</strong> based on <strong>domain knowledge and interpretability</strong>:</p>
    <ul>
        <li><strong>For exploratory research:</strong> Start with <strong>low K</strong> values (e.g., <strong>5-20 topics</strong>) to gain an <strong>overview of broad themes</strong>.</li>
        <li><strong>For fine-grained analysis:</strong> Use <strong>higher K values</strong> (e.g., <strong>30-100 topics</strong>) to capture <strong>more nuanced subtopics</strong>.</li>
        <li><strong>For benchmarking:</strong> Compare different <strong>K values</strong> using topic coherence scores and human interpretability.</li>
    </ul>
    <hr>
    <div class='references'>
        <h4><strong>References</strong></h4>

        <p><strong>Griffiths, T.L., & Steyvers, M.</strong></p>
        <p><i>Finding scientific topics.</i> <strong>Proceedings of the National Academy of Sciences</strong>, 101(suppl 1), 5228-5235.</p>

        <p><strong>Deveaud, R., Sanjuan, E., & Bellot, P.</strong></p>
        <p><i>Accurate and effective latent concept modeling for ad hoc information retrieval.</i> <strong>Document Numérique</strong>, 17, 61-84.</p>

        <p><strong>Cao, J., Xia, T., Li, J., Zhang, Y., & Tang, S.</strong></p>
        <p><i>A density-based method for adaptive LDA model selection.</i> <strong>Neurocomputing</strong>, 72(7), 1775-1781.</p>

        <p><strong>Arun, R., Suresh, V., Veni Madhavan, C.E., & Narasimha Murthy, M.N.</strong></p>
        <p><i>On finding the natural number of topics with latent Dirichlet allocation: Some observations.</i> In Zaki, M.J., Yu, J.X., Ravindran, B., & Pudi, V. (Eds.),
        <br><strong>Advances in Knowledge Discovery and Data Mining</strong> (pp. 391-402). Berlin, Heidelberg: Springer.</p>
    </div>

</body>"

  tmmodelestimation <- "<body>

    <h3><strong>Topic Modeling in TALL: Model Estimation</strong></strong></h3>

    <p>Topic modeling is a <strong>family of generative statistical models</strong> designed to uncover <strong>semantic structures</strong> within large document collections. These models aim to <strong>identify latent topics</strong> that explain the observed word distributions in text corpora, allowing for a <strong>low-dimensional representation</strong> of textual data.</p>

    <p>Through <strong>probabilistic modeling</strong>, topic modeling enables:</p>
    <ul>
    <li><strong>Discovery of underlying themes</strong> within a collection of documents.</li>
    <li><strong>Assignment of probabilistic membership scores</strong> to documents, indicating their association with different topics.</li>
    <li><strong>Dimensionality reduction</strong>, making it easier to analyze large text datasets by structuring them into meaningful clusters.</li>
    <li><strong>Human interpretability</strong>, as each topic is characterized by a set of <strong>highly associated terms</strong>, making it easier for users to extract insights.</li>
    </ul>
    <hr>
    <h4><strong>Latent Dirichlet Allocation (LDA) in TALL</strong></h4>
    <p>TALL implements the <strong>Latent Dirichlet Allocation (LDA) algorithm</strong> (<strong>Blei et al., 2003</strong>), one of the most widely used topic modeling techniques. LDA is a <strong>Bayesian probabilistic model</strong> that assumes:</p>
    <ul>
    <li><strong>Each document is a mixture of multiple topics</strong>, with different proportions.</li>
    <li><strong>Each topic is defined by a probability distribution over words</strong>, meaning that some words are more strongly associated with a given topic.</li>
    <li><strong>The goal of LDA is to infer these hidden topic distributions</strong>, making it possible to automatically organize, summarize, and analyze large textual datasets.</li>
    </ul>

    <p>LDA operates by:</p>
    <ul>
    <li>Assigning each word in a document to a <strong>latent topic</strong>, estimating topic-word distributions.</li>
    <li>Iteratively adjusting <strong>topic probabilities</strong> to maximize likelihood, ensuring that words are grouped into <strong>meaningful semantic structures</strong>.</li>
    <li>Producing a <strong>document-topic matrix</strong>, where each document is represented as a probability distribution over the identified topics.</li>
    </ul>
    <hr>
    <h4><strong>Advantages of Topic Modeling in TALL</strong></h4>
    <ul>
    <li><strong>Unsupervised Learning</strong> – No prior labeling is required; topics emerge naturally from the dataset.</li>
    <li><strong>Scalability</strong> – LDA efficiently handles <strong>large text corpora</strong>, making it useful for applications ranging from <strong>scientific literature</strong> to <strong>customer reviews</strong>.</li>
    <li><strong>Flexibility</strong> – Users can define <strong>K (number of topics)</strong> manually or use <strong>automatic estimation techniques</strong> (see the <strong>K Selection Menu</strong>).</li>
    <li><strong>Enhanced Text Understanding</strong> – Topics provide a <strong>thematic summary</strong> of a collection, improving text exploration and classification.</li>
    </ul>

    <p>By integrating <strong>state-of-the-art topic modeling techniques</strong>, TALL enables researchers and analysts to <strong>discover hidden structures in textual data</strong>, making it an essential tool for <strong>content analysis, knowledge extraction, and thematic clustering</strong>.</p>
    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>

      <p><strong>Blei, D.M., Ng, A.Y., & Jordan, M.I.</strong> <i>Latent Dirichlet Allocation.</i> <strong>Journal of Machine Learning Research</strong>, 3(Jan), 993-1022.</p>
      </div>

      </body>"


  polaritydetection <- "<body>

    <h3><strong>Polarity Detection in TALL</strong></strong></h3>

    <p>Polarity detection is a fundamental <strong>sentiment analysis technique</strong> used to determine whether a document expresses a <strong>positive, negative, or neutral</strong> sentiment. This process is essential in analyzing <strong>consumer feedback, financial reports, product reviews, and social media discussions</strong>, where understanding sentiment trends can provide valuable insights into public opinion and decision-making processes.</p>
    <hr>
    <h4><strong>How Polarity Detection Works in TALL</strong></h4>
    <p>TALL calculates <strong>document polarity</strong> using a <strong>lexicon-based approach</strong>, incorporating <strong>contextual adjustments</strong> to refine sentiment scoring. The methodology follows three key steps:</p>

    <h4>1. Lexicon-Based Sentiment Scoring</h4>
    <ul>
    <li>Each word in the text is assigned a <strong>polarity score</strong> based on its presence in <strong>sentiment lexicons</strong>.</li>
    <li><strong>Positive words</strong> (e.g., 'excellent,' 'happy') are assigned <strong>+1</strong>, while <strong>negative words</strong> (e.g., 'bad,' 'fail') receive <strong>-1</strong>.</li>
    <li>Words <strong>not found in sentiment lexicons</strong> are considered neutral and assigned a score of <strong>0</strong>.</li>
    </ul>

    <h4>2. Contextual Modifications Using Valence Shifters</h4>
    <ul>
    <li><strong>Negators:</strong> Words like “not,” “never,” or “no” <strong>invert the polarity</strong> of a nearby sentiment word (e.g., 'not happy' changes from <strong>+1 to -1</strong>).</li>
    <li><strong>Amplifiers:</strong> Words such as 'very,' 'extremely,' and 'highly' <strong>increase the intensity</strong> of a sentiment (e.g., 'very good' is weighted more than 'good').</li>
    <li><strong>De-amplifiers (Diminishers):</strong> Terms like 'slightly' or 'somewhat' <strong>reduce sentiment intensity</strong> (e.g., 'slightly disappointing' has a weaker negative score than 'disappointing').</li>
    </ul>

    <h4>3. Aggregation and Normalization</h4>
    <ul>
    <li>Sentiment scores are <strong>summed across the document</strong> to obtain an <strong>overall polarity score</strong>.</li>
    <li>An <strong>optional normalization step</strong> scales the final score within the <strong>[-1, 1] range</strong>, ensuring comparability across different text lengths.</li>
    <li>Documents with scores near <strong>0</strong> are classified as <strong>neutral</strong>, indicating a balanced mix of sentiment or the absence of strong emotions.</li>
    </ul>
    <hr>
    <h4><strong>Sentiment Lexicons Used in TALL</strong></h4>

    <h4>1. Hu and Liu (2004) - Opinion Lexicon</h4>
    <ul>
    <li>Designed for analyzing <strong>consumer reviews</strong>, categorizing words into <strong>positive and negative</strong> classes.</li>
    <li>Particularly useful for <strong>e-commerce platforms, review aggregation sites, and user-generated feedback</strong>.</li>
    <li><strong>Language:</strong> English</li>
    </ul>

    <h4>2. Loughran and McDonald (2016) - Financial Sentiment Dictionary</h4>
    <ul>
    <li>Developed for <strong>financial and accounting texts</strong>, including categories such as <strong>“positive,” “negative,” “uncertainty,” “litigious,” and “constraining”</strong>.</li>
    <li>Widely used in <strong>financial risk assessment, investor sentiment analysis, and stock market forecasting</strong>.</li>
    <li><strong>Language:</strong> English</li>
    </ul>

    <h4>3. NRC Emotion Lexicon (Mohammad & Turney, 2010)</h4>
    <ul>
    <li>Captures emotions beyond basic polarity, categorizing words into <strong>eight primary emotions</strong>: Joy, Sadness, Anger, Fear, Surprise, Disgust, Trust, and Anticipation.</li>
    <li>Useful for <strong>social media mining, psychological studies, and literary analysis</strong>.</li>
    <li><strong>Language:</strong> Multilingual</li>
    </ul>
    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>

      <p><strong>Hu, M., & Liu, B.</strong></p>
      <p><i>Mining and summarizing customer reviews.</i> <strong>Proceedings of the Tenth ACM SIGKDD International Conference on Knowledge Discovery and Data Mining</strong>, KDD ’04, 168-177. New York, NY, USA: Association for Computing Machinery.</p>

      <p><strong>Loughran, T., & McDonald, B.</strong></p>
      <p><i>Textual analysis in accounting and finance: A survey.</i> <strong>Journal of Accounting Research</strong>, 54(4), 1187-1230.</p>

      <p><strong>Mohammad, S., & Turney, P.</strong></p>
      <p><i>Emotions evoked by common words and phrases: Using Mechanical Turk to create an emotion lexicon.</i> In <strong>Proceedings of the NAACL HLT 2010 Workshop on Computational Approaches to Analysis and Generation of Emotion in Text</strong>, 26-34. Los Angeles, CA: Association for Computational Linguistics.</p>
      </div>

      </body>"

  summarization <- "<body>

    <h3><strong>Summarization in TALL</strong></h3>

    <p>Summarization is a <strong>key technique in text analysis</strong> that allows users to extract the <strong>most relevant information</strong> from a document
    while maintaining its core meaning.
    <br><strong>TALL implements extractive summarization</strong>, a method that selects and reorders the most important sentences <strong>directly from the original text</strong>
    to generate a <strong>coherent, condensed version</strong> of the content.</p>

    <p>Unlike <strong>abstractive summarization</strong>, which rephrases content using deep learning models, <strong>extractive summarization</strong> ensures that
    the summary remains <strong>factually consistent</strong> with the input document, making it a <strong>reliable method for automated text compression</strong>.</p>
    <hr>
    <h4><strong>How Summarization Works in TALL</strong></h4>

    <h4>1. Sentence Tokenization and Preprocessing</h4>
    <ul>
    <li>The text is split into <strong>individual sentences</strong> to form the basis of the summarization process.</li>
    <li>Sentences are <strong>preprocessed</strong>, removing unnecessary punctuation and stopwords to enhance <strong>semantic clarity</strong>.</li>
    </ul>

    <h4>2. Graph Construction Using Sentence Similarity</h4>
    <ul>
    <li>A <strong>graph-based representation</strong> of the document is created, where:</li>
    <ul>
    <li><strong>Nodes</strong> represent sentences.</li>
    <li><strong>Edges</strong> connect sentences based on their <strong>semantic similarity</strong> (measured using cosine similarity or word overlap).</li>
    </ul>
    <li>Sentences that share a <strong>high degree of lexical similarity</strong> are considered <strong>strongly connected</strong> in the graph.</li>
    </ul>

    <h4>3. Application of TextRank Algorithm</h4>
    <ul>
    <li>The <strong>TextRank algorithm</strong> assigns an <strong>importance score</strong> to each sentence based on its <strong>connectivity</strong> within the graph.</li>
    <li>Sentences with the <strong>highest PageRank scores</strong> are deemed <strong>the most representative</strong> of the overall document.</li>
    </ul>

    <h4>4. Sentence Selection and Ordering</h4>
    <ul>
    <li>The <strong>top-ranked sentences</strong> are selected for the summary.</li>
    <li>A <strong>reordering step</strong> ensures that sentences are presented in a <strong>logical and coherent structure</strong>, preserving the original document’s flow.</li>
    </ul>
    <hr>
    <h4><strong>Advantages of Summarization in TALL</strong></h4>
    <ul>
    <li><strong>Extractive and Factually Consistent</strong> – Ensures that summaries are directly sourced from the original text,
    <br>reducing the risk of hallucinations or misinterpretations.</li>
    <li><strong>Graph-Based Ranking for Objective Selection</strong> – Uses <strong>TextRank</strong>, an unsupervised method that
    <br><strong>ranks sentences purely based on semantic importance</strong>, eliminating bias.</li>
    <li><strong>Efficient and Scalable</strong> – Processes <strong>large documents quickly</strong>, making it ideal for summarizing
    <br><strong>research papers, news articles, legal documents, and reviews</strong>.</li>
    <li><strong>No Need for Pre-Trained Models</strong> – Unlike abstractive methods that require deep learning models,
    <br><strong>extractive summarization works effectively on any text without additional training</strong>.</li>
    <li><strong>Customizable Summary Length</strong> – Users can <strong>adjust the number of extracted sentences</strong> to control the
    <br><strong>level of detail</strong> in the summary.</li>
    </ul>
    <hr>
    <h4><strong>Implementation of Summarization in TALL</strong></h4>

    <p>TALL’s <strong>summarization routines</strong> are built upon the <strong>TextRank algorithm</strong>, with optimizations for handling
    <strong>preprocessed and structured corpora</strong>:</p>

    <ul>
    <li><strong>Customized Text Preprocessing</strong> – The system operates on <strong>tokenized, lemmatized, and PoS-tagged corpora</strong>,
    ensuring better sentence representation.</li>
    <li><strong>Sentence Similarity Based on Multiple Metrics</strong> – Supports <strong>TF-IDF, cosine similarity, and word embeddings</strong> for improved ranking.</li>
    <li><strong>Multi-Document Summarization (Future Work)</strong> – The framework is being expanded to support <strong>multi-document summarization</strong>,
    allowing users to extract summaries from <strong>multiple related texts</strong>.</li>
    </ul>

    <p>By integrating <strong>unsupervised graph-based techniques</strong>, TALL provides users with a <strong>robust and efficient summarization tool</strong>,
    ideal for <strong>academic, business, and legal applications</strong>.</p>
    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>

      <p><strong>Mihalcea, R., & Tarau, P.</strong></p>
      <p><i>TextRank: Bringing order into text.</i> <strong>Proceedings of the 2004 Conference on Empirical Methods in Natural Language Processing</strong>,
      404-411, Barcelona, Spain, July. Association for Computational Linguistics.</p>

      <p><strong>Page, L., Brin, S., Motwani, R., & Winograd, T.</strong></p>
      <p><i>The PageRank Citation Ranking: Bringing Order to the Web.</i> <strong>Technical report, Stanford Digital Library Technologies Project</strong>, 1998.</p>
      </div>

      </body>"

  return(list(importmenu = importmenu,
              split = split,
              random = random,
              externalinfo = externalinfo,
              tokenization = tokenization,
              specialentities = specialentities,
              multiwordcreation = multiwordcreation,
              multiwordlist =  multiwordlist,
              customterm = customterm,
              posselection = posselection,
              overview = overview,
              wordincontext = wordincontext,
              reinert = reinert,
              correspondenceanalysis = correspondenceanalysis,
              cowordanalysis = cowordanalysis,
              thematicmap = thematicmap,
              tmkchoice = tmkchoice,
              tmmodelestimation = tmmodelestimation,
              polaritydetection = polaritydetection,
              summarization = summarization
  ))
}



