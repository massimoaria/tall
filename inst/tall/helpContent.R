helpContent <- function() {
  ## import ----
  importmenu <- "
  <body>

  <h3><strong>Importing Data</strong></h3>

  <p>TALL imports textual data from several sources and in several file formats, so you can work with material that arrives in different shapes. Choose the format that matches your data, and TALL prepares the documents for analysis.</p>
  <hr>

  <h4><strong>Supported File Formats</strong></h4>

  <h4>1. Plain Text Files (.txt)</h4>
  <p>You can import plain text files in three ways, depending on how the data is structured:</p>
  <ul>
    <li><strong>A single file containing a single document:</strong> use this for an individual document, such as a speech transcript, a literary work, or a report.</li>
    <li><strong>A single file with multiple documents separated by alphanumeric codes</strong> (e.g., 'Chapter', '0001', '****'):
      <ul>
        <li>TALL detects these separators automatically and segments the file into documents.</li>
        <li>You can refine the segmentation further from the <strong>Edit → Split</strong> menu.</li>
      </ul>
    </li>
    <li><strong>Multiple .txt files, where each file is a separate document:</strong>
      <ul>
        <li>Select the files one by one, or import a compressed (.zip) folder that contains them.</li>
        <li>Each document takes its ID from its file name, which keeps the corpus organized.</li>
      </ul>
    </li>
  </ul>

  <h4>2. Tabular Data (.csv, .xlsx)</h4>
  <p>Tabular formats suit structured datasets such as online reviews, survey responses, or social media posts.</p>
  <ul>
    <li>The text to be analyzed must sit in a dedicated column named <strong>'text'</strong>, so that TALL can identify it.</li>
    <li>Each row of the dataset is treated as an individual document.</li>
    <li>Additional metadata (e.g., timestamps, user IDs, categories) can be kept for contextual analysis.</li>
  </ul>

  <h4>3. PDF Documents (.pdf)</h4>
  <p>TALL imports PDF files, which lets you analyze academic papers, reports, books, and other kinds of document.</p>
  <ul>
    <li>The text is extracted automatically and converted into a format suitable for processing.</li>
    <li><strong>Limitation:</strong> at the moment, TALL can import and process only PDFs with single-column formatting. PDFs with multi-column layouts, footnotes, or complex page structures may not be parsed correctly, and may need additional preprocessing.</li>
  </ul>

  <h4>4. Biblioshiny Export Files</h4>
  <p>TALL imports files exported from <strong>Biblioshiny</strong>, the graphical user interface of the <strong>Bibliometrix</strong> R package. You can then analyze the textual content of bibliographic metadata drawn from bibliometric databases such as Scopus or Web of Science.</p>
  <ul>
    <li>Load the exported file, which is typically in <strong>.csv</strong> format, into TALL.</li>
    <li>Specify which column (e.g., <strong>Abstract</strong>, <strong>Keywords</strong>, or <strong>Title</strong>) holds the main textual content for the analysis.</li>
    <li>Other fields (e.g., authors, year, journal) can be imported and used as metadata for grouping or filtering documents.</li>
  </ul>

  <hr>
  <h4><strong>TALL Structured Files (.tall)</strong></h4>
  <p>You can save the state of an analysis in a structured format and pick it up again in a later session.</p>
  <ul>
    <li><strong>Save Progress:</strong> export the current session as a <strong>.tall</strong> file, which preserves all imported data, configurations, and analytical steps.</li>
    <li><strong>Load Saved Sessions:</strong> reload a saved <strong>.tall</strong> file and resume your work without importing or preprocessing the data again.</li>
  </ul>

  <p>Flexible, structured import shortens the first steps of a text analysis, so you can spend your time on the results instead.</p>
  <hr>

  <div class='references'>
    <h4><strong>References</strong></h4>
    <p><strong>Aria, M., Cuccurullo, C., D’Aniello, L., Misuraca, M., & Spano, M. (2024).</strong> <i>Breaking Barriers with TALL: A Text Analysis Shiny app for ALL</i>. In A. Dister, D. Longrée (eds.), <i>Mots competes textes déchiffrés (JADT24)</i> Presses Universitaires De Louvain Vol.1 pp.39-48.</p>
    <p><strong>Aria, M., Cuccurullo, C., D’Aniello, L., Misuraca, M., & Spano, M. (2024).</strong> <i>TALL: A New Shiny App for Text Analysis</i>. In <i>Scientific Meeting of the Italian Statistical Society</i> (pp. 64-70). Cham: Springer Nature Switzerland.</p>
    <p><strong>Aria, M., Cuccurullo, C., D'Aniello, L., Misuraca, M., & Spano, M. (2023).</strong> <i>TALL: A New Shiny App of Text Analysis for All</i>. In <i>CLiC-it</i>.</p>
  </div>

</body>
"

  ## split ----
  split <- "<body>

    <h3><strong>Splitting the Corpus</strong></h3>

    <p>TALL lets you split textual data into smaller segments at a chosen sequence of characters. Use it for large documents that contain several sections, or for structured content whose parts you want to analyze separately.</p>
    <hr>
    <h4><strong>How It Works</strong></h4>
    <ul>
    <li>Define a <strong>delimiter</strong>, the sequence of characters at which the text is segmented.</li>
    <li>The delimiter must contain at least <strong>three characters</strong> for the split to be accurate.</li>
    <li>The split is <strong>case-sensitive</strong>, so uppercase and lowercase variants count as different delimiters (e.g., <code>'CHAPTER'</code> is not the same as <code>'chapter'</code>).</li>
    </ul>
    <hr>
    <h4><strong>Example Use Cases</strong></h4>
    <ul>
    <li><strong>Books or Reports:</strong> Split a novel into chapters with <code>'CHAPTER '</code> as the delimiter.</li>
    <li><strong>Survey Responses:</strong> Separate responses that are marked off by a string such as <code>'###'</code> between answers.</li>
    <li><strong>Transcriptions:</strong> Divide an interview transcript at the speaker labels (e.g., <code>'Speaker 1:'</code>).</li>
    </ul>

    <p>Because you choose the delimiter yourself, the segmentation follows the structure of your own material and your analytical needs, and the original organization of the text is preserved for interpretation.</p>

    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>
      <p><strong>Aria, M., Cuccurullo, C., D'Aniello, L., Misuraca, M., &amp; Spano, M.</strong> (2024) <i>Breaking Barriers with TALL: A Text Analysis Shiny app for ALL.</i> In A. Dister, D. Longr&eacute;e (eds.), <i>Mots competes textes d&eacute;chiffr&eacute;s (JADT24)</i>, Presses Universitaires De Louvain, Vol.1, pp.39-48.</p>
    </div>

</body>"

  # random sample ----
  random <- "<body>

    <h3><strong>Random Text Selection</strong></h3>

    <p>TALL lets you draw a random subset of the imported texts and analyze that instead of the whole corpus. Use it with large collections, when a representative sample is enough to explore the material.</p>
    <hr>
    <h4><strong>How It Works</strong></h4>
    <ul>
    <li>The total number of imported texts is shown, so you can see how large the dataset is.</li>
    <li>Set the <strong>sample size</strong> as a percentage (%) of the total corpus.</li>
    <li>The selection is <strong>random</strong>, which gives you an unbiased representation of the dataset.</li>
    </ul>
    <hr>
    <h4><strong>Example Use Cases</strong></h4>
    <ul>
    <li><strong>Analyzing Social Media Data:</strong> Select 10% of the tweets in a large dataset and run a sentiment analysis on them.</li>
    <li><strong>Survey Research:</strong> Extract a random subset of the open-ended responses for qualitative coding.</li>
    <li><strong>Document Sampling:</strong> Review a sample of the reports or articles instead of the full collection.</li>
    </ul>

    <p>Controlled sampling lets you weigh efficiency against analytical depth and keeps the exploration of a large corpus manageable.</p>

    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>
      <p><strong>Aria, M., Cuccurullo, C., D'Aniello, L., Misuraca, M., &amp; Spano, M.</strong> (2024) <i>Breaking Barriers with TALL: A Text Analysis Shiny app for ALL.</i> In A. Dister, D. Longr&eacute;e (eds.), <i>Mots competes textes d&eacute;chiffr&eacute;s (JADT24)</i>, Presses Universitaires De Louvain, Vol.1, pp.39-48.</p>
    </div>

    </body>"

  ## external info ----
  externalinfo <- "<body>

    <h3><strong>Importing External Information</strong></h3>

    <p>TALL lets you bring extra information into your analysis by importing an external dataset. Use it to enrich your text data with metadata, annotations, or categorical variables, so that you can explore textual patterns more thoroughly.</p>
    <hr>
    <h4><strong>How to Import External Data</strong></h4>
    <ul>
    <li>The external file must be in <strong>Excel format</strong> (<code>.xlsx</code>).</li>
    <li>The dataset must include a column labeled <strong>'doc_id'</strong>, which matches the external information to the text data you imported earlier.</li>
    <li>The <strong>'doc_id'</strong> values must correspond exactly to the document identifiers assigned during text import, otherwise the two sources will not align.</li>
    </ul>
    <hr>
    <h4><strong>Using External Information</strong></h4>
    <ul>
    <li>Use the imported data to <strong>filter</strong> or <strong>group documents</strong> by a specific attribute (e.g., author, category, sentiment).</li>
    <li>This lets you segment a text collection quickly and concentrate on the subsets that matter for your research questions.</li>
    </ul>
    <hr>
    <h4><strong>Download Document Identifiers</strong></h4>
    <p>To make the integration easier, download the list of <strong>'doc_id'</strong> values for the imported text files below. Use it to format the external data correctly before you upload it.</p>

    <p>By importing structured external data, you can add contextual information to the text analysis and read the results against what you already know about the documents.</p>

    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>
      <p><strong>Aria, M., Cuccurullo, C., D'Aniello, L., Misuraca, M., &amp; Spano, M.</strong> (2024) <i>Breaking Barriers with TALL: A Text Analysis Shiny app for ALL.</i> In A. Dister, D. Longr&eacute;e (eds.), <i>Mots competes textes d&eacute;chiffr&eacute;s (JADT24)</i>, Presses Universitaires De Louvain, Vol.1, pp.39-48.</p>
    </div>

    </body>"

  ## tokenization ----
  tokenization <- "<body>

    <h3><strong>Tokenization, Lemmatization, and PoS Tagging</strong></h3>

    <p>TALL preprocesses textual data with <strong>tokenization, lemmatization, and Part-of-Speech (PoS) tagging</strong>. These steps turn raw text into a structured format that you can analyze further.</p>
    <hr>
    <h4><strong>Powered by UDPipe for NLP Preprocessing</strong></h4>
    <p>TALL uses the <strong>UDPipe</strong> library for tokenization, tagging, lemmatization, and dependency parsing. The <a href='https://cran.r-project.org/web/packages/udpipe/index.html' target='_blank'>udpipe R package</a> gives you access to pre-trained annotation models for many languages.</p>

      <ul>
      <li><strong>Tokenization:</strong> splits raw text into individual words, or tokens.</li>
      <li><strong>Lemmatization:</strong> reduces each word to its base or dictionary form (for example, 'running' → 'run').</li>
      <li><strong>PoS Tagging:</strong> assigns a grammatical category, such as noun, verb, or adjective, to each word.</li>
      <li><strong>Dependency Parsing:</strong> identifies the syntactic relationships between the words in a sentence.</li>
      </ul>
      <hr>
      <h4><strong>Updated Pre-trained Language Models</strong></h4>
      <p>By default, UDPipe ships with models based on <a href='https://universaldependencies.org/' target='_blank'>Universal Dependencies (UD)</a> version 2.5, and those models had not been updated for some time. For greater accuracy, TALL now integrates updated <strong>pre-trained NLP language models</strong> built on <strong>Universal Dependencies (UD) version 2.15</strong>.</p>

        <p>These models were trained on <strong>gold standard annotated corpora</strong> from the UD project, which noticeably improves the quality of text analysis in TALL. You can find the updated pre-trained models used by TALL in our <a href='https://github.com/massimoaria/tall.language.models' target='_blank'>GitHub repository</a>.</p>
      <hr>
          <h4><strong>Applications in NLP and Text Analysis</strong></h4>
          <ul>
          <li><strong>Sentiment Analysis:</strong> a better account of how each word is used and in what context.</li>
          <li><strong>Topic Modeling:</strong> cleaner preprocessing, and so cleaner topic extraction.</li>
          <li><strong>Corpus Exploration:</strong> filtering and segmenting your texts by their linguistic attributes.</li>
          </ul>

          <p>With updated NLP models and these preprocessing steps, TALL supports high-quality text analysis for researchers and practitioners in computational linguistics.</p>
         <hr>
          <div class='references'>
            <h4><strong>References</strong></h4>
            <p><strong>TALL Pre-trained Models Repository:</strong> <a href='https://github.com/massimoaria/tall.language.models' target='_blank'>GitHub repository for pre-trained models</a></p>
              <p><strong>UDPipe R Package:</strong> <a href='https://cran.r-project.org/web/packages/udpipe/index.html' target='_blank'>CRAN link to UDPipe</a></p>
                <p><strong>Universal Dependencies Repository:</strong> <a href='https://universaldependencies.org/' target='_blank'>Universal Dependencies project</a></p>
                  </div>

                  </body>"

  ## special entities ----
  specialentities <- "<body>

    <h3><strong>Tagging Special Entities</strong></h3>

    <p>TALL automatically detects and tags <strong>special entities</strong> in your texts, so that key non-linguistic elements are identified and remain available for later analysis.
    <br>Recognizing these entities improves text preprocessing, pattern recognition, and contextual analysis.</p>
    <hr>
    <h4><strong>Detected Special Entities</strong></h4>
    <p>When TALL processes your textual data, it assigns a specific tag to each of the following entities:</p>
    <ul>
    <li><strong>Email Addresses:</strong> Recognizes and tags email formats (e.g., <code>example@domain.com</code>).</li>
    <li><strong>URLs:</strong> Detects web links, so that you can exclude them or analyze them separately (e.g., <code>https://www.example.com/path</code>).</li>
    <li><strong>Emojis:</strong> Identifies and classifies the emojis used in digital communication (e.g., 😊, 🚀, ❤️).</li>
    <li><strong>Hashtags:</strong> Extracts the hashtags common in social media and in categorization (e.g., <code>#ExampleTag</code>).</li>
    <li><strong>IP Addresses:</strong> Detects standard IP address formats (e.g., <code>192.168.1.1</code>), which can be useful in network-related text analysis.</li>
    <li><strong>Mentions:</strong> Identifies references to usernames, particularly in social media and chat applications (e.g., <code>@username</code>).</li>
    </ul>
    <hr>
    <h4><strong>Why Special Entity Tagging Matters</strong></h4>
    <ul>
    <li><strong>Enhanced Text Cleaning:</strong> Filter out or isolate the elements that contribute nothing to a linguistic analysis.</li>
    <li><strong>Social Media and Web Analysis:</strong> Extract meaningful patterns from hashtags, mentions, and URLs.</li>
    <li><strong>Sentiment and Emotion Studies:</strong> Analyze the role emojis play in sentiment-based communication.</li>
    <li><strong>Cybersecurity and Digital Forensics:</strong> Identify sensitive data points such as email addresses and IP addresses.</li>
    </ul>

    <p>Special entity recognition strengthens the preprocessing phase and structures these elements for more effective text analysis.</p>

    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>
      <p><strong>Jurafsky, D., &amp; Martin, J.H.</strong> (2024) <i>Speech and Language Processing.</i> 3rd edition (draft). Chapter 8: Sequence Labeling for Parts of Speech and Named Entities.</p>
      <p><strong>Nadeau, D., &amp; Sekine, S.</strong> (2007) <i>A survey of named entity recognition and classification.</i> <strong>Lingvisticae Investigationes</strong>, 30(1), 3-26.</p>
    </div>

    </body>"

  ## multiword creation ----

  multiwordcreation <- "
<body>
    <h3><strong>Algorithms for Automatic Multi-Word Extraction</strong></h3>
    <p>TALL implements six methods for extracting multi-word expressions from a corpus automatically. They are one <strong>syntactic approach</strong> based on dependency parsing and five <strong>statistical approaches</strong> based on co-occurrence measures.</p>

    <hr>
    <h4><strong>Syntactic Approach</strong></h4>

    <br><h4><strong>- Dependency Parsing (NP)</strong></h4>
    <p>This method extracts <strong>noun phrases</strong> by traversing the syntactic dependency tree built during tokenization. Unlike the statistical methods, which rely on sequential co-occurrence patterns, it identifies linguistically motivated multi-word units from their grammatical structure.</p>
    <p>The algorithm works as follows:</p>
    <ul>
      <li>For each <strong>NOUN</strong> or <strong>PROPN</strong> in the corpus, the algorithm traverses the dependency tree to find that token’s syntactic dependents.</li>
      <li>It collects dependents with the following relations: <strong>amod</strong> (adjective modifier, e.g., &quot;higher rate&quot;), <strong>compound</strong> (compound noun, e.g., &quot;machine learning&quot;), <strong>flat</strong> (flat multiword, e.g., &quot;New York&quot;), <strong>nmod</strong> (prepositional modifier, e.g., &quot;rate of prediabetes&quot;), and <strong>nummod</strong> (numeric modifier).</li>
      <li>For an <strong>nmod</strong> dependent, it also includes the connecting preposition (the <strong>case</strong> relation).</li>
      <li>It sorts the resulting tokens by position and assembles them into a phrase.</li>
    </ul>
    <p><strong>Advantages over statistical methods:</strong></p>
    <ul>
      <li>It captures <strong>non-adjacent</strong> multi-word units, such as &quot;rate of prediabetes&quot;, where &quot;of&quot; is a function word.</li>
      <li>It requires no <strong>PoS tag selection</strong>, because it follows the syntactic structure automatically.</li>
      <li>It yields <strong>no false positives</strong> from coincidental co-occurrence, since it groups only grammatically related words.</li>
      <li>It is implemented in <strong>C++</strong> for high performance on large corpora.</li>
    </ul>
    <p><strong>Parameters:</strong> <em>Max Phrase Length</em> (max tokens per phrase, default 5) and <em>Freq Min</em> (minimum occurrences).</p>
    <p><strong>Score:</strong> DEP = frequency &times; phrase length (longer frequent phrases score higher).</p>
    <p><strong>References:</strong><br>
    de Marneffe, M.-C., Manning, C.D., Nivre, J., &amp; Zeman, D. (2021). <em>Universal Dependencies</em>. Computational Linguistics, 47(2), 255-308.<br>
    Straka, M., &amp; Strakov&aacute;, J. (2017). <em>Tokenizing, POS Tagging, Lemmatizing and Parsing UD 2.0 with UDPipe</em>. In CoNLL Shared Task.</p>

    <hr>
    <h4><strong>Statistical Approaches</strong></h4>
    <p>The following methods identify multi-word expressions from statistical co-occurrence patterns. For each of them, select which <strong>PoS tags</strong> to consider for candidate terms.</p>

    <br><h4><strong>- Rapid Automatic Keyword Extraction (RAKE)</strong></h4>
    <p>RAKE is a domain-independent keyword extraction algorithm that identifies key phrases by analyzing word co-occurrences within a document. It splits the text into candidate keyword phrases at stopword delimiters, then scores each candidate on word co-occurrence and frequency. Higher-scoring phrases rank as more relevant multi-word expressions.</p>
    <p><strong>Reference:</strong><br>
    Rose, S., Engel, D., Cramer, N., &amp; Cowley, W. (2010). <em>Automatic keyword extraction from individual documents</em>. Text Mining: Applications and Theory, 1(1), 1-20.</p>

    <br><h4><strong>- Pointwise Mutual Information (PMI)</strong></h4>
    <p>PMI measures the strength of the association between two words. It is defined as:</p>
    <p style='text-align: center;'>
        <em>PMI(w<sub>1</sub>, w<sub>2</sub>) = log ( P(w<sub>1</sub>, w<sub>2</sub>) / (P(w<sub>1</sub>) P(w<sub>2</sub>)) )</em>
    </p>
    <p>where P(w<sub>1</sub>, w<sub>2</sub>) is the probability that w<sub>1</sub> and w<sub>2</sub> occur together, and P(w<sub>1</sub>) and P(w<sub>2</sub>) are their individual probabilities. A high PMI value indicates a strong association, which makes the phrase a good multi-word candidate.</p>
    <p><strong>Reference:</strong><br>
    Church, K. W., &amp; Hanks, P. (1990). <em>Word association norms, mutual information, and lexicography</em>. Computational Linguistics, 16(1), 22-29.</p>

    <br><h4><strong>- Mutual Dependency (MD)</strong></h4>
    <p>Mutual Dependency extends PMI by considering the full context of a multi-word expression rather than pairwise co-occurrence alone. It applies statistical dependency measures, so that every word in a sequence has to contribute significantly to its overall meaning. The approach is particularly useful for identifying multi-word units beyond simple bigrams.</p>
    <p><strong>Reference:</strong><br>
    Thanopoulos, A., Fakotakis, N., &amp; Kokkinakis, G. (2002, May). <em>Comparative Evaluation of Collocation Extraction Metrics.</em> In LREC (Vol. 2, pp. 620-625).</p>

    <br><h4><strong>- Log-Frequency Biased Mutual Dependency (LF-MD)</strong></h4>
    <p>LF-MD refines MD by bringing word frequency into the dependency calculation. It biases the selection of multi-word expressions toward frequent collocations while keeping a balance between statistical significance and linguistic relevance.</p>
    <p><strong>Reference:</strong><br>
    Thanopoulos, A., Fakotakis, N., &amp; Kokkinakis, G. (2002, May). <em>Comparative Evaluation of Collocation Extraction Metrics.</em> In LREC (Vol. 2, pp. 620-625).</p>

    <br><h4><strong>- IS Index (Absorption Index)</strong></h4>
    <p>The IS Index, proposed by Morrone (1993), measures the cohesiveness of a word sequence by combining three factors: word rarity, sequence frequency, and lexical density. The index is calculated as:</p>
    <p style='text-align: center;'>
        <em>IS(s) = (&Sigma; 1/freq(w<sub>i</sub>)) &times; freq(s) &times; n<sub>lexical</sub></em>
    </p>
    <p>where freq(w<sub>i</sub>) is the frequency of each word in the sequence, freq(s) is the frequency of the complete sequence, and n<sub>lexical</sub> is the number of lexical words in the sequence. The normalized version, IS<sub>norm</sub> = IS / L&sup2;, lets you compare sequences of different lengths fairly.</p>
    <p><strong>Reference:</strong><br>
    Morrone, A. (1993). <em>Alcuni criteri di valutazione della significativit&agrave; dei segmenti ripetuti</em>. In JADT (pp. 445-453).</p>

    <hr>
    <h4><strong>Choosing the Right Method</strong></h4>
    <table style='width:100%; border-collapse:collapse; margin-bottom:15px;'>
      <thead>
        <tr style='background-color:#f0f0f0; border-bottom:2px solid #ccc;'>
          <th style='padding:8px; text-align:left;'>Method</th>
          <th style='padding:8px; text-align:left;'>Basis</th>
          <th style='padding:8px; text-align:left;'>Best for</th>
          <th style='padding:8px; text-align:center;'>PoS selection</th>
        </tr>
      </thead>
      <tbody>
        <tr><td style='padding:6px;'><strong>Dep. Parsing (NP)</strong></td><td>Syntactic structure</td><td>Linguistically accurate phrases, prepositional phrases</td><td style='text-align:center;'>No</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'>RAKE</td><td>Co-occurrence + frequency</td><td>General-purpose keyword extraction</td><td style='text-align:center;'>Yes</td></tr>
        <tr><td style='padding:6px;'>PMI</td><td>Probabilistic association</td><td>Identifying strongly associated word pairs</td><td style='text-align:center;'>Yes</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'>MD / LF-MD</td><td>Mutual dependency</td><td>Balanced extraction in large corpora</td><td style='text-align:center;'>Yes</td></tr>
        <tr><td style='padding:6px;'>IS Index</td><td>Rarity + frequency + density</td><td>Terminology extraction, domain-specific terms</td><td style='text-align:center;'>No (auto)</td></tr>
      </tbody>
    </table>
</body>
"

  ## multiword list ----
  multiwordlist <- "<body>

    <h3><strong>Multi-Word Creation by a List</strong></h3>

    <p>TALL lets you define <strong>multi-word expressions (MWEs)</strong> by importing a predefined list of multi-word terms. Use it when specific phrases or domain-specific expressions have to be treated as single units during text processing, so that the linguistic analysis stays accurate.</p>
    <hr>
    <h4><strong>How to Import a Multi-Word List</strong></h4>
    <p>To bring multi-word expressions into the analysis, supply a list in the following format:</p>
    <ul>
    <li>The list must be in <strong>Excel (<code>.xlsx</code>) or CSV (<code>.csv</code>) format</strong>.</li>
    <li>The file must contain a <strong>single column</strong>, with one multi-word expression per row.</li>
    <li><strong>Separate each term within a multi-word expression with a single whitespace<br></strong> (e.g., <code>machine learning</code>, <code>natural language processing</code>).</li>
    </ul>
    <hr>
    <h4><strong>Why Use Multi-Word Expressions?</strong></h4>
    <ul>
    <li><strong>Preserving Meaningful Phrases:</strong> Key terms (e.g., <code>artificial intelligence</code>) are not split into separate words.</li>
    <li><strong>Improving Text Preprocessing:</strong> Tokenization and lemmatization treat the phrases as cohesive units.</li>
    <li><strong>Enhancing Domain-Specific Analysis:</strong> Useful in specialized fields such as legal, medical, or technical texts, where multi-word terms carry precise meanings.</li>
    </ul>

    <p>Multi-word recognition gives you more control over how your text is structured for analysis, and it keeps critical expressions intact so that they are identified and processed correctly.</p>

    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>
      <p><strong>Sag, I.A., Baldwin, T., Bond, F., Copestake, A., &amp; Flickinger, D.</strong> (2002) <i>Multiword expressions: A pain in the neck for NLP.</i> In <strong>Proceedings of the Third International Conference on Intelligent Text Processing and Computational Linguistics (CICLing)</strong>, 1-15.</p>
    </div>

    </body>"

  ## custom pos ----
  customterm <- "<body>

    <h3><strong>Custom PoS List</strong></h3>

    <p>TALL lets you define a <strong>Custom PoS List</strong>, which gives you finer control over text processing and linguistic analysis. Use it to assign your own PoS tags to specific terms, overriding the categories the language model assigns by default.</p>
    <hr>
    <h4><strong>Why Use a Custom PoS List?</strong></h4>
    <ul>
    <li><strong>Highlighting Specific Concepts:</strong> Mark key terms tied to methodologies, specialized vocabulary, or domain-specific jargon.</li>
    <li><strong>Filtering Stop Words:</strong> Remove terms that are irrelevant to your analysis and keep the dataset clean.</li>
    <li><strong>Enhancing Named Entity Recognition (NER):</strong> Tag by hand the words that the language model may misclassify.</li>
    <li><strong>Overriding Default PoS Assignments:</strong> Fix a category for certain terms so that tagging stays consistent across texts.</li>
    </ul>
    <hr>
    <h4><strong>How to Import a Custom PoS List</strong></h4>
    <p>To bring in a custom list of terms, supply a file in the right format:</p>
    <ul>
    <li>The list must be in <strong>Excel format (<code>.xlsx</code>)</strong>.</li>
    <li>The file must contain <strong>two columns</strong>:</li>
    <ul>
    <li><strong>First column:</strong> the terms to be tagged.</li>
    <li><strong>Second column:</strong> the Part-of-Speech (PoS) tag or user-defined category assigned to each term.</li>
    </ul>
    <li>The tags can be <strong>any custom category</strong> you define for your analysis needs (e.g., METHOD, APPLICATION, SETTING, TO_REMOVE, etc.), or standard PoS tags (NOUN, VERB, ADJ, etc.).</li>
    </ul>
    <p><strong>Note:</strong> Your custom tags appear in the PoS selection menus throughout the app, so you can filter, group, or exclude terms by your own categories.</p>
    <hr>
  <h4><strong>Example of Custom PoS List Format</strong></h4>

  <table border='1' cellspacing='0' cellpadding='5'>
  <tr>
  <th> -------- Term ---------- </th>
  <th> ------ Custom Tag ------ </th>
  </tr>
  <tr>
  <td>artificial intelligence</td>
  <td>METHOD</td>
  </tr>
  <tr>
  <td>deep learning</td>
  <td>METHOD</td>
  </tr>
  <tr>
  <td>preprocess</td>
  <td>DATA_HANDLING</td>
  </tr>
  <tr>
  <td>dataset</td>
  <td>DATA_HANDLING</td>
  </tr>
  <tr>
  <td>remove</td>
  <td>TO_REMOVE</td>
  </tr>
  <tr>
  <td>neural network</td>
  <td>APPLICATION</td>
  </tr>
  <tr>
  <td>batch size</td>
  <td>SETTING</td>
  </tr>
  </table>
<br>
    <p>Because you define the categories and control the tagging yourself, TALL adapts to domain-specific research and to more refined linguistic processing.</p>

    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>
      <p><strong>Petrov, S., Das, D., &amp; McDonald, R.</strong> (2012) <i>A universal part-of-speech tagset.</i> In <strong>Proceedings of the Eighth International Conference on Language Resources and Evaluation (LREC)</strong>, Istanbul, Turkey.</p>
      <p><strong>de Marneffe, M.-C., Manning, C.D., Nivre, J., &amp; Zeman, D.</strong> (2021) <i>Universal Dependencies.</i> <strong>Computational Linguistics</strong>, 47(2), 255-308.</p>
    </div>

    </body>"

  ## synonyms ----
  synonyms <- "<body>

    <h3><strong>Synonyms Merging</strong></h3>

    <p>Merge <strong>synonyms and variant forms</strong> into standardized target terms. Merging reduces vocabulary noise: it consolidates spelling variants, abbreviations, and domain-specific synonyms into a single canonical form, which improves the quality of later analyses such as topic modeling, co-occurrence networks, and frequency distributions.</p>
    <hr>
    <h4><strong>File Format Requirements</strong></h4>
    <p>The synonyms file must be a <strong>CSV</strong> or an <strong>XLSX</strong> file with the following structure:</p>
    <ul>
      <li><strong>Column 1 (target_term):</strong> the standardized term that replaces all of its synonyms.</li>
      <li><strong>Column 2 (upos):</strong> the Part-of-Speech tag to assign to the target term (for example, NOUN, VERB, ADJ).</li>
      <li><strong>Columns 3+ (synonym1, synonym2, ...):</strong> the alternative terms to be replaced.</li>
    </ul>
    <hr>
    <h4><strong>Example File Structure</strong></h4>

    <table style='width:100%; border-collapse:collapse; margin-bottom:15px;'>
      <thead>
        <tr style='background-color:#f0f0f0; border-bottom:2px solid #ccc;'>
          <th style='padding:8px; text-align:left;'>target_term</th>
          <th style='padding:8px; text-align:left;'>upos</th>
          <th style='padding:8px; text-align:left;'>synonym1</th>
          <th style='padding:8px; text-align:left;'>synonym2</th>
          <th style='padding:8px; text-align:left;'>synonym3</th>
        </tr>
      </thead>
      <tbody>
        <tr><td style='padding:6px;'>machine_learning</td><td>NOUN</td><td>ml</td><td>ML</td><td>machine learning</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'>artificial_intelligence</td><td>NOUN</td><td>ai</td><td>AI</td><td>A.I.</td></tr>
      </tbody>
    </table>

    <hr>
    <h4><strong>How It Works</strong></h4>
    <ol>
      <li><strong>Upload</strong> your synonyms file, in CSV or Excel format.</li>
      <li><strong>Select the replacement type:</strong> choose whether to match <em>tokens</em> or <em>lemmas</em>.</li>
      <li><strong>Preview</strong> your synonyms list in the preview tab before you apply it.</li>
      <li><strong>Apply:</strong> click Run to replace every synonym with its target term.</li>
      <li><strong>Review</strong> the processed data in the results tab.</li>
    </ol>
    <hr>
    <h4><strong>Processing Details</strong></h4>
    <ul>
      <li><strong>Token-based replacement:</strong> matches and replaces synonym tokens, and updates their <code>upos</code> tag.</li>
      <li><strong>Lemma-based replacement:</strong> matches and replaces synonym lemmas, and updates their <code>upos</code> tag.</li>
      <li><strong>Case-insensitive matching:</strong> matching ignores differences in case.</li>
      <li><strong>PoS tag update:</strong> when a synonym is replaced, its Part-of-Speech tag is set to the value you specified.</li>
    </ul>
    <hr>
    <h4><strong>Important Notes</strong></h4>
    <ul>
      <li>The merge is <strong>irreversible</strong> once you save, so always check your synonyms list in the preview tab before you apply it.</li>
      <li>Make sure your <code>upos</code> values are valid Universal PoS tags (NOUN, VERB, ADJ, ADV, PROPN, PRON, DET, ADP, NUM, CONJ, INTJ).</li>
      <li>Empty cells in the synonym columns are ignored.</li>
    </ul>

    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>
      <p><strong>Jurafsky, D., &amp; Martin, J.H.</strong> (2024) <i>Speech and Language Processing.</i> 3rd edition (draft). Chapter 6: Vector Semantics and Embeddings.</p>
      <p><strong>Manning, C.D., Raghavan, P., &amp; Sch&uuml;tze, H.</strong> (2008) <i>Introduction to Information Retrieval.</i> Cambridge University Press. Chapter 2: The term vocabulary and postings lists.</p>
    </div>

    </body>"

  ## pos selection ----
  posselection <- "<body>

    <h3><strong>PoS Tagging Selection</strong></h3>

    <p>TALL lets you <strong>select the Part-of-Speech (PoS) tags</strong> to be used in the analyses that follow.
    You decide which linguistic elements enter the processing,
    so that only the grammatical categories you care about are considered.</p>
    <hr>
    <h4><strong>Why Select PoS Tags?</strong></h4>
    <ul>
    <li><strong>Filtering out unnecessary elements:</strong> exclude determiners, conjunctions, or punctuation that contribute little to the analysis.</li>
    <li><strong>Focusing on key linguistic features:</strong> keep only nouns and verbs for topic modeling, or adjectives and adverbs for sentiment analysis.</li>
    <li><strong>Improving computational efficiency:</strong> reduce the size of the data and the processing time by analyzing only the most relevant word categories.</li>
    </ul>
    <hr>
    <h4><strong>How It Works</strong></h4>
    <ul>
    <li>You <strong>select or deselect</strong> PoS categories by hand from a predefined list.</li>
    <li>The available PoS tags follow the <strong>Universal Dependencies (UD) annotation scheme</strong>, which keeps them consistent across languages.</li>
    </ul>
    <hr>
    <h4><strong>Default Selected PoS Tags</strong></h4>
    <p>By default, TALL selects these PoS categories:</p>
    <ul>
    <li><strong>ADJ:</strong> Adjective – descriptive words (e.g., 'beautiful', 'quick').</li>
    <li><strong>NOUN:</strong> Noun – common nouns that name entities (e.g., 'dog', 'city').</li>
    <li><strong>PROPN:</strong> Proper Noun – the names of specific places, people, or organizations (e.g., 'London', 'NASA').</li>
    <li><strong>VERB:</strong> Verb – action words that name processes (e.g., 'run', 'speak').</li>
    <li><strong>HAPAX:</strong> words that appear only once in the text, useful for analyzing lexical richness.</li>
    </ul>
    <hr>
    <h4><strong>Available PoS Categories</strong></h4>
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
    <h4><strong>Custom Categories</strong></h4>
    <p>Besides the predefined PoS categories, you may also have <strong>generated custom categories</strong> through the <strong>Custom List</strong> and <strong>Multi-Word</strong> menus.
    <br>These tags of your own group specific terms under a classification of your choosing.</p>
    <hr>
    <h4><strong>Enhancing Analysis with PoS Selection</strong></h4>
    <p>Because you choose the PoS categories yourself, the analysis follows your research goals.
    <br>Whether you are working on <strong>keyword extraction, syntactic analysis, topic modeling, or sentiment analysis</strong>,
    a refined PoS selection makes the results more precise and easier to interpret.</p>

</body>"

  ## features ----
  featureroles <- "<body>

  <h3><strong>Feature Roles</strong></h3>

  <p>In the Feature Roles section you assign specific roles to the variables in your dataset. These assignments drive text analysis workflows across several TALL features, so that the right variable is used for each analytical purpose.</p>

  <hr>

  <h4><strong>Purpose</strong></h4>

  <p>Once you define the feature roles explicitly, TALL can configure analysis parameters automatically, based on the characteristics of your data. This keeps settings consistent across analytical modules and spares you from selecting the same variable again and again.</p>

  <hr>

  <h4><strong>Available Feature Roles</strong></h4>

  <h5><i class='fas fa-clock' style='color: #3f51b5;'></i> <strong>Time Variable</strong></h5>

  <p>A numeric or date variable that acts as the temporal indicator for diachronic text analysis.</p>

  <p><strong>Primary Applications:</strong></p>
  <ul>
    <li><strong>Longitudinal Topic Modeling:</strong> track how topics evolve over time</li>
    <li><strong>Temporal Trend Analysis:</strong> identify patterns and shifts in vocabulary across time frames</li>
    <li><strong>Time-Series Text Mining:</strong> analyze textual data with temporal dependencies</li>
    <li><strong>Diachronic Linguistic Studies:</strong> examine language change and evolution</li>
  </ul>

  <p><strong>Requirements:</strong> the variable must be numeric (e.g., a year or a month number) or in date format (e.g., YYYY-MM-DD).</p>

  <hr>

  <h5><i class='fas fa-tag' style='color: #00bcd4;'></i> <strong>Label Variable</strong></h5>

  <p>A categorical variable that holds the response or target class in supervised text classification.</p>

  <p><strong>Primary Applications:</strong></p>
  <ul>
    <li><strong>Supervised Machine Learning:</strong> train classification models such as Random Forest, Support Vector Machines (SVM), and Naive Bayes</li>
    <li><strong>Text Categorization:</strong> assign documents automatically to predefined categories</li>
    <li><strong>Sentiment Classification:</strong> predict sentiment labels (positive, negative, neutral)</li>
    <li><strong>Document Classification:</strong> classify documents by topic, genre, or another categorical attribute</li>
    <li><strong>Model Evaluation:</strong> validate classification performance against labeled data</li>
  </ul>

  <p><strong>Requirements:</strong> the variable should contain discrete categorical values. Binary classification needs two distinct categories; multi-class classification can use more.</p>

  <hr>

  <h5><i class='fas fa-right-left' style='color: #ff9800;'></i> <strong>Keyness Group Variable</strong></h5>

  <p>A binary or categorical variable that divides the corpus into distinct groups for comparison.</p>

  <p><strong>Primary Applications:</strong></p>
  <ul>
    <li><strong>Keyness Analysis:</strong> identify words and phrases that are statistically more characteristic of one group than of another</li>
    <li><strong>Comparative Corpus Linguistics:</strong> compare vocabulary across subcorpora</li>
    <li><strong>Distinctive Vocabulary Identification:</strong> discover the words that set groups apart</li>
    <li><strong>Contrastive Analysis:</strong> examine linguistic differences between categories (e.g., male vs. female authors, different time periods, geographic regions)</li>
  </ul>

  <p><strong>Requirements:</strong> ideally the variable contains two distinct categories for a binary comparison, but a categorical variable with more groups also works, through pairwise comparisons.</p>

  <hr>

  <h4><strong>Usage Guidelines</strong></h4>

  <ul>
    <li><strong>Available Features:</strong> only metadata columns can take a role. The technical columns generated during text processing (e.g., <code>token_id</code>, <code>lemma</code>, <code>upos</code>) are excluded automatically by the <code>noGroupLabels()</code> function.</li>
    <li><strong>Multiple Role Assignment:</strong> a single variable can take several roles, if that suits your workflow.</li>
    <li><strong>Session Persistence:</strong> role assignments hold for the whole TALL session, until you change or reset them.</li>
    <li><strong>Flexible Configuration:</strong> you can modify the roles at any time as your analysis changes.</li>
    <li><strong>No Mandatory Assignments:</strong> no role has to be filled. Configure only the ones your analysis needs.</li>
  </ul>

  <hr>

  <h4><strong>Best Practices</strong></h4>

  <ul>
    <li><strong>Data Quality:</strong> check that the variable you select contains valid, non-missing data suited to its role.</li>
    <li><strong>Temporal Consistency:</strong> for a time variable, verify that the temporal values are consistent and correctly formatted.</li>
    <li><strong>Balanced Labels:</strong> for a label variable in a classification task, consider class balance, which affects how biased the model is.</li>
    <li><strong>Clear Group Definitions:</strong> for keyness analysis, make sure the group categories are well defined and worth comparing.</li>
    <li><strong>Documentation:</strong> keep track of which variable holds which role, especially in a complex workflow.</li>
  </ul>

  <hr>

  <h4><strong>Technical Notes</strong></h4>

  <p>Feature role assignments are stored in the reactive values object (<code>values$timeVariable</code>, <code>values$labelVariable</code>, <code>values$keynessVariable</code>) and can be accessed programmatically anywhere in the TALL application. They tell downstream analytical functions which variables to use for a given task.</p>

</body>"

  ## overview ----
  overview <- "<body>
  <div class='container'>
    <h3><strong>Corpus Metrics</strong></h3>
    <p>These metrics summarize the key textual characteristics of the corpus you are analyzing.</p>

    <h4><strong>Corpus Size &amp; Structure</strong></h4>
    <ul>
    <li><strong>Documents →</strong> The total number of documents in the corpus.</li>
    <li><strong>Sentences →</strong> The total number of sentences in the corpus.</li>
    <li><strong>Tokens →</strong> The total number of words or linguistic units, including punctuation marks.</li>
    <li><strong>Types →</strong> The number of unique words in the corpus, a measure of vocabulary richness.</li>
    <li><strong>Lemma →</strong> The number of unique lemmas, counting each word under its base form.</li>
    </ul>

    <h4><strong>Average Length Metrics</strong></h4>
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

            <h4><strong>Lexical Metrics</strong></h4>
            <ul>
            <li><strong>Type-Token Ratio (TTR) →</strong> The ratio of unique words (types) to total words (tokens). Higher values indicate greater lexical diversity.<br>
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

                <li><strong>Hapax Legomena (%) →</strong> The percentage of words that appear only once in the corpus.<br>
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

                    <li><strong>Guiraud Index →</strong> A measure of lexical richness that corrects for text length.<br>
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

                        <h4><strong>Additional Lexical Measures</strong></h4>
                        <ul>
                        <li><strong>Lexical Density →</strong> The proportion of content words out of the total tokens.<br>
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

                            <li><strong>Nominal Ratio →</strong> The ratio of nouns to verbs.<br>
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

                                <li><strong>Gini Index →</strong> A measure of inequality in the distribution of word frequencies, calculated from their Lorenz curve.</li>

                                <li><strong>Yule’s K Index →</strong> A measure of lexical diversity based on word repetition.<br>
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

    <h4><strong>Morphological Features</strong></h4>
    <p>The <strong>Morphological Features</strong> tab shows the distribution of grammatical features taken from the <code>feats</code> column of the Universal Dependencies annotation. Each token carries morphological properties that reveal grammatical patterns in the corpus.</p>

    <h4><em>Available Features</em></h4>
    <table style='width:100%; border-collapse:collapse; margin-bottom:15px;'>
      <thead>
        <tr style='background-color:#f0f0f0; border-bottom:2px solid #ccc;'>
          <th style='padding:8px; text-align:left;'>Feature</th>
          <th style='padding:8px; text-align:left;'>Values</th>
          <th style='padding:8px; text-align:left;'>Applies to</th>
          <th style='padding:8px; text-align:left;'>What it reveals</th>
        </tr>
      </thead>
      <tbody>
        <tr><td style='padding:6px;'><strong>Tense</strong></td><td>Past, Pres, Fut</td><td>VERB, AUX</td><td>Narrative (Past) vs argumentative (Pres) style</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'><strong>Mood</strong></td><td>Ind, Sub, Imp, Cnd</td><td>VERB, AUX</td><td>Certainty (Ind) vs hedging/doubt (Sub, Cnd)</td></tr>
        <tr><td style='padding:6px;'><strong>Number</strong></td><td>Sing, Plur</td><td>NOUN, PRON, DET, ADJ, VERB</td><td>Generalization patterns</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'><strong>Person</strong></td><td>1, 2, 3</td><td>VERB, AUX, PRON</td><td>Personal (1st) vs impersonal (3rd) style</td></tr>
        <tr><td style='padding:6px;'><strong>VerbForm</strong></td><td>Fin, Inf, Part, Ger, Conv</td><td>VERB, AUX</td><td>Finite vs non-finite verb usage</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'><strong>Degree</strong></td><td>Pos, Cmp, Sup</td><td>ADJ, ADV</td><td>Use of comparatives and superlatives</td></tr>
        <tr><td style='padding:6px;'><strong>Gender</strong></td><td>Masc, Fem, Neut</td><td>NOUN, PRON, ADJ, DET</td><td>Grammatical gender distribution (language-dependent)</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'><strong>Case</strong></td><td>Nom, Acc, Gen, Dat, ...</td><td>NOUN, PRON, ADJ</td><td>Syntactic roles (language-dependent)</td></tr>
        <tr><td style='padding:6px;'><strong>Voice</strong></td><td>Act, Pass</td><td>VERB</td><td>Active vs passive constructions</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'><strong>Definite</strong></td><td>Def, Ind</td><td>DET, NOUN</td><td>Definiteness marking</td></tr>
        <tr><td style='padding:6px;'><strong>PronType</strong></td><td>Prs, Dem, Rel, Int, ...</td><td>PRON, DET</td><td>Pronoun/determiner types</td></tr>
      </tbody>
    </table>

    <p>The <strong>bar chart</strong> shows the frequency of each value of the selected feature. The <strong>cross-tabulation table</strong> shows how each feature value is distributed across PoS tags (e.g., Tense=Past appears in both VERB and AUX).</p>

    <p><strong>Note:</strong> The available features depend on the language model used for tokenization. Not every language annotates all of them.</p>

    <hr>
    <h4><strong>Dependency Tree Viewer</strong></h4>
    <p>The <strong>Dependency Tree</strong> tab visualizes the syntactic structure of single sentences. Select any document and sentence in the corpus to inspect how the parser has analyzed its grammatical structure.</p>

    <h4><em>How to Read the Tree</em></h4>
    <ul>
      <li><strong>Tokens</strong> run horizontally in sentence order, as labeled boxes colored by Part of Speech:
        <span style='color:#4F7942;'>NOUN/PROPN (green)</span>,
        <span style='color:#E41A1C;'>VERB/AUX (red)</span>,
        <span style='color:#377EB8;'>ADJ (blue)</span>,
        <span style='color:#FF7F00;'>ADV (orange)</span>,
        <span style='color:#984EA3;'>PRON (purple)</span>,
        <span style='color:#999999;'>DET/ADP (gray)</span>.</li>
      <li><strong>Arcs</strong> connect each token to its syntactic head. Arc height is proportional to the distance between the dependent and the head &mdash; longer arcs indicate more distant dependencies.</li>
      <li><strong>Arc labels</strong> show the dependency relation (e.g., <code>nsubj</code>, <code>obj</code>, <code>amod</code>).</li>
      <li><strong>Arc colors</strong> indicate the relation type:
        <span style='color:#E41A1C;'>subject relations (red)</span>,
        <span style='color:#377EB8;'>object relations (blue)</span>,
        <span style='color:#4DAF4A;'>nominal modifiers (green)</span>,
        <span style='color:#FF7F00;'>adverbial modifiers (orange)</span>,
        <span style='color:#984EA3;'>compounds (purple)</span>.</li>
    </ul>

    <h4><em>Token Details Table</em></h4>
    <p>Below the tree, a table shows the full annotation for each token: ID, Token, Lemma, PoS tag, Dependency Relation, Head ID, and Head Token. Read it to inspect the parse output precisely.</p>

    <h4><em>Common Dependency Relations</em></h4>
    <table style='width:100%; border-collapse:collapse; margin-bottom:15px;'>
      <thead>
        <tr style='background-color:#f0f0f0; border-bottom:2px solid #ccc;'>
          <th style='padding:8px; text-align:left;'>Relation</th>
          <th style='padding:8px; text-align:left;'>Meaning</th>
          <th style='padding:8px; text-align:left;'>Example</th>
        </tr>
      </thead>
      <tbody>
        <tr><td style='padding:6px;'><code>nsubj</code></td><td>Nominal subject</td><td><em>The <u>cat</u> sat</em> (cat &rarr; sat)</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'><code>obj</code></td><td>Direct object</td><td><em>She read a <u>book</u></em> (book &rarr; read)</td></tr>
        <tr><td style='padding:6px;'><code>amod</code></td><td>Adjective modifier</td><td><em>a <u>big</u> house</em> (big &rarr; house)</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'><code>nmod</code></td><td>Nominal modifier (via preposition)</td><td><em>rate of <u>growth</u></em> (growth &rarr; rate)</td></tr>
        <tr><td style='padding:6px;'><code>advmod</code></td><td>Adverbial modifier</td><td><em>runs <u>quickly</u></em> (quickly &rarr; runs)</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'><code>compound</code></td><td>Compound word</td><td><em><u>machine</u> learning</em> (machine &rarr; learning)</td></tr>
        <tr><td style='padding:6px;'><code>det</code></td><td>Determiner</td><td><em><u>the</u> house</em> (the &rarr; house)</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'><code>case</code></td><td>Preposition/postposition</td><td><em>rate <u>of</u> growth</em> (of &rarr; growth)</td></tr>
        <tr><td style='padding:6px;'><code>conj</code></td><td>Conjunct</td><td><em>cats <u>and</u> dogs</em> (dogs &rarr; cats)</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'><code>advcl</code></td><td>Adverbial clause</td><td><em>He left <u>because</u> it rained</em></td></tr>
        <tr><td style='padding:6px;'><code>acl</code></td><td>Adnominal clause</td><td><em>the man <u>who</u> came</em></td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'><code>xcomp</code></td><td>Open clausal complement</td><td><em>I want <u>to go</u></em></td></tr>
      </tbody>
    </table>

                                    <div class='references'>
                                      <h4><strong>References</strong></h4>
                                      <p><strong>Baayen, R. H.</strong> <i>The effect of lexical specialization on the growth curve of vocabulary.</i> <strong>Computational Linguistics</strong>, 22(2), 1996.</p>
                                      <p><strong>Bentz, C., Alikaniotis, D., Cysouw, M., &amp; Ferrer-i-Cancho, R.</strong> <i>The entropy of words &mdash; learnability and expressivity across more than 1000 languages.</i> <strong>Entropy</strong>, 19(6), 2017.</p>
                                      <p><strong>Biber, D.</strong> <i>Variation across speech and writing.</i> <strong>Cambridge University Press</strong>, 1988.</p>
                                      <p><strong>de Marneffe, M.-C., Manning, C.D., Nivre, J., &amp; Zeman, D.</strong> (2021) <i>Universal Dependencies.</i> <strong>Computational Linguistics</strong>, 47(2), 255-308.</p>
                                      <p><strong>Guiraud, P.</strong> <i>Les caract&egrave;res statistiques du vocabulaire.</i> <strong>Presse Universitaire de France</strong>, 1954.</p>
                                      <p><strong>Tweedie, F. J., &amp; Baayen, R. H.</strong> <i>How variable may a constant be? Measures of lexical richness in perspective.</i> <strong>Computers and the Humanities</strong>, 32(5), 323-352, 1998.</p>
                                      <p><strong>Ure, J.</strong> <i>Lexical density and register differentiation.</i> In G. Perren and J.L.M. Trim (eds), <strong>Applications of Linguistics</strong>, Cambridge University Press, 443-452, 1971.</p>
                                      <p><strong>Yule, G. U.</strong> <i>The statistical study of literary vocabulary.</i> <strong>Cambridge University Press</strong>, 1944.</p>
                                      <p><strong>Zeman, D.</strong> (2017) <i>Universal Dependencies v2: An evergrowing multilingual treebank collection.</i> In <strong>Proceedings of LREC</strong>.</p>
                                      </div>
                                      </div>
                                      </body>"

  ## word in context ----
  wordincontext <- "<body>

    <h3 style='color: #4F7942;'><strong>Words in Context</strong></h3>

    <p>The <strong>Words in Context</strong> feature in TALL lets you examine how specific words appear in your textual data, which gives you insight into <strong>semantic usage, contextual meaning, and discourse structure</strong>. It is especially useful for <strong>qualitative text analysis, linguistic research, and content exploration</strong> in fields such as <strong>the social sciences, digital humanities, marketing, and legal studies</strong>.</p>
    <hr>
    <h4 style='color: #4F7942;'><strong>How Words in Context Works</strong></h4>

    <h4 style='color: #4F7942;'><strong>1. Concordance Analysis (Keyword in Context - KWIC)</strong></h4>
    <ul>
    <li>Displays each word <strong>side by side with the text around it</strong> (its left and right neighbors).</li>
    <li>Helps you identify <strong>common phrases, recurring structures, and variations in usage</strong>.</li>
    <li>Useful for <strong>studying semantic shifts, idiomatic expressions, and collocations</strong>.</li>
    </ul>

    <div class='example'>
      <strong>Example:</strong><br>
      If you analyze the term <strong>'sustainable'</strong> in a corpus of news articles, KWIC might show:<br>
      - 'sustainable <strong>development</strong> is a key focus of international policies'<br>
      - 'the company promotes <strong>sustainable</strong> and ethical supply chains'<br>
      - 'concerns over <strong>sustainable</strong> agricultural practices are increasing'<br>
      This shows you <strong>how 'sustainable' is used across different thematic contexts</strong>.
    </div>

      <h4 style='color: #4F7942;'><strong>2. Context Window Customization</strong></h4>
      <ul>
      <li>Set the <strong>window size</strong> (the number of words before and after the target term) to control how much context you see.</li>
      <li>Short windows highlight <strong>immediate linguistic relationships</strong>, while larger windows let you analyze <strong>broader semantic dependencies</strong>.</li>
      </ul>

      <div class='example'>
        <strong>Example:</strong><br>
        When you study <strong>'risk'</strong> in financial reports, changing the window size shows whether the word occurs alongside:<br>
        - <strong>'risk management,' 'high-risk investments'</strong> (short window)<br>
        - <strong>'the recent economic downturn has increased financial risk for small businesses'</strong> (larger window)<br>
        </div>

        <h4 style='color: #4F7942;'><strong>3. Frequency and Distribution Insights</strong></h4>
        <ul>
        <li>Words that appear in <strong>several contexts</strong> can be examined for <strong>frequency trends</strong>, which points you to the <strong>dominant themes</strong> attached to a term.</li>
        <li>Shows whether a word is <strong>spread evenly</strong> across the corpus or <strong>clustered</strong> in particular sections or documents.</li>
        </ul>

        <div class='example'>
          <strong>Example:</strong><br>
          In a dataset of <strong>customer reviews</strong>, the word <strong>'expensive'</strong> might often co-occur with:<br>
          - <strong>'but worth it'</strong> in <strong>positive reviews</strong><br>
          - <strong>'not justified for the quality'</strong> in <strong>negative reviews</strong><br>
          This helps you tell <strong>when 'expensive' carries a neutral, positive, or negative connotation</strong>.
        </div>

          <p>With <strong>customizable, interactive exploration of the text</strong>, the <strong>Words in Context</strong> tool in TALL gives you <strong>a closer reading of the language patterns</strong> in large textual datasets.</p>


          </body>"

  ## keyness ----
  keyness <- "<body>

    <h3><strong>Keyness Analysis</strong></h3>

    <p>Keyness analysis is a statistical technique that identifies words that are <strong>significantly more or less frequent</strong> in a target corpus than in a reference corpus. Use it to <strong>detect the distinctive vocabulary</strong> and the <strong>linguistic features</strong> that characterize particular texts, genres, or discourse communities (<strong>Scott, 1997; Gabrielatos, 2018</strong>).</p>

    <p>In TALL, keyness is computed by comparing the word frequencies in your corpus with <strong>reference word frequency lists</strong> derived from large, general-purpose language datasets. You can therefore see which words are <strong>overused</strong> or <strong>underused</strong> in your texts relative to typical language use.</p>
    <hr>

    <h4><strong>Reference Word Frequency Lists</strong></h4>

    <p>TALL uses word frequency lists calculated from the <strong>OpenSubtitles corpus</strong>, a large collection of subtitle files from movies and TV series in many languages. The data comes from the <strong>OPUS NLPL project</strong> (<strong>Tiedemann, 2012; Lison & Tiedemann, 2016</strong>) and is available at <a href='https://opus.nlpl.eu' target='_blank'>https://opus.nlpl.eu</a>.</p>

    <p>The OpenSubtitles corpus gives a <strong>balanced picture of everyday spoken language</strong> across many contexts, a good baseline for keyness analysis. By comparing a specialized or domain-specific corpus with this general-purpose reference, you can identify the <strong>terminological distinctiveness</strong> and the <strong>stylistic features</strong> that set your corpus apart.</p>
    <hr>

    <h4><strong>Supported Languages</strong></h4>

    <p>TALL currently supports keyness analysis for the following <strong>69 languages</strong>:</p>

    <ul>
      <li>Afrikaans, Albanian, Arabic, Armenian</li>
      <li>Basque, Bengali, Bosnian, Breton, Bulgarian</li>
      <li>Catalan, Chinese (Simplified), Chinese (Traditional), Chinese (English), Chinese (Pinyin), Classical Chinese, Croatian, Czech</li>
      <li>Danish, Dutch</li>
      <li>English, Esperanto, Estonian</li>
      <li>Finnish, French</li>
      <li>Galician, Georgian, German, Greek</li>
      <li>Hebrew, Hindi, Hungarian</li>
      <li>Icelandic, Indonesian, Italian</li>
      <li>Japanese</li>
      <li>Kazakh, Korean</li>
      <li>Latvian, Lithuanian</li>
      <li>Macedonian, Malay, Malayalam</li>
      <li>Norwegian</li>
      <li>Persian, Polish, Portuguese (European), Portuguese (Brazilian)</li>
      <li>Romanian, Russian</li>
      <li>Serbian, Sinhala, Slovak, Slovenian, Spanish, Swedish</li>
      <li>Tagalog, Tamil, Telugu, Turkish</li>
      <li>Ukrainian, Urdu</li>
      <li>Vietnamese</li>
    </ul>

    <p>Each language has its own word frequency list, computed from the corresponding OpenSubtitles dataset, so that keyness is always calculated against <strong>language-specific reference data</strong>.</p>
    <hr>

    <h4><strong>Output Visualizations</strong></h4>

    <p>TALL presents the results of a keyness analysis in three complementary ways:</p>

    <h4><em>1. Keyness Plot</em></h4>
    <p>A horizontal bar chart showing the <strong>Top keywords</strong> ranked by keyness score. Words with <strong>positive keyness values</strong> (in blue) are overrepresented in the target corpus, and words with <strong>negative keyness values</strong> (in red) are underrepresented compared with the reference corpus. The length of each bar reflects the size of the keyness score, so the most distinctive words stand out at a glance.</p>

    <h4><em>2. Word Cloud</em></h4>
    <p>An interactive word cloud in which <strong>word size is proportional to keyness strength</strong>. It gives an intuitive overview of the most characteristic terms in the corpus, with the largest words carrying the highest keyness values. It is useful for spotting dominant themes and terminology quickly.</p>

    <h4><em>3. Statistical Table</em></h4>
    <p>A data table with <strong>detailed statistical metrics</strong> for every word analyzed. It reports several keyness measures and effect size indicators for closer quantitative analysis. You can sort, filter, and export the results for further statistical processing or reporting.</p>
    <hr>

    <h4><strong>Statistical Indices in Keyness Analysis</strong></h4>

    <p>TALL computes a set of statistical measures that assess the significance and the size of the lexical differences between target and reference corpus. Each index tells you something different about a word's distinctiveness:</p>

    <h4><em>G² (Log-Likelihood Ratio)</em></h4>
    <p>The <strong>Log-Likelihood test (G²)</strong> is the main keyness measure in TALL (<strong>Dunning, 1993</strong>). It tests whether the observed frequency difference between the target and reference corpus is statistically significant. The G² statistic follows a chi-squared distribution and is less sensitive to corpus size than the alternatives. Higher absolute values indicate stronger keyness: positive values mean overuse, negative values mean underuse.</p>

    <h4><em>Sig_corrected (Statistical Significance)</em></h4>
    <p>The <strong>corrected significance level</strong> tells you whether the keyness difference is still statistically significant after <strong>correction for multiple testing</strong> (for example, Bonferroni or FDR correction). The correction keeps random variation from being read as distinctiveness. The usual significance thresholds are p < .001, p < .01, and p < .05.</p>

    <h4><em>Obs_Freq (Observed Frequency)</em></h4>
    <p>The <strong>observed frequency</strong> is the actual count of the word in the target corpus. This raw count gives context for how common the term is in the texts you analyzed.</p>

    <h4><em>Exp_Freq (Expected Frequency)</em></h4>
    <p>The <strong>expected frequency</strong> is the count you would expect in the target corpus given the word's frequency in the reference corpus and the relative sizes of the two corpora. A large gap between the observed and the expected frequency indicates keyness.</p>

    <h4><em>RDF (Relative Document Frequency)</em></h4>
    <p>The <strong>Relative Document Frequency</strong> is the proportion of documents in the target corpus that contain the word. It separates words that occur often in a few documents from words that are spread across many, which tells you about vocabulary consistency and dispersion.</p>

    <h4><em>RateRatio</em></h4>
    <p>The <strong>Rate Ratio</strong>, also known as the <strong>Relative Risk</strong>, is the ratio of the word's frequency rate in the target corpus to its frequency rate in the reference corpus. A RateRatio > 1 indicates overuse in the target corpus, and a RateRatio < 1 indicates underuse. As an effect size measure, it gives an intuitive reading of how large the difference is.</p>

    <h4><em>OddsRatio</em></h4>
    <p>The <strong>Odds Ratio</strong> compares the odds of a word appearing in the target corpus with the odds of it appearing in the reference corpus (<strong>Everitt, 2002</strong>). It is calculated as:</p>
    <p><code>OddsRatio = (a × d) / (b × c)</code></p>
    <p>where <em>a</em> is the word's frequency in the target corpus, <em>b</em> is the frequency of the other words in the target corpus, <em>c</em> is the word's frequency in the reference corpus, and <em>d</em> is the frequency of the other words in the reference corpus. Values greater than 1 indicate overuse; values less than 1 indicate underuse.</p>

    <h4><em>LogOddsRatio</em></h4>
    <p>The <strong>Log Odds Ratio</strong> is the natural logarithm of the Odds Ratio, and it gives a <strong>symmetric measure</strong> of effect size. The transformation makes the values easier to read, because equal positive and negative magnitudes represent associations of equal strength in opposite directions. The Log Odds Ratio is well suited to comparing keyness across studies and corpora.</p>

    <h4><em>phi (Phi Coefficient)</em></h4>
    <p>The <strong>Phi coefficient</strong> measures the association between two binary variables, here whether a word appears in the target corpus or in the reference corpus. Values range from -1 to +1, and values closer to ±1 indicate a stronger association. Phi is related to the chi-squared statistic and gives a normalized effect size.</p>

    <h4><em>MI (Mutual Information)</em></h4>
    <p>The <strong>Mutual Information</strong> score quantifies how much information a word's occurrence shares with corpus membership (<strong>Church & Hanks, 1990</strong>). A high MI value means that the presence of the word says a great deal about whether a text belongs to the target corpus. MI is good at picking out highly specific terminology, but it can be biased toward low-frequency words.</p>

    <h4><em>PMI (Pointwise Mutual Information)</em></h4>
    <p>The <strong>Pointwise Mutual Information</strong> is a variant of MI that measures the strength of the association between one word and the target corpus. A PMI value tells you how much more likely the word is to appear in the target corpus than it would be by chance. Positive PMI values point to a positive association (overuse), and negative values to a negative association (underuse).</p>

    <h4><em>DeltaP (Delta P)</em></h4>
    <p>The <strong>Delta P statistic</strong> (<strong>Gries, 2013</strong>) measures the <strong>directional strength of the association</strong> between a word and corpus membership. It ranges from -1 to +1, where positive values indicate attraction to the target corpus and negative values indicate repulsion. Delta P is a dependable keyness measure because it takes the asymmetry of word-corpus associations into account.</p>
    <hr>

    <h4><strong>Interpreting Keyness Results</strong></h4>

    <p>When you read keyness results in TALL, keep the following in mind:</p>

    <ul>
      <li><strong>Statistical Significance vs. Effect Size:</strong> A word may be statistically significant (a low p-value) and still have a small effect size, or the reverse. Always look at both the significance tests (G², Sig_corrected) and the effect size measures (RateRatio, LogOddsRatio, Delta P) before you interpret a result.</li>
      <li><strong>Positive vs. Negative Keyness:</strong> Positive keyness (blue bars) marks the words that are characteristic of your corpus, while negative keyness (red bars) reveals the words that are markedly absent or underused compared with general language.</li>
      <li><strong>Frequency Context:</strong> A high keyness score for a low-frequency word (a small Obs_Freq) often points to specialized terminology, whereas high keyness for a high-frequency word suggests a fundamental stylistic or thematic difference.</li>
      <li><strong>Multiple Indices:</strong> Different indices highlight different aspects of keyness. G² emphasizes statistical significance, the Log Odds Ratio gives a symmetric effect size, and Delta P accounts for directional associations.</li>
    </ul>
    <hr>

    <h4><strong>Applications of Keyness Analysis</strong></h4>

    <ul>
      <li><strong>Genre and Register Analysis:</strong> Identifying the linguistic features that distinguish academic writing, legal texts, news articles, or social media discourse.</li>
      <li><strong>Author Attribution and Stylometry:</strong> Detecting the distinctive patterns of word use that characterize an individual author or writing style.</li>
      <li><strong>Comparative Corpus Linguistics:</strong> Comparing vocabulary across time periods, regions, or social groups.</li>
      <li><strong>Terminology Extraction:</strong> Identifying technical terms and domain-specific vocabulary in a specialized corpus.</li>
      <li><strong>Discourse Analysis:</strong> Revealing ideological or thematic emphases through the detection of overused keywords.</li>
      <li><strong>Content Analysis:</strong> Characterizing the distinctive features of different text types, publications, or communication channels.</li>
    </ul>
    <hr>

    <h4><strong>Advantages of Keyness Analysis</strong></h4>

    <ul>
      <li><strong>Multilingual Support:</strong> Reference frequency lists for 69 languages, which makes cross-lingual keyness studies possible.</li>
      <li><strong>Large-Scale Reference Data:</strong> The OpenSubtitles corpus contains millions of words per language, which gives comparisons a solid statistical basis.</li>
      <li><strong>Comprehensive Statistical Measures:</strong> Several keyness indices are computed, so you can choose the one that fits your research question.</li>
      <li><strong>Multiple Visualization Options:</strong> Plot, word cloud, and table views, for different analytical and presentation needs.</li>
      <li><strong>Integration with TALL's NLP Pipeline:</strong> Keyness works directly on TALL's tokenization, lemmatization, and PoS-tagging output.</li>
      <li><strong>Exportable Results:</strong> Every statistical result can be exported to Excel for further analysis, reporting, or use in other tools.</li>
    </ul>

    <p>By combining keyness analysis with <strong>OpenSubtitles-based reference data</strong> and <strong>comprehensive statistical measures</strong>, TALL lets you carry out <strong>rigorous comparative linguistic studies</strong> across many languages and text types.</p>
    <hr>

    <div class='references'>
      <h4><strong>References</strong></h4>

      <p><strong>Church, K. W., & Hanks, P.</strong></p>
      <p><i>Word association norms, mutual information, and lexicography.</i> <strong>Computational Linguistics</strong>, 16(1), 22-29, 1990.</p>

      <p><strong>Dunning, T.</strong></p>
      <p><i>Accurate methods for the statistics of surprise and coincidence.</i> <strong>Computational Linguistics</strong>, 19(1), 61-74, 1993.</p>

      <p><strong>Everitt, B. S.</strong></p>
      <p><i>The Cambridge Dictionary of Statistics</i> (2nd ed.). Cambridge University Press, 2002.</p>

      <p><strong>Gabrielatos, C.</strong></p>
      <p><i>Keyness Analysis: Nature, metrics and techniques.</i> In C. Taylor & A. Marchi (Eds.), <strong>Corpus Approaches to Discourse: A Critical Review</strong> (pp. 225-258). Routledge, 2018.</p>

      <p><strong>Gries, S. T.</strong></p>
      <p><i>50-something years of work on collocations: What is or should be next...</i> <strong>International Journal of Corpus Linguistics</strong>, 18(1), 137-166, 2013. DOI: <a href='https://doi.org/10.1075/ijcl.18.1.09gri' target='_blank'>10.1075/ijcl.18.1.09gri</a></p>

      <p><strong>Lison, P., & Tiedemann, J.</strong></p>
      <p><i>OpenSubtitles2016: Extracting Large Parallel Corpora from Movie and TV Subtitles.</i> <strong>Proceedings of the 10th International Conference on Language Resources and Evaluation (LREC 2016)</strong>, 2016.</p>

      <p><strong>Scott, M.</strong></p>
      <p><i>PC analysis of key words—And key key words.</i> <strong>System</strong>, 25(2), 233-245, 1997. DOI: <a href='https://doi.org/10.1016/S0346-251X(97)00011-0' target='_blank'>10.1016/S0346-251X(97)00011-0</a></p>

      <p><strong>Tiedemann, J.</strong></p>
      <p><i>Parallel Data, Tools and Interfaces in OPUS.</i> <strong>Proceedings of the 8th International Conference on Language Resources and Evaluation (LREC 2012)</strong>, 2012.</p>

      <p><strong>OPUS NLPL Project:</strong> <a href='https://opus.nlpl.eu' target='_blank'>https://opus.nlpl.eu</a></p>
    </div>

  </body>"

  ## reinert ----
  reinert <- "<body>

    <h3><strong>Reinert Clustering</strong></h3>

    <p>Reinert clustering is a <strong>hierarchical descending classification method</strong> for <strong>textual data clustering</strong>. It identifies <strong>lexically homogeneous word clusters</strong> from the <strong>co-occurrence of terms within textual contexts</strong>. <strong>Max Reinert (1983, 1990)</strong> developed the method, and it has since become a core technique in <strong>corpus linguistics, sociolinguistics, and content analysis</strong>.</p>

    <p>Reinert's method works well on <strong>large textual datasets</strong>, which makes it a useful tool for <strong>thematic segmentation, discourse analysis, and socio-linguistic research</strong>.</p>
    <hr>
    <h4><strong>How Reinert Clustering Works</strong></h4>

    <h4>1. Text Segmentation into Context Units</h4>
    <ul>
    <li>The text is divided into <strong>small context units (CUs)</strong>, usually <strong>paragraphs or fixed-length segments</strong>, so that local lexical co-occurrence patterns are captured.</li>
    <li>Each CU is treated as a <strong>vector</strong> of word frequencies.</li>
    </ul>

    <h4>2. Iterative Splitting of Clusters</h4>
    <ul>
    <li>The method starts with <strong>all CUs in a single group</strong>.</li>
    <li>A <strong>first split</strong> is performed that <strong>maximizes intra-cluster homogeneity</strong> while keeping the <strong>word distributions</strong> of the two groups apart.</li>
    <li>This <strong>recursive process</strong> continues until no further meaningful lexical differentiation remains.</li>
    </ul>

    <h4>3. Statistical Association of Words to Clusters</h4>
    <ul>
    <li>Words receive <strong>probabilistic weights</strong> according to their <strong>distribution within each cluster</strong>.</li>
    <li>The <strong>most characteristic words</strong> of each cluster are identified, and together they form the <strong>lexical profile</strong> of the topic.</li>
    </ul>

    <h4>4. Interpretation and Thematic Analysis</h4>
    <ul>
    <li>The final clusters represent <strong>coherent thematic units</strong>.</li>
    <li>To interpret them, <strong>read the most significant words in each cluster</strong>.</li>
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
      <h4><strong>Implementation of Reinert Clustering</strong></h4>

      <p><strong>Reinert clustering in TALL</strong> was <strong>inspired by the 'rainette' package</strong> (<strong>Barnier & Privé, 2023</strong>). The original routines have been <strong>adapted to the TALL data structure</strong>, which holds <strong>tokenized, lemmatized, and PoS-tagged corpora</strong>.</p>

      <p>The adaptation gives you:</p>
      <ul>
      <li><strong>Control over the size of the context unit</strong>, to fit different corpus structures.</li>
      <li><strong>Compatibility with preprocessed linguistic data</strong>, for greater accuracy in lexical clustering.</li>
      <li><strong>Better performance</strong> on large-scale text analysis, since it draws on <strong>TALL's text processing pipeline</strong>.</li>
      <li><strong>Graphical visualization</strong> of thematic structures, which helps with <strong>interpretation and reporting</strong>.</li>
      </ul>

      <p>With <strong>Reinert's methodology carried over to TALL's NLP framework</strong>, you can <strong>run advanced text clustering analyses</strong> on corpora prepared with <strong>current linguistic preprocessing techniques</strong>.</p>
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

  ## correspondece analysis ----
  correspondenceanalysis <- "<body>

    <h3><strong>Correspondence Analysis</strong></h3>

    <p>Correspondence Analysis (<strong>CA</strong>) is a foundational technique for exploring <strong>semantic relationships</strong> among the words of a text collection (<strong>Benzécri, 1982; Lebart et al., 1997</strong>). Through <strong>dimensionality reduction</strong>, CA represents the most relevant information in a low-rank vector space and uncovers the <strong>latent structures</strong> in the data. It then <strong>visualizes those structures on factorial maps</strong>, so you can read the associations between terms and documents.</p>
    <hr>
    <h4><strong>Why Use Correspondence Analysis?</strong></h4>
    <ul>
    <li><strong>Revealing Hidden Patterns:</strong> CA captures relationships between words and documents that you would otherwise miss.</li>
    <li><strong>Dimensionality Reduction:</strong> By projecting the data into a lower-dimensional space, CA simplifies a complex corpus while retaining its key semantic information.</li>
    <li><strong>Visualization on Factorial Maps:</strong> The results appear as a <strong>graphical representation</strong>, which makes term clusters and document similarities easy to read.</li>
    </ul>
    <hr>
    <h4><strong>Limitations of Correspondence Analysis</strong></h4>
    <p>The main difficulty with CA is that the <strong>new features</strong> produced by dimensionality reduction often lack <strong>direct interpretability</strong>. Because the transformation is data-driven, the extracted factors do not always correspond to clear linguistic or thematic constructs, so it is harder to draw <strong>explicit meaning</strong> from the analysis.</p>
    <hr>
    <h4><strong>Enhancing Interpretability: The Tandem Approach</strong></h4>
    <p>To address this limitation, <strong>TALL integrates a tandem approach</strong> that combines CA with <strong>clustering techniques</strong> to make the results easier to interpret (<strong>Misuraca & Spano, 2020</strong>). The approach works in <strong>two steps</strong>:</p>
    <ul>
    <li><strong>Dimensionality Reduction with CA:</strong> The text data becomes a set of <strong>orthogonal and ordered features</strong>, preserving the essential relationships while reducing complexity.</li>
    <li><strong>Hierarchical Clustering:</strong> Clustering then runs on the transformed data and aggregates terms and documents at <strong>several levels</strong>. Unlike simple factor analysis, this method yields <strong>non-overlapping clusters</strong>, which are easier to interpret.</li>
    </ul>
    <hr>
    <h4><strong>Applications of Correspondence Analysis in Text Mining</strong></h4>
    <ul>
    <li><strong>Exploring Co-occurrence Patterns:</strong> See how often particular words appear together in a corpus.</li>
    <li><strong>Thematic Segmentation:</strong> Group documents by the linguistic characteristics they share.</li>
    <li><strong>Semantic Mapping:</strong> Reveal the <strong>latent structures</strong> in unstructured text data.</li>
    <li><strong>Lexical Field Analysis:</strong> Understand how words are distributed and related within a text collection.</li>
    </ul>

    <p>By combining <strong>Correspondence Analysis</strong> with <strong>clustering methods</strong>, TALL makes text mining workflows easier to <strong>interpret and use</strong>, and supports the <strong>unsupervised exploration</strong> of large document collections.</p>
    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>
      <p><strong>Benzécri, J. P.</strong> (1982). <i>Histoire et préhistoire de l’analyse des données.</i> Paris: Dunod.</p>
      <p><strong>Lebart, L., Salem, A., & Berry, L.</strong> (1997). <i>Exploring textual data.</i> Volume 4. Springer Science & Business Media.</p>
      <p><strong>Misuraca, M., & Spano, M.</strong> (2020). <i>Unsupervised Analytic Strategies to Explore Large Document Collections.</i> Heidelberg: Springer, 06, 17-28.</p>
      </div>

      </body>"

  ## co-word analysis ----
  cowordanalysis <- "<body>

    <h3><strong>Word Network Analysis</strong></h3>

    <p>TALL builds word networks in two complementary ways: <strong>co-occurrence networks</strong>, based on statistical proximity, and <strong>dependency networks</strong>, based on syntactic structure. Both produce interactive network visualizations, with community detection for thematic clustering.</p>

    <hr>
    <h4><strong>Network Type: Co-occurrence</strong></h4>
    <p>Co-occurrence analysis is a classic <strong>network-based text mining technique</strong> that examines how often words appear together within the same context unit (<strong>Callon et al., 1983</strong>). Two words are connected when they co-occur in the same sentence, paragraph, document, or group.</p>
    <ul>
      <li><strong>Nodes</strong> represent words, the terms extracted from the corpus.</li>
      <li><strong>Edges</strong> represent co-occurrence relationships at the grouping level you select.</li>
      <li><strong>Edge weights</strong> reflect co-occurrence frequency: stronger relationships produce thicker connections.</li>
    </ul>
    <p><strong>Co-occurrence level:</strong> choose the unit of analysis (Sentences, Paragraphs, Documents, or Groups). Smaller units (sentences) capture tighter semantic relationships, while larger units (documents) capture broader thematic associations.</p>

    <hr>
    <h4><strong>Network Type: Dependency</strong></h4>
    <p>Dependency networks use the <strong>syntactic dependency tree</strong> produced during tokenization to connect words. Two words are linked only if one depends syntactically on the other in the parse tree, whatever their distance in the text.</p>
    <ul>
      <li><strong>Nodes</strong> represent words, as in co-occurrence networks.</li>
      <li><strong>Edges</strong> represent <strong>grammatical relationships</strong> (e.g., a noun modified by an adjective, or a verb with its subject or object).</li>
      <li><strong>Edge weights</strong> reflect how many times the syntactic relationship occurs across the corpus.</li>
    </ul>
    <p><strong>Relation filters:</strong></p>
    <ul>
      <li><strong>All syntactic</strong> &mdash; includes all the major dependency relations (nsubj, obj, amod, nmod, compound, conj, advcl, etc.).</li>
      <li><strong>Noun modifiers</strong> &mdash; focuses on noun-centered relations (amod, nmod, compound, flat, nummod, appos), and reveals the descriptive structure of concepts.</li>
      <li><strong>Subject-Verb-Object</strong> &mdash; focuses on core argument structure (nsubj, obj, iobj) and shows &quot;who does what to whom&quot;.</li>
      <li><strong>Custom</strong> &mdash; lets you select specific dependency relations.</li>
    </ul>
    <p>For a complete list and description of the syntactic dependency relations, see the <strong>Dependency</strong> tab in the <strong>Overview</strong> menu.</p>

    <hr>
    <h4><strong>Co-occurrence vs Dependency: When to Use Which</strong></h4>
    <table style='width:100%; border-collapse:collapse; margin-bottom:15px;'>
      <thead>
        <tr style='background-color:#f0f0f0; border-bottom:2px solid #ccc;'>
          <th style='padding:8px; text-align:left;'>Aspect</th>
          <th style='padding:8px; text-align:left;'>Co-occurrence</th>
          <th style='padding:8px; text-align:left;'>Dependency</th>
        </tr>
      </thead>
      <tbody>
        <tr><td style='padding:6px;'>Connection basis</td><td>Proximity (same context unit)</td><td>Grammatical relation</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'>Noise level</td><td>Higher (coincidental proximity)</td><td>Lower (only structural links)</td></tr>
        <tr><td style='padding:6px;'>Distant relations</td><td>Missed if outside window</td><td>Captured if syntactically linked</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'>Interpretability</td><td>Thematic association</td><td>Grammatical roles</td></tr>
        <tr><td style='padding:6px;'>Best for</td><td>Broad thematic mapping</td><td>Precise semantic relationships</td></tr>
      </tbody>
    </table>

    <hr>
    <h4><strong>Normalization Measures</strong></h4>
    <p>Both network types support the same normalization measures, which control for term frequency bias (<strong>Eck &amp; Waltman, 2009</strong>):</p>

    <h4><em>Association Index</em></h4>
    <p>Normalizes co-occurrence against the expected frequency: <code>AI<sub>ij</sub> = C<sub>ij</sub> / (C<sub>i</sub> &times; C<sub>j</sub>)</code></p>

    <h4><em>Cosine Similarity</em></h4>
    <p>A geometric similarity measure: <code>cos(&theta;) = C<sub>ij</sub> / sqrt(C<sub>i</sub> &times; C<sub>j</sub>)</code></p>

    <h4><em>Jaccard Similarity</em></h4>
    <p>A set-based overlap measure: <code>J<sub>ij</sub> = C<sub>ij</sub> / (C<sub>i</sub> + C<sub>j</sub> - C<sub>ij</sub>)</code></p>

    <hr>
    <h4><strong>Community Detection</strong></h4>
    <p>TALL applies the <strong>Louvain algorithm</strong> (<strong>Blondel et al., 2008</strong>) for community detection. It optimizes modularity to find dense clusters of connected words:</p>
    <ul>
      <li>It runs <strong>10 iterations</strong> with different random seeds and keeps the solution with the highest modularity.</li>
      <li>It groups words into <strong>non-overlapping clusters</strong> that represent latent topics or conceptual domains.</li>
      <li>It applies <strong>community repulsion</strong> to separate clusters spatially in the visualization.</li>
    </ul>

    <hr>
    <h4><strong>Applications</strong></h4>
    <ul>
      <li><strong>Thematic mapping:</strong> identify research trends and conceptual structures in academic literature.</li>
      <li><strong>Topic detection:</strong> extract underlying themes from news, reports, or social media.</li>
      <li><strong>Semantic role analysis</strong> (dependency mode): understand the &quot;who does what&quot; patterns of a corpus.</li>
      <li><strong>Terminology extraction:</strong> discover domain-specific concept networks.</li>
    </ul>

    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>

      <p><strong>Callon, M., Courtial, J.-P., Turner, W.A., &amp; Bauin, S.</strong> (1983) <i>From translations to problematic networks: An introduction to co-word analysis.</i> <strong>Social Science Information</strong>, 22(2), 191-235.</p>

      <p><strong>Eck, N.J.V., &amp; Waltman, L.</strong> (2009) <i>How to normalize co-occurrence data? An analysis of some well-known similarity measures.</i> <strong>Journal of the American Society for Information Science and Technology</strong>, 60(8), 1635-1651.</p>

      <p><strong>Blondel, V.D., Guillaume, J.-L., Lambiotte, R., &amp; Lefebvre, E.</strong> (2008) <i>Fast unfolding of communities in large networks.</i> <strong>Journal of Statistical Mechanics</strong>, 2008(10), P10008.</p>

      <p><strong>de Marneffe, M.-C., Manning, C.D., Nivre, J., &amp; Zeman, D.</strong> (2021) <i>Universal Dependencies.</i> <strong>Computational Linguistics</strong>, 47(2), 255-308.</p>

      <p><strong>Fortunato, S., &amp; Hric, D.</strong> (2016) <i>Community detection in networks: A user guide.</i> <strong>Physics Reports</strong>, 659, 1-44.</p>
    </div>

    </body>"

  ## thematic map ----
  thematicmap <- "
  <body>

  <h3><strong>Thematic Map</strong></h3>

  <p>
  The <strong>Thematic Map</strong> in TALL lets you explore the conceptual structure of a text corpus by mapping its most relevant topics. It rests on an unsupervised, network-based method that extracts, clusters, and characterizes groups of words standing for distinct semantic areas in the texts you analyze. The method comes from bibliometric research and has been adapted in TALL for general-purpose text analysis.
</p>

  <hr>

  <h4><strong>Methodological Framework</strong></h4>

  <p>
  Thematic mapping starts with a <strong>co-occurrence matrix</strong> built from the preprocessed text corpus. The association strength between terms is then computed to normalize the raw co-occurrence frequencies:
  </p>

  <math xmlns='http://www.w3.org/1998/Math/MathML' style='font-size: 1.1em; display: block; text-align: center; margin: 10px 0;'>
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
    where <em>AS<sub>jj'</sub></em> is the association strength between the terms <em>j</em> and <em>j'</em>, and <em>a<sub>jj'</sub></em> is their observed co-occurrence. The measure expresses how closely a pair of terms is related in meaning.
  </p>

  <p>
    A <strong>community detection algorithm</strong> (WalkTrap) is then applied to the normalized network to identify clusters of terms, that is, topics. Each cluster is projected onto a plane defined by two dimensions:
  </p>

  <ul>
    <li><strong>Callon Centrality (CC):</strong> measures how much a topic interacts with the others, which indicates its <em>relevance</em> in the corpus.</li>
    <li><strong>Callon Density (CD):</strong> measures the internal cohesion of a topic, which reflects its <em>development</em>.</li>
  </ul>

  <p>Its centrality and density values place each topic in one quadrant of a <strong>strategic diagram</strong>:</p>

  <ul>
    <li><strong>Upper-right (Hot Topics):</strong> high centrality and high density - well developed and important.</li>
    <li><strong>Lower-right (Basic Topics):</strong> high centrality and low density - important but still under development.</li>
    <li><strong>Upper-left (Niche Topics):</strong> low centrality and high density - well developed but marginal.</li>
    <li><strong>Lower-left (Peripheral Topics):</strong> low centrality and low density - weakly developed and marginal.</li>
  </ul>

  <hr>

  <h4><strong>Features</strong></h4>

  <ul>
    <li>Generate a thematic map from any textual dataset that you have preprocessed and tokenized in TALL.</li>
    <li>The algorithm runs automatically and does <strong>not require you to set the number of topics</strong> in advance.</li>
    <li>Each topic is labeled with the most frequent keywords in its cluster.</li>
    <li>The size of a topic, that is, the size of its bubble, represents the number of terms in the cluster.</li>
    <li>Select specific time slices or metadata filters to run a <strong>comparative thematic analysis</strong> across groups or periods.</li>
  </ul>

  <p>
    A thematic map gives a rich and readable picture of the structure of a discourse, which suits exploratory text mining and culturomic studies.
  </p>

  <hr>

  <div class='references'>
    <h4><strong>References</strong></h4>
    <p><strong>Aria, M., Cuccurullo, C., D’Aniello, L., Misuraca, M., & Spano, M. (2022).</strong> <i>Thematic Analysis as a New Culturomic Tool: The Social Media Coverage on COVID-19 Pandemic in Italy.</i> <strong>Sustainability</strong>, 14(6), 3643. https://doi.org/10.3390/su14063643</p>
    <p><strong>Cobo, M.J., López-Herrera, A.G., Herrera-Viedma, E., & Herrera, F. (2011).</strong> <i>An approach for detecting, quantifying, and visualising the evolution of a research field: A practical application to the fuzzy sets theory field.</i> <strong>Journal of Informetrics</strong>, 5(1), 146–166.</p>
  </div>

</body>"

  ## embedding training ----
  embeddingtrain <- "

    <body>
    <h3><strong>Training Word Embeddings</strong></h3>
    <p>
    The <strong>Training</strong> module builds <strong>custom word embeddings</strong> from your own corpus with the <strong>word2vec algorithm</strong>,
  which includes both the <strong>Continuous Bag-of-Words (CBOW)</strong> and <strong>Skip-gram</strong> architectures.
  These models produce dense vector representations that capture semantic and syntactic relationships among words from their distributional context.
  </p>

    <hr>
    <h4><strong>Available Architectures</strong></h4>
    <ul>
    <li><strong>CBOW:</strong> Predicts a word from its surrounding context. It is faster and works well with frequent words.</li>
    <li><strong>Skip-gram:</strong> Predicts the surrounding context words from a target word. It is slower but performs better with infrequent words.</li>
    </ul>

    <hr>
    <h4><strong>How It Works</strong></h4>
    <ul>
    <li>The text is lemmatized and filtered to exclude non-informative tokens (e.g., punctuation, auxiliaries, determiners).</li>
    <li>Training runs at the sentence level, to preserve local context.</li>
    <li>Stopwords are identified and excluded automatically.</li>
    <li>You can set parameters such as <code>dimensionality</code>, <code>number of iterations</code>, and <code>architecture (CBOW/Skip-gram)</code>.</li>
    </ul>

    <hr>
    <h4><strong>Outputs</strong></h4>
    <ul>
    <li>The word embedding matrix.</li>
    <li>Descriptive statistics for each vector dimension (mean, SD, skewness, kurtosis).</li>
    <li>A PCA that shows the variance explained by each component.</li>
    <li>Cosine similarity and Euclidean distance metrics for quality assessment.</li>
    </ul>

    <hr>
    <h4><strong>Example</strong></h4>
    <p>
    Training a word2vec model on a corpus of product reviews may show that terms such as <code>'delivery'</code> and <code>'shipping'</code> sit close together in the vector space,
  an indication that they are semantically similar in that context.
  </p>

    <hr>
    <h4><strong>References</strong></h4>
    <ul>
    <li>
    Mikolov, T., Chen, K., Corrado, G., & Dean, J. (2013).
  <i>Efficient Estimation of Word Representations in Vector Space</i>.
  <a href='https://arxiv.org/abs/1301.3781' target='_blank'>arXiv:1301.3781</a>
    </li>
    <li>
    Mikolov, T., Sutskever, I., Chen, K., Corrado, G., & Dean, J. (2013).
  <i>Distributed Representations of Words and Phrases and their Compositionality</i>.
  <a href='https://arxiv.org/abs/1310.4546' target='_blank'>arXiv:1310.4546</a>
    </li>
    </ul>
    </body>
  "

  ## embedding similarity ----
  embeddingsimilarity <- "<body>
    <h3><strong>Word Similarity Network</strong></h3>
    <p>
    The <strong>Similarity</strong> module lets you explore the semantic relationships between words through an interactive <strong>similarity network</strong> built from the word embeddings you trained in the <strong>Training</strong> tab. Those embeddings come from the <strong>word2vec</strong> algorithm, in either its CBOW or its Skip-gram variant.
  </p>

    <hr>
    <h4><strong>How It Works</strong></h4>
    <ul>
    <li>
    TALL selects the <strong>top 100 most frequent content words</strong> in the corpus, restricted to the POS tags NOUN, PROPN, and ADJ.
  </li>
    <li>
    For each of those 100 terms, it computes the <strong>10 most similar words</strong> by <strong>cosine similarity</strong> in the embedding space.
  </li>
    <li>
    The resulting network is made up of:
    <ul>
    <li><strong>Nodes:</strong> the 100 target words (triangles) and the terms similar to them (dots).</li>
    <li><strong>Edges:</strong> links that represent semantic similarity scores (cosine similarity ≥ 0.5), with width proportional to the similarity.</li>
    </ul>
    </li>
    <li>
    The network then goes through <strong>community detection</strong> with the Walktrap algorithm, which highlights thematic clusters.
  </li>
    </ul>

    <hr>
    <h4><strong>Visualization Tools</strong></h4>
    <ul>
    <li><strong>UMAP projection:</strong> a two-dimensional semantic map of every word in the embedding matrix.</li>
    <li><strong>Overlap reduction:</strong> improves readability by adjusting label positions and opacity in dense areas.</li>
    <li><strong>Interactive display:</strong> zoom, node highlighting, and a draggable layout, through <code>visNetwork</code>.</li>
    </ul>

    <hr>
    <h4><strong>Example</strong></h4>
    <p>
    After training on a corpus of scientific publications, the similarity network might show <code>'method'</code>, <code>'approach'</code>, and <code>'model'</code> among the most frequent terms, each one connected to related concepts such as <code>'algorithm'</code>, <code>'technique'</code>, or <code>'framework'</code>.
  </p>

    <hr>
<h4><strong>References</strong></h4>
<ul>
  <li>
    Mikolov, T., Sutskever, I., Chen, K., Corrado, G. S., & Dean, J. (2013).
    <i>Distributed Representations of Words and Phrases and their Compositionality.</i>
    In *Advances in Neural Information Processing Systems* (NeurIPS 2013), 26, 3111–3119.
    <a href='https://papers.nips.cc/paper_files/paper/2013/file/9aa42b31882ec039965f3c4923ce901b-Paper.pdf' target='_blank'>
      [View PDF]
    </a>
  </li>
  <li>
    Mikolov, T., Chen, K., Corrado, G., & Dean, J. (2013).
    <i>Efficient Estimation of Word Representations in Vector Space.</i>
    <a href='https://arxiv.org/abs/1301.3781' target='_blank'>arXiv:1301.3781</a>
  </li>
</ul>
    </body>"

  ## tm chioice ----
  tmkchoice <- "<body>

  <h3><strong>Topic Modeling: K Selection</strong></h3>

  <p>Choosing the <strong>optimal number of topics (K)</strong> is one of the most critical steps in topic modeling. TALL offers a K selection framework for three model types: <strong>LDA</strong>, <strong>CTM</strong>, and <strong>STM</strong>.</p>

  <hr>
  <h4><strong>Why is K Selection Important?</strong></h4>
  <ul>
    <li>A <strong>K that is too small</strong> may <strong>merge distinct topics</strong>, reducing the ability of the model to separate thematic structures.</li>
    <li>A <strong>K that is too large</strong> may <strong>fragment coherent topics</strong>, adding unnecessary complexity and reducing interpretability.</li>
    <li>The <strong>correct K</strong> gives topics that are <strong>coherent, interpretable, and representative</strong> of the dataset.</li>
  </ul>

  <hr>
  <h4><strong>Supported Models</strong></h4>
  <p>TALL supports K selection for three topic modeling approaches:</p>
  <ul>
    <li><strong>LDA (Latent Dirichlet Allocation)</strong> – the classic probabilistic topic model (Blei et al., 2003). Topics are assumed to be independent.</li>
    <li><strong>CTM (Correlated Topic Model)</strong> – an extension of LDA in which topics may correlate with each other (Blei & Lafferty, 2007), using a logistic normal distribution instead of a Dirichlet one.</li>
    <li><strong>STM (Structural Topic Model)</strong> – lets external covariates (e.g., time, source, metadata) influence topic prevalence and content (Roberts et al., 2019).</li>
  </ul>

  <hr>
  <h4><strong>Metrics for LDA and CTM</strong></h4>
  <p>For LDA and CTM, TALL computes four standard metrics across the K range and identifies the optimal K with the <strong>elbow method</strong> (maximum distance from the line):</p>

  <h4><em>Cao et al. (2009) – Topic Coherence</em></h4>
  <p>It computes the <strong>average pairwise cosine similarity</strong> between topics. The optimal K minimizes inter-topic similarity, which leaves topics well separated. <strong>Lower values are better.</strong></p>

  <h4><em>Arun et al. (2010) – KL Divergence</em></h4>
  <p>It compares the <strong>word-topic distribution</strong> (via SVD) with the <strong>document-topic distribution</strong>, using the <strong>symmetric Kullback-Leibler divergence</strong>. The optimal K is where the divergence stabilizes. <strong>Lower values are better.</strong></p>

  <h4><em>Deveaud et al. (2014) – Jensen-Shannon Divergence</em></h4>
  <p>It measures the <strong>pairwise Jensen-Shannon divergence</strong> between topic distributions, balancing topic coherence and diversity. <strong>Lower values indicate more separated topics.</strong></p>

  <h4><em>Perplexity (Blei et al., 2003)</em></h4>
  <p>A <strong>likelihood-based metric</strong> that measures how well the model generalizes. It is defined as the inverse geometric mean of the likelihood. <strong>Lower values indicate better generalization.</strong></p>

  <hr>
  <h4><strong>Metrics for STM</strong></h4>
  <p>For STM, TALL uses <code>stm::searchK()</code>, which reports quality metrics specific to STM:</p>

  <h4><em>Exclusivity</em></h4>
  <p>It measures how <strong>exclusive</strong> the top words of each topic are. High exclusivity means that topic words are not shared across topics. <strong>Higher values are better.</strong></p>

  <h4><em>Semantic Coherence (Mimno et al., 2011)</em></h4>
  <p>It measures how often the <strong>top words of a topic co-occur</strong> within documents. Highly coherent topics are easier to interpret. <strong>Higher (less negative) values are better.</strong></p>

  <h4><em>Combined Score (Exclusivity + Coherence)</em></h4>
  <p>A synthetic metric that sums exclusivity and semantic coherence, balancing the two dimensions. <strong>Higher values indicate a better trade-off.</strong></p>

  <h4><em>Lower Bound</em></h4>
  <p>The variational <strong>lower bound on the log-likelihood</strong>. Higher values indicate a better model fit.</p>

  <hr>
  <h4><strong>Analysis Tabs</strong></h4>
  <ul>
    <li><strong>Tuning Plot</strong> – an interactive plot of the selected metric across the K values. The optimal K (the elbow point) is highlighted in red.</li>
    <li><strong>Multi-Metric Comparison</strong> – all four metrics, normalized to [0, 1] and plotted together. The elbow point of each metric is marked with a diamond, so you can see where the metrics agree or disagree.</li>
    <li><strong>K Recommendation</strong> – a consensus panel that shows the optimal K suggested by each metric, together with the <strong>overall recommended K</strong> (the mode of all suggestions). The consensus is more reliable than any single metric.</li>
    <li><strong>Table</strong> – the full table of raw and normalized metric values for every K.</li>
  </ul>

  <hr>
  <h4><strong>Integration with Model Estimation</strong></h4>
  <p>When K selection finishes, the recommended K is <strong>transferred automatically</strong> to the Model Estimation panel. Accept the recommendation, or adjust K yourself using your domain knowledge.</p>

  <hr>
  <h4><strong>Practical Guidelines</strong></h4>
  <ul>
    <li><strong>For exploratory research:</strong> start with <strong>low K</strong> values (e.g., <strong>5-20 topics</strong>) to get an overview of the broad themes.</li>
    <li><strong>For fine-grained analysis:</strong> use <strong>higher K values</strong> (e.g., <strong>30-100 topics</strong>) to capture more nuanced subtopics.</li>
    <li><strong>For benchmarking:</strong> compare different K values in the Multi-Metric Comparison tab.</li>
    <li><strong>For STM:</strong> look for K values that maximize both exclusivity and semantic coherence at once (the upper-right quadrant of the Model Diagnostics scatter plot).</li>
  </ul>

  <hr>
  <div class='references'>
    <h4><strong>References</strong></h4>

    <p><strong>Blei, D.M., Ng, A.Y., & Jordan, M.I.</strong> (2003) <i>Latent Dirichlet Allocation.</i> <strong>Journal of Machine Learning Research</strong>, 3, 993-1022.</p>

    <p><strong>Blei, D.M. & Lafferty, J.D.</strong> (2007) <i>A correlated topic model of Science.</i> <strong>The Annals of Applied Statistics</strong>, 1(1), 17-35.</p>

    <p><strong>Cao, J., Xia, T., Li, J., Zhang, Y., & Tang, S.</strong> (2009) <i>A density-based method for adaptive LDA model selection.</i> <strong>Neurocomputing</strong>, 72(7), 1775-1781.</p>

    <p><strong>Arun, R., Suresh, V., Veni Madhavan, C.E., & Narasimha Murthy, M.N.</strong> (2010) <i>On finding the natural number of topics with latent Dirichlet allocation: Some observations.</i> In <strong>Advances in Knowledge Discovery and Data Mining</strong> (pp. 391-402). Springer.</p>

    <p><strong>Mimno, D., Wallach, H.M., Talley, E., Leenders, M., & McCallum, A.</strong> (2011) <i>Optimizing semantic coherence in topic models.</i> In <strong>Proceedings of EMNLP</strong> (pp. 262-272).</p>

    <p><strong>Deveaud, R., Sanjuan, E., & Bellot, P.</strong> (2014) <i>Accurate and effective latent concept modeling for ad hoc information retrieval.</i> <strong>Document Numerique</strong>, 17, 61-84.</p>

    <p><strong>Roberts, M.E., Stewart, B.M., & Tingley, D.</strong> (2019) <i>stm: An R package for structural topic models.</i> <strong>Journal of Statistical Software</strong>, 91(2), 1-40.</p>
  </div>

  </body>"

  ## topic model estimation ----
  tmmodelestimation <- "<body>

    <h3><strong>Topic Modeling: Model Estimation</strong></h3>

    <p>TALL implements three topic modeling approaches, each suited to a different analytical need. All three produce <strong>beta</strong> (term-topic) and <strong>theta</strong> (document-topic) probability matrices, which you can explore in interactive visualizations.</p>

    <hr>
    <h4><strong>LDA – Latent Dirichlet Allocation</strong></h4>
    <p><strong>Blei et al. (2003)</strong> introduced LDA as a Bayesian generative model in which:</p>
    <ul>
      <li>Each document is a <strong>mixture of topics</strong> in different proportions (theta).</li>
      <li>Each topic is a <strong>probability distribution over words</strong> (beta).</li>
      <li>Topics are assumed to be <strong>independent</strong> of each other.</li>
    </ul>
    <p>TALL estimates LDA with <strong>Gibbs sampling</strong> (500 iterations), which tends to give more stable results than variational inference on small-to-medium corpora.</p>

    <hr>
    <h4><strong>CTM – Correlated Topic Model</strong></h4>
    <p><strong>Blei & Lafferty (2007)</strong> extended LDA by replacing the Dirichlet prior on topic proportions with a <strong>logistic normal distribution</strong>, which lets topics correlate. Use CTM when:</p>
    <ul>
      <li>Topics are naturally <strong>related</strong> (e.g., &quot;politics&quot; and &quot;economics&quot; often co-occur).</li>
      <li>You want to <strong>model inter-topic dependencies</strong> rather than assume independence.</li>
    </ul>
    <p>The <strong>Topic Correlation</strong> tab is especially informative with CTM, because the correlations reflect the structure the model learned, not a post-hoc observation.</p>

    <hr>
    <h4><strong>STM – Structural Topic Model</strong></h4>
    <p><strong>Roberts, Stewart & Tingley (2019)</strong> developed STM to bring <strong>document-level metadata</strong> (covariates) into the model. STM supports:</p>
    <ul>
      <li><strong>Prevalence covariates</strong> – external variables (e.g., publication year, source, author) that influence <strong>how much</strong> of each topic appears in a document.</li>
      <li><strong>Correlated topics</strong> – as in CTM, topics are allowed to correlate.</li>
    </ul>
    <p>Select the covariates in the Options panel. After estimation, the <strong>Covariate Effects</strong> tab shows how each covariate influences topic prevalence, with regression coefficients and effect plots.</p>

    <hr>
    <h4><strong>Analysis Tabs</strong></h4>
    <ul>
      <li><strong>Topic by Words Plot</strong> – top words per topic, ranked by beta probability and shown in groups of three for easy comparison. Navigate with the arrow buttons.</li>
      <li><strong>Topic by Docs Plot</strong> – documents most associated with each topic, ranked by theta probability.</li>
      <li><strong>Beta Probability</strong> – the full term-topic probability table.</li>
      <li><strong>Theta Probability</strong> – the full document-topic probability table.</li>
      <li><strong>Topic Correlation</strong> – a heatmap of correlations between topics, based on their word distributions, with embedded mini-scatterplots showing the data distribution.</li>
      <li><strong>Model Diagnostics</strong> – global quality metrics and per-topic indicators:
        <ul>
          <li><strong>LDA/CTM:</strong> log-likelihood, topic share, word entropy, and top word probability per topic.</li>
          <li><strong>STM:</strong> variational lower bound, semantic coherence, and exclusivity per topic, plus a scatter plot of coherence against exclusivity (ideal topics fall in the upper-right quadrant).</li>
        </ul>
      </li>
      <li><strong>Covariate Effects</strong> (STM only) – for each prevalence covariate:
        <ul>
          <li><strong>Effect plots</strong> showing how the covariate influences the prevalence of each topic (continuous variables give trend lines with 95% CI; categorical ones give point estimates).</li>
          <li>A <strong>regression coefficients table</strong> with estimates, standard errors, t-values, and p-values per topic.</li>
        </ul>
      </li>
    </ul>

    <hr>
    <h4><strong>Choosing the Right Model</strong></h4>
    <table style='width:100%; border-collapse:collapse; margin-bottom:15px;'>
      <thead>
        <tr style='background-color:#f0f0f0; border-bottom:2px solid #ccc;'>
          <th style='padding:8px; text-align:left;'>Feature</th>
          <th style='padding:8px; text-align:center;'>LDA</th>
          <th style='padding:8px; text-align:center;'>CTM</th>
          <th style='padding:8px; text-align:center;'>STM</th>
        </tr>
      </thead>
      <tbody>
        <tr><td style='padding:6px;'>Topic independence</td><td style='text-align:center;'>Assumed</td><td style='text-align:center;'>Correlated</td><td style='text-align:center;'>Correlated</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'>External covariates</td><td style='text-align:center;'>No</td><td style='text-align:center;'>No</td><td style='text-align:center;'>Yes</td></tr>
        <tr><td style='padding:6px;'>Inference method</td><td style='text-align:center;'>Gibbs sampling</td><td style='text-align:center;'>Variational EM</td><td style='text-align:center;'>Variational EM</td></tr>
        <tr style='background-color:#f8f8f8;'><td style='padding:6px;'>Best for</td><td style='text-align:center;'>General use</td><td style='text-align:center;'>Correlated themes</td><td style='text-align:center;'>Metadata-rich corpora</td></tr>
        <tr><td style='padding:6px;'>Speed</td><td style='text-align:center;'>Moderate</td><td style='text-align:center;'>Fast</td><td style='text-align:center;'>Moderate</td></tr>
      </tbody>
    </table>

    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>

      <p><strong>Blei, D.M., Ng, A.Y., & Jordan, M.I.</strong> (2003) <i>Latent Dirichlet Allocation.</i> <strong>Journal of Machine Learning Research</strong>, 3, 993-1022.</p>

      <p><strong>Blei, D.M. & Lafferty, J.D.</strong> (2007) <i>A correlated topic model of Science.</i> <strong>The Annals of Applied Statistics</strong>, 1(1), 17-35.</p>

      <p><strong>Mimno, D., Wallach, H.M., Talley, E., Leenders, M., & McCallum, A.</strong> (2011) <i>Optimizing semantic coherence in topic models.</i> In <strong>Proceedings of EMNLP</strong> (pp. 262-272).</p>

      <p><strong>Roberts, M.E., Stewart, B.M., & Tingley, D.</strong> (2019) <i>stm: An R package for structural topic models.</i> <strong>Journal of Statistical Software</strong>, 91(2), 1-40.</p>
    </div>

    </body>"

  ## polarity detection ----
  ## syntactic complexity ----
  syntacticcomplexity <- "<body>

    <h3><strong>Syntactic Complexity Analysis</strong></h3>

    <p>Syntactic complexity analysis quantifies the structural properties of sentences in a corpus using <strong>dependency parsing</strong>. For each document, TALL computes a set of metrics that characterize writing style, readability, and linguistic sophistication.</p>

    <hr>
    <h4><strong>Metrics</strong></h4>

    <h4><em>Mean Sentence Length</em></h4>
    <p>The average number of content words per sentence, excluding punctuation. Longer sentences generally indicate more complex writing.</p>

    <h4><em>Mean Tree Depth</em></h4>
    <p>The average maximum depth of the dependency tree across sentences, based on a BFS traversal from the root. Deeper trees indicate more levels of syntactic embedding (e.g., nested relative clauses, complex noun phrases).</p>
    <div style='text-align: center; margin: 20px 0;'>
      <img src='dep_tree_example.png' style='max-width: 100%; border: 1px solid #ddd; border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.1);' alt='Dependency tree example' />
      <p style='color: #888; font-size: 12px; margin-top: 8px;'><em>A dependency tree showing the syntactic relations between tokens. Tree depth is the longest path from root to leaf.</em></p>
    </div>

    <h4><em>Mean Dependency Distance</em></h4>
    <p>The average absolute distance, in token positions, between each word and its syntactic head (<strong>Liu, 2008</strong>). Longer distances indicate heavier processing demands. Languages and styles with more head-final or head-initial constructions tend to show different profiles.</p>

    <h4><em>Clauses per Sentence</em></h4>
    <p>The average number of clauses per sentence, computed as 1 (main clause) + subordinate clauses + coordinate verb clauses. Higher values indicate more complex multi-clause constructions.</p>

    <h4><em>Subordination Ratio</em></h4>
    <p>The proportion of subordinate clauses (advcl, acl, ccomp, xcomp, csubj) relative to total clauses. Higher ratios indicate a more hypotactic writing style (subordination), whereas lower ratios suggest a paratactic style (coordination). This is a key indicator of academic vs. informal writing.</p>

    <h4><em>Branching Factor</em></h4>
    <p>The average number of direct dependents per non-leaf node in the dependency tree. Higher branching indicates flatter but wider syntactic structures (e.g., enumerations, multiple modifiers).</p>

    <hr>
    <h4><strong>Analysis Tabs</strong></h4>
    <ul>
      <li><strong>Document Metrics</strong> &mdash; A full table of every metric per document, sortable and filterable.</li>
      <li><strong>Corpus Summary</strong> &mdash; Six summary cards, one per metric, with corpus-level averages.</li>
      <li><strong>Distributions</strong> &mdash; Overlaid histograms of all the metrics (toggle visibility from the legend), showing how documents are distributed along each complexity dimension.</li>
    </ul>

    <hr>
    <h4><strong>Applications</strong></h4>
    <ul>
      <li><strong>Readability assessment:</strong> Greater tree depth and dependency distance correlate with lower readability.</li>
      <li><strong>Authorship analysis:</strong> Different authors show characteristic syntactic profiles.</li>
      <li><strong>Genre comparison:</strong> Academic writing typically has higher subordination ratios than journalistic or informal text.</li>
      <li><strong>Language development:</strong> Syntactic complexity increases with proficiency in L2 learner corpora.</li>
      <li><strong>Temporal analysis:</strong> Track how writing complexity evolves over time in a corpus.</li>
    </ul>

    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>

      <p><strong>Liu, H.</strong> (2008) <i>Dependency distance as a metric of language comprehension difficulty.</i> <strong>Journal of Cognitive Science</strong>, 9(2), 159-191.</p>

      <p><strong>Lu, X.</strong> (2010) <i>Automatic analysis of syntactic complexity in second language writing.</i> <strong>International Journal of Corpus Linguistics</strong>, 15(4), 474-496.</p>

      <p><strong>de Marneffe, M.-C., Manning, C.D., Nivre, J., &amp; Zeman, D.</strong> (2021) <i>Universal Dependencies.</i> <strong>Computational Linguistics</strong>, 47(2), 255-308.</p>
    </div>

    </body>"

  ## SVO triplets ----
  svo <- "<body>

    <h3><strong>SVO Triplet Extraction</strong></h3>

    <p>SVO (Subject-Verb-Object) triplet extraction uses <strong>dependency parsing</strong> to find structured semantic relationships in your text. For every verb in the corpus, the algorithm extracts that verb's syntactic subject and object, producing triplets that answer the question: <strong>who does what to whom?</strong></p>

    <hr>
    <h4><strong>How It Works</strong></h4>
    <p>The algorithm walks the dependency tree of each sentence and, for every verb (VERB/AUX), identifies:</p>
    <ul>
      <li><strong>Subject</strong> &mdash; tokens with the dependency relation <code>nsubj</code> (nominal subject), <code>nsubj:pass</code> (passive subject), or <code>csubj</code> (clausal subject).</li>
      <li><strong>Object</strong> &mdash; tokens with the relation <code>obj</code> (direct object), <code>iobj</code> (indirect object), <code>obl</code> (oblique argument), <code>xcomp</code> or <code>ccomp</code> (clausal complement).</li>
    </ul>
    <p>For a transitive verb, the algorithm generates every subject-object pair. For an intransitive verb, it emits a subject-verb pair with an empty object field. The extraction is implemented in <strong>C++</strong> for high performance.</p>

    <hr>
    <h4><strong>Analysis Tabs</strong></h4>
    <ul>
      <li><strong>SVO Table</strong> &mdash; the full table of extracted triplets with frequency counts. Sort and filter it by subject, verb, object, or relation type.</li>
      <li><strong>SVO Network</strong> &mdash; a Sankey diagram showing the flow from subjects (blue) through verbs (green) to objects (red). Edge width reflects frequency. The diagram reveals which actors perform which actions on which targets.</li>
      <li><strong>Verb Frequency</strong> &mdash; a bar chart of the most frequent verbs in the SVO triplets. It highlights the dominant actions in the corpus.</li>
    </ul>

    <hr>
    <h4><strong>Parameters</strong></h4>
    <ul>
      <li><strong>Min. Frequency</strong> &mdash; the minimum number of occurrences for a triplet to be included (default: 2). Raise it to focus on recurring patterns.</li>
      <li><strong>Top N Triplets</strong> &mdash; the number of top triplets shown in the Sankey network (default: 50). It controls visual complexity.</li>
    </ul>

    <hr>
    <h4><strong>Applications</strong></h4>
    <ul>
      <li><strong>Content analysis:</strong> identify the dominant actors, actions, and targets in a corpus (e.g., &quot;government implements policy&quot;).</li>
      <li><strong>Comparative studies:</strong> compare SVO patterns across groups, time periods, or sources.</li>
      <li><strong>Event extraction:</strong> discover recurring events described in news, scientific abstracts, or reports.</li>
      <li><strong>Narrative analysis:</strong> map the structure of a narrative by examining who does what.</li>
    </ul>

    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>

      <p><strong>de Marneffe, M.-C., Manning, C.D., Nivre, J., &amp; Zeman, D.</strong> (2021) <i>Universal Dependencies.</i> <strong>Computational Linguistics</strong>, 47(2), 255-308.</p>

      <p><strong>Jurafsky, D., &amp; Martin, J.H.</strong> (2024) <i>Speech and Language Processing.</i> 3rd edition (draft). Chapter 18: Information Extraction.</p>
    </div>

    </body>"

  polaritydetection <- "<body>

    <h3><strong>Polarity Detection</strong></h3>

    <p>Polarity detection is a basic <strong>sentiment analysis technique</strong> that determines whether a document expresses a <strong>positive, negative, or neutral</strong> sentiment. It is central to the analysis of <strong>consumer feedback, financial reports, product reviews, and social media discussions</strong>, where sentiment trends reveal much about public opinion and decision-making.</p>
    <hr>
    <h4><strong>How Polarity Detection Works</strong></h4>
    <p>TALL computes <strong>document polarity</strong> with a <strong>lexicon-based approach</strong> and applies <strong>contextual adjustments</strong> to refine the sentiment score. The method has three steps:</p>

    <h4>1. Lexicon-Based Sentiment Scoring</h4>
    <ul>
    <li>Each word in the text receives a <strong>polarity score</strong> according to its presence in the <strong>sentiment lexicons</strong>.</li>
    <li><strong>Positive words</strong> such as 'excellent' or 'happy' receive <strong>+1</strong>, and <strong>negative words</strong> such as 'bad' or 'fail' receive <strong>-1</strong>.</li>
    <li>Words <strong>not found in the sentiment lexicons</strong> count as neutral and receive a score of <strong>0</strong>.</li>
    </ul>

    <h4>2. Contextual Modifications Using Valence Shifters</h4>
    <ul>
    <li><strong>Negators:</strong> words such as 'not', 'never', or 'no' <strong>invert the polarity</strong> of a nearby sentiment word ('not happy' moves from <strong>+1 to -1</strong>).</li>
    <li><strong>Amplifiers:</strong> words such as 'very', 'extremely', and 'highly' <strong>increase the intensity</strong> of a sentiment ('very good' weighs more than 'good').</li>
    <li><strong>De-amplifiers (Diminishers):</strong> terms such as 'slightly' or 'somewhat' <strong>reduce the intensity</strong> of a sentiment ('slightly disappointing' carries a weaker negative score than 'disappointing').</li>
    </ul>

    <h4>3. Aggregation and Normalization</h4>
    <ul>
    <li>The sentiment scores are <strong>summed across the document</strong> to give an <strong>overall polarity score</strong>.</li>
    <li>An <strong>optional normalization step</strong> rescales the final score to the <strong>[-1, 1] range</strong>, so that documents of different lengths remain comparable.</li>
    <li>Documents with scores close to <strong>0</strong> are classified as <strong>neutral</strong>, which indicates either a balanced mix of sentiment or the absence of strong emotion.</li>
    </ul>
    <hr>
    <h4><strong>Sentiment Lexicons Used</strong></h4>

    <h4>1. Hu and Liu (2004) - Opinion Lexicon</h4>
    <ul>
    <li>Built for the analysis of <strong>consumer reviews</strong>, it sorts words into <strong>positive and negative</strong> classes.</li>
    <li>It is most useful for <strong>e-commerce platforms, review aggregation sites, and user-generated feedback</strong>.</li>
    <li><strong>Language:</strong> English</li>
    </ul>

    <h4>2. Loughran and McDonald (2016) - Financial Sentiment Dictionary</h4>
    <ul>
    <li>Developed for <strong>financial and accounting texts</strong>, it includes the categories <strong>'positive', 'negative', 'uncertainty', 'litigious', and 'constraining'</strong>.</li>
    <li>It is widely used in <strong>financial risk assessment, investor sentiment analysis, and stock market forecasting</strong>.</li>
    <li><strong>Language:</strong> English</li>
    </ul>

    <h4>3. NRC Emotion Lexicon (Mohammad & Turney, 2010)</h4>
    <ul>
    <li>It reaches beyond simple polarity and sorts words into <strong>eight primary emotions</strong>: Joy, Sadness, Anger, Fear, Surprise, Disgust, Trust, and Anticipation.</li>
    <li>It is useful for <strong>social media mining, psychological studies, and literary analysis</strong>.</li>
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

  ## emotion analysis ----
  emotionanalysis <- "<body>

    <h3><strong>Emotion Detection</strong></h3>

    <p>Emotion analysis goes beyond simple positive/negative polarity by identifying the <strong>specific emotions</strong> expressed in a text.
    TALL uses the <strong>NRC Word-Emotion Association Lexicon (EmoLex)</strong> to detect eight primary emotions:
    <strong>Anger, Anticipation, Disgust, Fear, Joy, Sadness, Surprise, and Trust</strong>.</p>
    <hr>
    <h4><strong>How Emotion Detection Works</strong></h4>

    <h4>1. Lexicon-Based Emotion Detection</h4>
    <ul>
    <li>Each lemma in the text is matched against the <strong>NRC EmoLex</strong>, which records binary associations between words and the eight emotions.</li>
    <li>A single word can be associated with <strong>several emotions</strong> (e.g., 'abandon' may evoke both <strong>fear</strong> and <strong>sadness</strong>).</li>
    </ul>

    <h4>2. Document-Level Aggregation</h4>
    <ul>
    <li>For each document, the emotion word counts are <strong>summed across all matched lemmas</strong>.</li>
    <li>The counts are <strong>normalized to proportions</strong>, so you can compare documents of different lengths.</li>
    </ul>

    <h4>3. Corpus-Level Summary</h4>
    <ul>
    <li>Emotion counts are aggregated across all documents to give a <strong>corpus-level emotion profile</strong>.</li>
    <li>The <strong>Emotion Distribution</strong> chart shows how prevalent each emotion is in the corpus.</li>
    </ul>

    <h4>4. Word-Level Analysis</h4>
    <ul>
    <li>For each emotion, TALL identifies the <strong>most frequent contributing words</strong>.</li>
    <li>These show you <strong>which vocabulary drives</strong> each emotion category.</li>
    </ul>
    <hr>
    <h4><strong>The NRC EmoLex</strong></h4>
    <ul>
    <li>It was created by <strong>Saif Mohammad and Peter Turney</strong> using crowdsourcing on Amazon Mechanical Turk.</li>
    <li>It contains roughly <strong>14,000 words</strong> annotated with the eight emotion categories.</li>
    <li>It is available in <strong>several languages</strong>, translated automatically from the English source.</li>
    <li>It is widely used in <strong>social media analysis, literary studies, psychological research, and opinion mining</strong>.</li>
    </ul>
    <hr>
    <div class='references'>
      <h4><strong>References</strong></h4>

      <p><strong>Mohammad, S.M., & Turney, P.D.</strong></p>
      <p><i>Crowdsourcing a word-emotion association lexicon.</i> <strong>Computational Intelligence</strong>, 29(3), 436-465, 2013.</p>

      <p><strong>Mohammad, S.M., & Turney, P.D.</strong></p>
      <p><i>Emotions evoked by common words and phrases: Using Mechanical Turk to create an emotion lexicon.</i>
      In <strong>Proceedings of the NAACL HLT 2010 Workshop on Computational Approaches to Analysis and Generation of Emotion in Text</strong>,
      26-34. Los Angeles, CA: Association for Computational Linguistics.</p>
      </div>

      </body>"

  ## summarzation ----
  summarization <- "<body>

    <h3><strong>Summarization</strong></h3>

    <p>Summarization extracts the <strong>most relevant information</strong> from a document while preserving its core meaning.
    <br><strong>TALL implements extractive summarization</strong>, a method that selects and reorders the most important sentences <strong>directly from the original text</strong>
    to produce a <strong>coherent, condensed version</strong> of the content.</p>

    <p>Unlike <strong>abstractive summarization</strong>, which rephrases content with deep learning models, <strong>extractive summarization</strong> keeps
    the summary <strong>factually consistent</strong> with the input document, which makes it a <strong>reliable method for automated text compression</strong>.</p>
    <hr>
    <h4><strong>How Summarization Works</strong></h4>

    <h4>1. Sentence Tokenization and Preprocessing</h4>
    <ul>
    <li>The text is split into <strong>individual sentences</strong>, which form the basis of the summarization process.</li>
    <li>Each sentence is <strong>preprocessed</strong>: unnecessary punctuation and stopwords are removed to sharpen <strong>semantic clarity</strong>.</li>
    </ul>

    <h4>2. Graph Construction Using Sentence Similarity</h4>
    <ul>
    <li>A <strong>graph-based representation</strong> of the document is built, in which:</li>
    <ul>
    <li><strong>Nodes</strong> represent sentences.</li>
    <li><strong>Edges</strong> connect sentences according to their <strong>semantic similarity</strong>, measured by cosine similarity or word overlap.</li>
    </ul>
    <li>Sentences that share a <strong>high degree of lexical similarity</strong> count as <strong>strongly connected</strong> in the graph.</li>
    </ul>

    <h4>3. Application of TextRank Algorithm</h4>
    <ul>
    <li>The <strong>TextRank algorithm</strong> assigns an <strong>importance score</strong> to each sentence according to how well <strong>connected</strong> it is within the graph.</li>
    <li>The sentences with the <strong>highest PageRank scores</strong> are taken as <strong>the most representative</strong> of the document as a whole.</li>
    </ul>

    <h4>4. Sentence Selection and Ordering</h4>
    <ul>
    <li>The <strong>top-ranked sentences</strong> are selected for the summary.</li>
    <li>A <strong>reordering step</strong> presents them in a <strong>logical, coherent sequence</strong> that preserves the flow of the original document.</li>
    </ul>
    <hr>
    <h4><strong>Advantages of Summarization</strong></h4>
    <ul>
    <li><strong>Extractive and Factually Consistent</strong> - Summaries come directly from the original text,
    <br>which reduces the risk of hallucination or misinterpretation.</li>
    <li><strong>Graph-Based Ranking for Objective Selection</strong> - <strong>TextRank</strong> is unsupervised and
    <br><strong>ranks sentences purely on semantic importance</strong>, so no bias is introduced.</li>
    <li><strong>Efficient and Scalable</strong> - It processes <strong>large documents quickly</strong>, which suits
    <br><strong>research papers, news articles, legal documents, and reviews</strong>.</li>
    <li><strong>No Need for Pre-Trained Models</strong> - Abstractive methods depend on deep learning models, whereas
    <br><strong>extractive summarization works on any text without additional training</strong>.</li>
    <li><strong>Customizable Summary Length</strong> - Adjust the <strong>number of extracted sentences</strong> to control the
    <br><strong>level of detail</strong> in the summary.</li>
    </ul>
    <hr>
    <h4><strong>Implementation of Summarization</strong></h4>

    <p>TALL's <strong>summarization routines</strong> build on the <strong>TextRank algorithm</strong>, with optimizations for
    <strong>preprocessed and structured corpora</strong>:</p>

    <ul>
    <li><strong>Customized Text Preprocessing</strong> - The routines operate on <strong>tokenized, lemmatized, and PoS-tagged corpora</strong>,
    which gives a better sentence representation.</li>
    <li><strong>Sentence Similarity Based on Multiple Metrics</strong> - <strong>TF-IDF, cosine similarity, and word embeddings</strong> are supported for improved ranking.</li>
    <li><strong>Multi-Document Summarization (Future Work)</strong> - The framework is being extended to <strong>multi-document summarization</strong>,
    so that you can draw a summary from <strong>several related texts</strong>.</li>
    </ul>

    <p>Through <strong>unsupervised graph-based techniques</strong>, TALL gives you an <strong>efficient summarization tool</strong>
    for <strong>academic, business, and legal work</strong>.</p>
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

  return(list(
    importmenu = importmenu,
    split = split,
    random = random,
    externalinfo = externalinfo,
    tokenization = tokenization,
    specialentities = specialentities,
    multiwordcreation = multiwordcreation,
    multiwordlist = multiwordlist,
    customterm = customterm,
    synonyms = synonyms,
    posselection = posselection,
    featureroles = featureroles,
    overview = overview,
    keyness = keyness,
    wordincontext = wordincontext,
    reinert = reinert,
    correspondenceanalysis = correspondenceanalysis,
    cowordanalysis = cowordanalysis,
    thematicmap = thematicmap,
    embeddingtrain = embeddingtrain,
    embeddingsimilarity = embeddingsimilarity,
    tmkchoice = tmkchoice,
    tmmodelestimation = tmmodelestimation,
    syntacticcomplexity = syntacticcomplexity,
    svo = svo,
    polaritydetection = polaritydetection,
    emotionanalysis = emotionanalysis,
    summarization = summarization
  ))
}
