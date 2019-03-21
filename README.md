# README

*github-scholar* is an R script that makes use of the API of GitHub to update your repository hosting publications (e.g., journal papers, conference proceedings, magazines, articles, books). This is of use to any scholar, scientist, and professional that would like to have a centralised aggregation point of self-publications. 

You can see an example of a GitHub Publications page after have ran the script [HERE](https://github.com/Vasileios-Mavroeidis/Publications)


## How Do I Use the *github-scholar* Code

### Before we run the publication.R script from terminal we need the following:

1. Know or create the repository that is going to host the README file with our publications.

2. A json format file that has all the publications that we want to make available online aggregated. Check publications.json in the main folder of github-scholar [HERE](https://github.com/Vasileios-Mavroeidis/github-scholar/blob/master/publications.json) . You can use this file as a reference to the format. In general the format is based on BibTeX. A json file can be parsed by any programming language using the basic libraries and can be used easily for interoperability purposes. For example the same file (publications.json) can be used to update the publications in our web application.

3. Generate a [GitHub Token](https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line)

4. Know your GitHub username and repository name (where you will host your publications README file)

5. If you want to parse your Google Scholar account to extract the number of citations and indexes to add additionally in your publications page you need the URL of your scholar profile. For example https://scholar.google.no/citations?user=dpPINyoAAAAJ&hl

### Run the publication.R script

Type *Rscript publication.R --help* to see the available flags

Version 1 of the script supports the following options:


+ -g, --github *Update publications on GitHub* **[REQUIRED]**
    
+ -n NAME, --name NAME *GitHub name* **[REQUIRED]**

+ -r REPOSITORY, --repository REPOSITORY *GitHub repository of the publications Readme.md file* **[REQUIRED]**

+ -f FILE, --file FILE Path *File of publications in .json format proposed that we want to upload on GitHub* **[REQUIRED]**

+ -t TOKEN, --token TOKEN *Authentication token from GitHub* **[REQUIRED (or use usernmame and password - NOT RECOMMENDED)]**

+ -u USERNAME, --username USERNAME *(NOT RECOMMENDED TO USE) GitHub username - Instead use token* **[OPTIONAL]**

+ -p PASSWORD, --password PASSWORD *(NOT RECOMMENDED TO USE) GitHub password - Instead use token* **[OPTIONAL]**

+ -s SCHOLAR, --scholar SCHOLAR *Add Google Scholar Profile to Extract TOTAL Number of citations* **[OPTIONAL]**

+ -h, --help *Show this help message and exit* **[OPTIONAL]**


## Sample Command

Open a terminal and issue:

```
rscript publication.R -g -n Vasileios-Mavroeidis -f publications.json -t TOKEN -r Publications -s https://scholar.google.no/citations?user=dpPINyoAAAAJ&hl
```





