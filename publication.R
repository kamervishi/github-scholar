#!/usr/bin/env Rscript


#publication Version 1.0#


#################################################################################################################################################
#Check for the required packages. If they are not installed, then they will be installed. Otherwise, they will be loaded.
list_of_packages <- c("jsonlite", "httpuv", "httr", "optparse", "stringr")
for (p in list_of_packages) {if(p %in% rownames(installed.packages()) == FALSE) {
        install.packages(p)
        library(p, character.only = TRUE)} else library(p, character.only = TRUE)
}
#################################################################################################################################################


#################################################################################################################################################
#Terminal Options - optparse library
option_list <- list(
        make_option(c("-g", "--github"), type = "character", action = "store_true", default= FALSE,
                    help = "update publications on GitHub (used with 1. -f file, 2. -t token, 3. -u username and - p password, 4. -s scholar, 5. format style (e.g., -m MLA))"),
        make_option(c("-n", "--name"), type = "character", default = NULL,
                    help = "GitHub name"),
        make_option(c("-r", "--repository"), type = "character", default = NULL,
                    help = "GitHub repository of the publications Readme.md file"),
        make_option(c("-f", "--file"), type = "character", default = NULL,
                    help = "path - file of publications in .json format proposed that we want to upload on GitHub"),
        make_option(c("-t", "--token"), type = "character", default = NULL,
                    help = "authentication token from GitHub (https://help.github.com/en/articles/creating-a-personal-access-token-for-the-command-line)"),
        make_option(c("-u", "--username"), type = "character", default = NULL,
                    help = "GitHub username - Recommended to use token"),
        make_option(c("-p", "--password"), type = "character", default = NULL,
                    help = "GitHub password - Recommended to use token"),
        make_option(c("-s", "--scholar"), type = "character", action = "character", default= NULL,
                    help = "add Google Scholar Profile to Extract TOTAL Number of citations")
        #make_option(c("-m", "--mla"), type = "character", action="store_true", default = NULL,
                   # help = "output the publications in MLA format. Needs -f additionally"),
        #make_option(c("-a", "--apa"), type = "character", action="store_true", default = NULL,
                   # help = "output the publications in APA format"),
        #make_option(c("-d", "--harvard"), type = "character", action="store_true", default = NULL,
                   # help = "output the publications in HARVARD format")
)
opt_parser <-  OptionParser(option_list=option_list);
opt <- parse_args(opt_parser);
#################################################################################################################################################


#################################################################################################################################################
#Assign IDs to the publications. That will allow us to SORT them based on the year. Mapping between IDs and Year. We create one function that does both named "assign_id_and_sort()"

assign_id_and_sort <- function(){
        publications <- get("publications", envir = parent.frame())
        id <- 0 #ID
        e <- 0 #a counter 
        
        list_ids_based_on_years <- list()
        
        for (i in publications$publication){
                id <- id+1
                e <- e+1
                publications$publication[[e]]$id <- id
                list_ids_based_on_years[[id]] <- publications$publication[[e]]$year
        }
        length_of_list <- length(list_ids_based_on_years)
        names(list_ids_based_on_years) <- c(1:length_of_list) #assign names at the list elements
        unlist_to_sort <- unlist(list_ids_based_on_years)
        sorted_list <- sort(unlist_to_sort, decreasing = TRUE)
        names_sorted_list <- names(sorted_list)
        names_sorted_list <- as.integer(names_sorted_list)
        names_sorted_list <<- names_sorted_list
}
#################################################################################################################################################


#################################################################################################################################################
#Google citations function
#This function is used by the "github" function to extract the number of citations from a google scholar page.
#This function adds the number of citations in addition to the publication specified in the .json file on GitHub page.

google_citations <- function(scholar) {
        
        scholar_prof <- GET(url = scholar)
        content_of_html <- content(scholar_prof, as = "text")
        #<td class="gsc_rsb_std">43</td>
        scholar_profile <- str_match_all(string = content_of_html, pattern = '<td class="gsc_rsb_std">[[:digit:]]*</td>')
        scholar_name <- str_match_all(string = content_of_html, pattern= '<div id="gsc_prf_in">[a-zA-Z0-9_ ]*')
        scholar_name <- unlist(scholar_name)
        scholar_name <- strsplit(scholar_name, split = ">")
        scholar_name <- scholar_name[[1]][2]

        citations_nr <<-  str_extract(string = scholar_profile[[1]][1], pattern = '[[:digit:]]+')
        h_index <<- str_extract(string = scholar_profile[[1]][3], pattern = '[[:digit:]]+')
        i10_index <<- str_extract(string = scholar_profile[[1]][5], pattern = '[[:digit:]]+')
        
        #now take the produced citation variables and append them to the content
        
        google_scholar <- paste0("\n","## Google Scholar Statistics \n", "Citations: ", citations_nr,"<br/>","h-index: ", h_index, "<br/>", "i10-index: ", i10_index)
        google_scholar <<- google_scholar
        print(paste("The Google scholar profile of", scholar_name, "was parsed"))
}
#################################################################################################################################################


#################################################################################################################################################
#GitHub Function
#Parse the .json file of the publications (sample json file is available in the repository under the name "publications.json")
#path <- "" #assignment of the path of the .json file
#publications <- read_json(path) #read the .json file (creates a list of elements based on the json objects)

github <- function(){
        publications <- read_json(file)
        url <- paste0("https://api.github.com/repos/",name,"/",repository,"/contents/README.md")
        print(paste("Accessing GitHub API:",url))
        req <- GET(url) #get a user's repos. The GET request will allow us to extract the hash of the last commit of the README.md file
        
        json = content(req) #extract content from a request
        
        sha <- json$sha #extract the hash of the last commit
        
        github_json=fromJSON('{"message": "","content": "","sha": ""}') #from json we create an R list to assign the information needed to use the GitHub API. The fields in the json created are the required only. The user can bring more in addition based on the GitHub API guidelines (https://developer.github.com/v3/repos/).
        
        github_json$message <- paste0("Updated Publication Page - ", Sys.Date()) #get the system date and add the favorable message for a new commit on the Publications page on GitHub
        
        github_json$sha <- sha #assign the hash of the last commit. Needed by the GitHub API.
        
        ##########################################
        #Assign IDs to the publications. That will allow us to SORT them based on the year. Mapping between IDs and Year.
        assign_id_and_sort()
        ##########################################
        
       if (is.null(opt$mla)==TRUE && is.null(opt$apa)==TRUE && is.null(opt$harvard)==TRUE){
               ieee_style()     
       }else if (is.null(opt$mla)==FALSE) {
               mla_style()
       }else if (is.null(opt$apa)==FALSE){
               apa_style()
       }else if (is.null(opt$harvard)==FALSE){
               harvard_style()
       }
               
        #If google scholar url is given in the "github" function then the "google_citations" function is called
        if (!is.null(scholar) == TRUE){
                google_scholar_url <- scholar
                #use function google_citations. It gets the URL if set in the first function, it parses the scholar page and extracts the number of citations etc.
                # the google_citations function also appends the appropriate formatting and number of citations to the finalpub variable that will be used to Upload the content on GitHub
                #e.g., "https://scholar.google.no/citations?user=dpPINyoAAAAJ&hl=en"
                google_citations(google_scholar_url)
                finalpub <- append(finalpub, google_scholar)
        }
        
        finalpub <- paste(finalpub, collapse = "")
        
        github_json$content <- base64_enc(finalpub) #transform content to base64 as per GitHub API request and assign it to the github list that we will transform to .json format
        github_json <- toJSON(github_json) #transform the list back to json format (object)
        github_json <- gsub(pattern = "\\[", replacement = "", github_json) #removes array symbols ("[") from the json file. The GitHub API does not parse them.
        github_json <- gsub(pattern = "\\]", replacement = "", github_json) #removes array symbols ("[") from the json file. The GitHub API does not parse them.
        
        
        #Authentication
        #(1)Username and Password of GitHub account. Its better to Use a Token or OAuth2 than a username and password.
        #PUT("https://api.github.com/repos/name/repository/contents/README.md", authenticate(username, password), body = b, accept_json())
        
        #(2)Generate Token from GitHub and use it.
        print(paste("Publications are uploaded on GitHub Repository", '"',repository,'"', "of", name, sep = " "))
        put <- PUT(paste0("https://api.github.com/repos/",name,"/",repository,"/contents/README.md?access_token=", token), body = github_json, accept_json())
        if (put$status_code==200){print("The Publications have been SUCCESSFULLY uploaded on GitHub")}else cat(paste("Failed Attempt. Please check the response from GitHub below for details:", put))
}
#################################################################################################################################################


#################################################################################################################################################
#When deciding how to cite your source, start by consulting the list of core elements. These are the general pieces of information that MLA suggests including in each Works Cited entry. In your citation, the elements should be listed in the following order:
#Author.
#Title of source.
#Title of container,
#Other contributors,
#Version,
#Number,
#Publisher,
#Publication date,
#Location.


mla_style <- function(file = NULL){
        publications <- get("publications", envir = parent.frame())
        j <- 0 #We are numbering the publications (such as [1],[2])
##################################################################################################################################################        
        #Parses the publications based on the sorted IDs and uses the MLA style. When we have more than one first names we keep only the first letter and then a ".""follows.
        for (i in names_sorted_list){
                j <- j+1
                counter_for_name <- 0
                for (name in publications$publication[[i]]$author) { #we loop through the publication names to check if any of the names has more than one first name to modify acorrding to MLA style
                        counter_for_name <- counter_for_name+1
                        #print(name)
                        firstname_check <- strsplit(name, split = " ")
                        publications$publication[[i]]$author[counter_for_name]<- firstname_check
                        if (length(firstname_check[[1]])>2){ #if the length of the name is more than 2 then it means that we have more than one first names
                                indexes_to_parse <- length(firstname_check[[1]]) - 1 #we find the indexes of the first names we need to parse. We dont want the last name and the 1st first name.
                                second_counter_for_name <- 1
                                for (name in firstname_check[[1]][2:indexes_to_parse]){
                                        second_counter_for_name <- second_counter_for_name+1 #counter to keep the index to put back the modified first name
                                        name <- substring(name, 1,1 )#take the first letter of the name and add a " . "
                                        name <- paste(name,".", sep = "")
                                        firstname_check[[1]][second_counter_for_name] <- name
                                        publications$publication[[i]]$author[counter_for_name]<- firstname_check
                                }
                        }
                }#with the above we parse one by one the publications and split and modify the names based on the MLA guidelines. Then we put the modified strings back to the  publications$publication[[i]]$author
##################################################################################################################################################
                authors <- c() #Because the authors are a json array in publication.json we parse the authors and connect them in one string according to the style we want to use.
                
                #In addition, we create the publication text to be assigned in a variable named "pub". We follow the syntax of MLA. First we take care of the names for the cases that we have one author, two authors, or multiple authors.
                if (length(publications$publication[[i]]$author) > 1){ #For two or more authors
                        len <- length(publications$publication[[i]]$author)
                        if (len==2){ #if the authors are 2 we use the style as when we have one author and then we add an "and"  with the second author without a comma between the last name and the first name.
                                author1 <- publications$publication[[i]]$author[[1]]
                                author1 <- rev(author1)
                                author1[1] <- paste(author1[1],",", sep = "")
                                author1 <- paste(author1, collapse = " ")
                                author2 <-publications$publication[[i]]$author[[2]]
                                author2 <- rev(author2)
                                author2 <- paste("and",author2[1],author2[2], sep = " ")
                                authors <- paste(author1, author2, collapse = "") #connect author1 and author 2 MLA style
                        }else if (len>2){ #For three or more authors, only include the first listed author???s name. Place the first author in reverse order, place a comma afterwards, and then add the Latin phrase, et al.
                                authors <- strsplit(publications$publication[[i]]$author[[1]], split = " ")
                                authors <- unlist(authors)
                                authors <- rev(authors)
                                authors <- paste(authors[1]," ", authors[2],",", " et al.", collapse = "", sep = "")
                        }
                }else if (length(publications$publication[[i]]$author) == 1){ #If there is only one author then in MLA STYLE we bring the last name first followed by a comma and the first name
                        authors <- strsplit(publications$publication[[i]]$author[[1]], split = " ")
                        authors <- unlist(authors)
                        authors <- rev(authors)
                        authors[1] <- paste(authors[1],",", sep = "")
                        authors <- paste(authors, collapse = " ")
                }
                ###################### 
                
                finalpub <- "# Publications \n"  #The heading in the publications page on GitHub. Also accomodates the contents of the PUT request.
                
                
                #check if the publication is a book - The title of the source should follow the author???s name. Depending upon the type of source, it should be listed in italics or quotation marks. A book should be in italics:
                if (!is.null(publications$publication[[i]]$type)){ #if the "type" field has been used for the publication in the json file then check if it is a book. MLA has different formating for books and everything else.
                        if (publications$publication[[i]]$type == "book"){
                                if (is.null(publications$publication[[i]]$organization)==TRUE){ #if the organization is not included in the publication
                                        pub <- paste0("[",j,"]",authors,". ","*",publications$publication[[i]]$title,"*",". ",publications$publication[[i]]$year,".","<br/><br/>") 
                                }else {pub <- paste0("[",j,"]",authors,". ","*",publications$publication[[i]]$title,"*"," .",publications$publication[[i]]$container, ". ",publications$publication[[i]]$year,".","<br/><br/>")}
                        }
                }else{
                        if (is.null(publications$publication[[i]]$organization)==TRUE && is.null(publications$publication[[i]]$pages)==TRUE){ #if the organization and the pages are not included in the publication
                                pub <- paste0("[",j,"]",authors,". ",'"',publications$publication[[i]]$title,'"', ". ",publications$publication[[i]]$year,".","<br/><br/>") 
                        }else if (is.null(publications$publication[[i]]$organization) == TRUE){ #if only the organization is not included in the publication
                                pub <- paste0("[",j,"]",authors,". ", '"',publications$publication[[i]]$title,'"', ". ", "*",publications$publication[[i]]$container,"*",". ", "p. ", publications$publication[[i]]$pages,". ", 
                                              publications$publication[[i]]$year,".","<br/><br/>")
                        }else if (is.null(publications$publication[[i]]$pages) == TRUE){ #if only the pages are not included in the publication
                                pub <- paste0("[",j,"]",authors,". ", '"',publications$publication[[i]]$title,'"', ". ", "*",publications$publication[[i]]$container,"*",". ",
                                              publications$publication[[i]]$organization,", ", publications$publication[[i]]$year,".","<br/><br/>");
                        }else pub <- paste0("[",j,"]",authors,". ", '"',publications$publication[[i]]$title,'"', ". ", "*",publications$publication[[i]]$container,"*",". ", "p. ", publications$publication[[i]]$pages,". ",
                                            publications$publication[[i]]$organization,", ", publications$publication[[i]]$year,".","<br/><br/>")
                }
                
                finalpub <- append(finalpub, pub) #append each publication to the finalpub variable
        }
}#end of function

#If you???re citing a source in its entirety, such as a full book, a movie, or a music album, then place the title in italics.
#If you???re citing a source, such as a chapter in a book, a song on an album, or an article in a journal or website, then place
#the title of the piece in quotations and add a period afterwards. 
#Follow it with the title of the full source, in italics, and then add a comma. This second portion is called the container. Containers hold the sources.

#################################################################################################################################################
#IEEE style format. It is used as default for our Publication style
ieee_style <- function(){
        publications <- get("publications", envir = parent.frame())
        finalpub <- "# Publications \n"  #The heading in the publications page on GitHub. Also accomodates the contents of the PUT request.
        j <- 0 #on Github Page we are numbering the publications (such as [1],[2])
        #Parses the publications based on the sorted IDs and creates the publication text to be assigned in a variable named "pub".
        for (i in names_sorted_list){
                j <- j+1
                authors <- c() #Because the authors are a json array in publication.json we parse the authors and connect them in one string according to the style we want to use.
                if (length(publications$publication[[i]]$author) > 1){
                        publications$publication[[i]]$author[length(publications$publication[[i]]$author)] <- paste("and",publications$publication[[i]]$author[length(publications$publication[[i]]$author)])
                        len <- length(publications$publication[[i]]$author)
                        if (len>2){
                                newlen <- len-2 #we are going to put commas to the authors except of the last one that has an "and"
                                for (z in 1:newlen){
                                        publications$publication[[i]]$author[z] <- paste(publications$publication[[i]]$author[z],",", sep = "")
                                }
                        }
                        for (author in publications$publication[[i]]$author){ #append everything in one string (vector)
                                
                                authors <- append(authors, author)
                        }
                        authors <- paste0(authors, collapse = " ")
                }
                if (is.null(publications$publication[[i]]$organization) == TRUE && is.null(publications$publication[[i]]$pages) == TRUE ){ #if the organization and the pages are not included in the publication
                        pub <- paste0("[",j,"]",authors,". ", publications$publication[[i]]$title, ". ", "*",publications$publication[[i]]$container,"*",". ", 
                                      publications$publication[[i]]$year,".","<br/><br/>") 
                }else if (is.null(publications$publication[[i]]$organization) == TRUE){ #if only the organization is not included in the publication
                        pub <- paste0("[",j,"]",authors,". ", publications$publication[[i]]$title, ". ", "*",publications$publication[[i]]$container,"*",". ", "p. ", publications$publication[[i]]$pages,". ", 
                                      publications$publication[[i]]$year,".","<br/><br/>")
                }else if (is.null(publications$publication[[i]]$pages) == TRUE){ #if only the pages are not included in the publication
                        pub <- paste0("[",j,"]",authors,". ", publications$publication[[i]]$title, ". ", "*",publications$publication[[i]]$container,"*",". ",
                                      publications$publication[[i]]$organization,", ", publications$publication[[i]]$year,".","<br/><br/>");
                }else pub <- paste0("[",j,"]",authors,". ", publications$publication[[i]]$title, ". ", "*",publications$publication[[i]]$container,"*",". ", "p. ", publications$publication[[i]]$pages,". ",
                                    publications$publication[[i]]$organization,", ", publications$publication[[i]]$year,".","<br/><br/>")
                
                finalpub <- append(finalpub, pub) #append each publication to the finalpub variable
                finalpub <<- finalpub
        }
}
#################################################################################################################################################


#################################################################################################################################################
#################################################################################################################################################
#Parse the commands in terminal and call the appropriate functions
{
if (opt$github == "TRUE"){ #if -g is set then we need a github name -n, a github repository -r, a path for the .json file of the publications -f, a GitHub token -t or a GitHub username -u and password -p, and the format style (e.g., -m for MLA).
        if(is.null(opt$name)){ 
                stop("GitHub name (nickname) is Required: Use -n NAME")
        }else {
                name <- opt$name
                print("GitHub name (nickname) was given as input")
        }
        if(is.null(opt$repository)){
                stop("GitHub repository is Required: Use -r REPOSITORY ")
        }else{
                repository <- opt$repository
                print("GitHub repository was given as input")
        }
        if (is.null(opt$file)){
                stop("The publications file path .json is Required: Use - f FILE ")
        }else{
                file <- opt$file
                print("Publications file path was given as input")
                
        }
        if(is.null(opt$token)){ #if the token is NULL we check for username and password
                if (!is.null(opt$username) && !is.null(opt$password)){
                        username <- opt$username
                        password <- opt$password
                        print("Username and Password were given as input (not recommended practice-instead use a GitHub generated token)")
                }else{#since no token or username and password are available we call the token_or_credentials function that will request the aforementioned.
                       stop("Token or Credentials are Required: Use -t TOKEN or -u and -p for USERNAME and PASSWORD")
                }
        }else{
                print("Token was given as input")
                token <- opt$token
        }
        if (!is.null(opt$scholar)==TRUE){
                print("Google Scholar Profile was given as input")
                scholar <- opt$scholar
        }else scholar <- NULL
        github()   #call the github function
        
}else{
        stop("Version 1 builds only on the GitHub API to update a record of publications. Thus, -g needs to be used. Re-run the script setting the -g flag. For more information use --help")
}
}


#################################################################################################################################################
#################################################################################################################################################

