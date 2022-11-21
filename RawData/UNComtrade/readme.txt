DATA DOWNLOAD STEPS:

1. Upload R libraries.
2. Read list of country and commodity codes into R.
3. Collect monthly data from UN Comtrade - using 'comtradr' package.
   - Run loops to download data, with limits of 100 requests per hour; ps, r and p are limited to 5 codes each.
   - Only one of the above codes may use the special ALL value in a given API call. Classification codes (cc) are limited to 20 items. ALL is always a valid 
     classification code (http://comtrade.un.org/data/doc/api/).
4. Collate and view final database as downloaded.
5. Drop duplicate values in data extracted by selecting maximum value for each unique flow.

GENERAL:

Data link (data files exceed 25MB limit): https://www.dropbox.com/sh/cmctemngvo9gyfe/AADT5SgsN93hPVyKqf132TQla?dl=0

Script and input data provided and used to download monthly bilateral trade data from UN Comtrade API. 'Clean' code/script provided in primary directory in the '00_Collect_Data_vYYYY-MM-DD.R' script.
