# CarSales
A project to collect cars classified ads from gumtree.com.au and conduct market analysis based on this data.

Description:

Scraper: This script collect cars content from gumtree.com.au, including 25 variables per listing. Note that this scraper is based on multiple htt get requests ,due to the lack of server side API. Hence, it might take a long period to complete. Excesive runs might get your IP to be blocked by the domain.
How to use: In a PC with scrapy installed and within this source code directory invoke the scraper with following syntex: scrapy crawl Gumtree -o [outputfile] eg. scrapy crawl Gumtree -o sample.csv The output will be avaiable in same code directory.
