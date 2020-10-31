import requests
import shutil
import urllib3
from bs4 import BeautifulSoup

# This script was written to download  csv files from the EA EMI database for JuDGE
# Cal - 7 / 4 / 2020


def downloadCSV(mydir_, url_, CSV_):

    global counter

    new_url = url_ + "/" + CSV_
    r = requests.get(new_url, verify=False, stream=True)

    if r.status_code != 200:
        print("Failed to open connection with   ", CSV_)
        exit()
    else:
        r.raw.decode_content = True
        with open(mydir_ + CSV_, "wb") as f:

            shutil.copyfileobj(r.raw, f)
        print("Successfully opened a connection with   ", CSV_)

        counter += 1


def listCSVs(url_, years_):
    page = requests.get(url_)

    if page.status_code != 200:
        print("Failed to establish an initial connection to the EA EMI website.")
        exit()

    soup = BeautifulSoup(page.text, 'html.parser')
    csvNameList = soup.find(class_='table table-striped table-condensed table-clean')
    csvNameListLinks = csvNameList.find_all('a')
    csvNameListLinks = csvNameListLinks[1:]
    limitedListLinks = [csvNameListLinks[inds].contents[0]
                        for inds in range(0, len(csvNameListLinks))
                        if inds % 3 == 0]

    # Currently only interested in certain years
    limitedListLinks = [csvName for csvName in limitedListLinks if csvName[0:4] in years_]

    return limitedListLinks


if __name__ == '__main__':

    urllib3.disable_warnings(urllib3.exceptions.InsecureRequestWarning)  # Just to keep the console clean

    # Download all demand data
    urls = ["https://www.emi.ea.govt.nz/Wholesale/Datasets/Metered_data/Grid_import"]
            # , "https://www.emi.ea.govt.nz/Wholesale/Datasets/Generation/Generation_MD"
            # , "https://www.emi.ea.govt.nz/Wholesale/Datasets/Final_pricing/Final_prices"]

    years = ["2010", "2011", "2012", "2013", "2014", "2015"] #, "2016", "2017", "2018", "2019"]


    # Where all downloaded files will be stored
    mydir = "C:/Users/calro/Documents/Academic/JuDGE project/LDC_Jamie/"

    # Tracks number of downloaded files
    counter = 0

    for url in urls:
        # All the CSVs that we want to pull
        CSVs = listCSVs(url, years)
        print("\nAccessing: ", url)
        for CSV in CSVs:
            # Download each CSV
            downloadCSV(mydir, url, CSV)

    print("\nFinished downloading.")
    print("Expected number of files downloaded: ", len(years)*len(urls)*12)  # Assumes whole year of data is available
    print("Actual number of files downloaded: ", counter)
