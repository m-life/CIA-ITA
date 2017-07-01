from selenium.webdriver.support import expected_conditions as EC
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.common.by import By
from selenium.webdriver import Chrome
from bs4 import BeautifulSoup
import re
import pandas as pd
import time

tot_tessere = pd.read_csv("tessere.csv")['tessera']

tessere_string = list(map(str, tot_tessere))
tessere_string = ["0" + i for i in tessere_string]

scraped_df = pd.DataFrame(columns=['tessera', 'data', 'stato', 'gara'])

chromedriver = "/Applications/chromedriver"
driver = Chrome(chromedriver)


def initialize():
    driver.get("https://servizi.fip.it/fol/html/common/login.html")
    WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.NAME, 'userName')))
    driver.find_element_by_name("userName").send_keys('m.vita_desa_crma')
    driver.find_element_by_name("password").send_keys('Marco40954')

    driver.find_element_by_class_name("GGBLLEADBU").click()
    WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.ID, 'gwt-uid-10')))
    driver.find_element_by_id("gwt-uid-10").click()
    driver.find_element_by_id("gwt-uid-6").click()
    WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CLASS_NAME, 'GLNMTTQDEHQ')))
    driver.find_element_by_class_name("GLNMTTQDEHQ").click()
    time.sleep(2)
    driver.find_element_by_xpath("//*/option[@value = '88']").click()
    driver.find_element_by_name("tesseraCIA").send_keys("040954")
    time.sleep(2)
    driver.find_element_by_class_name("GLNMTTQDLIR").click()
    time.sleep(2)
    driver.find_element_by_class_name("gwt-Image").click()


def scrape(ref_id):
    time.sleep(2)
    driver.find_element_by_class_name("GLNMTTQDEHQ").click()
    WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CLASS_NAME, 'GLNMTTQDLIR')))
    time.sleep(1)
    driver.find_element_by_xpath("//*/option[@value = '88']").click()
    driver.find_element_by_name("tesseraCIA").clear()
    driver.find_element_by_name("tesseraCIA").send_keys(ref_id)
    time.sleep(1)
    driver.find_element_by_class_name("GLNMTTQDLIR").click()
    WebDriverWait(driver, 10).until(EC.presence_of_element_located((By.CLASS_NAME, 'hoverRiga')))
    time.sleep(2)
    driver.find_element_by_class_name("gwt-Image").click()
    time.sleep(4)
    html = BeautifulSoup(driver.find_element_by_class_name("GLNMTTQDHCG").get_attribute("outerHTML"), "html.parser")
    table = html.find('form')
    rows = table.find_all('tr', attrs={'class': re.compile('hoverRiga')})

    for i in range(len(rows)):
        global scraped_df
        scraped_df = scraped_df.append(
            {'tessera': ref_id,
             'data': rows[i].find_all('td')[0].text,
             'stato': rows[i].find_all('td')[2].text,
             'gara': rows[i].find_all('td')[3].text}, ignore_index=True)
    html.clear()
    time.sleep(1)


tessere = tessere_string[0:5174]

temp = tessere[200:1000]

for tessera in temp:
    try:
        scrape(tessera)
        print(tessera)
    except:
        print("ERRORE:" + tessera)
        driver.close()
        chromedriver = "/Applications/chromedriver"
        driver = Chrome(chromedriver)
        initialize()

driver.close()
