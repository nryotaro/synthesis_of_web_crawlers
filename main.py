#!/usr/bin/env python
# -*- coding: utf-8 -*-

from lxml import html
import requests

attrs = set(['title', 'body', 'date'])

site = 'http://www.kyounoryouri.jp/'

specific_page = 'http://www.kyounoryouri.jp/'

sites = set([site])

expr_map = {site}

container = 'x'

html.fromstring(requests.get('http://www.kyounoryouri.jp/').content).xpath('/p')

e = {'http://www.kyounoryouri.jp/recipe/41153_%E3%82%AA%E3%83%BC%E3%83%96%E3%83%B3%E3%81%84%E3%82%89%E3%81%9A%E3%81%AE%E3%82%AA%E3%83%8B%E3%82%AA%E3%83%B3%E3%82%B0%E3%83%A9%E3%82%BF%E3%83%B3%E3%82%B9%E3%83%BC%E3%83%97.html': ("html/body/div[@id='container']/div[@id='main-row']/div[@id='main-col']",{'title': 'div[1]/div[2]/div[1]/h1/text()'})}

exprs = set(["html/body/div[@id='container']/div[@id='main-row']/div[@id='main-col']"])
# html/body/div[@id='container']/div[@id='main-row']/div[@id='main-col']/div[1]/div[2]/div[1]/h1/text() title

# $x("html/body/div[@id='container']/div[@id='main-row']/div[@id='main-col']/div[@class='top--main-beginners-recipe']")

o_knowledge_base = set()


def synthesize_deta_extractor(knowledge_base):
  # old_knowledge_base
    return ""





