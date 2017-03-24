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

# $x("html/body/div[@id='container']/div[@id='main-row']/div[@id='main-col']/div[@class='top--main-beginners-recipe']")





