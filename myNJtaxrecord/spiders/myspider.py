# -*- coding: utf-8 -*-
import scrapy
from scrapy import Spider
from scrapy.http import Request, FormRequest


class MyspiderSpider(scrapy.Spider):
    name = 'myspider'
    allowed_domains = ['tax1.co.monmouth.nj.us/cgi-bin/prc6.cgi?district=0906']
    start_urls = ["https://tax1.co.monmouth.nj.us/cgi-bin/prc6.cgi?district=0906&ms_user=monm"]

    def parse(self, response):
        app_url = ["http://tax1.co.monmouth.nj.us/cgi-bin/prc6.cgi?district=0906&ms_user=monm"]
        yield Request(response.urljoin(app_url), callback=self.parse_form)

    def parse_form(self, response):
        yield FormRequest.from_response(response,dont_filter=True,formdata={'district value':'0906','select_cc value':'0901'})

    def parse_pages(self, response):
        application_urls = response.xpath('//td/a/@href').extract()
        for url in application_urls:
            url = response.urljoin(url)
            yield Request(url, callback=self.parse_items)


    def parse_items(self, response):
	name = response.xpath('//tr[3]/td/text()').extract_first()
	url = response.url

	yield {'name': name,
		'url': url}
      

