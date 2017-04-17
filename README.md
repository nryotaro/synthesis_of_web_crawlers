# synthesis-crawlers

An implementation of [Cross-Supervised Synthesis of Web-Crawlers](http://dl.acm.org/citation.cfm?id=2884842)

## Usage

    (is (= (synthesis #{:title} 
                      {"http://www.foo.com" 
                       {:pages {"http://www.foo.com/blogs/1" "<html><body><span>hello world!</span></body></html>"}}
                       "http://www.bar.com" 
                       {:pages {"http://www.bar.com/1" "<html><body><div>hello world1</div></body></html>"}}}
                      {"http://www.foo.com" {"html > body" {:title "span"}}
                       "http://www.bar.com" {"" {:title nil}}}
                      0.5)
           {"http://www.foo.com" {"html > body" {:title "span"}} 
            "http://www.bar.com" {"html > body > div" {:title ""}}}))
 
Generating URL patterns is not supported. 

## License

Copyright © 2017 Nakamura, Ryotaro

Distributed under the MIT License either version 1.0 or (at
your option) any later version.
