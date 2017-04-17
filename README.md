# synthesis-crawlers

An implementation of [Cross-Supervised Synthesis of Web-Crawlers](http://dl.acm.org/citation.cfm?id=2884842)

## Usage

    (synthesis attributes
               {website-url1 {:pages {page-url1 page-text1}}
                website-url2 {:pages {page-url2 page-text2}} ..}
               {website-url1 {container-descriptor {attribute1 attribute-node-descriptor1
                                                    attribute2 attribute-node-descriptor2 ..}}
                website-url2 {"" {attribute1 nil
                                  attribute2 nil}}}
               threshold)

- `attributes` defines the types of data to be extracted
- `website-url1`: the root url of website1
- `page-url1`: a url of website1
- `container-descriptor` is a CSS selector describing an item container. A container is a sub-tree that contains all the attribute values you would like to extract.
- `attribute-node-descriptor1,2` are css selectors which are relative to the root of the container  
- `attribute1` is one of `attributes`

### Example               
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

`#{:title}` are attributes, which define the types o

URL pattern synthesis is not supported. 

CSS

## License

Copyright © 2017 Nakamura, Ryotaro

Distributed under the MIT License either version 1.0 or (at
your option) any later version.
