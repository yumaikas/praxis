(declare-project 
    :name "praxis"
    :author "Andrew Owen"
    :url "https://github.com/yumaikas/praxis"
    :license "MIT"
    :description ```
    A library designed to help you declare, validate, and render objects that follow a schema. 
    Inspired by ecto, but has an emphasis on building helping render data as well as validate it.

    Still very much in alpha, until it's properly used in a site or 3
    ```
    :dependencies [
        "https://github.com/yumaikas/janet-errs"
        "https://github.com/yumaikas/janet-datex"
        "https://github.com/yumaikas/janet-stringx"
        "https://github.com/yumaikas/janet-assertx"
        "https://github.com/pyrmont/testament"
        "https://github.com/swlkr/janet-html"
    ])
    
(declare-source 
    :source @["src/praxis.janet" "src/praxis"])
    
    
