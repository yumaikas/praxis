(declare-project 
    :name "praxis"
    :description "A library designed to help you declare, validate, and render objects that follow a schema. Inspired by ecto"
    :dependencies [
        "https://github.com/yumaikas/janet-errs"
        "https://github.com/yumaikas/janet-datex"
        "https://github.com/yumaikas/janet-stringx"
        # Relying on my fork for now, until pyrmont and I figure out
        # how best to define a suite
        "https://github.com/yumaikas/testament.git"
        "https://github.com/joy-framework/joy.git"
    ])
    
(declare-source 
    :source @["praxis.janet" "src/render.janet" "src/praxis.janet"])
    
    
