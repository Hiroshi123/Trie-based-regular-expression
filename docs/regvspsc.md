

# Regular expression to Parser Combinator

Parser Combinator covers context-free-grammer and regular exrepssion is a subset of it.

Therefore, translation from regexp to parser-combinator is possible but impossible on the
other way around.

# Basic Idea

|Regular expression|A function on parser combinator|
|:-|:-|
|Conjunction|Bind|
|Disjunction|MonadPlus|
|Cleene star|many|
|Plus|many1|
|Brace|one_of|



