


# master's thesis

This is a document I wrote with the great help of Dr. Bruno Loff in the year 2016. The abstract is as follows:

>While extending the semantics of programming languages is commonplace, extending their syntax is usually not possible. In this thesis, we are concerned with the problem of parsing programming languages that are simultaneously syntactically extensible and human-readable. The quintessential extensible programming language, Lisp, can be extended by two separate macro systems that allow user-defined syntactic features to be integrated naturally and directly into the parser. However, the default syntax of Lisp is notoriously unreadable. Other languages, such as Python, are highly readable, but are designed and parsed in ways that leave the syntax essentially frozen from the user’s perspective.
>
> We will propose a new parsing method, called Readtable-Macro TransducerChain parsing, that is inspired by Lisp’s reader algorithm. This algorithm is naively extended to add support for readable syntactic structures (such as infix notation), while retaining the spirit of simplicity and modularity that is its characteristic.
>
> As a proof-of-concept, we implement a compiler for a new programming language called Anoky — essentially an alternative syntax for Python.

### [Readtable-Macro Transducer-Chain Parsing](/aanvullend/rmtc.pdf)
