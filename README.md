
# pseudo

**pseudo** is a Common Lisp library for generating and expanding pseudocode into valid Common Lisp expressions using LLMs (e.g., Gemini). It provides macros and functions to facilitate code generation, documentation, and integration with modern AI models.

## Features

- Generate Common Lisp expressions from natural language pseudocode.
- Automatically document code using LLMs.
- Enforce coding style, idioms, and preferred libraries.
- Integrate with Gemini and other LLMs for code generation and documentation.
- Customizable coding style and macro affinity.

## Installation

Clone the repository and load it with your preferred Common Lisp implementation (e.g., SBCL):

```lisp
(ql:quickload :pseudo)
```

## Usage

### Generating Expressions

Use the `pseudo` macro to generate a Common Lisp expression from pseudocode:

```lisp
(pseudo "Sum the elements of a list.")
```

### Generating Functions

Use the `pseudefun` macro to generate a function from pseudocode:

```lisp
(pseudefun sum-list (lst) "Sum the elements of lst.")
```

### Automatic Documentation

The library can generate docstrings for functions, macros, and variables using LLMs. See `autodoc.lisp` for details.

## Configuration

You can customize coding style and macro affinity by setting the following parameters:

- `*coding-style*` — `:functional`, `:pure`, `:imperative`, `:object-oriented`
- `*loop-affinity*` — `:love`, `:like`, `:neutral`, `:dislike`, `:hate`

## Supported Libraries

Preferred third-party libraries include:

- `alexandria`, `str` (data manipulation)
- `hunchentoot`, `dexador`, `quri` (web development)
- `json`, `cl-yaml`, `cl-csv` (data serialization)
- `bordeaux-threads` (concurrency)
- `ironclad` (cryptography)
- `local-time` (dates and times)
- `cffi` (FFI)

## License

See LICENSE for details.

## Contributing

Pull requests and issues are welcome!
