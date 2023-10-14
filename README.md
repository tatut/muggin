# Muggin: HTML template engine for Prolog

Muggin is inspired by Pug and uses similar syntax (indentation driven short hand HTML).
Tested on SWI-Prolog.

## Usage

Use `parse/2` to parse a list of chars into a template.
And `render/1` to write the HTML of the template to the current output.


## Examples

Syntax example:
```
html
  head
    title This is my document
  body
    div#app.some-class
      | This is the text content
      button(onclick="alert('hello')") Click me
```
