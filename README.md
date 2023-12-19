# `todolist-json2tex`

[LaTex](https://www.latex-project.org/) preprocessor from JSON todo data - useful for rendering todo lists directly to `pdf` format

## Dependencies

* LaTex distribution
  * tested on `texlive-scheme-full` (fedora linux)
    * should provide `pdflatex` executable

### Development

* `ghcup`
* `stack`
* `hls`

## Example setup

1. Ensure that you have LaTeX distribution installed, along with `pdflatex` command

    ```sh
    which pdflatex
    ```

2. `git clone` this repo
3. Build the code

    ```sh
    make build
    ```

4. Run the preprocessor

    ```sh
    ./bin/todolist-json2tex \
      --title "My Todo" \
      --subtitle "My Todo Description" \
      --mustache-template "./examples/example-basic/mustache/template.tex" \
      --output "./examples/example-basic/tex-output/todo.tex" \
      ./examples/example-basic/todo.json
    ```

5. Compile the pdf

    ```sh
    pdflatex \
      -interaction=errorstopmode \
      -output-directory=./examples/example-basic/pdf-output \
      ./examples/example-basic/tex-output/todo.tex
    ```

6. View the [output](./examples/example-basic/pdf-output/todo.pdf)
7. Optional - customize the input [mustache template](./examples/example-basic/mustache/template.tex), for different styling

## Tree node types

There are 4 node types

### `"type": "ulNode"`

#### JSON

```json
{
  "type": "textNode",
  "text": "Example Text"
}
```

#### `.tex` output

```tex
\item Example Text
```

### `"type": "olNode"`

#### JSON

```json
{
  "type": "olNode",
  "text": "My Enumerate",
  "children": [
    {
      "type": "textNode",
      "text": "Example Item"
    }
  ]
}
```

#### `.tex` output

```tex
\begin{enumerate}
  \item Example Item
\end{enumerate}
```

### `"type": "textNode"`

#### JSON

```json
{
  "type": "ulNode",
  "text": "My Itemize",
  "children": [
    {
      "type": "textNode",
      "text": "Example Item"
    }
  ]
}
```

#### `.tex` output

```tex
\begin{itemize}
  \item Example Item
\end{itemize}
```

### Notes

Note that the entire todo context is wrapped in, so rendering `textNode` outside of scope of `olNode`, or `ulNode` isn't a problem

```tex
\section{Some section name}
\begin{itemize}
% contents
\end{itemize}
```

## Templating notes

`todolist-json2tex` uses [Mustache](https://mustache.github.io/mustache.5.html) templates under the hood. Templating context consists only of 3 basic variables

```tex
% retrieved from cli opt --title
{{title}}
% retrieved from cli opt --subtitle
{{subtitle}}
% transformed output from input JSON
{{todoContents}}
```

### Escaping values

The program doesn't perform any escaping/sanitizing of LaTeX output, reason for that is that template output isn't expected to be sent accross network unlike HTML for instance.

Speaking of HTML, by default `{{` [mustache]([Mustache](https://mustache.github.io/mustache.5.html)) notation escapes html, therefore JSON inputs containing symbols such as `>`, or `&` might not produced correct latex output, due to use of HTML escaping

Use of `{{{` (triple bracket) notation is recommended - as applied in [example template](./examples/example-basic/mustache/template.tex) - i.e

```tex
{{{todoContents}}}
```

Other than that, writing LaTeX into json should work - example

```json
{
  "type": "ulNode",
  "text": "Tesintg LaTeX",
  "children": [
    {
      "type": "textNode",
      "text": "$2 + 3 \\rightarrow 5$"
    }
  ]
}
```

Feel free to create GitHub issue in case of some encountered security risk

## More examples

### Attaching subtrees

Program can optionally attach 1 or more subtrees to the input JSON

Example - [`examples/example-subtree/subtree.json`](./examples/example-subtree/subtree.json)

Attaching following

```json
[
  {
    "attachTo": ["coding", "develop-feature", "refactor-code"],
    "subtrees": [
      {
        "type": "textNode",
        "text": "Refactor /lib"
      },
      {
        "type": "textNode",
        "text": "Refactor backend"
      },
      {
        "type": "textNode",
        "text": "Refactor UI"
      }
    ]
  }
]
```

to following [`todo.json`](./examples/example-subtree/todo.json)

Should produce following [pdf](./examples/example-subtree/pdf-output/todo.pdf)

```sh
./bin/todolist-json2tex \
  --title "My improved todo" \
  --subtitle "With attached subtree" \
  --mustache-template "./examples/example-subtree/mustache/template.tex" \
  --output "./examples/example-subtree/tex-output/todo.tex" \
  --attach-json-subtree "./examples/example-subtree/subtree.json" \
  --text-attachment-behaviour replace_itemize \
  ./examples/example-subtree/todo.json
pdflatex \
  -interaction=errorstopmode \
  -output-directory=./examples/example-subtree/pdf-output \
  ./examples/example-subtree/tex-output/todo.tex
```

#### Attaching to empty `ulNode`, or `olNode`

If `ulNode`, or `olNode` serves only for categoric purpose, rendering can be skipped with `skipIfNoChildren` boolean attribute

Following node will not render anything unless subtree is attached under `test-node`

```json
{
  "type": "ulNode",
  "alias": "test-node",
  "text": "Won't be rendered",
  "children": []
}
```

### Env-variable conditions

Nodes can be rendered conditionally based on env variables

Example syntax

```json
[
  {
    "type": "textNode",
    "text": "Condition true",
    "conditions": {
      "type": "envVariable",
      "envVariable": "TODOLIST_JSON2TEXT_EXAMPLE_CONDITION"
    }
  },
  {
    "type": "textNode",
    "text": "Condition false",
    "conditions": {
      "type": "notOperator",
      "condition": {
        "type": "envVariable",
        "envVariable": "TODOLIST_JSON2TEXT_EXAMPLE_CONDITION"
      }
    }
  }
]
```

Following example - [`examples/example-env-variables/todo.json`](./examples/example-env-variables/todo.json)

```sh
TODOLIST_JSON2TEX_EXAMPLE_CONDITION="true" ./bin/todolist-json2tex \
  --title "My todo" \
  --subtitle "With env flag" \
  --mustache-template "./examples/example-env-variables/mustache/template.tex" \
  --output "./examples/example-env-variables/tex-output/todo-true.tex" \
  ./examples/example-env-variables/todo.json
pdflatex \
  -interaction=errorstopmode \
  -output-directory=./examples/example-env-variables/pdf-output \
  ./examples/example-env-variables/tex-output/todo-true.tex
./bin/todolist-json2tex \
  --title "My todo" \
  --subtitle "Without env flag" \
  --mustache-template "./examples/example-env-variables/mustache/template.tex" \
  --output "./examples/example-env-variables/tex-output/todo-false.tex" \
  ./examples/example-env-variables/todo.json
pdflatex \
  -interaction=errorstopmode \
  -output-directory=./examples/example-env-variables/pdf-output \
  ./examples/example-env-variables/tex-output/todo-false.tex
```

should produce following 2 distinct outputs

* [`todo-true.pdf`](./examples/example-env-variables/pdf-output/todo-true.pdf)
* [`todo-false.pdf`](./examples/example-env-variables/pdf-output/todo-false.pdf)

#### Env-variable discovery

cli automatically discovers all env variables that start with `TODOLIST_JSON2TEX_` prefix

#### Values considered to be true

Anything other than `""` (empty string), `"0"`, `"false"`, or variable not being defined

#### Notation in Json

both notations - with/without prefix should work

```json
{
  "conditions": {
    "type": "envVariable",
    "envVariable": "TODOLIST_JSON2TEX_MY_CONDITION"
  }
}
```

```json
{
  "conditions": {
    "type": "envVariable",
    "envVariable": "MY_CONDITION"
  }
}
```

### `dayOfWeek` conditions

It's possible to define todo nodes depending on day of the week condition

Example

```json
{
  "type": "textNode",
  "text": "Task relevant only for working days",
  "conditions": {
    "type": "dayOfWeek",
    "allowedDays": {
      "Monday": true,
      "Tuesday": true,
      "Wednesday": true,
      "Thursday": true,
      "Friday": true
    }
  }
}
```

#### Testing / rendering ahead

You can utilize `--date` CLI option which accepts date in ISO format, i.e.

```sh
--date `date -I`
```

### Operators - and, or, not

Conditions support some basic logic operators

And

```json
{
  "conditions": {
    "type": "andOperator",
    "conditiotns": [
      {
        "type": "dayOfWeek",
        "allowedDays": {
          "Wednesday": true
        }
      },
      {
        "type": "envVariable",
        "envVariable": "VARIABLE_APPLICABLE_ON_WEDNESDAYS"
      },
    ]
  }
}
```

Or

```json
{
  "conditions": {
    "type": "orOperator",
    "conditiotns": [
      {
        "type": "dayOfWeek",
        "allowedDays": {
          "Wednesday": true
        }
      },
      {
        "type": "envVariable",
        "envVariable": "EITHER_THIS_VARIABLE_IS_SET_OR_ITS_WEDNESDAY"
      },
    ]
  }
}
```

Not

```json
{
  "type": "textNode",
  "text": "Condition false",
  "conditions": {
    "type": "notOperator",
    "condition": {
      "type": "envVariable",
      "envVariable": "NEGATED_VARIABLE"
    }
  }
}
```

## Bash completions

Can be generated through steps described by [`optparse-applicative`](https://hackage.haskell.org/package/optparse-applicative-0.18.1.0#bash-zsh-and-fish-completions) package (used for parsing)

```sh
source <(todolist-json2tex --bash-completion-script `which todolist-json2tex`)
```
