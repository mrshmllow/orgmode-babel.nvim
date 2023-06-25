# orgmode-babel.nvim

An experimental plugin that evaluates and tangles code blocks in
[nvim-orgmode](https://github.com/nvim-orgmode/orgmode) using
[babel](https://orgmode.org/worg/org-contrib/babel/) itself.

It uses `emacs` under the hood for perfect compatibility, but does not
require you to add anything extra to your `init.el`.

[Demonstration](https://github.com/mrshmllow/BetterRecipeBook/assets/40532058/b1ca7384-4bb3-47d8-9148-b85f3a2ea54a)

## Requirements

-   [nvim-orgmode](https://github.com/nvim-orgmode/orgmode)
-   [nvim-treesitter](https://github.com/nvim-treesitter/nvim-treesitter)
-   Neovim v0.9.0 or later
-   A working `emacs` installation (does not require configuration)

## Setup

### lazy.nvim

``` lua
{
  "mrshmllow/orgmode-babel.nvim",
  dependencies = {
    "nvim-orgmode/orgmode",
    "nvim-treesitter/nvim-treesitter"
  },
  cmd = { "OrgExecute", "OrgTangle" },
  opts = {
    -- by default, none are enabled
    langs = { "python", "lua", ... }
  }
},
```

## Usage

All commands accept a `!` to skip confirmation.

### `:OrgE[xecute]`

Evaluates every block in buffer.

See [Working with Source
Code](https://orgmode.org/manual/Working-with-Source-Code.html) in the
org manual.

### `:{range}OrgE[xecute]`

Evaluate every block in range.

### `:OrgE[xecute] [name]`

Evaluate `[name]` block.

### `:OrgT[angle]`

Tangles whole file.

See [Extracting Source
Code](https://orgmode.org/manual/Extracting-Source-Code.html) in the org
manual.

### `:{range}OrgT[angle]`

Tangles all blocks in range. If the range is NOT `%`, the tangled file
will likely only contain the contents of the last block, which is
expected behaviour.

### `:OrgT[angle] [name]`

Tangles `[name]` block.

## Advanced Configuration

### Adding extra org-mode languages

Your emacs `init.el` will be sourced during execution of `:OrgExecute`
and `:OrgTangle`, so packages you install there that provide extra babel
languages will be available!

Follow the package\'s installation steps, and if they tell you to
include it in `org-babel-load-languages`, additionally make sure that
you include it in `opts.langs`.

1.  Example

    As an example, lets add
    [ob-mermaid](https://github.com/arnm/ob-mermaid) for mermaid
    functionality in `orgmode-babel.nvim`!

    First, lets create a `~/.emacs.d/init.el`{.verbatim}.

        ; ~/.emacs.d/init.el

        ; Add the melpa package manager
        (require 'package)
        (add-to-list 'package-archives
          '("melpa" . "https://melpa.org/packages/") t)
        (package-initialize)

        ; Install ob-mermaid
        (unless (package-installed-p 'ob-mermaid)
          (package-install 'ob-mermaid))

    Then, in our plugin configuration, we can add `mermaid` to our
    `opts.langs`.

    ``` lua
    {
      "mrshmllow/orgmode-babel.nvim",
      ...
      opts = {
        langs = { ..., "mermaid" }
      }
    },
    ```
