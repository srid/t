See README.md for general project information.

## Coding Guidelines

- Haskell guidelines
    - Use `LambdaCase` wherever possible
    - If a new top-level function is used only in another function, use `where` to define it locally.
    - Fix all GHC warnings.
- Haskell library guidelines
    - We use `relude` as prelude. Thus, prefer relude's functions over Prelude's.
    - We use HTMX for interactive web UI with server-side rendering. Use the `htmx-lucid` library for HTMX attributes in Haskell.
      - HTMX attributes: `hxPost_`, `hxGet_`, `hxTarget_`, `hxSwap_`, etc.
      - Prefer partial page updates over full page reloads
    - We use Tailwind CSS via CDN for styling. Use utility classes for responsive, modern design.
      - Load via: `script_ [src_ "https://cdn.tailwindcss.com"] ("" :: Text)`
      - Prefer utility classes over custom CSS
- Project-specific guidelines
    - After making code changes, run `pre-commit run -a` to format the code and check for linting issues (this runs fourmolu and hlint).
- Refactoring & design
    - Prefer Juval Löwy's Volatility-Based Decomposition principle. For details, see https://www.informit.com/articles/article.aspx?p=2995357&seqNum=2


## Testing changes

- When possible, just rely on `cabal build` (or HLS). Running the application (via `cabal run` or `nix run`) should reserved for cases where it is necessary. I prefer a more rapid dev-feedback cycle. Remember, with Haskell when code compiles it usually runs correctly as well.
- Note that the terminal has direnv (nix develop) activated usually. If you are not in direnv, you can prefix it with `nix develop -c`.

## Your report

- Try to be concise in explaining changes.
- DO NOT AUTO-COMMIT CHANGES. That's my responsibility. But feel free to give me the recommended commit message that I can copy-paste to `git commit -m "..."`.
