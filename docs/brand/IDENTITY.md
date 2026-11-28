# BARUCH · visual identity: Rinko

**Mark:** Kata-igeta ni bun 傾井桁に文. **Ink:** Rinkō 燐光 (phosphorescence).
Part of the sumi family: brushed mark, faint three-level AMR ground in dashed strokes, carved seal (B) lower right.

The igeta is the Japanese crest of the timber frame round a well, and the Japanese name of the `#` sign. Here it leans
like a typed `#`, the heading mark of Markdown, and holds 文 (text, writing) in its centre square, inside a brushed sumi
ring. The ink is the phosphor green of an old terminal: a cool green-white paper by day, terminal black by night.

The ring and 文 take the ink colour; the igeta and the seal take the crest colour.

## In this repository

Applied 2026-11-28 (night `identity-rinko`): `docs/public/{logo,favicon,icons}` holds the logos, favicons, web manifest
and feature icons (day assets under the plain name, night ones under `-dark` or in `icons/dark/`);
`docs/.vitepress/theme/` the theme (`custom.css`, `index.ts`); `config.mts` the favicon and manifest links, the theme
colours and the navbar logo; `docs/index.md` the hero mark and the feature icons; `README.md` the mark. This folder keeps
the record (this page, the tokens, the lockup and the social preview); VitePress skips it (`srcExclude`). The paths
below are relative to `docs/`.

| File | Use |
|---|---|
| `public/logo/baruch-mark.svg`, `baruch-mark-dark.svg` | Full mark (AMR ground and seal), transparent: hero image, README |
| `public/logo/baruch-glyph.svg`, `baruch-glyph-dark.svg` | Ring, crest and 文, cropped, for small sizes: nav bar |
| `public/favicon/favicon.svg` | Adaptive favicon (switches with `prefers-color-scheme`) |
| `public/favicon/favicon.ico`, `favicon-16/32/48.png` | Fallback favicons |
| `public/favicon/apple-touch-icon.png`, `favicon-192.png`, `favicon-512.png` | Home-screen and manifest icons |
| `public/favicon/site.webmanifest` | Web app manifest (icon paths relative to it, so the `/BARUCH/` base holds) |
| `public/icons/{light,dark}/*.svg` | The ten feature icons drawn from the igeta: CommonMark reader, document tree, writer, streaming writer, semantic round trip, spec examples, hookable extensions, containers and front matter, math and directives, licences |
| `.vitepress/theme/custom.css`, `index.ts` | Theme override |
| `brand/tokens.json` | Colour tokens with contrast ratios (sumi family format) |
| `brand/logo/baruch-lockup-{day,night}.svg`, `-1200.png` | Mark and wordmark, for slides: the SVG wordmark is live text (Shippori Mincho and Inter), the PNGs need no font |
| `brand/social/baruch-social-1280x640.{svg,png}` | GitHub social preview (repository settings) |

The home page uses three icons (round trip, spec examples, hookable extensions); a new home-page feature takes one of the
others, not an emoji. Not copied from the bundle: the `mono/` icons (`currentColor`, crest in `--baruch-crest`) and
their sprite, the mark's `-tile` and `-bare` variants and the PNG marks; `custom.css` defines `--baruch-crest` and
`--baruch-mesh` for when they are needed.

## Palette

| Token | Day | Night | Use | VitePress |
|---|---|---|---|---|
| ground | `#eef4ec` | `#060c07` | page | `--vp-c-bg`, theme-color |
| raised | `#f5f9f4` | `#0c140d` | cards, code, favicon tile | `--vp-c-bg-alt` (day), `-elv`, `-soft`, alt buttons |
| line | `#d3e0cf` | `#18281a` | dividers | `--vp-c-divider`, alt-button hover |
| ink | `#14231a` | `#b8f5b0` | text, ring, 文 | `--vp-c-text-1`, hero name |
| muted | `#46594b` | `#7fbf78` | secondary text | `--vp-c-text-2` |
| mesh | `#5f7364` | `#4f8a4a` | AMR ground, decoration only | `--vp-c-border`, `--baruch-mesh` |
| acc | `#1f8a3b` | `#39ff6a` | the igeta crest and the seal | `--baruch-crest`; night `--vp-c-brand-2/3`, night button |
| link | `#187030` | `#6dff92` | links and buttons | `--vp-c-brand-1`; day `--vp-c-brand-3`, day button |

Buttons: day `#187030` with white text (6.2:1), hover `#10552a`; night `#39ff6a` with `#060c07` text (14.8:1), hover
`#2bd457` (10.0:1). Night `--vp-c-bg-alt` is `#030703`, a step below the ground, as the bundle's theme sets it.

Fonts: Shippori Mincho 800 (display: hero name, nav title), Inter (body), JetBrains Mono (code), from Google Fonts.

## Usage rules

- Day assets on light grounds, night (`-dark`) assets on dark ones; the favicon SVG switches by itself.
- The glyph for small sizes (nav bar, anything near favicon size); the full mark, with its AMR ground and seal, where
  it has room (hero, README).
- Link text: 5.5:1 by day, 15.4:1 by night. The bright crest green `#39ff6a` is for the crest, the seal and the night
  button (with dark text), never for body text on day paper; by day the crest `#1f8a3b` (3.95:1) is never text either.
- `mesh` is decoration only.
- No glow, gradient or shadow behind the mark (`--vp-home-hero-image-background-image: none`).
- The SVG paths are generated geometry: never edit them by hand, regenerate the kit.

## Deviations from the bundle

- `icons/{light,dark}/containers-front-matter.svg` and `math-directives.svg` had an unescaped `&` in their `aria-label`,
  which made them malformed XML (browsers refuse them as images); it is `&amp;` now. The same `&` is in the bundle's
  `mono/` icons and in the `<title>` of its sprite: the generator should escape it.
- The bundle's `custom.css` sets `--vp-c-brand-2` to the crest `#1f8a3b` by day; VitePress paints link hovers with it,
  which would put the crest on day paper as text at 3.95:1. Here it is `#10552a` (the button hover, 8.0:1).
- Added to the bundle's theme: brand-button borders and hover/active text (VitePress's default white hover text is
  1.9:1 on the night green), and alt buttons on the identity's grounds, as ELISHA.
