import { withMermaid } from 'vitepress-plugin-mermaid'

export default withMermaid({
  title: 'BARUCH',
  description: 'Bidirectional AST Reader and Unparser for CommonMark, Hookable: a pure Fortran Markdown parser and emitter, with opt-in MyST/VitePress extensions',
  base: '/BARUCH/',
  srcExclude: ['ford.md'],

  markdown: {
    math: true,
    languages: ['fortran-free-form', 'fortran-fixed-form'],
    languageAlias: {
      fortran: 'fortran-free-form',
      f90: 'fortran-free-form',
      f03: 'fortran-free-form',
      f08: 'fortran-free-form',
    },
  },

  themeConfig: {
    nav: [
      { text: 'Home',   link: '/' },
      { text: 'Guide',  link: '/guide/' },
      { text: 'GitHub', link: 'https://github.com/szaghi/BARUCH' },
    ],

    // Add your guide pages here.
    sidebar: {
      '/guide/': [
        {
          text: 'Introduction',
          items: [
            { text: 'About',        link: '/guide/' },
            { text: 'Installation', link: '/guide/installation' },
            { text: 'Quick Start',  link: '/guide/quickstart' },
          ],
        },
        {
          text: 'Project',
          items: [
            { text: 'Contributing', link: '/guide/contributing' },
            { text: 'Changelog',    link: '/guide/changelog' },
          ],
        },
      ],
    },

    socialLinks: [
      { icon: 'github', link: 'https://github.com/szaghi/BARUCH' },
    ],

    search: {
      provider: 'local',
    },

    footer: {
      message: 'Released under your choice of the GPL v3, BSD-2-Clause, BSD-3-Clause or MIT license.',
      copyright: 'Copyright © 2026 Stefano Zaghi',
    },
  },

  mermaid: {},

  vite: {
    // Build with an explicit modern JS target so the docs compile regardless of
    // which mermaid/vitepress/esbuild versions npm resolves. Vite's default
    // es2020 target forces esbuild to down-level modern syntax (e.g. the
    // destructuring mermaid 11.16+ emits), which it refuses to do and the build
    // dies. es2022 needs no lowering and is within VitePress's browser floor.
    build: {
      target: 'es2022',
    },
    // The dev server pre-bundles the dependencies with its own esbuild target (es2020), ignoring build.target:
    // without this, `vitepress dev` dies on the destructuring of mark.js and @vueuse. Bundling mermaid whole also
    // covers the CJS dependencies vitepress-plugin-mermaid does not list (as SETH's configuration).
    optimizeDeps: {
      include: ['mermaid'],
      esbuildOptions: {
        target: 'es2022',
      },
    },
  },
})
