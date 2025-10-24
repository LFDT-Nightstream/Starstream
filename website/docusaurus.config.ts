import type * as Preset from "@docusaurus/preset-classic";
import type { Config } from "@docusaurus/types";
import { themes as prismThemes } from "prism-react-renderer";
import rehypeKatex from "rehype-katex";
import remarkMath from "remark-math";

const config: Config = {
  title: "Starstream",

  // Future flags, see https://docusaurus.io/docs/api/docusaurus-config#future
  future: {
    v4: true, // Improve compatibility with the upcoming Docusaurus v4
  },

  // Set the production url of your site here
  url: "https://lfdt-nightstream.github.io/",
  // Set the /<baseUrl>/ pathname under which your site is served
  // For GitHub pages deployment, it is often '/<projectName>/'
  baseUrl: "/Starstream/",

  // GitHub pages deployment config.
  // If you aren't using GitHub pages, you don't need these.
  organizationName: "LFDT-Nightstream", // Usually your GitHub org/user name.
  projectName: "Starstream", // Usually your repo name.

  onBrokenLinks: "throw",
  onBrokenAnchors: "throw",

  // Even if you don't use internationalization, you can use this field to set
  // useful metadata like html lang. For example, if your site is Chinese, you
  // may want to replace "en" with "zh-Hans".
  i18n: {
    defaultLocale: "en",
    locales: ["en"],
  },

  presets: [
    [
      "classic",
      {
        docs: {
          routeBasePath: "/",
          path: "../docs",
          remarkPlugins: [remarkMath],
          rehypePlugins: [rehypeKatex],
          editUrl:
            "https://github.com/LFDT-Nightstream/Starstream/tree/main/docs/",
        },
        theme: {
          customCss: "./src/css/custom.css",
        },
      } satisfies Preset.Options,
    ],
  ],

  staticDirectories: ["static", "node_modules/katex/dist"],

  stylesheets: ["./katex.min.css"],

  themeConfig: {
    colorMode: {
      respectPrefersColorScheme: true,
    },
    navbar: {
      title: "Starstream",
      items: [
        {
          href: "https://github.com/LFDT-Nightstream/Starstream",
          label: "GitHub",
        },
        {
          href: "https://discord.com/invite/9eZaheySZE",
          label: "Discord",
        },
        {
          type: "docSidebar",
          sidebarId: "defaultSidebar",
          position: "left",
          label: "Documentation",
        },
        {
          to: "/sandbox/",
          label: "Sandbox",
        },
      ],
    },
    footer: {
      style: "dark",
      copyright: `Copyright © ${new Date().getFullYear()} Paima Studios LTD.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
    },
  } satisfies Preset.ThemeConfig,

  plugins: [
    /*
      Note: This plugin makes some assumptions about the structure of your docs:
      - The docs directory contains your documentation
      - Each category has a _category_.yml file that contains the category metadata
      - Each page has frontmatter metadata
      - For top-level Markdown pages, there is a sidebar_position field in the metadata
    */
    "docusaurus-plugin-generate-llms-txt",
  ],
};

export default config;
