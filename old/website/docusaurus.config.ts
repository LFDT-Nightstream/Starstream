import { themes as prismThemes } from "prism-react-renderer";
import type { Config } from "@docusaurus/types";
import type * as Preset from "@docusaurus/preset-classic";

// This runs in Node.js - Don't use client-side code here (browser APIs, JSX...)

const config: Config = {
  title: "Starstream",
  tagline:
    "A blockchain and zero-knowledge VM concept built around delimited continuations",
  //favicon: 'img/favicon.ico',

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
  onBrokenMarkdownLinks: "warn",

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
          sidebarPath: "./sidebars.ts",
          editUrl:
            "https://github.com/LFDT-Nightstream/Starstream/tree/main/website/",
        },
        blog: {
          showReadingTime: false,
          feedOptions: {
            type: ["rss", "atom"],
            xslt: true,
          },
          // Please change this to your repo.
          // Remove this to remove the "edit this page" links.
          editUrl:
            "https://github.com/LFDT-Nightstream/Starstream/tree/main/website/",
          // Useful options to enforce blogging best practices
          onInlineTags: "warn",
          onInlineAuthors: "warn",
          onUntruncatedBlogPosts: "warn",
        },
        theme: {
          customCss: "./src/css/custom.css",
        },
      } satisfies Preset.Options,
    ],
  ],

  themeConfig: {
    // Replace with your project's social card
    //image: 'img/docusaurus-social-card.jpg',
    navbar: {
      title: "Starstream",
      /*logo: {
        alt: 'My Site Logo',
        src: 'img/logo.svg',
      },*/
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
          sidebarId: "tutorialSidebar",
          position: "left",
          label: "Documentation",
        },
        {
          to: "/sandbox/",
          label: "Sandbox",
        },
        {
          to: "/blog/",
          label: "Posts",
        },
      ],
    },
    footer: {
      style: "dark",
      /*links: [
        {
          title: 'External',
          items: [
            {
              label: 'Discord',
              href: 'https://discord.com/invite/9eZaheySZE',
            },
            {
              label: 'GitHub',
              href: 'https://github.com/LFDT-Nightstream/Starstream',
            },
          ],
        },
      ],*/
      copyright: `© ${new Date().getFullYear()} Paima Studios LTD.`,
    },
    prism: {
      theme: prismThemes.github,
      darkTheme: prismThemes.dracula,
    },
  } satisfies Preset.ThemeConfig,
};

export default config;
