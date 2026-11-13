import elmPlugin from "bun-plugin-elm";
import tailwindPlugin from "bun-plugin-tailwind";

const result = await Bun.build({
  entrypoints: ["index.html"],
  outdir: "./dist",
  plugins: [elmPlugin, tailwindPlugin],
  minify: true,
});

await Bun.$`cp logo.png dist/`;

console.log(result);
