import { defineConfig } from "vite";
import react from "@vitejs/plugin-react";

// Production is served from https://choxos.github.io/mlumr/app/, so built asset
// URLs must be prefixed with that subpath; without it they resolve to the
// domain root (https://choxos.github.io/assets/...) and 404. Dev and preview
// stay at "/" for convenience. The deploy workflow also passes --base as a
// belt-and-suspenders guard.
export default defineConfig(({ command }) => ({
  base: command === "build" ? "/mlumr/app/" : "/",
  plugins: [react()],
  server: {
    headers: {
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Embedder-Policy": "require-corp"
    }
  },
  preview: {
    headers: {
      "Cross-Origin-Opener-Policy": "same-origin",
      "Cross-Origin-Embedder-Policy": "require-corp"
    }
  }
}));
