import React from "react";
import { createRoot } from "react-dom/client";
// Self-hosted IBM Plex fonts (offline, no Google Fonts / CDN fetch). The design
// uses 400/500/600/700 sans and 400/500 mono; the 550/650 weights referenced in
// the design tokens are not real font weights and fall back to the nearest one.
import "@fontsource/ibm-plex-sans/400.css";
import "@fontsource/ibm-plex-sans/500.css";
import "@fontsource/ibm-plex-sans/600.css";
import "@fontsource/ibm-plex-sans/700.css";
import "@fontsource/ibm-plex-mono/400.css";
import "@fontsource/ibm-plex-mono/500.css";
import { App } from "./App";
import "./styles.css";

createRoot(document.getElementById("root") as HTMLElement).render(
  <React.StrictMode>
    <App />
  </React.StrictMode>
);
