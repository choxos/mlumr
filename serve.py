"""Serve site/ with the headers that make the page cross-origin isolated.

Cross-origin isolation gives webR its SharedArrayBuffer channel and the Stan
bridge its shared buffer, so fits run in parallel TinyStan workers and Ctrl+C
in the console interrupts them. Every response asks the browser to revalidate,
so a rebuilt site is picked up on the next reload.
Usage: python3 serve.py [port]   (default port 8080)
"""
import functools
import http.server
import os
import sys


class Handler(http.server.SimpleHTTPRequestHandler):
    def end_headers(self):
        self.send_header("Cross-Origin-Opener-Policy", "same-origin")
        self.send_header("Cross-Origin-Embedder-Policy", "require-corp")
        self.send_header("Cross-Origin-Resource-Policy", "same-origin")
        self.send_header("Cache-Control", "no-cache")
        super().end_headers()


Handler.extensions_map[".wasm"] = "application/wasm"
port = int(sys.argv[1]) if len(sys.argv) > 1 else 8080
root = os.path.join(os.path.dirname(os.path.abspath(__file__)), "site")
http.server.ThreadingHTTPServer(
    ("127.0.0.1", port), functools.partial(Handler, directory=root)
).serve_forever()
