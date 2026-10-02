// Cross-origin isolation for hosts that cannot send the headers themselves
// (GitHub Pages). The page and its workers need COOP and COEP for webR's
// shared-memory channel and the Stan bridge, so this service worker adds them,
// but only to documents and worker scripts: every other request goes straight
// to the network. (Adding them to every response, as general-purpose
// versions of this worker do, stalled reloads here: webR's worker reads
// packages with synchronous requests.)
self.addEventListener('install', () => self.skipWaiting());
self.addEventListener('activate', (event) => event.waitUntil(self.clients.claim()));
self.addEventListener('fetch', (event) => {
  const req = event.request;
  if (req.mode !== 'navigate' && req.destination !== 'worker' && req.destination !== 'sharedworker') return;
  event.respondWith(fetch(req).then((res) => {
    if (res.status === 0) return res;
    const headers = new Headers(res.headers);
    headers.set('Cross-Origin-Embedder-Policy', 'require-corp');
    headers.set('Cross-Origin-Opener-Policy', 'same-origin');
    return new Response(res.body, { status: res.status, statusText: res.statusText, headers });
  }));
});
