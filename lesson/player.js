(() => {
  // packages/player/dist/clock.js
  var AudioClock = class {
    media;
    constructor(media) {
      this.media = media;
    }
    /** Current time, rounded to 10 ms (matches the exemplar's resolution). */
    get t() {
      return Math.round(this.media.currentTime * 100) / 100;
    }
    get playing() {
      return !this.media.paused;
    }
    get duration() {
      return Number.isFinite(this.media.duration) ? this.media.duration : 0;
    }
    async play() {
      try {
        await this.media.play();
        return true;
      } catch (error) {
        console.warn("audio play() rejected:", error);
        return false;
      }
    }
    pause() {
      this.media.pause();
    }
    seek(t2) {
      this.media.currentTime = Math.max(0, t2);
    }
    on(event, handler) {
      this.media.addEventListener(event, handler);
    }
  };

  // node_modules/.pnpm/@preact+signals-core@1.14.4/node_modules/@preact/signals-core/dist/signals-core.module.js
  var i = /* @__PURE__ */ Symbol.for("preact-signals");
  function t() {
    if (!(v > 1)) {
      var i3, t2 = false;
      !(function() {
        var i4 = c;
        c = void 0;
        while (void 0 !== i4) {
          var t3 = i4.S;
          if (t3.v === i4.v) {
            for (var n2 = t3.t; void 0 !== n2; n2 = n2.x) if (n2.i === i4.i) n2.i = t3.i;
          }
          i4 = i4.o;
        }
      })();
      while (void 0 !== h) {
        var n = h;
        h = void 0;
        s++;
        while (void 0 !== n) {
          var r2 = n.u;
          n.u = void 0;
          n.f &= -3;
          if (!(8 & n.f) && w(n)) try {
            n.c();
          } catch (n2) {
            if (!t2) {
              i3 = n2;
              t2 = true;
            }
          }
          n = r2;
        }
      }
      s = 0;
      v--;
      if (t2) throw i3;
    } else v--;
  }
  var r;
  var o = void 0;
  function f(i3) {
    var t2 = o, n = r;
    o = void 0;
    r = void 0;
    try {
      return i3();
    } finally {
      o = t2;
      r = n;
    }
  }
  var h = void 0;
  var v = 0;
  var s = 0;
  var e = 0;
  var c = void 0;
  var d = 0;
  function a(i3) {
    if (void 0 !== o) {
      var t2 = i3.n;
      if (void 0 === t2 || t2.t !== o) {
        t2 = { i: 0, S: i3, p: o.s, n: void 0, t: o, e: void 0, x: void 0, r: t2 };
        if (void 0 !== o.s) o.s.n = t2;
        o.s = t2;
        i3.n = t2;
        if (32 & o.f) i3.S(t2);
        return t2;
      } else if (-1 === t2.i) {
        t2.i = 0;
        if (void 0 !== t2.n) {
          t2.n.p = t2.p;
          if (void 0 !== t2.p) t2.p.n = t2.n;
          t2.p = o.s;
          t2.n = void 0;
          o.s.n = t2;
          o.s = t2;
        }
        return t2;
      }
    }
  }
  function l(i3, t2) {
    this.v = i3;
    this.i = 0;
    this.n = void 0;
    this.t = void 0;
    this.l = 0;
    this.W = null == t2 ? void 0 : t2.watched;
    this.Z = null == t2 ? void 0 : t2.unwatched;
    this.name = null == t2 ? void 0 : t2.name;
  }
  l.prototype.brand = i;
  l.prototype.h = function() {
    return true;
  };
  l.prototype.S = function(i3) {
    var t2 = this, n = this.t;
    if (n !== i3 && void 0 === i3.e) {
      i3.x = n;
      this.t = i3;
      if (void 0 !== n) n.e = i3;
      else f(function() {
        var i4;
        null == (i4 = t2.W) || i4.call(t2);
      });
    }
  };
  l.prototype.U = function(i3) {
    var t2 = this;
    if (void 0 !== this.t) {
      var n = i3.e, r2 = i3.x;
      if (void 0 !== n) {
        n.x = r2;
        i3.e = void 0;
      }
      if (void 0 !== r2) {
        r2.e = n;
        i3.x = void 0;
      }
      if (i3 === this.t) {
        this.t = r2;
        if (void 0 === r2) f(function() {
          var i4;
          null == (i4 = t2.Z) || i4.call(t2);
        });
      }
    }
  };
  l.prototype.subscribe = function(i3) {
    var t2 = this;
    return j(function() {
      var n = t2.value;
      f(function() {
        return i3(n);
      });
    }, { name: "sub" });
  };
  l.prototype.valueOf = function() {
    return this.value;
  };
  l.prototype.toString = function() {
    return this.value + "";
  };
  l.prototype.toJSON = function() {
    return this.value;
  };
  l.prototype.peek = function() {
    var i3 = this;
    return f(function() {
      return i3.value;
    });
  };
  Object.defineProperty(l.prototype, "value", { get: function() {
    var i3 = a(this);
    if (void 0 !== i3) i3.i = this.i;
    return this.v;
  }, set: function(i3) {
    if (i3 !== this.v) {
      if (s > 100) throw new Error("Cycle detected");
      !(function(i4) {
        if (0 !== v && 0 === s) {
          if (i4.l !== e) {
            i4.l = e;
            c = { S: i4, v: i4.v, i: i4.i, o: c };
          }
        }
      })(this);
      this.v = i3;
      this.i++;
      d++;
      v++;
      try {
        for (var n = this.t; void 0 !== n; n = n.x) n.t.N();
      } finally {
        t();
      }
    }
  } });
  function y(i3, t2) {
    return new l(i3, t2);
  }
  function w(i3) {
    for (var t2 = i3.s; void 0 !== t2; t2 = t2.n) if (t2.S.i !== t2.i || !t2.S.h() || t2.S.i !== t2.i) return true;
    return false;
  }
  function _(i3) {
    for (var t2 = i3.s; void 0 !== t2; t2 = t2.n) {
      var n = t2.S.n;
      if (void 0 !== n) t2.r = n;
      t2.S.n = t2;
      t2.i = -1;
      if (void 0 === t2.n) {
        i3.s = t2;
        break;
      }
    }
  }
  function b(i3) {
    var t2 = i3.s, n = void 0;
    while (void 0 !== t2) {
      var r2 = t2.p;
      if (-1 === t2.i) {
        t2.S.U(t2);
        if (void 0 !== r2) r2.n = t2.n;
        if (void 0 !== t2.n) t2.n.p = r2;
      } else n = t2;
      t2.S.n = t2.r;
      if (void 0 !== t2.r) t2.r = void 0;
      t2 = r2;
    }
    i3.s = n;
  }
  function p(i3, t2) {
    l.call(this, void 0, t2);
    this.x = i3;
    this.s = void 0;
    this.g = d - 1;
    this.f = 4;
  }
  p.prototype = new l();
  p.prototype.h = function() {
    this.f &= -3;
    if (1 & this.f) return false;
    if (32 == (36 & this.f)) return true;
    this.f &= -5;
    if (this.g === d) return true;
    this.g = d;
    this.f |= 1;
    if (this.i > 0 && !w(this)) {
      this.f &= -2;
      return true;
    }
    var i3 = o;
    try {
      _(this);
      o = this;
      var t2 = this.x();
      if (16 & this.f || this.v !== t2 || 0 === this.i) {
        this.v = t2;
        this.f &= -17;
        this.i++;
      }
    } catch (i4) {
      this.v = i4;
      this.f |= 16;
      this.i++;
    }
    o = i3;
    b(this);
    this.f &= -2;
    return true;
  };
  p.prototype.S = function(i3) {
    if (void 0 === this.t) {
      this.f |= 36;
      for (var t2 = this.s; void 0 !== t2; t2 = t2.n) t2.S.S(t2);
    }
    l.prototype.S.call(this, i3);
  };
  p.prototype.U = function(i3) {
    if (void 0 !== this.t) {
      l.prototype.U.call(this, i3);
      if (void 0 === this.t) {
        this.f &= -33;
        for (var t2 = this.s; void 0 !== t2; t2 = t2.n) t2.S.U(t2);
      }
    }
  };
  p.prototype.N = function() {
    if (!(2 & this.f)) {
      this.f |= 6;
      for (var i3 = this.t; void 0 !== i3; i3 = i3.x) i3.t.N();
    }
  };
  Object.defineProperty(p.prototype, "value", { get: function() {
    if (1 & this.f) throw new Error("Cycle detected");
    var i3 = a(this);
    this.h();
    if (void 0 !== i3) i3.i = this.i;
    if (16 & this.f) throw this.v;
    return this.v;
  } });
  function S(i3) {
    var n = i3.m;
    i3.m = void 0;
    if ("function" == typeof n) {
      v++;
      var r2 = o;
      o = void 0;
      try {
        n();
      } catch (t2) {
        i3.f &= -2;
        i3.f |= 8;
        m(i3);
        throw t2;
      } finally {
        o = r2;
        t();
      }
    }
  }
  function m(i3) {
    for (var t2 = i3.s; void 0 !== t2; t2 = t2.n) t2.S.U(t2);
    i3.x = void 0;
    i3.s = void 0;
    S(i3);
  }
  function x(i3) {
    if (o !== this) throw new Error("Out-of-order effect");
    b(this);
    o = i3;
    this.f &= -2;
    if (8 & this.f) m(this);
    t();
  }
  function E(i3, t2) {
    this.x = i3;
    this.m = void 0;
    this.s = void 0;
    this.u = void 0;
    this.f = 32;
    this.name = null == t2 ? void 0 : t2.name;
    if (r) r.push(this);
  }
  E.prototype.c = function() {
    var i3 = this.S();
    try {
      if (8 & this.f) return;
      if (void 0 === this.x) return;
      var t2 = this.x();
      if ("function" == typeof t2) this.m = t2;
    } finally {
      i3();
    }
  };
  E.prototype.S = function() {
    if (1 & this.f) throw new Error("Cycle detected");
    this.f |= 1;
    this.f &= -9;
    S(this);
    _(this);
    v++;
    var i3 = o;
    o = this;
    return x.bind(this, i3);
  };
  E.prototype.N = function() {
    if (!(2 & this.f)) {
      this.f |= 2;
      this.u = h;
      h = this;
    }
  };
  E.prototype.d = function() {
    this.f |= 8;
    if (!(1 & this.f)) m(this);
  };
  E.prototype.dispose = function() {
    this.d();
  };
  function j(i3, t2) {
    var n = new E(i3, t2);
    try {
      n.c();
    } catch (i4) {
      n.d();
      throw i4;
    }
    var r2 = n.d.bind(n);
    r2[Symbol.dispose] = r2;
    return r2;
  }

  // packages/player/dist/store.js
  var StateStore = class {
    signals = /* @__PURE__ */ new Map();
    plain = {};
    meta = /* @__PURE__ */ new Map();
    constructor(schema2) {
      for (const [key, spec] of Object.entries(schema2)) {
        const v2 = clone(spec.default);
        this.signals.set(key, y(v2));
        this.plain[key] = v2;
        this.meta.set(key, { holdT: -Infinity, touchT: -Infinity, touchedEver: false, modified: false, dragging: false });
      }
    }
    keys() {
      return [...this.signals.keys()];
    }
    signal(key) {
      const s2 = this.signals.get(key);
      if (!s2)
        throw new Error(`unknown parameter: ${key}`);
      return s2;
    }
    /** Write a displayed value; updates the signal + mirror only if it changed. */
    set(key, value) {
      const cur = this.plain[key];
      if (cur !== void 0 && valuesEqual(cur, value))
        return;
      const stored = writeInto(cur, value);
      this.plain[key] = stored;
      this.signals.get(key).value = stored;
    }
    /** Record a user interaction on a parameter (for the Reconciler). */
    touch(key, value, t2) {
      const m2 = this.meta.get(key);
      if (!m2)
        return;
      m2.userValue = clone(value);
      m2.holdT = t2;
      m2.touchT = t2;
      m2.touchedEver = true;
      m2.modified = true;
    }
    setDragging(key, dragging) {
      const m2 = this.meta.get(key);
      if (m2)
        m2.dragging = dragging;
    }
    /** Freeze the currently displayed value for a paused interaction. */
    freezeInteraction(key) {
      const m2 = this.meta.get(key);
      if (m2)
        m2.userValue = clone(this.plain[key]);
    }
    resetInteraction(key) {
      const m2 = this.meta.get(key);
      if (!m2)
        return;
      m2.userValue = void 0;
      m2.holdT = -Infinity;
      m2.touchT = -Infinity;
      m2.touchedEver = false;
      m2.modified = false;
      m2.dragging = false;
    }
    /** Clear all interaction state (on seek: rejoin the narration). */
    resetInteractions() {
      for (const m2 of this.meta.values()) {
        m2.userValue = void 0;
        m2.holdT = -Infinity;
        m2.touchT = -Infinity;
        m2.touchedEver = false;
        m2.modified = false;
        m2.dragging = false;
      }
    }
  };
  function valuesEqual(a2, b2) {
    if (a2 === b2)
      return true;
    if (Array.isArray(a2) && Array.isArray(b2))
      return a2.length === b2.length && a2.every((x2, i3) => x2 === b2[i3]);
    if (isOrbit(a2) && isOrbit(b2))
      return a2.distance === b2.distance && a2.azimuth === b2.azimuth && a2.elevation === b2.elevation && a2.target.every((x2, i3) => x2 === b2.target[i3]);
    return false;
  }
  function writeInto(cur, value) {
    if (Array.isArray(value)) {
      if (Array.isArray(cur) && cur.length === value.length) {
        for (let i3 = 0; i3 < value.length; i3++)
          cur[i3] = value[i3];
        return cur;
      }
      return value.slice();
    }
    if (isOrbit(value)) {
      if (isOrbit(cur)) {
        cur.target[0] = value.target[0];
        cur.target[1] = value.target[1];
        cur.target[2] = value.target[2];
        cur.distance = value.distance;
        cur.azimuth = value.azimuth;
        cur.elevation = value.elevation;
        return cur;
      }
      return clone(value);
    }
    return value;
  }
  function clone(v2) {
    if (Array.isArray(v2))
      return v2.slice();
    if (isOrbit(v2))
      return { target: [...v2.target], distance: v2.distance, azimuth: v2.azimuth, elevation: v2.elevation };
    return v2;
  }
  function isOrbit(v2) {
    return typeof v2 === "object" && v2 !== null && !Array.isArray(v2) && "azimuth" in v2;
  }

  // packages/player/dist/parameter-activity.js
  var DEFAULT_FADE_SECONDS = 0.55;
  var ParameterActivityTracker = class {
    tracks;
    fadeSeconds;
    now;
    activity = {};
    userTouchedAt = /* @__PURE__ */ new Map();
    constructor(tracks = {}, fadeSeconds = DEFAULT_FADE_SECONDS, now = () => performance.now() / 1e3) {
      this.tracks = tracks;
      this.fadeSeconds = fadeSeconds;
      this.now = now;
    }
    noteUser(param) {
      this.userTouchedAt.set(param, this.now());
    }
    evaluate(lessonTime, interaction, assistant = {}) {
      narrationActivityAt(this.tracks, lessonTime, this.fadeSeconds, this.activity);
      for (const [param, strength] of Object.entries(assistant)) {
        if (strength > 0)
          this.activity[param] = { source: "assistant", strength };
      }
      const now = this.now();
      for (const [param, meta] of interaction) {
        if (!meta.dragging)
          continue;
        this.userTouchedAt.set(param, now);
        this.activity[param] = { source: "user", strength: 1 };
      }
      for (const [param, touchedAt] of this.userTouchedAt) {
        if (interaction.get(param)?.dragging)
          continue;
        const strength = fadeStrength(now - touchedAt, this.fadeSeconds);
        if (strength > 0)
          this.activity[param] = { source: "user", strength };
        else
          this.userTouchedAt.delete(param);
      }
      return this.activity;
    }
  };
  function narrationActivityAt(tracks, t2, fadeSeconds = DEFAULT_FADE_SECONDS, out = {}) {
    for (const param of Object.keys(out))
      delete out[param];
    for (const [param, keyframes] of Object.entries(tracks)) {
      const strength = trackStrengthAt(keyframes, t2, fadeSeconds);
      if (strength > 0)
        out[param] = { source: "narration", strength };
    }
    return out;
  }
  function trackStrengthAt(keyframes, t2, fadeSeconds) {
    const index = keyframeAtOrBefore(keyframes, t2);
    if (index < 0)
      return 0;
    const current = keyframes[index];
    const next = keyframes[index + 1];
    if (next?.ease !== void 0 && t2 < next.t)
      return 1;
    return fadeStrength(t2 - current.t, fadeSeconds);
  }
  function keyframeAtOrBefore(keyframes, t2) {
    if (!keyframes.length || t2 < keyframes[0].t)
      return -1;
    let lo = 0;
    let hi = keyframes.length - 1;
    while (lo < hi) {
      const mid = lo + hi + 1 >> 1;
      if (keyframes[mid].t <= t2)
        lo = mid;
      else
        hi = mid - 1;
    }
    return lo;
  }
  function fadeStrength(age, fadeSeconds) {
    if (age < 0 || age >= fadeSeconds)
      return 0;
    return 1 - age / fadeSeconds;
  }

  // packages/core/dist/types.js
  var DEFAULT_ASSISTANT_LIMITS = {
    request: {
      bodyBytes: 64 * 1024,
      questionCharacters: 1e3,
      historyTurns: 8,
      positionCharacters: 2e3
    },
    response: {
      outputTokens: 1200,
      beats: 6,
      beatCharacters: 600,
      answerCharacters: 2e3,
      transitionSeconds: 2
    },
    rate: {
      browserRequestsPerTenMinutes: 8,
      ipRequestsPerTenMinutes: 40,
      globalRequestsPerHour: 120,
      globalRequestsPerDay: 500,
      concurrentProviderCalls: 2
    },
    queue: {
      maxPendingRequests: 0,
      waitTimeoutSeconds: 20
    },
    providerTimeoutSeconds: 30
  };

  // packages/core/dist/easing.js
  var BACK_C1 = 1.70158;
  var EASINGS = {
    linear: (t2) => t2,
    inCubic: (t2) => t2 * t2 * t2,
    outCubic: (t2) => 1 - Math.pow(1 - t2, 3),
    inOutCubic: (t2) => t2 < 0.5 ? 4 * t2 * t2 * t2 : 1 - Math.pow(-2 * t2 + 2, 3) / 2,
    spring: (t2) => 1 + (BACK_C1 + 1) * Math.pow(t2 - 1, 3) + BACK_C1 * Math.pow(t2 - 1, 2)
  };
  function getEasing(name) {
    const fn = EASINGS[name];
    if (!fn)
      throw new Error(`unknown easing: ${name}`);
    return fn;
  }

  // packages/core/dist/interpolate.js
  function buildIndex(tracks, schema2) {
    const entries = {};
    for (const [key, spec] of Object.entries(schema2)) {
      entries[key] = {
        keyframes: tracks[key] ?? [],
        mode: spec.interpolate,
        def: spec.default,
        cursor: 0
      };
    }
    return { entries, keys: Object.keys(entries) };
  }
  function findSegment(kf, t2, hint) {
    const n = kf.length;
    if (n === 0)
      return -1;
    if (t2 < kf[0].t)
      return -1;
    if (t2 >= kf[n - 1].t)
      return n - 1;
    if (hint >= 0 && hint < n - 1 && kf[hint].t <= t2 && t2 < kf[hint + 1].t)
      return hint;
    let lo = 0;
    let hi = n - 1;
    while (lo < hi) {
      const mid = lo + hi + 1 >> 1;
      if (kf[mid].t <= t2)
        lo = mid;
      else
        hi = mid - 1;
    }
    return lo;
  }
  function evaluate(index, t2, out = {}) {
    for (const key of index.keys) {
      const e2 = index.entries[key];
      const kf = e2.keyframes;
      const i3 = findSegment(kf, t2, e2.cursor);
      e2.cursor = i3 < 0 ? 0 : i3;
      if (i3 < 0) {
        assignInto(out, key, e2.def);
        continue;
      }
      const k0 = kf[i3];
      const next = kf[i3 + 1];
      if (!next || next.ease === void 0 || e2.mode === "snap") {
        assignInto(out, key, k0.v);
        continue;
      }
      const dur = next.t - k0.t;
      const localT = dur > 0 ? (t2 - k0.t) / dur : 1;
      const u = getEasing(next.ease)(localT);
      interpInto(out, key, e2.mode, k0.v, next.v, u);
    }
    return out;
  }
  function assignInto(out, key, v2) {
    if (Array.isArray(v2)) {
      const dst = ensureArray(out, key, v2.length);
      for (let j2 = 0; j2 < v2.length; j2++)
        dst[j2] = v2[j2];
    } else if (isOrbit2(v2)) {
      copyOrbit(ensureOrbit(out, key), v2);
    } else {
      out[key] = v2;
    }
  }
  function ensureArray(out, key, len) {
    const cur = out[key];
    if (Array.isArray(cur) && cur.length === len)
      return cur;
    const arr = new Array(len).fill(0);
    out[key] = arr;
    return arr;
  }
  function ensureOrbit(out, key) {
    const cur = out[key];
    if (isOrbit2(cur))
      return cur;
    const o2 = { target: [0, 0, 0], distance: 0, azimuth: 0, elevation: 0 };
    out[key] = o2;
    return o2;
  }
  function copyOrbit(dst, src) {
    dst.target[0] = src.target[0];
    dst.target[1] = src.target[1];
    dst.target[2] = src.target[2];
    dst.distance = src.distance;
    dst.azimuth = src.azimuth;
    dst.elevation = src.elevation;
  }
  function interpInto(out, key, mode, a2, b2, u) {
    switch (mode) {
      case "lerp":
        if (typeof a2 === "number" && typeof b2 === "number")
          out[key] = a2 + (b2 - a2) * u;
        else
          lerpArrayInto(ensureArray(out, key, a2.length), a2, b2, u);
        return;
      case "nlerp":
        nlerpInto(ensureArray(out, key, a2.length), a2, b2, u);
        return;
      case "orbit":
        orbitInto(ensureOrbit(out, key), a2, b2, u);
        return;
      case "typewriter":
        out[key] = typewriter(a2, b2, u);
        return;
      case "snap":
        assignInto(out, key, a2);
        return;
    }
  }
  function typewriter(a2, b2, u) {
    if (u <= 0)
      return a2;
    if (u >= 1)
      return b2;
    let shared = 0;
    while (shared < a2.length && shared < b2.length && a2[shared] === b2[shared])
      shared++;
    const deleted = [...a2.slice(shared)].reverse();
    const inserted = [...b2.slice(shared)];
    const weight = (char) => char === "\n" ? 4 : 1;
    const total = [...deleted, ...inserted].reduce((sum, char) => sum + weight(char), 0);
    let budget = u * total;
    let remaining = a2.length;
    for (const char of deleted) {
      const cost = weight(char);
      if (budget < cost)
        return a2.slice(0, remaining);
      budget -= cost;
      remaining--;
    }
    let typed = "";
    for (const char of inserted) {
      const cost = weight(char);
      if (budget < cost)
        break;
      budget -= cost;
      typed += char;
    }
    return a2.slice(0, shared) + typed;
  }
  function lerpArrayInto(dst, a2, b2, u) {
    for (let j2 = 0; j2 < a2.length; j2++)
      dst[j2] = a2[j2] + (b2[j2] - a2[j2]) * u;
  }
  function nlerpInto(dst, a2, b2, u) {
    let dot = 0;
    for (let j2 = 0; j2 < a2.length; j2++)
      dot += a2[j2] * b2[j2];
    const s2 = dot < 0 ? -1 : 1;
    let mag = 0;
    for (let j2 = 0; j2 < a2.length; j2++) {
      const val = a2[j2] + (s2 * b2[j2] - a2[j2]) * u;
      dst[j2] = val;
      mag += val * val;
    }
    mag = Math.sqrt(mag) || 1;
    for (let j2 = 0; j2 < dst.length; j2++)
      dst[j2] /= mag;
  }
  var _dirA = [0, 0, 0];
  var _dirB = [0, 0, 0];
  function orbitInto(dst, a2, b2, u) {
    sphericalTo(_dirA, a2.azimuth, a2.elevation);
    sphericalTo(_dirB, b2.azimuth, b2.elevation);
    let x2 = _dirA[0] + (_dirB[0] - _dirA[0]) * u;
    let y2 = _dirA[1] + (_dirB[1] - _dirA[1]) * u;
    let z = _dirA[2] + (_dirB[2] - _dirA[2]) * u;
    const mag = Math.sqrt(x2 * x2 + y2 * y2 + z * z) || 1;
    x2 /= mag;
    y2 /= mag;
    z /= mag;
    dst.azimuth = Math.atan2(x2, z);
    dst.elevation = Math.asin(Math.max(-1, Math.min(1, y2)));
    dst.distance = a2.distance + (b2.distance - a2.distance) * u;
    for (let j2 = 0; j2 < 3; j2++)
      dst.target[j2] = a2.target[j2] + (b2.target[j2] - a2.target[j2]) * u;
  }
  function sphericalTo(out, az, el2) {
    const ce = Math.cos(el2);
    out[0] = ce * Math.sin(az);
    out[1] = Math.sin(el2);
    out[2] = ce * Math.cos(az);
  }
  function isOrbit2(v2) {
    return typeof v2 === "object" && v2 !== null && !Array.isArray(v2) && "azimuth" in v2;
  }
  function blend(mode, a2, b2, u) {
    if (mode === "snap")
      return b2;
    const out = {};
    interpInto(out, "v", mode, a2, b2, u);
    return out["v"];
  }
  function converged(a2, b2, eps) {
    if (typeof a2 === "number" && typeof b2 === "number")
      return Math.abs(a2 - b2) <= eps;
    if (Array.isArray(a2) && Array.isArray(b2)) {
      for (let i3 = 0; i3 < a2.length; i3++)
        if (Math.abs(a2[i3] - b2[i3]) > eps)
          return false;
      return true;
    }
    if (isOrbit2(a2) && isOrbit2(b2)) {
      return Math.abs(a2.distance - b2.distance) <= eps && Math.abs(a2.azimuth - b2.azimuth) <= eps && Math.abs(a2.elevation - b2.elevation) <= eps && a2.target.every((x2, i3) => Math.abs(x2 - b2.target[i3]) <= eps);
    }
    return a2 === b2;
  }

  // packages/core/dist/reconcile.js
  var DEFAULT_HOLD = 3;
  var DEFAULT_TAU = 0.2;
  function approachU(dt, tau = DEFAULT_TAU) {
    return 1 - Math.exp(-dt / tau);
  }
  function holdActive(now, lastTouched, hold = DEFAULT_HOLD) {
    return now - lastTouched < hold;
  }

  // packages/player/dist/timeline.js
  var SEEK_THRESHOLD = 0.25;
  var TimelineDriver = class {
    clock;
    index;
    store;
    hooks;
    reconciler;
    buf = {};
    lastT = 0;
    lastNow = -1;
    raf = 0;
    running = false;
    lastPlaying;
    constructor(clock, index, store, hooks = {}, reconciler) {
      this.clock = clock;
      this.index = index;
      this.store = store;
      this.hooks = hooks;
      this.reconciler = reconciler;
      this.lastPlaying = clock.playing;
    }
    start() {
      this.running = true;
      const loop = () => {
        if (!this.running)
          return;
        this.tick();
        this.raf = requestAnimationFrame(loop);
      };
      this.raf = requestAnimationFrame(loop);
    }
    stop() {
      this.running = false;
      if (this.raf)
        cancelAnimationFrame(this.raf);
    }
    /** One frame; public so tests can step deterministically without rAF. */
    tick() {
      const t2 = this.clock.t;
      const now = this.hooks.now?.() ?? performance.now() / 1e3;
      const dt = this.lastNow < 0 ? 0 : Math.max(0, now - this.lastNow);
      const seeked = Math.abs(t2 - this.lastT) > SEEK_THRESHOLD;
      const playing = this.clock.playing;
      evaluate(this.index, t2, this.buf);
      if (seeked)
        this.reconciler?.reset();
      if (!seeked && this.lastPlaying && !playing)
        this.reconciler?.freeze(t2);
      if (!seeked && !this.lastPlaying && playing)
        this.reconciler?.resume(t2);
      if (this.reconciler)
        this.reconciler.reconcile(this.buf, t2, dt, playing);
      else
        for (const key of this.index.keys)
          this.store.set(key, this.buf[key]);
      if (seeked)
        this.hooks.onSeek?.(t2);
      this.hooks.onFrame?.(t2);
      this.lastT = t2;
      this.lastNow = now;
      this.lastPlaying = playing;
    }
  };

  // packages/player/dist/scene-host.js
  var SceneHost = class {
    instance;
    constructor(module, ctx) {
      this.instance = module.create(ctx);
    }
    render(state, frame) {
      this.instance.render(state, frame);
    }
    handles() {
      return this.instance.handles();
    }
    dispose() {
      this.instance.dispose();
    }
  };

  // packages/player/dist/interaction.js
  var InteractionManager = class {
    canvas;
    target;
    store;
    clock;
    displayedState;
    onWrite;
    active;
    constructor(canvas, target2, store, clock, displayedState = () => store.plain, onWrite) {
      this.canvas = canvas;
      this.target = target2;
      this.store = store;
      this.clock = clock;
      this.displayedState = displayedState;
      this.onWrite = onWrite;
      canvas.addEventListener("pointerdown", this.onDown);
      canvas.addEventListener("pointermove", this.onMove);
      canvas.addEventListener("pointerup", this.onUp);
      canvas.addEventListener("pointercancel", this.onUp);
      canvas.addEventListener("wheel", this.onWheel, { passive: false });
    }
    dispose() {
      this.canvas.removeEventListener("pointerdown", this.onDown);
      this.canvas.removeEventListener("pointermove", this.onMove);
      this.canvas.removeEventListener("pointerup", this.onUp);
      this.canvas.removeEventListener("pointercancel", this.onUp);
      this.canvas.removeEventListener("wheel", this.onWheel);
    }
    toCanvas(e2) {
      const r2 = this.canvas.getBoundingClientRect();
      const sx = r2.width ? this.canvas.width / r2.width : 1;
      const sy = r2.height ? this.canvas.height / r2.height : 1;
      return [(e2.clientX - r2.left) * sx, (e2.clientY - r2.top) * sy];
    }
    onDown = (e2) => {
      const [px, py] = this.toCanvas(e2);
      const state = this.displayedState();
      for (const h2 of this.target.handles()) {
        if (!h2.hitTest(px, py, state))
          continue;
        this.active = h2;
        this.canvas.setPointerCapture?.(e2.pointerId);
        for (const p2 of h2.params)
          this.store.setDragging(p2, true);
        h2.onDown?.(px, py, state);
        this.write(h2.onDrag(px, py, state));
        break;
      }
    };
    onMove = (e2) => {
      if (!this.active)
        return;
      const [px, py] = this.toCanvas(e2);
      this.write(this.active.onDrag(px, py, this.displayedState()));
    };
    onUp = (e2) => {
      if (!this.active)
        return;
      for (const p2 of this.active.params)
        this.store.setDragging(p2, false);
      this.canvas.releasePointerCapture?.(e2.pointerId);
      this.active = void 0;
    };
    onWheel = (e2) => {
      const [px, py] = this.toCanvas(e2);
      const state = this.displayedState();
      for (const h2 of this.target.handles()) {
        if (!h2.onWheel || !h2.hitTest(px, py, state))
          continue;
        e2.preventDefault();
        this.write(h2.onWheel(px, py, e2.deltaY, state));
        break;
      }
    };
    write(writes) {
      const t2 = this.clock.t;
      for (const [param, value] of Object.entries(writes)) {
        this.store.touch(param, value, t2);
        this.onWrite?.(param);
        if (param === "scene" && this.clock.playing)
          this.clock.pause();
      }
    }
  };

  // packages/player/dist/reconciler.js
  var EPS = 1e-3;
  var Reconciler = class {
    store;
    index;
    schema;
    hold;
    tau;
    keys;
    constructor(store, index, schema2, cfg = {}) {
      this.store = store;
      this.index = index;
      this.schema = schema2;
      this.hold = cfg.hold ?? 3;
      this.tau = cfg.tau ?? 0.2;
      this.keys = Object.keys(schema2);
    }
    /** Merge scripted → displayed for every parameter and write to the store. */
    reconcile(scripted, t2, dt, playing = true) {
      for (const key of this.keys)
        this.store.set(key, this.compute(key, scripted[key], t2, dt, playing));
    }
    /** Snapshot modified values when playback pauses so no catch-up continues. */
    freeze(t2) {
      for (const key of this.keys) {
        const meta = this.store.meta.get(key);
        if (meta.modified && !meta.dragging && meta.holdT !== t2)
          this.store.freezeInteraction(key);
      }
    }
    /** Give script-owned interactions a fresh playback-time hold after resume. */
    resume(t2) {
      for (const key of this.keys) {
        const meta = this.store.meta.get(key);
        if (meta.modified && this.schema[key].ownership === "script")
          meta.holdT = t2;
      }
    }
    /** Clear all interaction state (called on seek). */
    reset() {
      this.store.resetInteractions();
    }
    compute(key, sc, t2, dt, playing) {
      const meta = this.store.meta.get(key);
      if (meta.dragging)
        return meta.userValue;
      if (!meta.touchedEver || !meta.modified)
        return sc;
      if (!playing)
        return meta.userValue;
      const spec = this.schema[key];
      const prev = this.store.plain[key];
      const discrete = spec.interpolate === "snap";
      switch (spec.ownership) {
        case "viewer":
          return meta.userValue;
        case "script":
          if (holdActive(t2, meta.holdT, this.hold))
            return meta.userValue;
          return discrete ? this.revert(meta, sc) : this.glide(meta, prev, sc, spec.interpolate, dt);
        case "shared":
          if (t2 < this.nextKeyframeAfter(key, meta.touchT))
            return meta.userValue;
          return discrete ? this.revert(meta, sc) : this.glide(meta, prev, sc, spec.interpolate, dt);
      }
    }
    revert(meta, sc) {
      meta.modified = false;
      return sc;
    }
    glide(meta, prev, sc, mode, dt) {
      const next = blend(mode, prev, sc, approachU(dt, this.tau));
      if (converged(next, sc, EPS)) {
        meta.modified = false;
        return sc;
      }
      return next;
    }
    nextKeyframeAfter(key, touchT) {
      for (const k of this.index.entries[key]?.keyframes ?? [])
        if (k.t > touchT)
          return k.t;
      return Infinity;
    }
  };

  // node_modules/.pnpm/katex@0.16.47/node_modules/katex/dist/katex.mjs
  var ParseError = class _ParseError extends Error {
    // The underlying error message without any context added.
    constructor(message, token) {
      var error = "KaTeX parse error: " + message;
      var start;
      var end;
      var loc = token && token.loc;
      if (loc && loc.start <= loc.end) {
        var input = loc.lexer.input;
        start = loc.start;
        end = loc.end;
        if (start === input.length) {
          error += " at end of input: ";
        } else {
          error += " at position " + (start + 1) + ": ";
        }
        var underlined = input.slice(start, end).replace(/[^]/g, "$&\u0332");
        var left;
        if (start > 15) {
          left = "\u2026" + input.slice(start - 15, start);
        } else {
          left = input.slice(0, start);
        }
        var right;
        if (end + 15 < input.length) {
          right = input.slice(end, end + 15) + "\u2026";
        } else {
          right = input.slice(end);
        }
        error += left + underlined + right;
      }
      super(error);
      this.name = "ParseError";
      this.position = void 0;
      this.length = void 0;
      this.rawMessage = void 0;
      Object.setPrototypeOf(this, _ParseError.prototype);
      this.position = start;
      if (start != null && end != null) {
        this.length = end - start;
      }
      this.rawMessage = message;
    }
  };
  var uppercase = /([A-Z])/g;
  var hyphenate = (str) => str.replace(uppercase, "-$1").toLowerCase();
  var ESCAPE_LOOKUP = {
    "&": "&amp;",
    ">": "&gt;",
    "<": "&lt;",
    '"': "&quot;",
    "'": "&#x27;"
  };
  var ESCAPE_REGEX = /[&><"']/g;
  var escape = (text2) => String(text2).replace(ESCAPE_REGEX, (match) => ESCAPE_LOOKUP[match]);
  var getBaseElem = (group) => {
    if (group.type === "ordgroup") {
      if (group.body.length === 1) {
        return getBaseElem(group.body[0]);
      } else {
        return group;
      }
    } else if (group.type === "color") {
      if (group.body.length === 1) {
        return getBaseElem(group.body[0]);
      } else {
        return group;
      }
    } else if (group.type === "font") {
      return getBaseElem(group.body);
    } else {
      return group;
    }
  };
  var characterNodesTypes = /* @__PURE__ */ new Set(["mathord", "textord", "atom"]);
  var isCharacterBox = (group) => characterNodesTypes.has(getBaseElem(group).type);
  var protocolFromUrl = (url) => {
    var protocol = /^[\x00-\x20]*([^\\/#?]*?)(:|&#0*58|&#x0*3a|&colon)/i.exec(url);
    if (!protocol) {
      return "_relative";
    }
    if (protocol[2] !== ":") {
      return null;
    }
    if (!/^[a-zA-Z][a-zA-Z0-9+\-.]*$/.test(protocol[1])) {
      return null;
    }
    return protocol[1].toLowerCase();
  };
  var SETTINGS_SCHEMA = {
    displayMode: {
      type: "boolean",
      description: "Render math in display mode, which puts the math in display style (so \\int and \\sum are large, for example), and centers the math on the page on its own line.",
      cli: "-d, --display-mode"
    },
    output: {
      type: {
        enum: ["htmlAndMathml", "html", "mathml"]
      },
      description: "Determines the markup language of the output.",
      cli: "-F, --format <type>"
    },
    leqno: {
      type: "boolean",
      description: "Render display math in leqno style (left-justified tags)."
    },
    fleqn: {
      type: "boolean",
      description: "Render display math flush left."
    },
    throwOnError: {
      type: "boolean",
      default: true,
      cli: "-t, --no-throw-on-error",
      cliDescription: "Render errors (in the color given by --error-color) instead of throwing a ParseError exception when encountering an error."
    },
    errorColor: {
      type: "string",
      default: "#cc0000",
      cli: "-c, --error-color <color>",
      cliDescription: "A color string given in the format 'rgb' or 'rrggbb' (no #). This option determines the color of errors rendered by the -t option.",
      cliProcessor: (color) => "#" + color
    },
    macros: {
      type: "object",
      cli: "-m, --macro <def>",
      cliDescription: "Define custom macro of the form '\\foo:expansion' (use multiple -m arguments for multiple macros).",
      cliDefault: [],
      cliProcessor: (def, defs) => {
        defs.push(def);
        return defs;
      }
    },
    minRuleThickness: {
      type: "number",
      description: "Specifies a minimum thickness, in ems, for fraction lines, `\\sqrt` top lines, `{array}` vertical lines, `\\hline`, `\\hdashline`, `\\underline`, `\\overline`, and the borders of `\\fbox`, `\\boxed`, and `\\fcolorbox`.",
      processor: (t2) => Math.max(0, t2),
      cli: "--min-rule-thickness <size>",
      cliProcessor: parseFloat
    },
    colorIsTextColor: {
      type: "boolean",
      description: "Makes \\color behave like LaTeX's 2-argument \\textcolor, instead of LaTeX's one-argument \\color mode change.",
      cli: "-b, --color-is-text-color"
    },
    strict: {
      type: [{
        enum: ["warn", "ignore", "error"]
      }, "boolean", "function"],
      description: "Turn on strict / LaTeX faithfulness mode, which throws an error if the input uses features that are not supported by LaTeX.",
      cli: "-S, --strict",
      cliDefault: false
    },
    trust: {
      type: ["boolean", "function"],
      description: "Trust the input, enabling all HTML features such as \\url.",
      cli: "-T, --trust"
    },
    maxSize: {
      type: "number",
      default: Infinity,
      description: "If non-zero, all user-specified sizes, e.g. in \\rule{500em}{500em}, will be capped to maxSize ems. Otherwise, elements and spaces can be arbitrarily large",
      processor: (s2) => Math.max(0, s2),
      cli: "-s, --max-size <n>",
      cliProcessor: parseInt
    },
    maxExpand: {
      type: "number",
      default: 1e3,
      description: "Limit the number of macro expansions to the specified number, to prevent e.g. infinite macro loops. If set to Infinity, the macro expander will try to fully expand as in LaTeX.",
      processor: (n) => Math.max(0, n),
      cli: "-e, --max-expand <n>",
      cliProcessor: (n) => n === "Infinity" ? Infinity : parseInt(n)
    },
    globalGroup: {
      type: "boolean",
      cli: false
    }
  };
  function getImplicitDefault(type) {
    if (typeof type !== "string") {
      return type.enum[0];
    }
    switch (type) {
      case "boolean":
        return false;
      case "string":
        return "";
      case "number":
        return 0;
      case "object":
        return {};
      default:
        throw new Error("Unexpected schema type; settings must declare an explicit default.");
    }
  }
  function getDefaultValue(schema2) {
    if (schema2.default !== void 0) {
      return schema2.default;
    }
    var type = Array.isArray(schema2.type) ? schema2.type[0] : schema2.type;
    return getImplicitDefault(type);
  }
  function applySetting(target2, prop, options, schema2) {
    var optionValue = options[prop];
    target2[prop] = optionValue !== void 0 ? schema2.processor ? schema2.processor(optionValue) : optionValue : getDefaultValue(schema2);
  }
  var Settings = class {
    constructor(options) {
      if (options === void 0) {
        options = {};
      }
      this.displayMode = void 0;
      this.output = void 0;
      this.leqno = void 0;
      this.fleqn = void 0;
      this.throwOnError = void 0;
      this.errorColor = void 0;
      this.macros = void 0;
      this.minRuleThickness = void 0;
      this.colorIsTextColor = void 0;
      this.strict = void 0;
      this.trust = void 0;
      this.maxSize = void 0;
      this.maxExpand = void 0;
      this.globalGroup = void 0;
      options = options || {};
      for (var prop of Object.keys(SETTINGS_SCHEMA)) {
        var schema2 = SETTINGS_SCHEMA[prop];
        if (schema2) {
          applySetting(this, prop, options, schema2);
        }
      }
    }
    /**
     * Report nonstrict (non-LaTeX-compatible) input.
     * Can safely not be called if `this.strict` is false in JavaScript.
     */
    reportNonstrict(errorCode, errorMsg, token) {
      var strict = this.strict;
      if (typeof strict === "function") {
        strict = strict(errorCode, errorMsg, token);
      }
      if (!strict || strict === "ignore") {
        return;
      } else if (strict === true || strict === "error") {
        throw new ParseError("LaTeX-incompatible input and strict mode is set to 'error': " + (errorMsg + " [" + errorCode + "]"), token);
      } else if (strict === "warn") {
        typeof console !== "undefined" && console.warn("LaTeX-incompatible input and strict mode is set to 'warn': " + (errorMsg + " [" + errorCode + "]"));
      } else {
        typeof console !== "undefined" && console.warn("LaTeX-incompatible input and strict mode is set to " + ("unrecognized '" + strict + "': " + errorMsg + " [" + errorCode + "]"));
      }
    }
    /**
     * Check whether to apply strict (LaTeX-adhering) behavior for unusual
     * input (like `\\`).  Unlike `nonstrict`, will not throw an error;
     * instead, "error" translates to a return value of `true`, while "ignore"
     * translates to a return value of `false`.  May still print a warning:
     * "warn" prints a warning and returns `false`.
     * This is for the second category of `errorCode`s listed in the README.
     */
    useStrictBehavior(errorCode, errorMsg, token) {
      var strict = this.strict;
      if (typeof strict === "function") {
        try {
          strict = strict(errorCode, errorMsg, token);
        } catch (error) {
          strict = "error";
        }
      }
      if (!strict || strict === "ignore") {
        return false;
      } else if (strict === true || strict === "error") {
        return true;
      } else if (strict === "warn") {
        typeof console !== "undefined" && console.warn("LaTeX-incompatible input and strict mode is set to 'warn': " + (errorMsg + " [" + errorCode + "]"));
        return false;
      } else {
        typeof console !== "undefined" && console.warn("LaTeX-incompatible input and strict mode is set to " + ("unrecognized '" + strict + "': " + errorMsg + " [" + errorCode + "]"));
        return false;
      }
    }
    /**
     * Check whether to test potentially dangerous input, and return
     * `true` (trusted) or `false` (untrusted).  The sole argument `context`
     * should be an object with `command` field specifying the relevant LaTeX
     * command (as a string starting with `\`), and any other arguments, etc.
     * If `context` has a `url` field, a `protocol` field will automatically
     * get added by this function (changing the specified object).
     */
    isTrusted(context) {
      if ("url" in context && context.url && !context.protocol) {
        var protocol = protocolFromUrl(context.url);
        if (protocol == null) {
          return false;
        }
        context.protocol = protocol;
      }
      var trust = typeof this.trust === "function" ? this.trust(context) : this.trust;
      return Boolean(trust);
    }
  };
  var Style = class {
    constructor(id, size, cramped) {
      this.id = void 0;
      this.size = void 0;
      this.cramped = void 0;
      this.id = id;
      this.size = size;
      this.cramped = cramped;
    }
    /**
     * Get the style of a superscript given a base in the current style.
     */
    sup() {
      return styles[sup[this.id]];
    }
    /**
     * Get the style of a subscript given a base in the current style.
     */
    sub() {
      return styles[sub[this.id]];
    }
    /**
     * Get the style of a fraction numerator given the fraction in the current
     * style.
     */
    fracNum() {
      return styles[fracNum[this.id]];
    }
    /**
     * Get the style of a fraction denominator given the fraction in the current
     * style.
     */
    fracDen() {
      return styles[fracDen[this.id]];
    }
    /**
     * Get the cramped version of a style (in particular, cramping a cramped style
     * doesn't change the style).
     */
    cramp() {
      return styles[cramp[this.id]];
    }
    /**
     * Get a text or display version of this style.
     */
    text() {
      return styles[text$1[this.id]];
    }
    /**
     * Return true if this style is tightly spaced (scriptstyle/scriptscriptstyle)
     */
    isTight() {
      return this.size >= 2;
    }
  };
  var D = 0;
  var Dc = 1;
  var T = 2;
  var Tc = 3;
  var S2 = 4;
  var Sc = 5;
  var SS = 6;
  var SSc = 7;
  var styles = [new Style(D, 0, false), new Style(Dc, 0, true), new Style(T, 1, false), new Style(Tc, 1, true), new Style(S2, 2, false), new Style(Sc, 2, true), new Style(SS, 3, false), new Style(SSc, 3, true)];
  var sup = [S2, Sc, S2, Sc, SS, SSc, SS, SSc];
  var sub = [Sc, Sc, Sc, Sc, SSc, SSc, SSc, SSc];
  var fracNum = [T, Tc, S2, Sc, SS, SSc, SS, SSc];
  var fracDen = [Tc, Tc, Sc, Sc, SSc, SSc, SSc, SSc];
  var cramp = [Dc, Dc, Tc, Tc, Sc, Sc, SSc, SSc];
  var text$1 = [D, Dc, T, Tc, T, Tc, T, Tc];
  var Style$1 = {
    DISPLAY: styles[D],
    TEXT: styles[T],
    SCRIPT: styles[S2],
    SCRIPTSCRIPT: styles[SS]
  };
  var scriptData = [{
    // Latin characters beyond the Latin-1 characters we have metrics for.
    // Needed for Czech, Hungarian and Turkish text, for example.
    name: "latin",
    blocks: [
      [256, 591],
      // Latin Extended-A and Latin Extended-B
      [768, 879]
      // Combining Diacritical marks
    ]
  }, {
    // The Cyrillic script used by Russian and related languages.
    // A Cyrillic subset used to be supported as explicitly defined
    // symbols in symbols.js
    name: "cyrillic",
    blocks: [[1024, 1279]]
  }, {
    // Armenian
    name: "armenian",
    blocks: [[1328, 1423]]
  }, {
    // The Brahmic scripts of South and Southeast Asia
    // Devanagari (0900–097F)
    // Bengali (0980–09FF)
    // Gurmukhi (0A00–0A7F)
    // Gujarati (0A80–0AFF)
    // Oriya (0B00–0B7F)
    // Tamil (0B80–0BFF)
    // Telugu (0C00–0C7F)
    // Kannada (0C80–0CFF)
    // Malayalam (0D00–0D7F)
    // Sinhala (0D80–0DFF)
    // Thai (0E00–0E7F)
    // Lao (0E80–0EFF)
    // Tibetan (0F00–0FFF)
    // Myanmar (1000–109F)
    name: "brahmic",
    blocks: [[2304, 4255]]
  }, {
    name: "georgian",
    blocks: [[4256, 4351]]
  }, {
    // Chinese and Japanese.
    // The "k" in cjk is for Korean, but we've separated Korean out
    name: "cjk",
    blocks: [
      [12288, 12543],
      // CJK symbols and punctuation, Hiragana, Katakana
      [19968, 40879],
      // CJK ideograms
      [65280, 65376]
      // Fullwidth punctuation
      // TODO: add halfwidth Katakana and Romanji glyphs
    ]
  }, {
    // Korean
    name: "hangul",
    blocks: [[44032, 55215]]
  }];
  function scriptFromCodepoint(codepoint) {
    for (var i3 = 0; i3 < scriptData.length; i3++) {
      var script2 = scriptData[i3];
      for (var _i6 = 0; _i6 < script2.blocks.length; _i6++) {
        var block2 = script2.blocks[_i6];
        if (codepoint >= block2[0] && codepoint <= block2[1]) {
          return script2.name;
        }
      }
    }
    return null;
  }
  var allBlocks = [];
  scriptData.forEach((s2) => s2.blocks.forEach((b2) => allBlocks.push(...b2)));
  function supportedCodepoint(codepoint) {
    for (var i3 = 0; i3 < allBlocks.length; i3 += 2) {
      if (codepoint >= allBlocks[i3] && codepoint <= allBlocks[i3 + 1]) {
        return true;
      }
    }
    return false;
  }
  var doubleBrushStroke = (svgPath) => svgPath + " " + svgPath;
  var hLinePad = 80;
  var sqrtMain = function sqrtMain2(extraVinculum, hLinePad2) {
    return "M95," + (622 + extraVinculum + hLinePad2) + "\nc-2.7,0,-7.17,-2.7,-13.5,-8c-5.8,-5.3,-9.5,-10,-9.5,-14\nc0,-2,0.3,-3.3,1,-4c1.3,-2.7,23.83,-20.7,67.5,-54\nc44.2,-33.3,65.8,-50.3,66.5,-51c1.3,-1.3,3,-2,5,-2c4.7,0,8.7,3.3,12,10\ns173,378,173,378c0.7,0,35.3,-71,104,-213c68.7,-142,137.5,-285,206.5,-429\nc69,-144,104.5,-217.7,106.5,-221\nl" + extraVinculum / 2.075 + " -" + extraVinculum + "\nc5.3,-9.3,12,-14,20,-14\nH400000v" + (40 + extraVinculum) + "H845.2724\ns-225.272,467,-225.272,467s-235,486,-235,486c-2.7,4.7,-9,7,-19,7\nc-6,0,-10,-1,-12,-3s-194,-422,-194,-422s-65,47,-65,47z\nM" + (834 + extraVinculum) + " " + hLinePad2 + "h400000v" + (40 + extraVinculum) + "h-400000z";
  };
  var sqrtSize1 = function sqrtSize12(extraVinculum, hLinePad2) {
    return "M263," + (601 + extraVinculum + hLinePad2) + "c0.7,0,18,39.7,52,119\nc34,79.3,68.167,158.7,102.5,238c34.3,79.3,51.8,119.3,52.5,120\nc340,-704.7,510.7,-1060.3,512,-1067\nl" + extraVinculum / 2.084 + " -" + extraVinculum + "\nc4.7,-7.3,11,-11,19,-11\nH40000v" + (40 + extraVinculum) + "H1012.3\ns-271.3,567,-271.3,567c-38.7,80.7,-84,175,-136,283c-52,108,-89.167,185.3,-111.5,232\nc-22.3,46.7,-33.8,70.3,-34.5,71c-4.7,4.7,-12.3,7,-23,7s-12,-1,-12,-1\ns-109,-253,-109,-253c-72.7,-168,-109.3,-252,-110,-252c-10.7,8,-22,16.7,-34,26\nc-22,17.3,-33.3,26,-34,26s-26,-26,-26,-26s76,-59,76,-59s76,-60,76,-60z\nM" + (1001 + extraVinculum) + " " + hLinePad2 + "h400000v" + (40 + extraVinculum) + "h-400000z";
  };
  var sqrtSize2 = function sqrtSize22(extraVinculum, hLinePad2) {
    return "M983 " + (10 + extraVinculum + hLinePad2) + "\nl" + extraVinculum / 3.13 + " -" + extraVinculum + "\nc4,-6.7,10,-10,18,-10 H400000v" + (40 + extraVinculum) + "\nH1013.1s-83.4,268,-264.1,840c-180.7,572,-277,876.3,-289,913c-4.7,4.7,-12.7,7,-24,7\ns-12,0,-12,0c-1.3,-3.3,-3.7,-11.7,-7,-25c-35.3,-125.3,-106.7,-373.3,-214,-744\nc-10,12,-21,25,-33,39s-32,39,-32,39c-6,-5.3,-15,-14,-27,-26s25,-30,25,-30\nc26.7,-32.7,52,-63,76,-91s52,-60,52,-60s208,722,208,722\nc56,-175.3,126.3,-397.3,211,-666c84.7,-268.7,153.8,-488.2,207.5,-658.5\nc53.7,-170.3,84.5,-266.8,92.5,-289.5z\nM" + (1001 + extraVinculum) + " " + hLinePad2 + "h400000v" + (40 + extraVinculum) + "h-400000z";
  };
  var sqrtSize3 = function sqrtSize32(extraVinculum, hLinePad2) {
    return "M424," + (2398 + extraVinculum + hLinePad2) + "\nc-1.3,-0.7,-38.5,-172,-111.5,-514c-73,-342,-109.8,-513.3,-110.5,-514\nc0,-2,-10.7,14.3,-32,49c-4.7,7.3,-9.8,15.7,-15.5,25c-5.7,9.3,-9.8,16,-12.5,20\ns-5,7,-5,7c-4,-3.3,-8.3,-7.7,-13,-13s-13,-13,-13,-13s76,-122,76,-122s77,-121,77,-121\ns209,968,209,968c0,-2,84.7,-361.7,254,-1079c169.3,-717.3,254.7,-1077.7,256,-1081\nl" + extraVinculum / 4.223 + " -" + extraVinculum + "c4,-6.7,10,-10,18,-10 H400000\nv" + (40 + extraVinculum) + "H1014.6\ns-87.3,378.7,-272.6,1166c-185.3,787.3,-279.3,1182.3,-282,1185\nc-2,6,-10,9,-24,9\nc-8,0,-12,-0.7,-12,-2z M" + (1001 + extraVinculum) + " " + hLinePad2 + "\nh400000v" + (40 + extraVinculum) + "h-400000z";
  };
  var sqrtSize4 = function sqrtSize42(extraVinculum, hLinePad2) {
    return "M473," + (2713 + extraVinculum + hLinePad2) + "\nc339.3,-1799.3,509.3,-2700,510,-2702 l" + extraVinculum / 5.298 + " -" + extraVinculum + "\nc3.3,-7.3,9.3,-11,18,-11 H400000v" + (40 + extraVinculum) + "H1017.7\ns-90.5,478,-276.2,1466c-185.7,988,-279.5,1483,-281.5,1485c-2,6,-10,9,-24,9\nc-8,0,-12,-0.7,-12,-2c0,-1.3,-5.3,-32,-16,-92c-50.7,-293.3,-119.7,-693.3,-207,-1200\nc0,-1.3,-5.3,8.7,-16,30c-10.7,21.3,-21.3,42.7,-32,64s-16,33,-16,33s-26,-26,-26,-26\ns76,-153,76,-153s77,-151,77,-151c0.7,0.7,35.7,202,105,604c67.3,400.7,102,602.7,104,\n606zM" + (1001 + extraVinculum) + " " + hLinePad2 + "h400000v" + (40 + extraVinculum) + "H1017.7z";
  };
  var phasePath = function phasePath2(y2) {
    var x2 = y2 / 2;
    return "M400000 " + y2 + " H0 L" + x2 + " 0 l65 45 L145 " + (y2 - 80) + " H400000z";
  };
  var sqrtTall = function sqrtTall2(extraVinculum, hLinePad2, viewBoxHeight) {
    var vertSegment = viewBoxHeight - 54 - hLinePad2 - extraVinculum;
    return "M702 " + (extraVinculum + hLinePad2) + "H400000" + (40 + extraVinculum) + "\nH742v" + vertSegment + "l-4 4-4 4c-.667.7 -2 1.5-4 2.5s-4.167 1.833-6.5 2.5-5.5 1-9.5 1\nh-12l-28-84c-16.667-52-96.667 -294.333-240-727l-212 -643 -85 170\nc-4-3.333-8.333-7.667-13 -13l-13-13l77-155 77-156c66 199.333 139 419.667\n219 661 l218 661zM702 " + hLinePad2 + "H400000v" + (40 + extraVinculum) + "H742z";
  };
  var sqrtPath = function sqrtPath2(size, extraVinculum, viewBoxHeight) {
    extraVinculum = 1e3 * extraVinculum;
    var path2 = "";
    switch (size) {
      case "sqrtMain":
        path2 = sqrtMain(extraVinculum, hLinePad);
        break;
      case "sqrtSize1":
        path2 = sqrtSize1(extraVinculum, hLinePad);
        break;
      case "sqrtSize2":
        path2 = sqrtSize2(extraVinculum, hLinePad);
        break;
      case "sqrtSize3":
        path2 = sqrtSize3(extraVinculum, hLinePad);
        break;
      case "sqrtSize4":
        path2 = sqrtSize4(extraVinculum, hLinePad);
        break;
      case "sqrtTall":
        path2 = sqrtTall(extraVinculum, hLinePad, viewBoxHeight);
    }
    return path2;
  };
  var innerPath = function innerPath2(name, height) {
    switch (name) {
      case "\u239C":
        return doubleBrushStroke("M291 0 H417 V" + height + " H291z");
      case "\u2223":
        return doubleBrushStroke("M145 0 H188 V" + height + " H145z");
      case "\u2225":
        return doubleBrushStroke("M145 0 H188 V" + height + " H145z") + doubleBrushStroke("M367 0 H410 V" + height + " H367z");
      case "\u239F":
        return doubleBrushStroke("M457 0 H583 V" + height + " H457z");
      case "\u23A2":
        return doubleBrushStroke("M319 0 H403 V" + height + " H319z");
      case "\u23A5":
        return doubleBrushStroke("M263 0 H347 V" + height + " H263z");
      case "\u23AA":
        return doubleBrushStroke("M384 0 H504 V" + height + " H384z");
      case "\u23D0":
        return doubleBrushStroke("M312 0 H355 V" + height + " H312z");
      case "\u2016":
        return doubleBrushStroke("M257 0 H300 V" + height + " H257z") + doubleBrushStroke("M478 0 H521 V" + height + " H478z");
      default:
        return "";
    }
  };
  var path = {
    // The doubleleftarrow geometry is from glyph U+21D0 in the font KaTeX Main
    doubleleftarrow: "M262 157\nl10-10c34-36 62.7-77 86-123 3.3-8 5-13.3 5-16 0-5.3-6.7-8-20-8-7.3\n 0-12.2.5-14.5 1.5-2.3 1-4.8 4.5-7.5 10.5-49.3 97.3-121.7 169.3-217 216-28\n 14-57.3 25-88 33-6.7 2-11 3.8-13 5.5-2 1.7-3 4.2-3 7.5s1 5.8 3 7.5\nc2 1.7 6.3 3.5 13 5.5 68 17.3 128.2 47.8 180.5 91.5 52.3 43.7 93.8 96.2 124.5\n 157.5 9.3 8 15.3 12.3 18 13h6c12-.7 18-4 18-10 0-2-1.7-7-5-15-23.3-46-52-87\n-86-123l-10-10h399738v-40H218c328 0 0 0 0 0l-10-8c-26.7-20-65.7-43-117-69 2.7\n-2 6-3.7 10-5 36.7-16 72.3-37.3 107-64l10-8h399782v-40z\nm8 0v40h399730v-40zm0 194v40h399730v-40z",
    // doublerightarrow is from glyph U+21D2 in font KaTeX Main
    doublerightarrow: "M399738 392l\n-10 10c-34 36-62.7 77-86 123-3.3 8-5 13.3-5 16 0 5.3 6.7 8 20 8 7.3 0 12.2-.5\n 14.5-1.5 2.3-1 4.8-4.5 7.5-10.5 49.3-97.3 121.7-169.3 217-216 28-14 57.3-25 88\n-33 6.7-2 11-3.8 13-5.5 2-1.7 3-4.2 3-7.5s-1-5.8-3-7.5c-2-1.7-6.3-3.5-13-5.5-68\n-17.3-128.2-47.8-180.5-91.5-52.3-43.7-93.8-96.2-124.5-157.5-9.3-8-15.3-12.3-18\n-13h-6c-12 .7-18 4-18 10 0 2 1.7 7 5 15 23.3 46 52 87 86 123l10 10H0v40h399782\nc-328 0 0 0 0 0l10 8c26.7 20 65.7 43 117 69-2.7 2-6 3.7-10 5-36.7 16-72.3 37.3\n-107 64l-10 8H0v40zM0 157v40h399730v-40zm0 194v40h399730v-40z",
    // leftarrow is from glyph U+2190 in font KaTeX Main
    leftarrow: "M400000 241H110l3-3c68.7-52.7 113.7-120\n 135-202 4-14.7 6-23 6-25 0-7.3-7-11-21-11-8 0-13.2.8-15.5 2.5-2.3 1.7-4.2 5.8\n-5.5 12.5-1.3 4.7-2.7 10.3-4 17-12 48.7-34.8 92-68.5 130S65.3 228.3 18 247\nc-10 4-16 7.7-18 11 0 8.7 6 14.3 18 17 47.3 18.7 87.8 47 121.5 85S196 441.3 208\n 490c.7 2 1.3 5 2 9s1.2 6.7 1.5 8c.3 1.3 1 3.3 2 6s2.2 4.5 3.5 5.5c1.3 1 3.3\n 1.8 6 2.5s6 1 10 1c14 0 21-3.7 21-11 0-2-2-10.3-6-25-20-79.3-65-146.7-135-202\n l-3-3h399890zM100 241v40h399900v-40z",
    // overbrace is from glyphs U+23A9/23A8/23A7 in font KaTeX_Size4-Regular
    leftbrace: "M6 548l-6-6v-35l6-11c56-104 135.3-181.3 238-232 57.3-28.7 117\n-45 179-50h399577v120H403c-43.3 7-81 15-113 26-100.7 33-179.7 91-237 174-2.7\n 5-6 9-10 13-.7 1-7.3 1-20 1H6z",
    leftbraceunder: "M0 6l6-6h17c12.688 0 19.313.3 20 1 4 4 7.313 8.3 10 13\n 35.313 51.3 80.813 93.8 136.5 127.5 55.688 33.7 117.188 55.8 184.5 66.5.688\n 0 2 .3 4 1 18.688 2.7 76 4.3 172 5h399450v120H429l-6-1c-124.688-8-235-61.7\n-331-161C60.687 138.7 32.312 99.3 7 54L0 41V6z",
    // overgroup is from the MnSymbol package (public domain)
    leftgroup: "M400000 80\nH435C64 80 168.3 229.4 21 260c-5.9 1.2-18 0-18 0-2 0-3-1-3-3v-38C76 61 257 0\n 435 0h399565z",
    leftgroupunder: "M400000 262\nH435C64 262 168.3 112.6 21 82c-5.9-1.2-18 0-18 0-2 0-3 1-3 3v38c76 158 257 219\n 435 219h399565z",
    // Harpoons are from glyph U+21BD in font KaTeX Main
    leftharpoon: "M0 267c.7 5.3 3 10 7 14h399993v-40H93c3.3\n-3.3 10.2-9.5 20.5-18.5s17.8-15.8 22.5-20.5c50.7-52 88-110.3 112-175 4-11.3 5\n-18.3 3-21-1.3-4-7.3-6-18-6-8 0-13 .7-15 2s-4.7 6.7-8 16c-42 98.7-107.3 174.7\n-196 228-6.7 4.7-10.7 8-12 10-1.3 2-2 5.7-2 11zm100-26v40h399900v-40z",
    leftharpoonplus: "M0 267c.7 5.3 3 10 7 14h399993v-40H93c3.3-3.3 10.2-9.5\n 20.5-18.5s17.8-15.8 22.5-20.5c50.7-52 88-110.3 112-175 4-11.3 5-18.3 3-21-1.3\n-4-7.3-6-18-6-8 0-13 .7-15 2s-4.7 6.7-8 16c-42 98.7-107.3 174.7-196 228-6.7 4.7\n-10.7 8-12 10-1.3 2-2 5.7-2 11zm100-26v40h399900v-40zM0 435v40h400000v-40z\nm0 0v40h400000v-40z",
    leftharpoondown: "M7 241c-4 4-6.333 8.667-7 14 0 5.333.667 9 2 11s5.333\n 5.333 12 10c90.667 54 156 130 196 228 3.333 10.667 6.333 16.333 9 17 2 .667 5\n 1 9 1h5c10.667 0 16.667-2 18-6 2-2.667 1-9.667-3-21-32-87.333-82.667-157.667\n-152-211l-3-3h399907v-40zM93 281 H400000 v-40L7 241z",
    leftharpoondownplus: "M7 435c-4 4-6.3 8.7-7 14 0 5.3.7 9 2 11s5.3 5.3 12\n 10c90.7 54 156 130 196 228 3.3 10.7 6.3 16.3 9 17 2 .7 5 1 9 1h5c10.7 0 16.7\n-2 18-6 2-2.7 1-9.7-3-21-32-87.3-82.7-157.7-152-211l-3-3h399907v-40H7zm93 0\nv40h399900v-40zM0 241v40h399900v-40zm0 0v40h399900v-40z",
    // hook is from glyph U+21A9 in font KaTeX Main
    lefthook: "M400000 281 H103s-33-11.2-61-33.5S0 197.3 0 164s14.2-61.2 42.5\n-83.5C70.8 58.2 104 47 142 47 c16.7 0 25 6.7 25 20 0 12-8.7 18.7-26 20-40 3.3\n-68.7 15.7-86 37-10 12-15 25.3-15 40 0 22.7 9.8 40.7 29.5 54 19.7 13.3 43.5 21\n 71.5 23h399859zM103 281v-40h399897v40z",
    leftlinesegment: doubleBrushStroke("M40 281 V428 H0 V94 H40 V241 H400000 v40z"),
    leftbracketunder: doubleBrushStroke("M0 0 h120 V290 H399995 v120 H0z"),
    leftbracketover: doubleBrushStroke("M0 440 h120 V150 H399995 v-120 H0z"),
    leftmapsto: doubleBrushStroke("M40 281 V448H0V74H40V241H400000v40z"),
    // tofrom is from glyph U+21C4 in font KaTeX AMS Regular
    leftToFrom: "M0 147h400000v40H0zm0 214c68 40 115.7 95.7 143 167h22c15.3 0 23\n-.3 23-1 0-1.3-5.3-13.7-16-37-18-35.3-41.3-69-70-101l-7-8h399905v-40H95l7-8\nc28.7-32 52-65.7 70-101 10.7-23.3 16-35.7 16-37 0-.7-7.7-1-23-1h-22C115.7 265.3\n 68 321 0 361zm0-174v-40h399900v40zm100 154v40h399900v-40z",
    longequal: doubleBrushStroke("M0 50 h400000 v40H0z m0 194h40000v40H0z"),
    midbrace: "M200428 334\nc-100.7-8.3-195.3-44-280-108-55.3-42-101.7-93-139-153l-9-14c-2.7 4-5.7 8.7-9 14\n-53.3 86.7-123.7 153-211 199-66.7 36-137.3 56.3-212 62H0V214h199568c178.3-11.7\n 311.7-78.3 403-201 6-8 9.7-12 11-12 .7-.7 6.7-1 18-1s17.3.3 18 1c1.3 0 5 4 11\n 12 44.7 59.3 101.3 106.3 170 141s145.3 54.3 229 60h199572v120z",
    midbraceunder: "M199572 214\nc100.7 8.3 195.3 44 280 108 55.3 42 101.7 93 139 153l9 14c2.7-4 5.7-8.7 9-14\n 53.3-86.7 123.7-153 211-199 66.7-36 137.3-56.3 212-62h199568v120H200432c-178.3\n 11.7-311.7 78.3-403 201-6 8-9.7 12-11 12-.7.7-6.7 1-18 1s-17.3-.3-18-1c-1.3 0\n-5-4-11-12-44.7-59.3-101.3-106.3-170-141s-145.3-54.3-229-60H0V214z",
    oiintSize1: "M512.6 71.6c272.6 0 320.3 106.8 320.3 178.2 0 70.8-47.7 177.6\n-320.3 177.6S193.1 320.6 193.1 249.8c0-71.4 46.9-178.2 319.5-178.2z\nm368.1 178.2c0-86.4-60.9-215.4-368.1-215.4-306.4 0-367.3 129-367.3 215.4 0 85.8\n60.9 214.8 367.3 214.8 307.2 0 368.1-129 368.1-214.8z",
    oiintSize2: "M757.8 100.1c384.7 0 451.1 137.6 451.1 230 0 91.3-66.4 228.8\n-451.1 228.8-386.3 0-452.7-137.5-452.7-228.8 0-92.4 66.4-230 452.7-230z\nm502.4 230c0-111.2-82.4-277.2-502.4-277.2s-504 166-504 277.2\nc0 110 84 276 504 276s502.4-166 502.4-276z",
    oiiintSize1: "M681.4 71.6c408.9 0 480.5 106.8 480.5 178.2 0 70.8-71.6 177.6\n-480.5 177.6S202.1 320.6 202.1 249.8c0-71.4 70.5-178.2 479.3-178.2z\nm525.8 178.2c0-86.4-86.8-215.4-525.7-215.4-437.9 0-524.7 129-524.7 215.4 0\n85.8 86.8 214.8 524.7 214.8 438.9 0 525.7-129 525.7-214.8z",
    oiiintSize2: "M1021.2 53c603.6 0 707.8 165.8 707.8 277.2 0 110-104.2 275.8\n-707.8 275.8-606 0-710.2-165.8-710.2-275.8C311 218.8 415.2 53 1021.2 53z\nm770.4 277.1c0-131.2-126.4-327.6-770.5-327.6S248.4 198.9 248.4 330.1\nc0 130 128.8 326.4 772.7 326.4s770.5-196.4 770.5-326.4z",
    rightarrow: "M0 241v40h399891c-47.3 35.3-84 78-110 128\n-16.7 32-27.7 63.7-33 95 0 1.3-.2 2.7-.5 4-.3 1.3-.5 2.3-.5 3 0 7.3 6.7 11 20\n 11 8 0 13.2-.8 15.5-2.5 2.3-1.7 4.2-5.5 5.5-11.5 2-13.3 5.7-27 11-41 14.7-44.7\n 39-84.5 73-119.5s73.7-60.2 119-75.5c6-2 9-5.7 9-11s-3-9-9-11c-45.3-15.3-85\n-40.5-119-75.5s-58.3-74.8-73-119.5c-4.7-14-8.3-27.3-11-40-1.3-6.7-3.2-10.8-5.5\n-12.5-2.3-1.7-7.5-2.5-15.5-2.5-14 0-21 3.7-21 11 0 2 2 10.3 6 25 20.7 83.3 67\n 151.7 139 205zm0 0v40h399900v-40z",
    rightbrace: "M400000 542l\n-6 6h-17c-12.7 0-19.3-.3-20-1-4-4-7.3-8.3-10-13-35.3-51.3-80.8-93.8-136.5-127.5\ns-117.2-55.8-184.5-66.5c-.7 0-2-.3-4-1-18.7-2.7-76-4.3-172-5H0V214h399571l6 1\nc124.7 8 235 61.7 331 161 31.3 33.3 59.7 72.7 85 118l7 13v35z",
    rightbraceunder: "M399994 0l6 6v35l-6 11c-56 104-135.3 181.3-238 232-57.3\n 28.7-117 45-179 50H-300V214h399897c43.3-7 81-15 113-26 100.7-33 179.7-91 237\n-174 2.7-5 6-9 10-13 .7-1 7.3-1 20-1h17z",
    rightgroup: "M0 80h399565c371 0 266.7 149.4 414 180 5.9 1.2 18 0 18 0 2 0\n 3-1 3-3v-38c-76-158-257-219-435-219H0z",
    rightgroupunder: "M0 262h399565c371 0 266.7-149.4 414-180 5.9-1.2 18 0 18\n 0 2 0 3 1 3 3v38c-76 158-257 219-435 219H0z",
    rightharpoon: "M0 241v40h399993c4.7-4.7 7-9.3 7-14 0-9.3\n-3.7-15.3-11-18-92.7-56.7-159-133.7-199-231-3.3-9.3-6-14.7-8-16-2-1.3-7-2-15-2\n-10.7 0-16.7 2-18 6-2 2.7-1 9.7 3 21 15.3 42 36.7 81.8 64 119.5 27.3 37.7 58\n 69.2 92 94.5zm0 0v40h399900v-40z",
    rightharpoonplus: "M0 241v40h399993c4.7-4.7 7-9.3 7-14 0-9.3-3.7-15.3-11\n-18-92.7-56.7-159-133.7-199-231-3.3-9.3-6-14.7-8-16-2-1.3-7-2-15-2-10.7 0-16.7\n 2-18 6-2 2.7-1 9.7 3 21 15.3 42 36.7 81.8 64 119.5 27.3 37.7 58 69.2 92 94.5z\nm0 0v40h399900v-40z m100 194v40h399900v-40zm0 0v40h399900v-40z",
    rightharpoondown: "M399747 511c0 7.3 6.7 11 20 11 8 0 13-.8 15-2.5s4.7-6.8\n 8-15.5c40-94 99.3-166.3 178-217 13.3-8 20.3-12.3 21-13 5.3-3.3 8.5-5.8 9.5\n-7.5 1-1.7 1.5-5.2 1.5-10.5s-2.3-10.3-7-15H0v40h399908c-34 25.3-64.7 57-92 95\n-27.3 38-48.7 77.7-64 119-3.3 8.7-5 14-5 16zM0 241v40h399900v-40z",
    rightharpoondownplus: "M399747 705c0 7.3 6.7 11 20 11 8 0 13-.8\n 15-2.5s4.7-6.8 8-15.5c40-94 99.3-166.3 178-217 13.3-8 20.3-12.3 21-13 5.3-3.3\n 8.5-5.8 9.5-7.5 1-1.7 1.5-5.2 1.5-10.5s-2.3-10.3-7-15H0v40h399908c-34 25.3\n-64.7 57-92 95-27.3 38-48.7 77.7-64 119-3.3 8.7-5 14-5 16zM0 435v40h399900v-40z\nm0-194v40h400000v-40zm0 0v40h400000v-40z",
    righthook: "M399859 241c-764 0 0 0 0 0 40-3.3 68.7-15.7 86-37 10-12 15-25.3\n 15-40 0-22.7-9.8-40.7-29.5-54-19.7-13.3-43.5-21-71.5-23-17.3-1.3-26-8-26-20 0\n-13.3 8.7-20 26-20 38 0 71 11.2 99 33.5 0 0 7 5.6 21 16.7 14 11.2 21 33.5 21\n 66.8s-14 61.2-42 83.5c-28 22.3-61 33.5-99 33.5L0 241z M0 281v-40h399859v40z",
    rightlinesegment: doubleBrushStroke("M399960 241 V94 h40 V428 h-40 V281 H0 v-40z"),
    rightbracketunder: doubleBrushStroke("M399995 0 h-120 V290 H0 v120 H400000z"),
    rightbracketover: doubleBrushStroke("M399995 440 h-120 V150 H0 v-120 H399995z"),
    rightToFrom: "M400000 167c-70.7-42-118-97.7-142-167h-23c-15.3 0-23 .3-23\n 1 0 1.3 5.3 13.7 16 37 18 35.3 41.3 69 70 101l7 8H0v40h399905l-7 8c-28.7 32\n-52 65.7-70 101-10.7 23.3-16 35.7-16 37 0 .7 7.7 1 23 1h23c24-69.3 71.3-125 142\n-167z M100 147v40h399900v-40zM0 341v40h399900v-40z",
    // twoheadleftarrow is from glyph U+219E in font KaTeX AMS Regular
    twoheadleftarrow: "M0 167c68 40\n 115.7 95.7 143 167h22c15.3 0 23-.3 23-1 0-1.3-5.3-13.7-16-37-18-35.3-41.3-69\n-70-101l-7-8h125l9 7c50.7 39.3 85 86 103 140h46c0-4.7-6.3-18.7-19-42-18-35.3\n-40-67.3-66-96l-9-9h399716v-40H284l9-9c26-28.7 48-60.7 66-96 12.7-23.333 19\n-37.333 19-42h-46c-18 54-52.3 100.7-103 140l-9 7H95l7-8c28.7-32 52-65.7 70-101\n 10.7-23.333 16-35.7 16-37 0-.7-7.7-1-23-1h-22C115.7 71.3 68 127 0 167z",
    twoheadrightarrow: "M400000 167\nc-68-40-115.7-95.7-143-167h-22c-15.3 0-23 .3-23 1 0 1.3 5.3 13.7 16 37 18 35.3\n 41.3 69 70 101l7 8h-125l-9-7c-50.7-39.3-85-86-103-140h-46c0 4.7 6.3 18.7 19 42\n 18 35.3 40 67.3 66 96l9 9H0v40h399716l-9 9c-26 28.7-48 60.7-66 96-12.7 23.333\n-19 37.333-19 42h46c18-54 52.3-100.7 103-140l9-7h125l-7 8c-28.7 32-52 65.7-70\n 101-10.7 23.333-16 35.7-16 37 0 .7 7.7 1 23 1h22c27.3-71.3 75-127 143-167z",
    // tilde1 is a modified version of a glyph from the MnSymbol package
    tilde1: "M200 55.538c-77 0-168 73.953-177 73.953-3 0-7\n-2.175-9-5.437L2 97c-1-2-2-4-2-6 0-4 2-7 5-9l20-12C116 12 171 0 207 0c86 0\n 114 68 191 68 78 0 168-68 177-68 4 0 7 2 9 5l12 19c1 2.175 2 4.35 2 6.525 0\n 4.35-2 7.613-5 9.788l-19 13.05c-92 63.077-116.937 75.308-183 76.128\n-68.267.847-113-73.952-191-73.952z",
    // ditto tilde2, tilde3, & tilde4
    tilde2: "M344 55.266c-142 0-300.638 81.316-311.5 86.418\n-8.01 3.762-22.5 10.91-23.5 5.562L1 120c-1-2-1-3-1-4 0-5 3-9 8-10l18.4-9C160.9\n 31.9 283 0 358 0c148 0 188 122 331 122s314-97 326-97c4 0 8 2 10 7l7 21.114\nc1 2.14 1 3.21 1 4.28 0 5.347-3 9.626-7 10.696l-22.3 12.622C852.6 158.372 751\n 181.476 676 181.476c-149 0-189-126.21-332-126.21z",
    tilde3: "M786 59C457 59 32 175.242 13 175.242c-6 0-10-3.457\n-11-10.37L.15 138c-1-7 3-12 10-13l19.2-6.4C378.4 40.7 634.3 0 804.3 0c337 0\n 411.8 157 746.8 157 328 0 754-112 773-112 5 0 10 3 11 9l1 14.075c1 8.066-.697\n 16.595-6.697 17.492l-21.052 7.31c-367.9 98.146-609.15 122.696-778.15 122.696\n -338 0-409-156.573-744-156.573z",
    tilde4: "M786 58C457 58 32 177.487 13 177.487c-6 0-10-3.345\n-11-10.035L.15 143c-1-7 3-12 10-13l22-6.7C381.2 35 637.15 0 807.15 0c337 0 409\n 177 744 177 328 0 754-127 773-127 5 0 10 3 11 9l1 14.794c1 7.805-3 13.38-9\n 14.495l-20.7 5.574c-366.85 99.79-607.3 139.372-776.3 139.372-338 0-409\n -175.236-744-175.236z",
    // vec is from glyph U+20D7 in font KaTeX Main
    vec: "M377 20c0-5.333 1.833-10 5.5-14S391 0 397 0c4.667 0 8.667 1.667 12 5\n3.333 2.667 6.667 9 10 19 6.667 24.667 20.333 43.667 41 57 7.333 4.667 11\n10.667 11 18 0 6-1 10-3 12s-6.667 5-14 9c-28.667 14.667-53.667 35.667-75 63\n-1.333 1.333-3.167 3.5-5.5 6.5s-4 4.833-5 5.5c-1 .667-2.5 1.333-4.5 2s-4.333 1\n-7 1c-4.667 0-9.167-1.833-13.5-5.5S337 184 337 178c0-12.667 15.667-32.333 47-59\nH213l-171-1c-8.667-6-13-12.333-13-19 0-4.667 4.333-11.333 13-20h359\nc-16-25.333-24-45-24-59z",
    // widehat1 is a modified version of a glyph from the MnSymbol package
    widehat1: "M529 0h5l519 115c5 1 9 5 9 10 0 1-1 2-1 3l-4 22\nc-1 5-5 9-11 9h-2L532 67 19 159h-2c-5 0-9-4-11-9l-5-22c-1-6 2-12 8-13z",
    // ditto widehat2, widehat3, & widehat4
    widehat2: "M1181 0h2l1171 176c6 0 10 5 10 11l-2 23c-1 6-5 10\n-11 10h-1L1182 67 15 220h-1c-6 0-10-4-11-10l-2-23c-1-6 4-11 10-11z",
    widehat3: "M1181 0h2l1171 236c6 0 10 5 10 11l-2 23c-1 6-5 10\n-11 10h-1L1182 67 15 280h-1c-6 0-10-4-11-10l-2-23c-1-6 4-11 10-11z",
    widehat4: "M1181 0h2l1171 296c6 0 10 5 10 11l-2 23c-1 6-5 10\n-11 10h-1L1182 67 15 340h-1c-6 0-10-4-11-10l-2-23c-1-6 4-11 10-11z",
    // widecheck paths are all inverted versions of widehat
    widecheck1: "M529,159h5l519,-115c5,-1,9,-5,9,-10c0,-1,-1,-2,-1,-3l-4,-22c-1,\n-5,-5,-9,-11,-9h-2l-512,92l-513,-92h-2c-5,0,-9,4,-11,9l-5,22c-1,6,2,12,8,13z",
    widecheck2: "M1181,220h2l1171,-176c6,0,10,-5,10,-11l-2,-23c-1,-6,-5,-10,\n-11,-10h-1l-1168,153l-1167,-153h-1c-6,0,-10,4,-11,10l-2,23c-1,6,4,11,10,11z",
    widecheck3: "M1181,280h2l1171,-236c6,0,10,-5,10,-11l-2,-23c-1,-6,-5,-10,\n-11,-10h-1l-1168,213l-1167,-213h-1c-6,0,-10,4,-11,10l-2,23c-1,6,4,11,10,11z",
    widecheck4: "M1181,340h2l1171,-296c6,0,10,-5,10,-11l-2,-23c-1,-6,-5,-10,\n-11,-10h-1l-1168,273l-1167,-273h-1c-6,0,-10,4,-11,10l-2,23c-1,6,4,11,10,11z",
    // The next ten paths support reaction arrows from the mhchem package.
    // Arrows for \ce{<-->} are offset from xAxis by 0.22ex, per mhchem in LaTeX
    // baraboveleftarrow is mostly from glyph U+2190 in font KaTeX Main
    baraboveleftarrow: "M400000 620h-399890l3 -3c68.7 -52.7 113.7 -120 135 -202\nc4 -14.7 6 -23 6 -25c0 -7.3 -7 -11 -21 -11c-8 0 -13.2 0.8 -15.5 2.5\nc-2.3 1.7 -4.2 5.8 -5.5 12.5c-1.3 4.7 -2.7 10.3 -4 17c-12 48.7 -34.8 92 -68.5 130\ns-74.2 66.3 -121.5 85c-10 4 -16 7.7 -18 11c0 8.7 6 14.3 18 17c47.3 18.7 87.8 47\n121.5 85s56.5 81.3 68.5 130c0.7 2 1.3 5 2 9s1.2 6.7 1.5 8c0.3 1.3 1 3.3 2 6\ns2.2 4.5 3.5 5.5c1.3 1 3.3 1.8 6 2.5s6 1 10 1c14 0 21 -3.7 21 -11\nc0 -2 -2 -10.3 -6 -25c-20 -79.3 -65 -146.7 -135 -202l-3 -3h399890z\nM100 620v40h399900v-40z M0 241v40h399900v-40zM0 241v40h399900v-40z",
    // rightarrowabovebar is mostly from glyph U+2192, KaTeX Main
    rightarrowabovebar: "M0 241v40h399891c-47.3 35.3-84 78-110 128-16.7 32\n-27.7 63.7-33 95 0 1.3-.2 2.7-.5 4-.3 1.3-.5 2.3-.5 3 0 7.3 6.7 11 20 11 8 0\n13.2-.8 15.5-2.5 2.3-1.7 4.2-5.5 5.5-11.5 2-13.3 5.7-27 11-41 14.7-44.7 39\n-84.5 73-119.5s73.7-60.2 119-75.5c6-2 9-5.7 9-11s-3-9-9-11c-45.3-15.3-85-40.5\n-119-75.5s-58.3-74.8-73-119.5c-4.7-14-8.3-27.3-11-40-1.3-6.7-3.2-10.8-5.5\n-12.5-2.3-1.7-7.5-2.5-15.5-2.5-14 0-21 3.7-21 11 0 2 2 10.3 6 25 20.7 83.3 67\n151.7 139 205zm96 379h399894v40H0zm0 0h399904v40H0z",
    // The short left harpoon has 0.5em (i.e. 500 units) kern on the left end.
    // Ref from mhchem.sty: \rlap{\raisebox{-.22ex}{$\kern0.5em
    baraboveshortleftharpoon: "M507,435c-4,4,-6.3,8.7,-7,14c0,5.3,0.7,9,2,11\nc1.3,2,5.3,5.3,12,10c90.7,54,156,130,196,228c3.3,10.7,6.3,16.3,9,17\nc2,0.7,5,1,9,1c0,0,5,0,5,0c10.7,0,16.7,-2,18,-6c2,-2.7,1,-9.7,-3,-21\nc-32,-87.3,-82.7,-157.7,-152,-211c0,0,-3,-3,-3,-3l399351,0l0,-40\nc-398570,0,-399437,0,-399437,0z M593 435 v40 H399500 v-40z\nM0 281 v-40 H399908 v40z M0 281 v-40 H399908 v40z",
    rightharpoonaboveshortbar: "M0,241 l0,40c399126,0,399993,0,399993,0\nc4.7,-4.7,7,-9.3,7,-14c0,-9.3,-3.7,-15.3,-11,-18c-92.7,-56.7,-159,-133.7,-199,\n-231c-3.3,-9.3,-6,-14.7,-8,-16c-2,-1.3,-7,-2,-15,-2c-10.7,0,-16.7,2,-18,6\nc-2,2.7,-1,9.7,3,21c15.3,42,36.7,81.8,64,119.5c27.3,37.7,58,69.2,92,94.5z\nM0 241 v40 H399908 v-40z M0 475 v-40 H399500 v40z M0 475 v-40 H399500 v40z",
    shortbaraboveleftharpoon: "M7,435c-4,4,-6.3,8.7,-7,14c0,5.3,0.7,9,2,11\nc1.3,2,5.3,5.3,12,10c90.7,54,156,130,196,228c3.3,10.7,6.3,16.3,9,17c2,0.7,5,1,9,\n1c0,0,5,0,5,0c10.7,0,16.7,-2,18,-6c2,-2.7,1,-9.7,-3,-21c-32,-87.3,-82.7,-157.7,\n-152,-211c0,0,-3,-3,-3,-3l399907,0l0,-40c-399126,0,-399993,0,-399993,0z\nM93 435 v40 H400000 v-40z M500 241 v40 H400000 v-40z M500 241 v40 H400000 v-40z",
    shortrightharpoonabovebar: "M53,241l0,40c398570,0,399437,0,399437,0\nc4.7,-4.7,7,-9.3,7,-14c0,-9.3,-3.7,-15.3,-11,-18c-92.7,-56.7,-159,-133.7,-199,\n-231c-3.3,-9.3,-6,-14.7,-8,-16c-2,-1.3,-7,-2,-15,-2c-10.7,0,-16.7,2,-18,6\nc-2,2.7,-1,9.7,3,21c15.3,42,36.7,81.8,64,119.5c27.3,37.7,58,69.2,92,94.5z\nM500 241 v40 H399408 v-40z M500 435 v40 H400000 v-40z"
  };
  var tallDelim = function tallDelim2(label, midHeight) {
    switch (label) {
      case "lbrack":
        return "M403 1759 V84 H666 V0 H319 V1759 v" + midHeight + " v1759 v84 h347 v-84\nH403z M403 1759 V0 H319 V1759 v" + midHeight + " v1759 v84 h84z";
      case "rbrack":
        return "M347 1759 V0 H0 V84 H263 V1759 v" + midHeight + " v1759 H0 v84 H347z\nM347 1759 V0 H263 V1759 v" + midHeight + " v1759 h84z";
      case "vert":
        return "M145 15 v585 v" + midHeight + " v585 c2.667,10,9.667,15,21,15\nc10,0,16.667,-5,20,-15 v-585 v" + -midHeight + " v-585 c-2.667,-10,-9.667,-15,-21,-15\nc-10,0,-16.667,5,-20,15z M188 15 H145 v585 v" + midHeight + " v585 h43z";
      case "doublevert":
        return "M145 15 v585 v" + midHeight + " v585 c2.667,10,9.667,15,21,15\nc10,0,16.667,-5,20,-15 v-585 v" + -midHeight + " v-585 c-2.667,-10,-9.667,-15,-21,-15\nc-10,0,-16.667,5,-20,15z M188 15 H145 v585 v" + midHeight + " v585 h43z\nM367 15 v585 v" + midHeight + " v585 c2.667,10,9.667,15,21,15\nc10,0,16.667,-5,20,-15 v-585 v" + -midHeight + " v-585 c-2.667,-10,-9.667,-15,-21,-15\nc-10,0,-16.667,5,-20,15z M410 15 H367 v585 v" + midHeight + " v585 h43z";
      case "lfloor":
        return "M319 602 V0 H403 V602 v" + midHeight + " v1715 h263 v84 H319z\nMM319 602 V0 H403 V602 v" + midHeight + " v1715 H319z";
      case "rfloor":
        return "M319 602 V0 H403 V602 v" + midHeight + " v1799 H0 v-84 H319z\nMM319 602 V0 H403 V602 v" + midHeight + " v1715 H319z";
      case "lceil":
        return "M403 1759 V84 H666 V0 H319 V1759 v" + midHeight + " v602 h84z\nM403 1759 V0 H319 V1759 v" + midHeight + " v602 h84z";
      case "rceil":
        return "M347 1759 V0 H0 V84 H263 V1759 v" + midHeight + " v602 h84z\nM347 1759 V0 h-84 V1759 v" + midHeight + " v602 h84z";
      case "lparen":
        return "M863,9c0,-2,-2,-5,-6,-9c0,0,-17,0,-17,0c-12.7,0,-19.3,0.3,-20,1\nc-5.3,5.3,-10.3,11,-15,17c-242.7,294.7,-395.3,682,-458,1162c-21.3,163.3,-33.3,349,\n-36,557 l0," + (midHeight + 84) + "c0.2,6,0,26,0,60c2,159.3,10,310.7,24,454c53.3,528,210,\n949.7,470,1265c4.7,6,9.7,11.7,15,17c0.7,0.7,7,1,19,1c0,0,18,0,18,0c4,-4,6,-7,6,-9\nc0,-2.7,-3.3,-8.7,-10,-18c-135.3,-192.7,-235.5,-414.3,-300.5,-665c-65,-250.7,-102.5,\n-544.7,-112.5,-882c-2,-104,-3,-167,-3,-189\nl0,-" + (midHeight + 92) + "c0,-162.7,5.7,-314,17,-454c20.7,-272,63.7,-513,129,-723c65.3,\n-210,155.3,-396.3,270,-559c6.7,-9.3,10,-15.3,10,-18z";
      case "rparen":
        return "M76,0c-16.7,0,-25,3,-25,9c0,2,2,6.3,6,13c21.3,28.7,42.3,60.3,\n63,95c96.7,156.7,172.8,332.5,228.5,527.5c55.7,195,92.8,416.5,111.5,664.5\nc11.3,139.3,17,290.7,17,454c0,28,1.7,43,3.3,45l0," + (midHeight + 9) + "\nc-3,4,-3.3,16.7,-3.3,38c0,162,-5.7,313.7,-17,455c-18.7,248,-55.8,469.3,-111.5,664\nc-55.7,194.7,-131.8,370.3,-228.5,527c-20.7,34.7,-41.7,66.3,-63,95c-2,3.3,-4,7,-6,11\nc0,7.3,5.7,11,17,11c0,0,11,0,11,0c9.3,0,14.3,-0.3,15,-1c5.3,-5.3,10.3,-11,15,-17\nc242.7,-294.7,395.3,-681.7,458,-1161c21.3,-164.7,33.3,-350.7,36,-558\nl0,-" + (midHeight + 144) + "c-2,-159.3,-10,-310.7,-24,-454c-53.3,-528,-210,-949.7,\n-470,-1265c-4.7,-6,-9.7,-11.7,-15,-17c-0.7,-0.7,-6.7,-1,-18,-1z";
      default:
        throw new Error("Unknown stretchy delimiter.");
    }
  };
  function isMathDomNode(node) {
    return "toText" in node;
  }
  var DocumentFragment = class {
    // Never used; needed for satisfying interface.
    constructor(children) {
      this.children = void 0;
      this.classes = void 0;
      this.height = void 0;
      this.depth = void 0;
      this.maxFontSize = void 0;
      this.style = void 0;
      this.children = children;
      this.classes = [];
      this.height = 0;
      this.depth = 0;
      this.maxFontSize = 0;
      this.style = {};
    }
    hasClass(className) {
      return this.classes.includes(className);
    }
    /** Convert the fragment into a node. */
    toNode() {
      var frag = document.createDocumentFragment();
      for (var i3 = 0; i3 < this.children.length; i3++) {
        frag.appendChild(this.children[i3].toNode());
      }
      return frag;
    }
    /** Convert the fragment into HTML markup. */
    toMarkup() {
      var markup = "";
      for (var i3 = 0; i3 < this.children.length; i3++) {
        markup += this.children[i3].toMarkup();
      }
      return markup;
    }
    /**
     * Converts the math node into a string, similar to innerText. Applies to
     * MathDomNode's only.
     */
    toText() {
      return this.children.map((child) => {
        if (isMathDomNode(child)) {
          return child.toText();
        }
        throw new Error("Expected MathDomNode with toText, got " + child.constructor.name);
      }).join("");
    }
  };
  var ptPerUnit = {
    // https://en.wikibooks.org/wiki/LaTeX/Lengths and
    // https://tex.stackexchange.com/a/8263
    "pt": 1,
    // TeX point
    "mm": 7227 / 2540,
    // millimeter
    "cm": 7227 / 254,
    // centimeter
    "in": 72.27,
    // inch
    "bp": 803 / 800,
    // big (PostScript) points
    "pc": 12,
    // pica
    "dd": 1238 / 1157,
    // didot
    "cc": 14856 / 1157,
    // cicero (12 didot)
    "nd": 685 / 642,
    // new didot
    "nc": 1370 / 107,
    // new cicero (12 new didot)
    "sp": 1 / 65536,
    // scaled point (TeX's internal smallest unit)
    // https://tex.stackexchange.com/a/41371
    "px": 803 / 800
    // \pdfpxdimen defaults to 1 bp in pdfTeX and LuaTeX
  };
  var relativeUnit = {
    "ex": true,
    "em": true,
    "mu": true
  };
  var validUnit = function validUnit2(unit) {
    if (typeof unit !== "string") {
      unit = unit.unit;
    }
    return unit in ptPerUnit || unit in relativeUnit || unit === "ex";
  };
  var calculateSize = function calculateSize2(sizeValue, options) {
    var scale;
    if (sizeValue.unit in ptPerUnit) {
      scale = ptPerUnit[sizeValue.unit] / options.fontMetrics().ptPerEm / options.sizeMultiplier;
    } else if (sizeValue.unit === "mu") {
      scale = options.fontMetrics().cssEmPerMu;
    } else {
      var unitOptions;
      if (options.style.isTight()) {
        unitOptions = options.havingStyle(options.style.text());
      } else {
        unitOptions = options;
      }
      if (sizeValue.unit === "ex") {
        scale = unitOptions.fontMetrics().xHeight;
      } else if (sizeValue.unit === "em") {
        scale = unitOptions.fontMetrics().quad;
      } else {
        throw new ParseError("Invalid unit: '" + sizeValue.unit + "'");
      }
      if (unitOptions !== options) {
        scale *= unitOptions.sizeMultiplier / options.sizeMultiplier;
      }
    }
    return Math.min(sizeValue.number * scale, options.maxSize);
  };
  var makeEm = function makeEm2(n) {
    return +n.toFixed(4) + "em";
  };
  var createClass = function createClass2(classes) {
    return classes.filter((cls) => cls).join(" ");
  };
  var cssStyleToString = function cssStyleToString2(style) {
    var styles2 = "";
    for (var key of Object.keys(style)) {
      var value = style[key];
      if (value !== void 0) {
        styles2 += hyphenate(key) + ":" + value + ";";
      }
    }
    return styles2;
  };
  var initNode = function initNode2(classes, options, style) {
    this.classes = classes || [];
    this.attributes = {};
    this.height = 0;
    this.depth = 0;
    this.maxFontSize = 0;
    this.style = style || {};
    if (options) {
      if (options.style.isTight()) {
        this.classes.push("mtight");
      }
      var color = options.getColor();
      if (color) {
        this.style.color = color;
      }
    }
  };
  var toNode = function toNode2(tagName) {
    var node = document.createElement(tagName);
    node.className = createClass(this.classes);
    Object.assign(node.style, this.style);
    for (var attr of Object.keys(this.attributes)) {
      node.setAttribute(attr, this.attributes[attr]);
    }
    for (var i3 = 0; i3 < this.children.length; i3++) {
      node.appendChild(this.children[i3].toNode());
    }
    return node;
  };
  var invalidAttributeNameRegex = /[\s"'>/=\x00-\x1f]/;
  var toMarkup = function toMarkup2(tagName) {
    var markup = "<" + tagName;
    if (this.classes.length) {
      markup += ' class="' + escape(createClass(this.classes)) + '"';
    }
    var styles2 = cssStyleToString(this.style);
    if (styles2) {
      markup += ' style="' + escape(styles2) + '"';
    }
    for (var attr of Object.keys(this.attributes)) {
      if (invalidAttributeNameRegex.test(attr)) {
        throw new ParseError("Invalid attribute name '" + attr + "'");
      }
      markup += " " + attr + '="' + escape(this.attributes[attr]) + '"';
    }
    markup += ">";
    for (var i3 = 0; i3 < this.children.length; i3++) {
      markup += this.children[i3].toMarkup();
    }
    markup += "</" + tagName + ">";
    return markup;
  };
  var Span = class {
    constructor(classes, children, options, style) {
      this.children = void 0;
      this.attributes = void 0;
      this.classes = void 0;
      this.height = void 0;
      this.depth = void 0;
      this.width = void 0;
      this.maxFontSize = void 0;
      this.style = void 0;
      this.italic = void 0;
      initNode.call(this, classes, options, style);
      this.children = children || [];
    }
    /**
     * Sets an arbitrary attribute on the span. Warning: use this wisely. Not
     * all browsers support attributes the same, and having too many custom
     * attributes is probably bad.
     */
    setAttribute(attribute, value) {
      this.attributes[attribute] = value;
    }
    hasClass(className) {
      return this.classes.includes(className);
    }
    toNode() {
      return toNode.call(this, "span");
    }
    toMarkup() {
      return toMarkup.call(this, "span");
    }
  };
  var Anchor = class {
    constructor(href, classes, children, options) {
      this.children = void 0;
      this.attributes = void 0;
      this.classes = void 0;
      this.height = void 0;
      this.depth = void 0;
      this.maxFontSize = void 0;
      this.style = void 0;
      initNode.call(this, classes, options);
      this.children = children || [];
      this.setAttribute("href", href);
    }
    setAttribute(attribute, value) {
      this.attributes[attribute] = value;
    }
    hasClass(className) {
      return this.classes.includes(className);
    }
    toNode() {
      return toNode.call(this, "a");
    }
    toMarkup() {
      return toMarkup.call(this, "a");
    }
  };
  var Img = class {
    constructor(src, alt, style) {
      this.src = void 0;
      this.alt = void 0;
      this.classes = void 0;
      this.height = void 0;
      this.depth = void 0;
      this.maxFontSize = void 0;
      this.style = void 0;
      this.alt = alt;
      this.src = src;
      this.classes = ["mord"];
      this.height = 0;
      this.depth = 0;
      this.maxFontSize = 0;
      this.style = style;
    }
    hasClass(className) {
      return this.classes.includes(className);
    }
    toNode() {
      var node = document.createElement("img");
      node.src = this.src;
      node.alt = this.alt;
      node.className = "mord";
      Object.assign(node.style, this.style);
      return node;
    }
    toMarkup() {
      var markup = '<img src="' + escape(this.src) + '"' + (' alt="' + escape(this.alt) + '"');
      var styles2 = cssStyleToString(this.style);
      if (styles2) {
        markup += ' style="' + escape(styles2) + '"';
      }
      markup += "'/>";
      return markup;
    }
  };
  var iCombinations = {
    "\xEE": "\u0131\u0302",
    "\xEF": "\u0131\u0308",
    "\xED": "\u0131\u0301",
    // 'ī': '\u0131\u0304', // enable when we add Extended Latin
    "\xEC": "\u0131\u0300"
  };
  var SymbolNode = class {
    constructor(text2, height, depth, italic2, skew, width, classes, style) {
      this.text = void 0;
      this.height = void 0;
      this.depth = void 0;
      this.italic = void 0;
      this.skew = void 0;
      this.width = void 0;
      this.maxFontSize = void 0;
      this.classes = void 0;
      this.style = void 0;
      this.text = text2;
      this.height = height || 0;
      this.depth = depth || 0;
      this.italic = italic2 || 0;
      this.skew = skew || 0;
      this.width = width || 0;
      this.classes = classes || [];
      this.style = style || {};
      this.maxFontSize = 0;
      var script2 = scriptFromCodepoint(this.text.charCodeAt(0));
      if (script2) {
        this.classes.push(script2 + "_fallback");
      }
      if (/[îïíì]/.test(this.text)) {
        this.text = iCombinations[this.text];
      }
    }
    hasClass(className) {
      return this.classes.includes(className);
    }
    /**
     * Creates a text node or span from a symbol node. Note that a span is only
     * created if it is needed.
     */
    toNode() {
      var node = document.createTextNode(this.text);
      var span = null;
      if (this.italic > 0) {
        span = document.createElement("span");
        span.style.marginRight = makeEm(this.italic);
      }
      if (this.classes.length > 0) {
        span = span || document.createElement("span");
        span.className = createClass(this.classes);
      }
      if (Object.keys(this.style).length > 0) {
        span = span || document.createElement("span");
        Object.assign(span.style, this.style);
      }
      if (span) {
        span.appendChild(node);
        return span;
      } else {
        return node;
      }
    }
    /**
     * Creates markup for a symbol node.
     */
    toMarkup() {
      var needsSpan = false;
      var markup = "<span";
      if (this.classes.length) {
        needsSpan = true;
        markup += ' class="';
        markup += escape(createClass(this.classes));
        markup += '"';
      }
      var styles2 = "";
      if (this.italic > 0) {
        styles2 += "margin-right:" + makeEm(this.italic) + ";";
      }
      styles2 += cssStyleToString(this.style);
      if (styles2) {
        needsSpan = true;
        markup += ' style="' + escape(styles2) + '"';
      }
      var escaped = escape(this.text);
      if (needsSpan) {
        markup += ">";
        markup += escaped;
        markup += "</span>";
        return markup;
      } else {
        return escaped;
      }
    }
  };
  var SvgNode = class {
    constructor(children, attributes) {
      this.children = void 0;
      this.attributes = void 0;
      this.children = children || [];
      this.attributes = attributes || {};
    }
    toNode() {
      var svgNS = "http://www.w3.org/2000/svg";
      var node = document.createElementNS(svgNS, "svg");
      for (var attr of Object.keys(this.attributes)) {
        node.setAttribute(attr, this.attributes[attr]);
      }
      for (var i3 = 0; i3 < this.children.length; i3++) {
        node.appendChild(this.children[i3].toNode());
      }
      return node;
    }
    toMarkup() {
      var markup = '<svg xmlns="http://www.w3.org/2000/svg"';
      for (var attr of Object.keys(this.attributes)) {
        markup += " " + attr + '="' + escape(this.attributes[attr]) + '"';
      }
      markup += ">";
      for (var i3 = 0; i3 < this.children.length; i3++) {
        markup += this.children[i3].toMarkup();
      }
      markup += "</svg>";
      return markup;
    }
  };
  var PathNode = class {
    constructor(pathName, alternate) {
      this.pathName = void 0;
      this.alternate = void 0;
      this.pathName = pathName;
      this.alternate = alternate;
    }
    toNode() {
      var svgNS = "http://www.w3.org/2000/svg";
      var node = document.createElementNS(svgNS, "path");
      if (this.alternate) {
        node.setAttribute("d", this.alternate);
      } else {
        node.setAttribute("d", path[this.pathName]);
      }
      return node;
    }
    toMarkup() {
      if (this.alternate) {
        return '<path d="' + escape(this.alternate) + '"/>';
      } else {
        return '<path d="' + escape(path[this.pathName]) + '"/>';
      }
    }
  };
  var LineNode = class {
    constructor(attributes) {
      this.attributes = void 0;
      this.attributes = attributes || {};
    }
    toNode() {
      var svgNS = "http://www.w3.org/2000/svg";
      var node = document.createElementNS(svgNS, "line");
      for (var attr of Object.keys(this.attributes)) {
        node.setAttribute(attr, this.attributes[attr]);
      }
      return node;
    }
    toMarkup() {
      var markup = "<line";
      for (var attr of Object.keys(this.attributes)) {
        markup += " " + attr + '="' + escape(this.attributes[attr]) + '"';
      }
      markup += "/>";
      return markup;
    }
  };
  function assertSymbolDomNode(group) {
    if (group instanceof SymbolNode) {
      return group;
    } else {
      throw new Error("Expected symbolNode but got " + String(group) + ".");
    }
  }
  function assertSpan(group) {
    if (group instanceof Span) {
      return group;
    } else {
      throw new Error("Expected span<HtmlDomNode> but got " + String(group) + ".");
    }
  }
  var hasHtmlDomChildren = (node) => node instanceof Span || node instanceof Anchor || node instanceof DocumentFragment;
  var fontMetricsData = {
    "AMS-Regular": {
      "32": [0, 0, 0, 0, 0.25],
      "65": [0, 0.68889, 0, 0, 0.72222],
      "66": [0, 0.68889, 0, 0, 0.66667],
      "67": [0, 0.68889, 0, 0, 0.72222],
      "68": [0, 0.68889, 0, 0, 0.72222],
      "69": [0, 0.68889, 0, 0, 0.66667],
      "70": [0, 0.68889, 0, 0, 0.61111],
      "71": [0, 0.68889, 0, 0, 0.77778],
      "72": [0, 0.68889, 0, 0, 0.77778],
      "73": [0, 0.68889, 0, 0, 0.38889],
      "74": [0.16667, 0.68889, 0, 0, 0.5],
      "75": [0, 0.68889, 0, 0, 0.77778],
      "76": [0, 0.68889, 0, 0, 0.66667],
      "77": [0, 0.68889, 0, 0, 0.94445],
      "78": [0, 0.68889, 0, 0, 0.72222],
      "79": [0.16667, 0.68889, 0, 0, 0.77778],
      "80": [0, 0.68889, 0, 0, 0.61111],
      "81": [0.16667, 0.68889, 0, 0, 0.77778],
      "82": [0, 0.68889, 0, 0, 0.72222],
      "83": [0, 0.68889, 0, 0, 0.55556],
      "84": [0, 0.68889, 0, 0, 0.66667],
      "85": [0, 0.68889, 0, 0, 0.72222],
      "86": [0, 0.68889, 0, 0, 0.72222],
      "87": [0, 0.68889, 0, 0, 1],
      "88": [0, 0.68889, 0, 0, 0.72222],
      "89": [0, 0.68889, 0, 0, 0.72222],
      "90": [0, 0.68889, 0, 0, 0.66667],
      "107": [0, 0.68889, 0, 0, 0.55556],
      "160": [0, 0, 0, 0, 0.25],
      "165": [0, 0.675, 0.025, 0, 0.75],
      "174": [0.15559, 0.69224, 0, 0, 0.94666],
      "240": [0, 0.68889, 0, 0, 0.55556],
      "295": [0, 0.68889, 0, 0, 0.54028],
      "710": [0, 0.825, 0, 0, 2.33334],
      "732": [0, 0.9, 0, 0, 2.33334],
      "770": [0, 0.825, 0, 0, 2.33334],
      "771": [0, 0.9, 0, 0, 2.33334],
      "989": [0.08167, 0.58167, 0, 0, 0.77778],
      "1008": [0, 0.43056, 0.04028, 0, 0.66667],
      "8245": [0, 0.54986, 0, 0, 0.275],
      "8463": [0, 0.68889, 0, 0, 0.54028],
      "8487": [0, 0.68889, 0, 0, 0.72222],
      "8498": [0, 0.68889, 0, 0, 0.55556],
      "8502": [0, 0.68889, 0, 0, 0.66667],
      "8503": [0, 0.68889, 0, 0, 0.44445],
      "8504": [0, 0.68889, 0, 0, 0.66667],
      "8513": [0, 0.68889, 0, 0, 0.63889],
      "8592": [-0.03598, 0.46402, 0, 0, 0.5],
      "8594": [-0.03598, 0.46402, 0, 0, 0.5],
      "8602": [-0.13313, 0.36687, 0, 0, 1],
      "8603": [-0.13313, 0.36687, 0, 0, 1],
      "8606": [0.01354, 0.52239, 0, 0, 1],
      "8608": [0.01354, 0.52239, 0, 0, 1],
      "8610": [0.01354, 0.52239, 0, 0, 1.11111],
      "8611": [0.01354, 0.52239, 0, 0, 1.11111],
      "8619": [0, 0.54986, 0, 0, 1],
      "8620": [0, 0.54986, 0, 0, 1],
      "8621": [-0.13313, 0.37788, 0, 0, 1.38889],
      "8622": [-0.13313, 0.36687, 0, 0, 1],
      "8624": [0, 0.69224, 0, 0, 0.5],
      "8625": [0, 0.69224, 0, 0, 0.5],
      "8630": [0, 0.43056, 0, 0, 1],
      "8631": [0, 0.43056, 0, 0, 1],
      "8634": [0.08198, 0.58198, 0, 0, 0.77778],
      "8635": [0.08198, 0.58198, 0, 0, 0.77778],
      "8638": [0.19444, 0.69224, 0, 0, 0.41667],
      "8639": [0.19444, 0.69224, 0, 0, 0.41667],
      "8642": [0.19444, 0.69224, 0, 0, 0.41667],
      "8643": [0.19444, 0.69224, 0, 0, 0.41667],
      "8644": [0.1808, 0.675, 0, 0, 1],
      "8646": [0.1808, 0.675, 0, 0, 1],
      "8647": [0.1808, 0.675, 0, 0, 1],
      "8648": [0.19444, 0.69224, 0, 0, 0.83334],
      "8649": [0.1808, 0.675, 0, 0, 1],
      "8650": [0.19444, 0.69224, 0, 0, 0.83334],
      "8651": [0.01354, 0.52239, 0, 0, 1],
      "8652": [0.01354, 0.52239, 0, 0, 1],
      "8653": [-0.13313, 0.36687, 0, 0, 1],
      "8654": [-0.13313, 0.36687, 0, 0, 1],
      "8655": [-0.13313, 0.36687, 0, 0, 1],
      "8666": [0.13667, 0.63667, 0, 0, 1],
      "8667": [0.13667, 0.63667, 0, 0, 1],
      "8669": [-0.13313, 0.37788, 0, 0, 1],
      "8672": [-0.064, 0.437, 0, 0, 1.334],
      "8674": [-0.064, 0.437, 0, 0, 1.334],
      "8705": [0, 0.825, 0, 0, 0.5],
      "8708": [0, 0.68889, 0, 0, 0.55556],
      "8709": [0.08167, 0.58167, 0, 0, 0.77778],
      "8717": [0, 0.43056, 0, 0, 0.42917],
      "8722": [-0.03598, 0.46402, 0, 0, 0.5],
      "8724": [0.08198, 0.69224, 0, 0, 0.77778],
      "8726": [0.08167, 0.58167, 0, 0, 0.77778],
      "8733": [0, 0.69224, 0, 0, 0.77778],
      "8736": [0, 0.69224, 0, 0, 0.72222],
      "8737": [0, 0.69224, 0, 0, 0.72222],
      "8738": [0.03517, 0.52239, 0, 0, 0.72222],
      "8739": [0.08167, 0.58167, 0, 0, 0.22222],
      "8740": [0.25142, 0.74111, 0, 0, 0.27778],
      "8741": [0.08167, 0.58167, 0, 0, 0.38889],
      "8742": [0.25142, 0.74111, 0, 0, 0.5],
      "8756": [0, 0.69224, 0, 0, 0.66667],
      "8757": [0, 0.69224, 0, 0, 0.66667],
      "8764": [-0.13313, 0.36687, 0, 0, 0.77778],
      "8765": [-0.13313, 0.37788, 0, 0, 0.77778],
      "8769": [-0.13313, 0.36687, 0, 0, 0.77778],
      "8770": [-0.03625, 0.46375, 0, 0, 0.77778],
      "8774": [0.30274, 0.79383, 0, 0, 0.77778],
      "8776": [-0.01688, 0.48312, 0, 0, 0.77778],
      "8778": [0.08167, 0.58167, 0, 0, 0.77778],
      "8782": [0.06062, 0.54986, 0, 0, 0.77778],
      "8783": [0.06062, 0.54986, 0, 0, 0.77778],
      "8785": [0.08198, 0.58198, 0, 0, 0.77778],
      "8786": [0.08198, 0.58198, 0, 0, 0.77778],
      "8787": [0.08198, 0.58198, 0, 0, 0.77778],
      "8790": [0, 0.69224, 0, 0, 0.77778],
      "8791": [0.22958, 0.72958, 0, 0, 0.77778],
      "8796": [0.08198, 0.91667, 0, 0, 0.77778],
      "8806": [0.25583, 0.75583, 0, 0, 0.77778],
      "8807": [0.25583, 0.75583, 0, 0, 0.77778],
      "8808": [0.25142, 0.75726, 0, 0, 0.77778],
      "8809": [0.25142, 0.75726, 0, 0, 0.77778],
      "8812": [0.25583, 0.75583, 0, 0, 0.5],
      "8814": [0.20576, 0.70576, 0, 0, 0.77778],
      "8815": [0.20576, 0.70576, 0, 0, 0.77778],
      "8816": [0.30274, 0.79383, 0, 0, 0.77778],
      "8817": [0.30274, 0.79383, 0, 0, 0.77778],
      "8818": [0.22958, 0.72958, 0, 0, 0.77778],
      "8819": [0.22958, 0.72958, 0, 0, 0.77778],
      "8822": [0.1808, 0.675, 0, 0, 0.77778],
      "8823": [0.1808, 0.675, 0, 0, 0.77778],
      "8828": [0.13667, 0.63667, 0, 0, 0.77778],
      "8829": [0.13667, 0.63667, 0, 0, 0.77778],
      "8830": [0.22958, 0.72958, 0, 0, 0.77778],
      "8831": [0.22958, 0.72958, 0, 0, 0.77778],
      "8832": [0.20576, 0.70576, 0, 0, 0.77778],
      "8833": [0.20576, 0.70576, 0, 0, 0.77778],
      "8840": [0.30274, 0.79383, 0, 0, 0.77778],
      "8841": [0.30274, 0.79383, 0, 0, 0.77778],
      "8842": [0.13597, 0.63597, 0, 0, 0.77778],
      "8843": [0.13597, 0.63597, 0, 0, 0.77778],
      "8847": [0.03517, 0.54986, 0, 0, 0.77778],
      "8848": [0.03517, 0.54986, 0, 0, 0.77778],
      "8858": [0.08198, 0.58198, 0, 0, 0.77778],
      "8859": [0.08198, 0.58198, 0, 0, 0.77778],
      "8861": [0.08198, 0.58198, 0, 0, 0.77778],
      "8862": [0, 0.675, 0, 0, 0.77778],
      "8863": [0, 0.675, 0, 0, 0.77778],
      "8864": [0, 0.675, 0, 0, 0.77778],
      "8865": [0, 0.675, 0, 0, 0.77778],
      "8872": [0, 0.69224, 0, 0, 0.61111],
      "8873": [0, 0.69224, 0, 0, 0.72222],
      "8874": [0, 0.69224, 0, 0, 0.88889],
      "8876": [0, 0.68889, 0, 0, 0.61111],
      "8877": [0, 0.68889, 0, 0, 0.61111],
      "8878": [0, 0.68889, 0, 0, 0.72222],
      "8879": [0, 0.68889, 0, 0, 0.72222],
      "8882": [0.03517, 0.54986, 0, 0, 0.77778],
      "8883": [0.03517, 0.54986, 0, 0, 0.77778],
      "8884": [0.13667, 0.63667, 0, 0, 0.77778],
      "8885": [0.13667, 0.63667, 0, 0, 0.77778],
      "8888": [0, 0.54986, 0, 0, 1.11111],
      "8890": [0.19444, 0.43056, 0, 0, 0.55556],
      "8891": [0.19444, 0.69224, 0, 0, 0.61111],
      "8892": [0.19444, 0.69224, 0, 0, 0.61111],
      "8901": [0, 0.54986, 0, 0, 0.27778],
      "8903": [0.08167, 0.58167, 0, 0, 0.77778],
      "8905": [0.08167, 0.58167, 0, 0, 0.77778],
      "8906": [0.08167, 0.58167, 0, 0, 0.77778],
      "8907": [0, 0.69224, 0, 0, 0.77778],
      "8908": [0, 0.69224, 0, 0, 0.77778],
      "8909": [-0.03598, 0.46402, 0, 0, 0.77778],
      "8910": [0, 0.54986, 0, 0, 0.76042],
      "8911": [0, 0.54986, 0, 0, 0.76042],
      "8912": [0.03517, 0.54986, 0, 0, 0.77778],
      "8913": [0.03517, 0.54986, 0, 0, 0.77778],
      "8914": [0, 0.54986, 0, 0, 0.66667],
      "8915": [0, 0.54986, 0, 0, 0.66667],
      "8916": [0, 0.69224, 0, 0, 0.66667],
      "8918": [0.0391, 0.5391, 0, 0, 0.77778],
      "8919": [0.0391, 0.5391, 0, 0, 0.77778],
      "8920": [0.03517, 0.54986, 0, 0, 1.33334],
      "8921": [0.03517, 0.54986, 0, 0, 1.33334],
      "8922": [0.38569, 0.88569, 0, 0, 0.77778],
      "8923": [0.38569, 0.88569, 0, 0, 0.77778],
      "8926": [0.13667, 0.63667, 0, 0, 0.77778],
      "8927": [0.13667, 0.63667, 0, 0, 0.77778],
      "8928": [0.30274, 0.79383, 0, 0, 0.77778],
      "8929": [0.30274, 0.79383, 0, 0, 0.77778],
      "8934": [0.23222, 0.74111, 0, 0, 0.77778],
      "8935": [0.23222, 0.74111, 0, 0, 0.77778],
      "8936": [0.23222, 0.74111, 0, 0, 0.77778],
      "8937": [0.23222, 0.74111, 0, 0, 0.77778],
      "8938": [0.20576, 0.70576, 0, 0, 0.77778],
      "8939": [0.20576, 0.70576, 0, 0, 0.77778],
      "8940": [0.30274, 0.79383, 0, 0, 0.77778],
      "8941": [0.30274, 0.79383, 0, 0, 0.77778],
      "8994": [0.19444, 0.69224, 0, 0, 0.77778],
      "8995": [0.19444, 0.69224, 0, 0, 0.77778],
      "9416": [0.15559, 0.69224, 0, 0, 0.90222],
      "9484": [0, 0.69224, 0, 0, 0.5],
      "9488": [0, 0.69224, 0, 0, 0.5],
      "9492": [0, 0.37788, 0, 0, 0.5],
      "9496": [0, 0.37788, 0, 0, 0.5],
      "9585": [0.19444, 0.68889, 0, 0, 0.88889],
      "9586": [0.19444, 0.74111, 0, 0, 0.88889],
      "9632": [0, 0.675, 0, 0, 0.77778],
      "9633": [0, 0.675, 0, 0, 0.77778],
      "9650": [0, 0.54986, 0, 0, 0.72222],
      "9651": [0, 0.54986, 0, 0, 0.72222],
      "9654": [0.03517, 0.54986, 0, 0, 0.77778],
      "9660": [0, 0.54986, 0, 0, 0.72222],
      "9661": [0, 0.54986, 0, 0, 0.72222],
      "9664": [0.03517, 0.54986, 0, 0, 0.77778],
      "9674": [0.11111, 0.69224, 0, 0, 0.66667],
      "9733": [0.19444, 0.69224, 0, 0, 0.94445],
      "10003": [0, 0.69224, 0, 0, 0.83334],
      "10016": [0, 0.69224, 0, 0, 0.83334],
      "10731": [0.11111, 0.69224, 0, 0, 0.66667],
      "10846": [0.19444, 0.75583, 0, 0, 0.61111],
      "10877": [0.13667, 0.63667, 0, 0, 0.77778],
      "10878": [0.13667, 0.63667, 0, 0, 0.77778],
      "10885": [0.25583, 0.75583, 0, 0, 0.77778],
      "10886": [0.25583, 0.75583, 0, 0, 0.77778],
      "10887": [0.13597, 0.63597, 0, 0, 0.77778],
      "10888": [0.13597, 0.63597, 0, 0, 0.77778],
      "10889": [0.26167, 0.75726, 0, 0, 0.77778],
      "10890": [0.26167, 0.75726, 0, 0, 0.77778],
      "10891": [0.48256, 0.98256, 0, 0, 0.77778],
      "10892": [0.48256, 0.98256, 0, 0, 0.77778],
      "10901": [0.13667, 0.63667, 0, 0, 0.77778],
      "10902": [0.13667, 0.63667, 0, 0, 0.77778],
      "10933": [0.25142, 0.75726, 0, 0, 0.77778],
      "10934": [0.25142, 0.75726, 0, 0, 0.77778],
      "10935": [0.26167, 0.75726, 0, 0, 0.77778],
      "10936": [0.26167, 0.75726, 0, 0, 0.77778],
      "10937": [0.26167, 0.75726, 0, 0, 0.77778],
      "10938": [0.26167, 0.75726, 0, 0, 0.77778],
      "10949": [0.25583, 0.75583, 0, 0, 0.77778],
      "10950": [0.25583, 0.75583, 0, 0, 0.77778],
      "10955": [0.28481, 0.79383, 0, 0, 0.77778],
      "10956": [0.28481, 0.79383, 0, 0, 0.77778],
      "57350": [0.08167, 0.58167, 0, 0, 0.22222],
      "57351": [0.08167, 0.58167, 0, 0, 0.38889],
      "57352": [0.08167, 0.58167, 0, 0, 0.77778],
      "57353": [0, 0.43056, 0.04028, 0, 0.66667],
      "57356": [0.25142, 0.75726, 0, 0, 0.77778],
      "57357": [0.25142, 0.75726, 0, 0, 0.77778],
      "57358": [0.41951, 0.91951, 0, 0, 0.77778],
      "57359": [0.30274, 0.79383, 0, 0, 0.77778],
      "57360": [0.30274, 0.79383, 0, 0, 0.77778],
      "57361": [0.41951, 0.91951, 0, 0, 0.77778],
      "57366": [0.25142, 0.75726, 0, 0, 0.77778],
      "57367": [0.25142, 0.75726, 0, 0, 0.77778],
      "57368": [0.25142, 0.75726, 0, 0, 0.77778],
      "57369": [0.25142, 0.75726, 0, 0, 0.77778],
      "57370": [0.13597, 0.63597, 0, 0, 0.77778],
      "57371": [0.13597, 0.63597, 0, 0, 0.77778]
    },
    "Caligraphic-Regular": {
      "32": [0, 0, 0, 0, 0.25],
      "65": [0, 0.68333, 0, 0.19445, 0.79847],
      "66": [0, 0.68333, 0.03041, 0.13889, 0.65681],
      "67": [0, 0.68333, 0.05834, 0.13889, 0.52653],
      "68": [0, 0.68333, 0.02778, 0.08334, 0.77139],
      "69": [0, 0.68333, 0.08944, 0.11111, 0.52778],
      "70": [0, 0.68333, 0.09931, 0.11111, 0.71875],
      "71": [0.09722, 0.68333, 0.0593, 0.11111, 0.59487],
      "72": [0, 0.68333, 965e-5, 0.11111, 0.84452],
      "73": [0, 0.68333, 0.07382, 0, 0.54452],
      "74": [0.09722, 0.68333, 0.18472, 0.16667, 0.67778],
      "75": [0, 0.68333, 0.01445, 0.05556, 0.76195],
      "76": [0, 0.68333, 0, 0.13889, 0.68972],
      "77": [0, 0.68333, 0, 0.13889, 1.2009],
      "78": [0, 0.68333, 0.14736, 0.08334, 0.82049],
      "79": [0, 0.68333, 0.02778, 0.11111, 0.79611],
      "80": [0, 0.68333, 0.08222, 0.08334, 0.69556],
      "81": [0.09722, 0.68333, 0, 0.11111, 0.81667],
      "82": [0, 0.68333, 0, 0.08334, 0.8475],
      "83": [0, 0.68333, 0.075, 0.13889, 0.60556],
      "84": [0, 0.68333, 0.25417, 0, 0.54464],
      "85": [0, 0.68333, 0.09931, 0.08334, 0.62583],
      "86": [0, 0.68333, 0.08222, 0, 0.61278],
      "87": [0, 0.68333, 0.08222, 0.08334, 0.98778],
      "88": [0, 0.68333, 0.14643, 0.13889, 0.7133],
      "89": [0.09722, 0.68333, 0.08222, 0.08334, 0.66834],
      "90": [0, 0.68333, 0.07944, 0.13889, 0.72473],
      "160": [0, 0, 0, 0, 0.25]
    },
    "Fraktur-Regular": {
      "32": [0, 0, 0, 0, 0.25],
      "33": [0, 0.69141, 0, 0, 0.29574],
      "34": [0, 0.69141, 0, 0, 0.21471],
      "38": [0, 0.69141, 0, 0, 0.73786],
      "39": [0, 0.69141, 0, 0, 0.21201],
      "40": [0.24982, 0.74947, 0, 0, 0.38865],
      "41": [0.24982, 0.74947, 0, 0, 0.38865],
      "42": [0, 0.62119, 0, 0, 0.27764],
      "43": [0.08319, 0.58283, 0, 0, 0.75623],
      "44": [0, 0.10803, 0, 0, 0.27764],
      "45": [0.08319, 0.58283, 0, 0, 0.75623],
      "46": [0, 0.10803, 0, 0, 0.27764],
      "47": [0.24982, 0.74947, 0, 0, 0.50181],
      "48": [0, 0.47534, 0, 0, 0.50181],
      "49": [0, 0.47534, 0, 0, 0.50181],
      "50": [0, 0.47534, 0, 0, 0.50181],
      "51": [0.18906, 0.47534, 0, 0, 0.50181],
      "52": [0.18906, 0.47534, 0, 0, 0.50181],
      "53": [0.18906, 0.47534, 0, 0, 0.50181],
      "54": [0, 0.69141, 0, 0, 0.50181],
      "55": [0.18906, 0.47534, 0, 0, 0.50181],
      "56": [0, 0.69141, 0, 0, 0.50181],
      "57": [0.18906, 0.47534, 0, 0, 0.50181],
      "58": [0, 0.47534, 0, 0, 0.21606],
      "59": [0.12604, 0.47534, 0, 0, 0.21606],
      "61": [-0.13099, 0.36866, 0, 0, 0.75623],
      "63": [0, 0.69141, 0, 0, 0.36245],
      "65": [0, 0.69141, 0, 0, 0.7176],
      "66": [0, 0.69141, 0, 0, 0.88397],
      "67": [0, 0.69141, 0, 0, 0.61254],
      "68": [0, 0.69141, 0, 0, 0.83158],
      "69": [0, 0.69141, 0, 0, 0.66278],
      "70": [0.12604, 0.69141, 0, 0, 0.61119],
      "71": [0, 0.69141, 0, 0, 0.78539],
      "72": [0.06302, 0.69141, 0, 0, 0.7203],
      "73": [0, 0.69141, 0, 0, 0.55448],
      "74": [0.12604, 0.69141, 0, 0, 0.55231],
      "75": [0, 0.69141, 0, 0, 0.66845],
      "76": [0, 0.69141, 0, 0, 0.66602],
      "77": [0, 0.69141, 0, 0, 1.04953],
      "78": [0, 0.69141, 0, 0, 0.83212],
      "79": [0, 0.69141, 0, 0, 0.82699],
      "80": [0.18906, 0.69141, 0, 0, 0.82753],
      "81": [0.03781, 0.69141, 0, 0, 0.82699],
      "82": [0, 0.69141, 0, 0, 0.82807],
      "83": [0, 0.69141, 0, 0, 0.82861],
      "84": [0, 0.69141, 0, 0, 0.66899],
      "85": [0, 0.69141, 0, 0, 0.64576],
      "86": [0, 0.69141, 0, 0, 0.83131],
      "87": [0, 0.69141, 0, 0, 1.04602],
      "88": [0, 0.69141, 0, 0, 0.71922],
      "89": [0.18906, 0.69141, 0, 0, 0.83293],
      "90": [0.12604, 0.69141, 0, 0, 0.60201],
      "91": [0.24982, 0.74947, 0, 0, 0.27764],
      "93": [0.24982, 0.74947, 0, 0, 0.27764],
      "94": [0, 0.69141, 0, 0, 0.49965],
      "97": [0, 0.47534, 0, 0, 0.50046],
      "98": [0, 0.69141, 0, 0, 0.51315],
      "99": [0, 0.47534, 0, 0, 0.38946],
      "100": [0, 0.62119, 0, 0, 0.49857],
      "101": [0, 0.47534, 0, 0, 0.40053],
      "102": [0.18906, 0.69141, 0, 0, 0.32626],
      "103": [0.18906, 0.47534, 0, 0, 0.5037],
      "104": [0.18906, 0.69141, 0, 0, 0.52126],
      "105": [0, 0.69141, 0, 0, 0.27899],
      "106": [0, 0.69141, 0, 0, 0.28088],
      "107": [0, 0.69141, 0, 0, 0.38946],
      "108": [0, 0.69141, 0, 0, 0.27953],
      "109": [0, 0.47534, 0, 0, 0.76676],
      "110": [0, 0.47534, 0, 0, 0.52666],
      "111": [0, 0.47534, 0, 0, 0.48885],
      "112": [0.18906, 0.52396, 0, 0, 0.50046],
      "113": [0.18906, 0.47534, 0, 0, 0.48912],
      "114": [0, 0.47534, 0, 0, 0.38919],
      "115": [0, 0.47534, 0, 0, 0.44266],
      "116": [0, 0.62119, 0, 0, 0.33301],
      "117": [0, 0.47534, 0, 0, 0.5172],
      "118": [0, 0.52396, 0, 0, 0.5118],
      "119": [0, 0.52396, 0, 0, 0.77351],
      "120": [0.18906, 0.47534, 0, 0, 0.38865],
      "121": [0.18906, 0.47534, 0, 0, 0.49884],
      "122": [0.18906, 0.47534, 0, 0, 0.39054],
      "160": [0, 0, 0, 0, 0.25],
      "8216": [0, 0.69141, 0, 0, 0.21471],
      "8217": [0, 0.69141, 0, 0, 0.21471],
      "58112": [0, 0.62119, 0, 0, 0.49749],
      "58113": [0, 0.62119, 0, 0, 0.4983],
      "58114": [0.18906, 0.69141, 0, 0, 0.33328],
      "58115": [0.18906, 0.69141, 0, 0, 0.32923],
      "58116": [0.18906, 0.47534, 0, 0, 0.50343],
      "58117": [0, 0.69141, 0, 0, 0.33301],
      "58118": [0, 0.62119, 0, 0, 0.33409],
      "58119": [0, 0.47534, 0, 0, 0.50073]
    },
    "Main-Bold": {
      "32": [0, 0, 0, 0, 0.25],
      "33": [0, 0.69444, 0, 0, 0.35],
      "34": [0, 0.69444, 0, 0, 0.60278],
      "35": [0.19444, 0.69444, 0, 0, 0.95833],
      "36": [0.05556, 0.75, 0, 0, 0.575],
      "37": [0.05556, 0.75, 0, 0, 0.95833],
      "38": [0, 0.69444, 0, 0, 0.89444],
      "39": [0, 0.69444, 0, 0, 0.31944],
      "40": [0.25, 0.75, 0, 0, 0.44722],
      "41": [0.25, 0.75, 0, 0, 0.44722],
      "42": [0, 0.75, 0, 0, 0.575],
      "43": [0.13333, 0.63333, 0, 0, 0.89444],
      "44": [0.19444, 0.15556, 0, 0, 0.31944],
      "45": [0, 0.44444, 0, 0, 0.38333],
      "46": [0, 0.15556, 0, 0, 0.31944],
      "47": [0.25, 0.75, 0, 0, 0.575],
      "48": [0, 0.64444, 0, 0, 0.575],
      "49": [0, 0.64444, 0, 0, 0.575],
      "50": [0, 0.64444, 0, 0, 0.575],
      "51": [0, 0.64444, 0, 0, 0.575],
      "52": [0, 0.64444, 0, 0, 0.575],
      "53": [0, 0.64444, 0, 0, 0.575],
      "54": [0, 0.64444, 0, 0, 0.575],
      "55": [0, 0.64444, 0, 0, 0.575],
      "56": [0, 0.64444, 0, 0, 0.575],
      "57": [0, 0.64444, 0, 0, 0.575],
      "58": [0, 0.44444, 0, 0, 0.31944],
      "59": [0.19444, 0.44444, 0, 0, 0.31944],
      "60": [0.08556, 0.58556, 0, 0, 0.89444],
      "61": [-0.10889, 0.39111, 0, 0, 0.89444],
      "62": [0.08556, 0.58556, 0, 0, 0.89444],
      "63": [0, 0.69444, 0, 0, 0.54305],
      "64": [0, 0.69444, 0, 0, 0.89444],
      "65": [0, 0.68611, 0, 0, 0.86944],
      "66": [0, 0.68611, 0, 0, 0.81805],
      "67": [0, 0.68611, 0, 0, 0.83055],
      "68": [0, 0.68611, 0, 0, 0.88194],
      "69": [0, 0.68611, 0, 0, 0.75555],
      "70": [0, 0.68611, 0, 0, 0.72361],
      "71": [0, 0.68611, 0, 0, 0.90416],
      "72": [0, 0.68611, 0, 0, 0.9],
      "73": [0, 0.68611, 0, 0, 0.43611],
      "74": [0, 0.68611, 0, 0, 0.59444],
      "75": [0, 0.68611, 0, 0, 0.90138],
      "76": [0, 0.68611, 0, 0, 0.69166],
      "77": [0, 0.68611, 0, 0, 1.09166],
      "78": [0, 0.68611, 0, 0, 0.9],
      "79": [0, 0.68611, 0, 0, 0.86388],
      "80": [0, 0.68611, 0, 0, 0.78611],
      "81": [0.19444, 0.68611, 0, 0, 0.86388],
      "82": [0, 0.68611, 0, 0, 0.8625],
      "83": [0, 0.68611, 0, 0, 0.63889],
      "84": [0, 0.68611, 0, 0, 0.8],
      "85": [0, 0.68611, 0, 0, 0.88472],
      "86": [0, 0.68611, 0.01597, 0, 0.86944],
      "87": [0, 0.68611, 0.01597, 0, 1.18888],
      "88": [0, 0.68611, 0, 0, 0.86944],
      "89": [0, 0.68611, 0.02875, 0, 0.86944],
      "90": [0, 0.68611, 0, 0, 0.70277],
      "91": [0.25, 0.75, 0, 0, 0.31944],
      "92": [0.25, 0.75, 0, 0, 0.575],
      "93": [0.25, 0.75, 0, 0, 0.31944],
      "94": [0, 0.69444, 0, 0, 0.575],
      "95": [0.31, 0.13444, 0.03194, 0, 0.575],
      "97": [0, 0.44444, 0, 0, 0.55902],
      "98": [0, 0.69444, 0, 0, 0.63889],
      "99": [0, 0.44444, 0, 0, 0.51111],
      "100": [0, 0.69444, 0, 0, 0.63889],
      "101": [0, 0.44444, 0, 0, 0.52708],
      "102": [0, 0.69444, 0.10903, 0, 0.35139],
      "103": [0.19444, 0.44444, 0.01597, 0, 0.575],
      "104": [0, 0.69444, 0, 0, 0.63889],
      "105": [0, 0.69444, 0, 0, 0.31944],
      "106": [0.19444, 0.69444, 0, 0, 0.35139],
      "107": [0, 0.69444, 0, 0, 0.60694],
      "108": [0, 0.69444, 0, 0, 0.31944],
      "109": [0, 0.44444, 0, 0, 0.95833],
      "110": [0, 0.44444, 0, 0, 0.63889],
      "111": [0, 0.44444, 0, 0, 0.575],
      "112": [0.19444, 0.44444, 0, 0, 0.63889],
      "113": [0.19444, 0.44444, 0, 0, 0.60694],
      "114": [0, 0.44444, 0, 0, 0.47361],
      "115": [0, 0.44444, 0, 0, 0.45361],
      "116": [0, 0.63492, 0, 0, 0.44722],
      "117": [0, 0.44444, 0, 0, 0.63889],
      "118": [0, 0.44444, 0.01597, 0, 0.60694],
      "119": [0, 0.44444, 0.01597, 0, 0.83055],
      "120": [0, 0.44444, 0, 0, 0.60694],
      "121": [0.19444, 0.44444, 0.01597, 0, 0.60694],
      "122": [0, 0.44444, 0, 0, 0.51111],
      "123": [0.25, 0.75, 0, 0, 0.575],
      "124": [0.25, 0.75, 0, 0, 0.31944],
      "125": [0.25, 0.75, 0, 0, 0.575],
      "126": [0.35, 0.34444, 0, 0, 0.575],
      "160": [0, 0, 0, 0, 0.25],
      "163": [0, 0.69444, 0, 0, 0.86853],
      "168": [0, 0.69444, 0, 0, 0.575],
      "172": [0, 0.44444, 0, 0, 0.76666],
      "176": [0, 0.69444, 0, 0, 0.86944],
      "177": [0.13333, 0.63333, 0, 0, 0.89444],
      "184": [0.17014, 0, 0, 0, 0.51111],
      "198": [0, 0.68611, 0, 0, 1.04166],
      "215": [0.13333, 0.63333, 0, 0, 0.89444],
      "216": [0.04861, 0.73472, 0, 0, 0.89444],
      "223": [0, 0.69444, 0, 0, 0.59722],
      "230": [0, 0.44444, 0, 0, 0.83055],
      "247": [0.13333, 0.63333, 0, 0, 0.89444],
      "248": [0.09722, 0.54167, 0, 0, 0.575],
      "305": [0, 0.44444, 0, 0, 0.31944],
      "338": [0, 0.68611, 0, 0, 1.16944],
      "339": [0, 0.44444, 0, 0, 0.89444],
      "567": [0.19444, 0.44444, 0, 0, 0.35139],
      "710": [0, 0.69444, 0, 0, 0.575],
      "711": [0, 0.63194, 0, 0, 0.575],
      "713": [0, 0.59611, 0, 0, 0.575],
      "714": [0, 0.69444, 0, 0, 0.575],
      "715": [0, 0.69444, 0, 0, 0.575],
      "728": [0, 0.69444, 0, 0, 0.575],
      "729": [0, 0.69444, 0, 0, 0.31944],
      "730": [0, 0.69444, 0, 0, 0.86944],
      "732": [0, 0.69444, 0, 0, 0.575],
      "733": [0, 0.69444, 0, 0, 0.575],
      "915": [0, 0.68611, 0, 0, 0.69166],
      "916": [0, 0.68611, 0, 0, 0.95833],
      "920": [0, 0.68611, 0, 0, 0.89444],
      "923": [0, 0.68611, 0, 0, 0.80555],
      "926": [0, 0.68611, 0, 0, 0.76666],
      "928": [0, 0.68611, 0, 0, 0.9],
      "931": [0, 0.68611, 0, 0, 0.83055],
      "933": [0, 0.68611, 0, 0, 0.89444],
      "934": [0, 0.68611, 0, 0, 0.83055],
      "936": [0, 0.68611, 0, 0, 0.89444],
      "937": [0, 0.68611, 0, 0, 0.83055],
      "8211": [0, 0.44444, 0.03194, 0, 0.575],
      "8212": [0, 0.44444, 0.03194, 0, 1.14999],
      "8216": [0, 0.69444, 0, 0, 0.31944],
      "8217": [0, 0.69444, 0, 0, 0.31944],
      "8220": [0, 0.69444, 0, 0, 0.60278],
      "8221": [0, 0.69444, 0, 0, 0.60278],
      "8224": [0.19444, 0.69444, 0, 0, 0.51111],
      "8225": [0.19444, 0.69444, 0, 0, 0.51111],
      "8242": [0, 0.55556, 0, 0, 0.34444],
      "8407": [0, 0.72444, 0.15486, 0, 0.575],
      "8463": [0, 0.69444, 0, 0, 0.66759],
      "8465": [0, 0.69444, 0, 0, 0.83055],
      "8467": [0, 0.69444, 0, 0, 0.47361],
      "8472": [0.19444, 0.44444, 0, 0, 0.74027],
      "8476": [0, 0.69444, 0, 0, 0.83055],
      "8501": [0, 0.69444, 0, 0, 0.70277],
      "8592": [-0.10889, 0.39111, 0, 0, 1.14999],
      "8593": [0.19444, 0.69444, 0, 0, 0.575],
      "8594": [-0.10889, 0.39111, 0, 0, 1.14999],
      "8595": [0.19444, 0.69444, 0, 0, 0.575],
      "8596": [-0.10889, 0.39111, 0, 0, 1.14999],
      "8597": [0.25, 0.75, 0, 0, 0.575],
      "8598": [0.19444, 0.69444, 0, 0, 1.14999],
      "8599": [0.19444, 0.69444, 0, 0, 1.14999],
      "8600": [0.19444, 0.69444, 0, 0, 1.14999],
      "8601": [0.19444, 0.69444, 0, 0, 1.14999],
      "8636": [-0.10889, 0.39111, 0, 0, 1.14999],
      "8637": [-0.10889, 0.39111, 0, 0, 1.14999],
      "8640": [-0.10889, 0.39111, 0, 0, 1.14999],
      "8641": [-0.10889, 0.39111, 0, 0, 1.14999],
      "8656": [-0.10889, 0.39111, 0, 0, 1.14999],
      "8657": [0.19444, 0.69444, 0, 0, 0.70277],
      "8658": [-0.10889, 0.39111, 0, 0, 1.14999],
      "8659": [0.19444, 0.69444, 0, 0, 0.70277],
      "8660": [-0.10889, 0.39111, 0, 0, 1.14999],
      "8661": [0.25, 0.75, 0, 0, 0.70277],
      "8704": [0, 0.69444, 0, 0, 0.63889],
      "8706": [0, 0.69444, 0.06389, 0, 0.62847],
      "8707": [0, 0.69444, 0, 0, 0.63889],
      "8709": [0.05556, 0.75, 0, 0, 0.575],
      "8711": [0, 0.68611, 0, 0, 0.95833],
      "8712": [0.08556, 0.58556, 0, 0, 0.76666],
      "8715": [0.08556, 0.58556, 0, 0, 0.76666],
      "8722": [0.13333, 0.63333, 0, 0, 0.89444],
      "8723": [0.13333, 0.63333, 0, 0, 0.89444],
      "8725": [0.25, 0.75, 0, 0, 0.575],
      "8726": [0.25, 0.75, 0, 0, 0.575],
      "8727": [-0.02778, 0.47222, 0, 0, 0.575],
      "8728": [-0.02639, 0.47361, 0, 0, 0.575],
      "8729": [-0.02639, 0.47361, 0, 0, 0.575],
      "8730": [0.18, 0.82, 0, 0, 0.95833],
      "8733": [0, 0.44444, 0, 0, 0.89444],
      "8734": [0, 0.44444, 0, 0, 1.14999],
      "8736": [0, 0.69224, 0, 0, 0.72222],
      "8739": [0.25, 0.75, 0, 0, 0.31944],
      "8741": [0.25, 0.75, 0, 0, 0.575],
      "8743": [0, 0.55556, 0, 0, 0.76666],
      "8744": [0, 0.55556, 0, 0, 0.76666],
      "8745": [0, 0.55556, 0, 0, 0.76666],
      "8746": [0, 0.55556, 0, 0, 0.76666],
      "8747": [0.19444, 0.69444, 0.12778, 0, 0.56875],
      "8764": [-0.10889, 0.39111, 0, 0, 0.89444],
      "8768": [0.19444, 0.69444, 0, 0, 0.31944],
      "8771": [222e-5, 0.50222, 0, 0, 0.89444],
      "8773": [0.027, 0.638, 0, 0, 0.894],
      "8776": [0.02444, 0.52444, 0, 0, 0.89444],
      "8781": [222e-5, 0.50222, 0, 0, 0.89444],
      "8801": [222e-5, 0.50222, 0, 0, 0.89444],
      "8804": [0.19667, 0.69667, 0, 0, 0.89444],
      "8805": [0.19667, 0.69667, 0, 0, 0.89444],
      "8810": [0.08556, 0.58556, 0, 0, 1.14999],
      "8811": [0.08556, 0.58556, 0, 0, 1.14999],
      "8826": [0.08556, 0.58556, 0, 0, 0.89444],
      "8827": [0.08556, 0.58556, 0, 0, 0.89444],
      "8834": [0.08556, 0.58556, 0, 0, 0.89444],
      "8835": [0.08556, 0.58556, 0, 0, 0.89444],
      "8838": [0.19667, 0.69667, 0, 0, 0.89444],
      "8839": [0.19667, 0.69667, 0, 0, 0.89444],
      "8846": [0, 0.55556, 0, 0, 0.76666],
      "8849": [0.19667, 0.69667, 0, 0, 0.89444],
      "8850": [0.19667, 0.69667, 0, 0, 0.89444],
      "8851": [0, 0.55556, 0, 0, 0.76666],
      "8852": [0, 0.55556, 0, 0, 0.76666],
      "8853": [0.13333, 0.63333, 0, 0, 0.89444],
      "8854": [0.13333, 0.63333, 0, 0, 0.89444],
      "8855": [0.13333, 0.63333, 0, 0, 0.89444],
      "8856": [0.13333, 0.63333, 0, 0, 0.89444],
      "8857": [0.13333, 0.63333, 0, 0, 0.89444],
      "8866": [0, 0.69444, 0, 0, 0.70277],
      "8867": [0, 0.69444, 0, 0, 0.70277],
      "8868": [0, 0.69444, 0, 0, 0.89444],
      "8869": [0, 0.69444, 0, 0, 0.89444],
      "8900": [-0.02639, 0.47361, 0, 0, 0.575],
      "8901": [-0.02639, 0.47361, 0, 0, 0.31944],
      "8902": [-0.02778, 0.47222, 0, 0, 0.575],
      "8968": [0.25, 0.75, 0, 0, 0.51111],
      "8969": [0.25, 0.75, 0, 0, 0.51111],
      "8970": [0.25, 0.75, 0, 0, 0.51111],
      "8971": [0.25, 0.75, 0, 0, 0.51111],
      "8994": [-0.13889, 0.36111, 0, 0, 1.14999],
      "8995": [-0.13889, 0.36111, 0, 0, 1.14999],
      "9651": [0.19444, 0.69444, 0, 0, 1.02222],
      "9657": [-0.02778, 0.47222, 0, 0, 0.575],
      "9661": [0.19444, 0.69444, 0, 0, 1.02222],
      "9667": [-0.02778, 0.47222, 0, 0, 0.575],
      "9711": [0.19444, 0.69444, 0, 0, 1.14999],
      "9824": [0.12963, 0.69444, 0, 0, 0.89444],
      "9825": [0.12963, 0.69444, 0, 0, 0.89444],
      "9826": [0.12963, 0.69444, 0, 0, 0.89444],
      "9827": [0.12963, 0.69444, 0, 0, 0.89444],
      "9837": [0, 0.75, 0, 0, 0.44722],
      "9838": [0.19444, 0.69444, 0, 0, 0.44722],
      "9839": [0.19444, 0.69444, 0, 0, 0.44722],
      "10216": [0.25, 0.75, 0, 0, 0.44722],
      "10217": [0.25, 0.75, 0, 0, 0.44722],
      "10815": [0, 0.68611, 0, 0, 0.9],
      "10927": [0.19667, 0.69667, 0, 0, 0.89444],
      "10928": [0.19667, 0.69667, 0, 0, 0.89444],
      "57376": [0.19444, 0.69444, 0, 0, 0]
    },
    "Main-BoldItalic": {
      "32": [0, 0, 0, 0, 0.25],
      "33": [0, 0.69444, 0.11417, 0, 0.38611],
      "34": [0, 0.69444, 0.07939, 0, 0.62055],
      "35": [0.19444, 0.69444, 0.06833, 0, 0.94444],
      "37": [0.05556, 0.75, 0.12861, 0, 0.94444],
      "38": [0, 0.69444, 0.08528, 0, 0.88555],
      "39": [0, 0.69444, 0.12945, 0, 0.35555],
      "40": [0.25, 0.75, 0.15806, 0, 0.47333],
      "41": [0.25, 0.75, 0.03306, 0, 0.47333],
      "42": [0, 0.75, 0.14333, 0, 0.59111],
      "43": [0.10333, 0.60333, 0.03306, 0, 0.88555],
      "44": [0.19444, 0.14722, 0, 0, 0.35555],
      "45": [0, 0.44444, 0.02611, 0, 0.41444],
      "46": [0, 0.14722, 0, 0, 0.35555],
      "47": [0.25, 0.75, 0.15806, 0, 0.59111],
      "48": [0, 0.64444, 0.13167, 0, 0.59111],
      "49": [0, 0.64444, 0.13167, 0, 0.59111],
      "50": [0, 0.64444, 0.13167, 0, 0.59111],
      "51": [0, 0.64444, 0.13167, 0, 0.59111],
      "52": [0.19444, 0.64444, 0.13167, 0, 0.59111],
      "53": [0, 0.64444, 0.13167, 0, 0.59111],
      "54": [0, 0.64444, 0.13167, 0, 0.59111],
      "55": [0.19444, 0.64444, 0.13167, 0, 0.59111],
      "56": [0, 0.64444, 0.13167, 0, 0.59111],
      "57": [0, 0.64444, 0.13167, 0, 0.59111],
      "58": [0, 0.44444, 0.06695, 0, 0.35555],
      "59": [0.19444, 0.44444, 0.06695, 0, 0.35555],
      "61": [-0.10889, 0.39111, 0.06833, 0, 0.88555],
      "63": [0, 0.69444, 0.11472, 0, 0.59111],
      "64": [0, 0.69444, 0.09208, 0, 0.88555],
      "65": [0, 0.68611, 0, 0, 0.86555],
      "66": [0, 0.68611, 0.0992, 0, 0.81666],
      "67": [0, 0.68611, 0.14208, 0, 0.82666],
      "68": [0, 0.68611, 0.09062, 0, 0.87555],
      "69": [0, 0.68611, 0.11431, 0, 0.75666],
      "70": [0, 0.68611, 0.12903, 0, 0.72722],
      "71": [0, 0.68611, 0.07347, 0, 0.89527],
      "72": [0, 0.68611, 0.17208, 0, 0.8961],
      "73": [0, 0.68611, 0.15681, 0, 0.47166],
      "74": [0, 0.68611, 0.145, 0, 0.61055],
      "75": [0, 0.68611, 0.14208, 0, 0.89499],
      "76": [0, 0.68611, 0, 0, 0.69777],
      "77": [0, 0.68611, 0.17208, 0, 1.07277],
      "78": [0, 0.68611, 0.17208, 0, 0.8961],
      "79": [0, 0.68611, 0.09062, 0, 0.85499],
      "80": [0, 0.68611, 0.0992, 0, 0.78721],
      "81": [0.19444, 0.68611, 0.09062, 0, 0.85499],
      "82": [0, 0.68611, 0.02559, 0, 0.85944],
      "83": [0, 0.68611, 0.11264, 0, 0.64999],
      "84": [0, 0.68611, 0.12903, 0, 0.7961],
      "85": [0, 0.68611, 0.17208, 0, 0.88083],
      "86": [0, 0.68611, 0.18625, 0, 0.86555],
      "87": [0, 0.68611, 0.18625, 0, 1.15999],
      "88": [0, 0.68611, 0.15681, 0, 0.86555],
      "89": [0, 0.68611, 0.19803, 0, 0.86555],
      "90": [0, 0.68611, 0.14208, 0, 0.70888],
      "91": [0.25, 0.75, 0.1875, 0, 0.35611],
      "93": [0.25, 0.75, 0.09972, 0, 0.35611],
      "94": [0, 0.69444, 0.06709, 0, 0.59111],
      "95": [0.31, 0.13444, 0.09811, 0, 0.59111],
      "97": [0, 0.44444, 0.09426, 0, 0.59111],
      "98": [0, 0.69444, 0.07861, 0, 0.53222],
      "99": [0, 0.44444, 0.05222, 0, 0.53222],
      "100": [0, 0.69444, 0.10861, 0, 0.59111],
      "101": [0, 0.44444, 0.085, 0, 0.53222],
      "102": [0.19444, 0.69444, 0.21778, 0, 0.4],
      "103": [0.19444, 0.44444, 0.105, 0, 0.53222],
      "104": [0, 0.69444, 0.09426, 0, 0.59111],
      "105": [0, 0.69326, 0.11387, 0, 0.35555],
      "106": [0.19444, 0.69326, 0.1672, 0, 0.35555],
      "107": [0, 0.69444, 0.11111, 0, 0.53222],
      "108": [0, 0.69444, 0.10861, 0, 0.29666],
      "109": [0, 0.44444, 0.09426, 0, 0.94444],
      "110": [0, 0.44444, 0.09426, 0, 0.64999],
      "111": [0, 0.44444, 0.07861, 0, 0.59111],
      "112": [0.19444, 0.44444, 0.07861, 0, 0.59111],
      "113": [0.19444, 0.44444, 0.105, 0, 0.53222],
      "114": [0, 0.44444, 0.11111, 0, 0.50167],
      "115": [0, 0.44444, 0.08167, 0, 0.48694],
      "116": [0, 0.63492, 0.09639, 0, 0.385],
      "117": [0, 0.44444, 0.09426, 0, 0.62055],
      "118": [0, 0.44444, 0.11111, 0, 0.53222],
      "119": [0, 0.44444, 0.11111, 0, 0.76777],
      "120": [0, 0.44444, 0.12583, 0, 0.56055],
      "121": [0.19444, 0.44444, 0.105, 0, 0.56166],
      "122": [0, 0.44444, 0.13889, 0, 0.49055],
      "126": [0.35, 0.34444, 0.11472, 0, 0.59111],
      "160": [0, 0, 0, 0, 0.25],
      "168": [0, 0.69444, 0.11473, 0, 0.59111],
      "176": [0, 0.69444, 0, 0, 0.94888],
      "184": [0.17014, 0, 0, 0, 0.53222],
      "198": [0, 0.68611, 0.11431, 0, 1.02277],
      "216": [0.04861, 0.73472, 0.09062, 0, 0.88555],
      "223": [0.19444, 0.69444, 0.09736, 0, 0.665],
      "230": [0, 0.44444, 0.085, 0, 0.82666],
      "248": [0.09722, 0.54167, 0.09458, 0, 0.59111],
      "305": [0, 0.44444, 0.09426, 0, 0.35555],
      "338": [0, 0.68611, 0.11431, 0, 1.14054],
      "339": [0, 0.44444, 0.085, 0, 0.82666],
      "567": [0.19444, 0.44444, 0.04611, 0, 0.385],
      "710": [0, 0.69444, 0.06709, 0, 0.59111],
      "711": [0, 0.63194, 0.08271, 0, 0.59111],
      "713": [0, 0.59444, 0.10444, 0, 0.59111],
      "714": [0, 0.69444, 0.08528, 0, 0.59111],
      "715": [0, 0.69444, 0, 0, 0.59111],
      "728": [0, 0.69444, 0.10333, 0, 0.59111],
      "729": [0, 0.69444, 0.12945, 0, 0.35555],
      "730": [0, 0.69444, 0, 0, 0.94888],
      "732": [0, 0.69444, 0.11472, 0, 0.59111],
      "733": [0, 0.69444, 0.11472, 0, 0.59111],
      "915": [0, 0.68611, 0.12903, 0, 0.69777],
      "916": [0, 0.68611, 0, 0, 0.94444],
      "920": [0, 0.68611, 0.09062, 0, 0.88555],
      "923": [0, 0.68611, 0, 0, 0.80666],
      "926": [0, 0.68611, 0.15092, 0, 0.76777],
      "928": [0, 0.68611, 0.17208, 0, 0.8961],
      "931": [0, 0.68611, 0.11431, 0, 0.82666],
      "933": [0, 0.68611, 0.10778, 0, 0.88555],
      "934": [0, 0.68611, 0.05632, 0, 0.82666],
      "936": [0, 0.68611, 0.10778, 0, 0.88555],
      "937": [0, 0.68611, 0.0992, 0, 0.82666],
      "8211": [0, 0.44444, 0.09811, 0, 0.59111],
      "8212": [0, 0.44444, 0.09811, 0, 1.18221],
      "8216": [0, 0.69444, 0.12945, 0, 0.35555],
      "8217": [0, 0.69444, 0.12945, 0, 0.35555],
      "8220": [0, 0.69444, 0.16772, 0, 0.62055],
      "8221": [0, 0.69444, 0.07939, 0, 0.62055]
    },
    "Main-Italic": {
      "32": [0, 0, 0, 0, 0.25],
      "33": [0, 0.69444, 0.12417, 0, 0.30667],
      "34": [0, 0.69444, 0.06961, 0, 0.51444],
      "35": [0.19444, 0.69444, 0.06616, 0, 0.81777],
      "37": [0.05556, 0.75, 0.13639, 0, 0.81777],
      "38": [0, 0.69444, 0.09694, 0, 0.76666],
      "39": [0, 0.69444, 0.12417, 0, 0.30667],
      "40": [0.25, 0.75, 0.16194, 0, 0.40889],
      "41": [0.25, 0.75, 0.03694, 0, 0.40889],
      "42": [0, 0.75, 0.14917, 0, 0.51111],
      "43": [0.05667, 0.56167, 0.03694, 0, 0.76666],
      "44": [0.19444, 0.10556, 0, 0, 0.30667],
      "45": [0, 0.43056, 0.02826, 0, 0.35778],
      "46": [0, 0.10556, 0, 0, 0.30667],
      "47": [0.25, 0.75, 0.16194, 0, 0.51111],
      "48": [0, 0.64444, 0.13556, 0, 0.51111],
      "49": [0, 0.64444, 0.13556, 0, 0.51111],
      "50": [0, 0.64444, 0.13556, 0, 0.51111],
      "51": [0, 0.64444, 0.13556, 0, 0.51111],
      "52": [0.19444, 0.64444, 0.13556, 0, 0.51111],
      "53": [0, 0.64444, 0.13556, 0, 0.51111],
      "54": [0, 0.64444, 0.13556, 0, 0.51111],
      "55": [0.19444, 0.64444, 0.13556, 0, 0.51111],
      "56": [0, 0.64444, 0.13556, 0, 0.51111],
      "57": [0, 0.64444, 0.13556, 0, 0.51111],
      "58": [0, 0.43056, 0.0582, 0, 0.30667],
      "59": [0.19444, 0.43056, 0.0582, 0, 0.30667],
      "61": [-0.13313, 0.36687, 0.06616, 0, 0.76666],
      "63": [0, 0.69444, 0.1225, 0, 0.51111],
      "64": [0, 0.69444, 0.09597, 0, 0.76666],
      "65": [0, 0.68333, 0, 0, 0.74333],
      "66": [0, 0.68333, 0.10257, 0, 0.70389],
      "67": [0, 0.68333, 0.14528, 0, 0.71555],
      "68": [0, 0.68333, 0.09403, 0, 0.755],
      "69": [0, 0.68333, 0.12028, 0, 0.67833],
      "70": [0, 0.68333, 0.13305, 0, 0.65277],
      "71": [0, 0.68333, 0.08722, 0, 0.77361],
      "72": [0, 0.68333, 0.16389, 0, 0.74333],
      "73": [0, 0.68333, 0.15806, 0, 0.38555],
      "74": [0, 0.68333, 0.14028, 0, 0.525],
      "75": [0, 0.68333, 0.14528, 0, 0.76888],
      "76": [0, 0.68333, 0, 0, 0.62722],
      "77": [0, 0.68333, 0.16389, 0, 0.89666],
      "78": [0, 0.68333, 0.16389, 0, 0.74333],
      "79": [0, 0.68333, 0.09403, 0, 0.76666],
      "80": [0, 0.68333, 0.10257, 0, 0.67833],
      "81": [0.19444, 0.68333, 0.09403, 0, 0.76666],
      "82": [0, 0.68333, 0.03868, 0, 0.72944],
      "83": [0, 0.68333, 0.11972, 0, 0.56222],
      "84": [0, 0.68333, 0.13305, 0, 0.71555],
      "85": [0, 0.68333, 0.16389, 0, 0.74333],
      "86": [0, 0.68333, 0.18361, 0, 0.74333],
      "87": [0, 0.68333, 0.18361, 0, 0.99888],
      "88": [0, 0.68333, 0.15806, 0, 0.74333],
      "89": [0, 0.68333, 0.19383, 0, 0.74333],
      "90": [0, 0.68333, 0.14528, 0, 0.61333],
      "91": [0.25, 0.75, 0.1875, 0, 0.30667],
      "93": [0.25, 0.75, 0.10528, 0, 0.30667],
      "94": [0, 0.69444, 0.06646, 0, 0.51111],
      "95": [0.31, 0.12056, 0.09208, 0, 0.51111],
      "97": [0, 0.43056, 0.07671, 0, 0.51111],
      "98": [0, 0.69444, 0.06312, 0, 0.46],
      "99": [0, 0.43056, 0.05653, 0, 0.46],
      "100": [0, 0.69444, 0.10333, 0, 0.51111],
      "101": [0, 0.43056, 0.07514, 0, 0.46],
      "102": [0.19444, 0.69444, 0.21194, 0, 0.30667],
      "103": [0.19444, 0.43056, 0.08847, 0, 0.46],
      "104": [0, 0.69444, 0.07671, 0, 0.51111],
      "105": [0, 0.65536, 0.1019, 0, 0.30667],
      "106": [0.19444, 0.65536, 0.14467, 0, 0.30667],
      "107": [0, 0.69444, 0.10764, 0, 0.46],
      "108": [0, 0.69444, 0.10333, 0, 0.25555],
      "109": [0, 0.43056, 0.07671, 0, 0.81777],
      "110": [0, 0.43056, 0.07671, 0, 0.56222],
      "111": [0, 0.43056, 0.06312, 0, 0.51111],
      "112": [0.19444, 0.43056, 0.06312, 0, 0.51111],
      "113": [0.19444, 0.43056, 0.08847, 0, 0.46],
      "114": [0, 0.43056, 0.10764, 0, 0.42166],
      "115": [0, 0.43056, 0.08208, 0, 0.40889],
      "116": [0, 0.61508, 0.09486, 0, 0.33222],
      "117": [0, 0.43056, 0.07671, 0, 0.53666],
      "118": [0, 0.43056, 0.10764, 0, 0.46],
      "119": [0, 0.43056, 0.10764, 0, 0.66444],
      "120": [0, 0.43056, 0.12042, 0, 0.46389],
      "121": [0.19444, 0.43056, 0.08847, 0, 0.48555],
      "122": [0, 0.43056, 0.12292, 0, 0.40889],
      "126": [0.35, 0.31786, 0.11585, 0, 0.51111],
      "160": [0, 0, 0, 0, 0.25],
      "168": [0, 0.66786, 0.10474, 0, 0.51111],
      "176": [0, 0.69444, 0, 0, 0.83129],
      "184": [0.17014, 0, 0, 0, 0.46],
      "198": [0, 0.68333, 0.12028, 0, 0.88277],
      "216": [0.04861, 0.73194, 0.09403, 0, 0.76666],
      "223": [0.19444, 0.69444, 0.10514, 0, 0.53666],
      "230": [0, 0.43056, 0.07514, 0, 0.71555],
      "248": [0.09722, 0.52778, 0.09194, 0, 0.51111],
      "338": [0, 0.68333, 0.12028, 0, 0.98499],
      "339": [0, 0.43056, 0.07514, 0, 0.71555],
      "710": [0, 0.69444, 0.06646, 0, 0.51111],
      "711": [0, 0.62847, 0.08295, 0, 0.51111],
      "713": [0, 0.56167, 0.10333, 0, 0.51111],
      "714": [0, 0.69444, 0.09694, 0, 0.51111],
      "715": [0, 0.69444, 0, 0, 0.51111],
      "728": [0, 0.69444, 0.10806, 0, 0.51111],
      "729": [0, 0.66786, 0.11752, 0, 0.30667],
      "730": [0, 0.69444, 0, 0, 0.83129],
      "732": [0, 0.66786, 0.11585, 0, 0.51111],
      "733": [0, 0.69444, 0.1225, 0, 0.51111],
      "915": [0, 0.68333, 0.13305, 0, 0.62722],
      "916": [0, 0.68333, 0, 0, 0.81777],
      "920": [0, 0.68333, 0.09403, 0, 0.76666],
      "923": [0, 0.68333, 0, 0, 0.69222],
      "926": [0, 0.68333, 0.15294, 0, 0.66444],
      "928": [0, 0.68333, 0.16389, 0, 0.74333],
      "931": [0, 0.68333, 0.12028, 0, 0.71555],
      "933": [0, 0.68333, 0.11111, 0, 0.76666],
      "934": [0, 0.68333, 0.05986, 0, 0.71555],
      "936": [0, 0.68333, 0.11111, 0, 0.76666],
      "937": [0, 0.68333, 0.10257, 0, 0.71555],
      "8211": [0, 0.43056, 0.09208, 0, 0.51111],
      "8212": [0, 0.43056, 0.09208, 0, 1.02222],
      "8216": [0, 0.69444, 0.12417, 0, 0.30667],
      "8217": [0, 0.69444, 0.12417, 0, 0.30667],
      "8220": [0, 0.69444, 0.1685, 0, 0.51444],
      "8221": [0, 0.69444, 0.06961, 0, 0.51444],
      "8463": [0, 0.68889, 0, 0, 0.54028]
    },
    "Main-Regular": {
      "32": [0, 0, 0, 0, 0.25],
      "33": [0, 0.69444, 0, 0, 0.27778],
      "34": [0, 0.69444, 0, 0, 0.5],
      "35": [0.19444, 0.69444, 0, 0, 0.83334],
      "36": [0.05556, 0.75, 0, 0, 0.5],
      "37": [0.05556, 0.75, 0, 0, 0.83334],
      "38": [0, 0.69444, 0, 0, 0.77778],
      "39": [0, 0.69444, 0, 0, 0.27778],
      "40": [0.25, 0.75, 0, 0, 0.38889],
      "41": [0.25, 0.75, 0, 0, 0.38889],
      "42": [0, 0.75, 0, 0, 0.5],
      "43": [0.08333, 0.58333, 0, 0, 0.77778],
      "44": [0.19444, 0.10556, 0, 0, 0.27778],
      "45": [0, 0.43056, 0, 0, 0.33333],
      "46": [0, 0.10556, 0, 0, 0.27778],
      "47": [0.25, 0.75, 0, 0, 0.5],
      "48": [0, 0.64444, 0, 0, 0.5],
      "49": [0, 0.64444, 0, 0, 0.5],
      "50": [0, 0.64444, 0, 0, 0.5],
      "51": [0, 0.64444, 0, 0, 0.5],
      "52": [0, 0.64444, 0, 0, 0.5],
      "53": [0, 0.64444, 0, 0, 0.5],
      "54": [0, 0.64444, 0, 0, 0.5],
      "55": [0, 0.64444, 0, 0, 0.5],
      "56": [0, 0.64444, 0, 0, 0.5],
      "57": [0, 0.64444, 0, 0, 0.5],
      "58": [0, 0.43056, 0, 0, 0.27778],
      "59": [0.19444, 0.43056, 0, 0, 0.27778],
      "60": [0.0391, 0.5391, 0, 0, 0.77778],
      "61": [-0.13313, 0.36687, 0, 0, 0.77778],
      "62": [0.0391, 0.5391, 0, 0, 0.77778],
      "63": [0, 0.69444, 0, 0, 0.47222],
      "64": [0, 0.69444, 0, 0, 0.77778],
      "65": [0, 0.68333, 0, 0, 0.75],
      "66": [0, 0.68333, 0, 0, 0.70834],
      "67": [0, 0.68333, 0, 0, 0.72222],
      "68": [0, 0.68333, 0, 0, 0.76389],
      "69": [0, 0.68333, 0, 0, 0.68056],
      "70": [0, 0.68333, 0, 0, 0.65278],
      "71": [0, 0.68333, 0, 0, 0.78472],
      "72": [0, 0.68333, 0, 0, 0.75],
      "73": [0, 0.68333, 0, 0, 0.36111],
      "74": [0, 0.68333, 0, 0, 0.51389],
      "75": [0, 0.68333, 0, 0, 0.77778],
      "76": [0, 0.68333, 0, 0, 0.625],
      "77": [0, 0.68333, 0, 0, 0.91667],
      "78": [0, 0.68333, 0, 0, 0.75],
      "79": [0, 0.68333, 0, 0, 0.77778],
      "80": [0, 0.68333, 0, 0, 0.68056],
      "81": [0.19444, 0.68333, 0, 0, 0.77778],
      "82": [0, 0.68333, 0, 0, 0.73611],
      "83": [0, 0.68333, 0, 0, 0.55556],
      "84": [0, 0.68333, 0, 0, 0.72222],
      "85": [0, 0.68333, 0, 0, 0.75],
      "86": [0, 0.68333, 0.01389, 0, 0.75],
      "87": [0, 0.68333, 0.01389, 0, 1.02778],
      "88": [0, 0.68333, 0, 0, 0.75],
      "89": [0, 0.68333, 0.025, 0, 0.75],
      "90": [0, 0.68333, 0, 0, 0.61111],
      "91": [0.25, 0.75, 0, 0, 0.27778],
      "92": [0.25, 0.75, 0, 0, 0.5],
      "93": [0.25, 0.75, 0, 0, 0.27778],
      "94": [0, 0.69444, 0, 0, 0.5],
      "95": [0.31, 0.12056, 0.02778, 0, 0.5],
      "97": [0, 0.43056, 0, 0, 0.5],
      "98": [0, 0.69444, 0, 0, 0.55556],
      "99": [0, 0.43056, 0, 0, 0.44445],
      "100": [0, 0.69444, 0, 0, 0.55556],
      "101": [0, 0.43056, 0, 0, 0.44445],
      "102": [0, 0.69444, 0.07778, 0, 0.30556],
      "103": [0.19444, 0.43056, 0.01389, 0, 0.5],
      "104": [0, 0.69444, 0, 0, 0.55556],
      "105": [0, 0.66786, 0, 0, 0.27778],
      "106": [0.19444, 0.66786, 0, 0, 0.30556],
      "107": [0, 0.69444, 0, 0, 0.52778],
      "108": [0, 0.69444, 0, 0, 0.27778],
      "109": [0, 0.43056, 0, 0, 0.83334],
      "110": [0, 0.43056, 0, 0, 0.55556],
      "111": [0, 0.43056, 0, 0, 0.5],
      "112": [0.19444, 0.43056, 0, 0, 0.55556],
      "113": [0.19444, 0.43056, 0, 0, 0.52778],
      "114": [0, 0.43056, 0, 0, 0.39167],
      "115": [0, 0.43056, 0, 0, 0.39445],
      "116": [0, 0.61508, 0, 0, 0.38889],
      "117": [0, 0.43056, 0, 0, 0.55556],
      "118": [0, 0.43056, 0.01389, 0, 0.52778],
      "119": [0, 0.43056, 0.01389, 0, 0.72222],
      "120": [0, 0.43056, 0, 0, 0.52778],
      "121": [0.19444, 0.43056, 0.01389, 0, 0.52778],
      "122": [0, 0.43056, 0, 0, 0.44445],
      "123": [0.25, 0.75, 0, 0, 0.5],
      "124": [0.25, 0.75, 0, 0, 0.27778],
      "125": [0.25, 0.75, 0, 0, 0.5],
      "126": [0.35, 0.31786, 0, 0, 0.5],
      "160": [0, 0, 0, 0, 0.25],
      "163": [0, 0.69444, 0, 0, 0.76909],
      "167": [0.19444, 0.69444, 0, 0, 0.44445],
      "168": [0, 0.66786, 0, 0, 0.5],
      "172": [0, 0.43056, 0, 0, 0.66667],
      "176": [0, 0.69444, 0, 0, 0.75],
      "177": [0.08333, 0.58333, 0, 0, 0.77778],
      "182": [0.19444, 0.69444, 0, 0, 0.61111],
      "184": [0.17014, 0, 0, 0, 0.44445],
      "198": [0, 0.68333, 0, 0, 0.90278],
      "215": [0.08333, 0.58333, 0, 0, 0.77778],
      "216": [0.04861, 0.73194, 0, 0, 0.77778],
      "223": [0, 0.69444, 0, 0, 0.5],
      "230": [0, 0.43056, 0, 0, 0.72222],
      "247": [0.08333, 0.58333, 0, 0, 0.77778],
      "248": [0.09722, 0.52778, 0, 0, 0.5],
      "305": [0, 0.43056, 0, 0, 0.27778],
      "338": [0, 0.68333, 0, 0, 1.01389],
      "339": [0, 0.43056, 0, 0, 0.77778],
      "567": [0.19444, 0.43056, 0, 0, 0.30556],
      "710": [0, 0.69444, 0, 0, 0.5],
      "711": [0, 0.62847, 0, 0, 0.5],
      "713": [0, 0.56778, 0, 0, 0.5],
      "714": [0, 0.69444, 0, 0, 0.5],
      "715": [0, 0.69444, 0, 0, 0.5],
      "728": [0, 0.69444, 0, 0, 0.5],
      "729": [0, 0.66786, 0, 0, 0.27778],
      "730": [0, 0.69444, 0, 0, 0.75],
      "732": [0, 0.66786, 0, 0, 0.5],
      "733": [0, 0.69444, 0, 0, 0.5],
      "915": [0, 0.68333, 0, 0, 0.625],
      "916": [0, 0.68333, 0, 0, 0.83334],
      "920": [0, 0.68333, 0, 0, 0.77778],
      "923": [0, 0.68333, 0, 0, 0.69445],
      "926": [0, 0.68333, 0, 0, 0.66667],
      "928": [0, 0.68333, 0, 0, 0.75],
      "931": [0, 0.68333, 0, 0, 0.72222],
      "933": [0, 0.68333, 0, 0, 0.77778],
      "934": [0, 0.68333, 0, 0, 0.72222],
      "936": [0, 0.68333, 0, 0, 0.77778],
      "937": [0, 0.68333, 0, 0, 0.72222],
      "8211": [0, 0.43056, 0.02778, 0, 0.5],
      "8212": [0, 0.43056, 0.02778, 0, 1],
      "8216": [0, 0.69444, 0, 0, 0.27778],
      "8217": [0, 0.69444, 0, 0, 0.27778],
      "8220": [0, 0.69444, 0, 0, 0.5],
      "8221": [0, 0.69444, 0, 0, 0.5],
      "8224": [0.19444, 0.69444, 0, 0, 0.44445],
      "8225": [0.19444, 0.69444, 0, 0, 0.44445],
      "8230": [0, 0.123, 0, 0, 1.172],
      "8242": [0, 0.55556, 0, 0, 0.275],
      "8407": [0, 0.71444, 0.15382, 0, 0.5],
      "8463": [0, 0.68889, 0, 0, 0.54028],
      "8465": [0, 0.69444, 0, 0, 0.72222],
      "8467": [0, 0.69444, 0, 0.11111, 0.41667],
      "8472": [0.19444, 0.43056, 0, 0.11111, 0.63646],
      "8476": [0, 0.69444, 0, 0, 0.72222],
      "8501": [0, 0.69444, 0, 0, 0.61111],
      "8592": [-0.13313, 0.36687, 0, 0, 1],
      "8593": [0.19444, 0.69444, 0, 0, 0.5],
      "8594": [-0.13313, 0.36687, 0, 0, 1],
      "8595": [0.19444, 0.69444, 0, 0, 0.5],
      "8596": [-0.13313, 0.36687, 0, 0, 1],
      "8597": [0.25, 0.75, 0, 0, 0.5],
      "8598": [0.19444, 0.69444, 0, 0, 1],
      "8599": [0.19444, 0.69444, 0, 0, 1],
      "8600": [0.19444, 0.69444, 0, 0, 1],
      "8601": [0.19444, 0.69444, 0, 0, 1],
      "8614": [0.011, 0.511, 0, 0, 1],
      "8617": [0.011, 0.511, 0, 0, 1.126],
      "8618": [0.011, 0.511, 0, 0, 1.126],
      "8636": [-0.13313, 0.36687, 0, 0, 1],
      "8637": [-0.13313, 0.36687, 0, 0, 1],
      "8640": [-0.13313, 0.36687, 0, 0, 1],
      "8641": [-0.13313, 0.36687, 0, 0, 1],
      "8652": [0.011, 0.671, 0, 0, 1],
      "8656": [-0.13313, 0.36687, 0, 0, 1],
      "8657": [0.19444, 0.69444, 0, 0, 0.61111],
      "8658": [-0.13313, 0.36687, 0, 0, 1],
      "8659": [0.19444, 0.69444, 0, 0, 0.61111],
      "8660": [-0.13313, 0.36687, 0, 0, 1],
      "8661": [0.25, 0.75, 0, 0, 0.61111],
      "8704": [0, 0.69444, 0, 0, 0.55556],
      "8706": [0, 0.69444, 0.05556, 0.08334, 0.5309],
      "8707": [0, 0.69444, 0, 0, 0.55556],
      "8709": [0.05556, 0.75, 0, 0, 0.5],
      "8711": [0, 0.68333, 0, 0, 0.83334],
      "8712": [0.0391, 0.5391, 0, 0, 0.66667],
      "8715": [0.0391, 0.5391, 0, 0, 0.66667],
      "8722": [0.08333, 0.58333, 0, 0, 0.77778],
      "8723": [0.08333, 0.58333, 0, 0, 0.77778],
      "8725": [0.25, 0.75, 0, 0, 0.5],
      "8726": [0.25, 0.75, 0, 0, 0.5],
      "8727": [-0.03472, 0.46528, 0, 0, 0.5],
      "8728": [-0.05555, 0.44445, 0, 0, 0.5],
      "8729": [-0.05555, 0.44445, 0, 0, 0.5],
      "8730": [0.2, 0.8, 0, 0, 0.83334],
      "8733": [0, 0.43056, 0, 0, 0.77778],
      "8734": [0, 0.43056, 0, 0, 1],
      "8736": [0, 0.69224, 0, 0, 0.72222],
      "8739": [0.25, 0.75, 0, 0, 0.27778],
      "8741": [0.25, 0.75, 0, 0, 0.5],
      "8743": [0, 0.55556, 0, 0, 0.66667],
      "8744": [0, 0.55556, 0, 0, 0.66667],
      "8745": [0, 0.55556, 0, 0, 0.66667],
      "8746": [0, 0.55556, 0, 0, 0.66667],
      "8747": [0.19444, 0.69444, 0.11111, 0, 0.41667],
      "8764": [-0.13313, 0.36687, 0, 0, 0.77778],
      "8768": [0.19444, 0.69444, 0, 0, 0.27778],
      "8771": [-0.03625, 0.46375, 0, 0, 0.77778],
      "8773": [-0.022, 0.589, 0, 0, 0.778],
      "8776": [-0.01688, 0.48312, 0, 0, 0.77778],
      "8781": [-0.03625, 0.46375, 0, 0, 0.77778],
      "8784": [-0.133, 0.673, 0, 0, 0.778],
      "8801": [-0.03625, 0.46375, 0, 0, 0.77778],
      "8804": [0.13597, 0.63597, 0, 0, 0.77778],
      "8805": [0.13597, 0.63597, 0, 0, 0.77778],
      "8810": [0.0391, 0.5391, 0, 0, 1],
      "8811": [0.0391, 0.5391, 0, 0, 1],
      "8826": [0.0391, 0.5391, 0, 0, 0.77778],
      "8827": [0.0391, 0.5391, 0, 0, 0.77778],
      "8834": [0.0391, 0.5391, 0, 0, 0.77778],
      "8835": [0.0391, 0.5391, 0, 0, 0.77778],
      "8838": [0.13597, 0.63597, 0, 0, 0.77778],
      "8839": [0.13597, 0.63597, 0, 0, 0.77778],
      "8846": [0, 0.55556, 0, 0, 0.66667],
      "8849": [0.13597, 0.63597, 0, 0, 0.77778],
      "8850": [0.13597, 0.63597, 0, 0, 0.77778],
      "8851": [0, 0.55556, 0, 0, 0.66667],
      "8852": [0, 0.55556, 0, 0, 0.66667],
      "8853": [0.08333, 0.58333, 0, 0, 0.77778],
      "8854": [0.08333, 0.58333, 0, 0, 0.77778],
      "8855": [0.08333, 0.58333, 0, 0, 0.77778],
      "8856": [0.08333, 0.58333, 0, 0, 0.77778],
      "8857": [0.08333, 0.58333, 0, 0, 0.77778],
      "8866": [0, 0.69444, 0, 0, 0.61111],
      "8867": [0, 0.69444, 0, 0, 0.61111],
      "8868": [0, 0.69444, 0, 0, 0.77778],
      "8869": [0, 0.69444, 0, 0, 0.77778],
      "8872": [0.249, 0.75, 0, 0, 0.867],
      "8900": [-0.05555, 0.44445, 0, 0, 0.5],
      "8901": [-0.05555, 0.44445, 0, 0, 0.27778],
      "8902": [-0.03472, 0.46528, 0, 0, 0.5],
      "8904": [5e-3, 0.505, 0, 0, 0.9],
      "8942": [0.03, 0.903, 0, 0, 0.278],
      "8943": [-0.19, 0.313, 0, 0, 1.172],
      "8945": [-0.1, 0.823, 0, 0, 1.282],
      "8968": [0.25, 0.75, 0, 0, 0.44445],
      "8969": [0.25, 0.75, 0, 0, 0.44445],
      "8970": [0.25, 0.75, 0, 0, 0.44445],
      "8971": [0.25, 0.75, 0, 0, 0.44445],
      "8994": [-0.14236, 0.35764, 0, 0, 1],
      "8995": [-0.14236, 0.35764, 0, 0, 1],
      "9136": [0.244, 0.744, 0, 0, 0.412],
      "9137": [0.244, 0.745, 0, 0, 0.412],
      "9651": [0.19444, 0.69444, 0, 0, 0.88889],
      "9657": [-0.03472, 0.46528, 0, 0, 0.5],
      "9661": [0.19444, 0.69444, 0, 0, 0.88889],
      "9667": [-0.03472, 0.46528, 0, 0, 0.5],
      "9711": [0.19444, 0.69444, 0, 0, 1],
      "9824": [0.12963, 0.69444, 0, 0, 0.77778],
      "9825": [0.12963, 0.69444, 0, 0, 0.77778],
      "9826": [0.12963, 0.69444, 0, 0, 0.77778],
      "9827": [0.12963, 0.69444, 0, 0, 0.77778],
      "9837": [0, 0.75, 0, 0, 0.38889],
      "9838": [0.19444, 0.69444, 0, 0, 0.38889],
      "9839": [0.19444, 0.69444, 0, 0, 0.38889],
      "10216": [0.25, 0.75, 0, 0, 0.38889],
      "10217": [0.25, 0.75, 0, 0, 0.38889],
      "10222": [0.244, 0.744, 0, 0, 0.412],
      "10223": [0.244, 0.745, 0, 0, 0.412],
      "10229": [0.011, 0.511, 0, 0, 1.609],
      "10230": [0.011, 0.511, 0, 0, 1.638],
      "10231": [0.011, 0.511, 0, 0, 1.859],
      "10232": [0.024, 0.525, 0, 0, 1.609],
      "10233": [0.024, 0.525, 0, 0, 1.638],
      "10234": [0.024, 0.525, 0, 0, 1.858],
      "10236": [0.011, 0.511, 0, 0, 1.638],
      "10815": [0, 0.68333, 0, 0, 0.75],
      "10927": [0.13597, 0.63597, 0, 0, 0.77778],
      "10928": [0.13597, 0.63597, 0, 0, 0.77778],
      "57376": [0.19444, 0.69444, 0, 0, 0]
    },
    "Math-BoldItalic": {
      "32": [0, 0, 0, 0, 0.25],
      "48": [0, 0.44444, 0, 0, 0.575],
      "49": [0, 0.44444, 0, 0, 0.575],
      "50": [0, 0.44444, 0, 0, 0.575],
      "51": [0.19444, 0.44444, 0, 0, 0.575],
      "52": [0.19444, 0.44444, 0, 0, 0.575],
      "53": [0.19444, 0.44444, 0, 0, 0.575],
      "54": [0, 0.64444, 0, 0, 0.575],
      "55": [0.19444, 0.44444, 0, 0, 0.575],
      "56": [0, 0.64444, 0, 0, 0.575],
      "57": [0.19444, 0.44444, 0, 0, 0.575],
      "65": [0, 0.68611, 0, 0, 0.86944],
      "66": [0, 0.68611, 0.04835, 0, 0.8664],
      "67": [0, 0.68611, 0.06979, 0, 0.81694],
      "68": [0, 0.68611, 0.03194, 0, 0.93812],
      "69": [0, 0.68611, 0.05451, 0, 0.81007],
      "70": [0, 0.68611, 0.15972, 0, 0.68889],
      "71": [0, 0.68611, 0, 0, 0.88673],
      "72": [0, 0.68611, 0.08229, 0, 0.98229],
      "73": [0, 0.68611, 0.07778, 0, 0.51111],
      "74": [0, 0.68611, 0.10069, 0, 0.63125],
      "75": [0, 0.68611, 0.06979, 0, 0.97118],
      "76": [0, 0.68611, 0, 0, 0.75555],
      "77": [0, 0.68611, 0.11424, 0, 1.14201],
      "78": [0, 0.68611, 0.11424, 0, 0.95034],
      "79": [0, 0.68611, 0.03194, 0, 0.83666],
      "80": [0, 0.68611, 0.15972, 0, 0.72309],
      "81": [0.19444, 0.68611, 0, 0, 0.86861],
      "82": [0, 0.68611, 421e-5, 0, 0.87235],
      "83": [0, 0.68611, 0.05382, 0, 0.69271],
      "84": [0, 0.68611, 0.15972, 0, 0.63663],
      "85": [0, 0.68611, 0.11424, 0, 0.80027],
      "86": [0, 0.68611, 0.25555, 0, 0.67778],
      "87": [0, 0.68611, 0.15972, 0, 1.09305],
      "88": [0, 0.68611, 0.07778, 0, 0.94722],
      "89": [0, 0.68611, 0.25555, 0, 0.67458],
      "90": [0, 0.68611, 0.06979, 0, 0.77257],
      "97": [0, 0.44444, 0, 0, 0.63287],
      "98": [0, 0.69444, 0, 0, 0.52083],
      "99": [0, 0.44444, 0, 0, 0.51342],
      "100": [0, 0.69444, 0, 0, 0.60972],
      "101": [0, 0.44444, 0, 0, 0.55361],
      "102": [0.19444, 0.69444, 0.11042, 0, 0.56806],
      "103": [0.19444, 0.44444, 0.03704, 0, 0.5449],
      "104": [0, 0.69444, 0, 0, 0.66759],
      "105": [0, 0.69326, 0, 0, 0.4048],
      "106": [0.19444, 0.69326, 0.0622, 0, 0.47083],
      "107": [0, 0.69444, 0.01852, 0, 0.6037],
      "108": [0, 0.69444, 88e-4, 0, 0.34815],
      "109": [0, 0.44444, 0, 0, 1.0324],
      "110": [0, 0.44444, 0, 0, 0.71296],
      "111": [0, 0.44444, 0, 0, 0.58472],
      "112": [0.19444, 0.44444, 0, 0, 0.60092],
      "113": [0.19444, 0.44444, 0.03704, 0, 0.54213],
      "114": [0, 0.44444, 0.03194, 0, 0.5287],
      "115": [0, 0.44444, 0, 0, 0.53125],
      "116": [0, 0.63492, 0, 0, 0.41528],
      "117": [0, 0.44444, 0, 0, 0.68102],
      "118": [0, 0.44444, 0.03704, 0, 0.56666],
      "119": [0, 0.44444, 0.02778, 0, 0.83148],
      "120": [0, 0.44444, 0, 0, 0.65903],
      "121": [0.19444, 0.44444, 0.03704, 0, 0.59028],
      "122": [0, 0.44444, 0.04213, 0, 0.55509],
      "160": [0, 0, 0, 0, 0.25],
      "915": [0, 0.68611, 0.15972, 0, 0.65694],
      "916": [0, 0.68611, 0, 0, 0.95833],
      "920": [0, 0.68611, 0.03194, 0, 0.86722],
      "923": [0, 0.68611, 0, 0, 0.80555],
      "926": [0, 0.68611, 0.07458, 0, 0.84125],
      "928": [0, 0.68611, 0.08229, 0, 0.98229],
      "931": [0, 0.68611, 0.05451, 0, 0.88507],
      "933": [0, 0.68611, 0.15972, 0, 0.67083],
      "934": [0, 0.68611, 0, 0, 0.76666],
      "936": [0, 0.68611, 0.11653, 0, 0.71402],
      "937": [0, 0.68611, 0.04835, 0, 0.8789],
      "945": [0, 0.44444, 0, 0, 0.76064],
      "946": [0.19444, 0.69444, 0.03403, 0, 0.65972],
      "947": [0.19444, 0.44444, 0.06389, 0, 0.59003],
      "948": [0, 0.69444, 0.03819, 0, 0.52222],
      "949": [0, 0.44444, 0, 0, 0.52882],
      "950": [0.19444, 0.69444, 0.06215, 0, 0.50833],
      "951": [0.19444, 0.44444, 0.03704, 0, 0.6],
      "952": [0, 0.69444, 0.03194, 0, 0.5618],
      "953": [0, 0.44444, 0, 0, 0.41204],
      "954": [0, 0.44444, 0, 0, 0.66759],
      "955": [0, 0.69444, 0, 0, 0.67083],
      "956": [0.19444, 0.44444, 0, 0, 0.70787],
      "957": [0, 0.44444, 0.06898, 0, 0.57685],
      "958": [0.19444, 0.69444, 0.03021, 0, 0.50833],
      "959": [0, 0.44444, 0, 0, 0.58472],
      "960": [0, 0.44444, 0.03704, 0, 0.68241],
      "961": [0.19444, 0.44444, 0, 0, 0.6118],
      "962": [0.09722, 0.44444, 0.07917, 0, 0.42361],
      "963": [0, 0.44444, 0.03704, 0, 0.68588],
      "964": [0, 0.44444, 0.13472, 0, 0.52083],
      "965": [0, 0.44444, 0.03704, 0, 0.63055],
      "966": [0.19444, 0.44444, 0, 0, 0.74722],
      "967": [0.19444, 0.44444, 0, 0, 0.71805],
      "968": [0.19444, 0.69444, 0.03704, 0, 0.75833],
      "969": [0, 0.44444, 0.03704, 0, 0.71782],
      "977": [0, 0.69444, 0, 0, 0.69155],
      "981": [0.19444, 0.69444, 0, 0, 0.7125],
      "982": [0, 0.44444, 0.03194, 0, 0.975],
      "1009": [0.19444, 0.44444, 0, 0, 0.6118],
      "1013": [0, 0.44444, 0, 0, 0.48333],
      "57649": [0, 0.44444, 0, 0, 0.39352],
      "57911": [0.19444, 0.44444, 0, 0, 0.43889]
    },
    "Math-Italic": {
      "32": [0, 0, 0, 0, 0.25],
      "48": [0, 0.43056, 0, 0, 0.5],
      "49": [0, 0.43056, 0, 0, 0.5],
      "50": [0, 0.43056, 0, 0, 0.5],
      "51": [0.19444, 0.43056, 0, 0, 0.5],
      "52": [0.19444, 0.43056, 0, 0, 0.5],
      "53": [0.19444, 0.43056, 0, 0, 0.5],
      "54": [0, 0.64444, 0, 0, 0.5],
      "55": [0.19444, 0.43056, 0, 0, 0.5],
      "56": [0, 0.64444, 0, 0, 0.5],
      "57": [0.19444, 0.43056, 0, 0, 0.5],
      "65": [0, 0.68333, 0, 0.13889, 0.75],
      "66": [0, 0.68333, 0.05017, 0.08334, 0.75851],
      "67": [0, 0.68333, 0.07153, 0.08334, 0.71472],
      "68": [0, 0.68333, 0.02778, 0.05556, 0.82792],
      "69": [0, 0.68333, 0.05764, 0.08334, 0.7382],
      "70": [0, 0.68333, 0.13889, 0.08334, 0.64306],
      "71": [0, 0.68333, 0, 0.08334, 0.78625],
      "72": [0, 0.68333, 0.08125, 0.05556, 0.83125],
      "73": [0, 0.68333, 0.07847, 0.11111, 0.43958],
      "74": [0, 0.68333, 0.09618, 0.16667, 0.55451],
      "75": [0, 0.68333, 0.07153, 0.05556, 0.84931],
      "76": [0, 0.68333, 0, 0.02778, 0.68056],
      "77": [0, 0.68333, 0.10903, 0.08334, 0.97014],
      "78": [0, 0.68333, 0.10903, 0.08334, 0.80347],
      "79": [0, 0.68333, 0.02778, 0.08334, 0.76278],
      "80": [0, 0.68333, 0.13889, 0.08334, 0.64201],
      "81": [0.19444, 0.68333, 0, 0.08334, 0.79056],
      "82": [0, 0.68333, 773e-5, 0.08334, 0.75929],
      "83": [0, 0.68333, 0.05764, 0.08334, 0.6132],
      "84": [0, 0.68333, 0.13889, 0.08334, 0.58438],
      "85": [0, 0.68333, 0.10903, 0.02778, 0.68278],
      "86": [0, 0.68333, 0.22222, 0, 0.58333],
      "87": [0, 0.68333, 0.13889, 0, 0.94445],
      "88": [0, 0.68333, 0.07847, 0.08334, 0.82847],
      "89": [0, 0.68333, 0.22222, 0, 0.58056],
      "90": [0, 0.68333, 0.07153, 0.08334, 0.68264],
      "97": [0, 0.43056, 0, 0, 0.52859],
      "98": [0, 0.69444, 0, 0, 0.42917],
      "99": [0, 0.43056, 0, 0.05556, 0.43276],
      "100": [0, 0.69444, 0, 0.16667, 0.52049],
      "101": [0, 0.43056, 0, 0.05556, 0.46563],
      "102": [0.19444, 0.69444, 0.10764, 0.16667, 0.48959],
      "103": [0.19444, 0.43056, 0.03588, 0.02778, 0.47697],
      "104": [0, 0.69444, 0, 0, 0.57616],
      "105": [0, 0.65952, 0, 0, 0.34451],
      "106": [0.19444, 0.65952, 0.05724, 0, 0.41181],
      "107": [0, 0.69444, 0.03148, 0, 0.5206],
      "108": [0, 0.69444, 0.01968, 0.08334, 0.29838],
      "109": [0, 0.43056, 0, 0, 0.87801],
      "110": [0, 0.43056, 0, 0, 0.60023],
      "111": [0, 0.43056, 0, 0.05556, 0.48472],
      "112": [0.19444, 0.43056, 0, 0.08334, 0.50313],
      "113": [0.19444, 0.43056, 0.03588, 0.08334, 0.44641],
      "114": [0, 0.43056, 0.02778, 0.05556, 0.45116],
      "115": [0, 0.43056, 0, 0.05556, 0.46875],
      "116": [0, 0.61508, 0, 0.08334, 0.36111],
      "117": [0, 0.43056, 0, 0.02778, 0.57246],
      "118": [0, 0.43056, 0.03588, 0.02778, 0.48472],
      "119": [0, 0.43056, 0.02691, 0.08334, 0.71592],
      "120": [0, 0.43056, 0, 0.02778, 0.57153],
      "121": [0.19444, 0.43056, 0.03588, 0.05556, 0.49028],
      "122": [0, 0.43056, 0.04398, 0.05556, 0.46505],
      "160": [0, 0, 0, 0, 0.25],
      "915": [0, 0.68333, 0.13889, 0.08334, 0.61528],
      "916": [0, 0.68333, 0, 0.16667, 0.83334],
      "920": [0, 0.68333, 0.02778, 0.08334, 0.76278],
      "923": [0, 0.68333, 0, 0.16667, 0.69445],
      "926": [0, 0.68333, 0.07569, 0.08334, 0.74236],
      "928": [0, 0.68333, 0.08125, 0.05556, 0.83125],
      "931": [0, 0.68333, 0.05764, 0.08334, 0.77986],
      "933": [0, 0.68333, 0.13889, 0.05556, 0.58333],
      "934": [0, 0.68333, 0, 0.08334, 0.66667],
      "936": [0, 0.68333, 0.11, 0.05556, 0.61222],
      "937": [0, 0.68333, 0.05017, 0.08334, 0.7724],
      "945": [0, 0.43056, 37e-4, 0.02778, 0.6397],
      "946": [0.19444, 0.69444, 0.05278, 0.08334, 0.56563],
      "947": [0.19444, 0.43056, 0.05556, 0, 0.51773],
      "948": [0, 0.69444, 0.03785, 0.05556, 0.44444],
      "949": [0, 0.43056, 0, 0.08334, 0.46632],
      "950": [0.19444, 0.69444, 0.07378, 0.08334, 0.4375],
      "951": [0.19444, 0.43056, 0.03588, 0.05556, 0.49653],
      "952": [0, 0.69444, 0.02778, 0.08334, 0.46944],
      "953": [0, 0.43056, 0, 0.05556, 0.35394],
      "954": [0, 0.43056, 0, 0, 0.57616],
      "955": [0, 0.69444, 0, 0, 0.58334],
      "956": [0.19444, 0.43056, 0, 0.02778, 0.60255],
      "957": [0, 0.43056, 0.06366, 0.02778, 0.49398],
      "958": [0.19444, 0.69444, 0.04601, 0.11111, 0.4375],
      "959": [0, 0.43056, 0, 0.05556, 0.48472],
      "960": [0, 0.43056, 0.03588, 0, 0.57003],
      "961": [0.19444, 0.43056, 0, 0.08334, 0.51702],
      "962": [0.09722, 0.43056, 0.07986, 0.08334, 0.36285],
      "963": [0, 0.43056, 0.03588, 0, 0.57141],
      "964": [0, 0.43056, 0.1132, 0.02778, 0.43715],
      "965": [0, 0.43056, 0.03588, 0.02778, 0.54028],
      "966": [0.19444, 0.43056, 0, 0.08334, 0.65417],
      "967": [0.19444, 0.43056, 0, 0.05556, 0.62569],
      "968": [0.19444, 0.69444, 0.03588, 0.11111, 0.65139],
      "969": [0, 0.43056, 0.03588, 0, 0.62245],
      "977": [0, 0.69444, 0, 0.08334, 0.59144],
      "981": [0.19444, 0.69444, 0, 0.08334, 0.59583],
      "982": [0, 0.43056, 0.02778, 0, 0.82813],
      "1009": [0.19444, 0.43056, 0, 0.08334, 0.51702],
      "1013": [0, 0.43056, 0, 0.05556, 0.4059],
      "57649": [0, 0.43056, 0, 0.02778, 0.32246],
      "57911": [0.19444, 0.43056, 0, 0.08334, 0.38403]
    },
    "SansSerif-Bold": {
      "32": [0, 0, 0, 0, 0.25],
      "33": [0, 0.69444, 0, 0, 0.36667],
      "34": [0, 0.69444, 0, 0, 0.55834],
      "35": [0.19444, 0.69444, 0, 0, 0.91667],
      "36": [0.05556, 0.75, 0, 0, 0.55],
      "37": [0.05556, 0.75, 0, 0, 1.02912],
      "38": [0, 0.69444, 0, 0, 0.83056],
      "39": [0, 0.69444, 0, 0, 0.30556],
      "40": [0.25, 0.75, 0, 0, 0.42778],
      "41": [0.25, 0.75, 0, 0, 0.42778],
      "42": [0, 0.75, 0, 0, 0.55],
      "43": [0.11667, 0.61667, 0, 0, 0.85556],
      "44": [0.10556, 0.13056, 0, 0, 0.30556],
      "45": [0, 0.45833, 0, 0, 0.36667],
      "46": [0, 0.13056, 0, 0, 0.30556],
      "47": [0.25, 0.75, 0, 0, 0.55],
      "48": [0, 0.69444, 0, 0, 0.55],
      "49": [0, 0.69444, 0, 0, 0.55],
      "50": [0, 0.69444, 0, 0, 0.55],
      "51": [0, 0.69444, 0, 0, 0.55],
      "52": [0, 0.69444, 0, 0, 0.55],
      "53": [0, 0.69444, 0, 0, 0.55],
      "54": [0, 0.69444, 0, 0, 0.55],
      "55": [0, 0.69444, 0, 0, 0.55],
      "56": [0, 0.69444, 0, 0, 0.55],
      "57": [0, 0.69444, 0, 0, 0.55],
      "58": [0, 0.45833, 0, 0, 0.30556],
      "59": [0.10556, 0.45833, 0, 0, 0.30556],
      "61": [-0.09375, 0.40625, 0, 0, 0.85556],
      "63": [0, 0.69444, 0, 0, 0.51945],
      "64": [0, 0.69444, 0, 0, 0.73334],
      "65": [0, 0.69444, 0, 0, 0.73334],
      "66": [0, 0.69444, 0, 0, 0.73334],
      "67": [0, 0.69444, 0, 0, 0.70278],
      "68": [0, 0.69444, 0, 0, 0.79445],
      "69": [0, 0.69444, 0, 0, 0.64167],
      "70": [0, 0.69444, 0, 0, 0.61111],
      "71": [0, 0.69444, 0, 0, 0.73334],
      "72": [0, 0.69444, 0, 0, 0.79445],
      "73": [0, 0.69444, 0, 0, 0.33056],
      "74": [0, 0.69444, 0, 0, 0.51945],
      "75": [0, 0.69444, 0, 0, 0.76389],
      "76": [0, 0.69444, 0, 0, 0.58056],
      "77": [0, 0.69444, 0, 0, 0.97778],
      "78": [0, 0.69444, 0, 0, 0.79445],
      "79": [0, 0.69444, 0, 0, 0.79445],
      "80": [0, 0.69444, 0, 0, 0.70278],
      "81": [0.10556, 0.69444, 0, 0, 0.79445],
      "82": [0, 0.69444, 0, 0, 0.70278],
      "83": [0, 0.69444, 0, 0, 0.61111],
      "84": [0, 0.69444, 0, 0, 0.73334],
      "85": [0, 0.69444, 0, 0, 0.76389],
      "86": [0, 0.69444, 0.01528, 0, 0.73334],
      "87": [0, 0.69444, 0.01528, 0, 1.03889],
      "88": [0, 0.69444, 0, 0, 0.73334],
      "89": [0, 0.69444, 0.0275, 0, 0.73334],
      "90": [0, 0.69444, 0, 0, 0.67223],
      "91": [0.25, 0.75, 0, 0, 0.34306],
      "93": [0.25, 0.75, 0, 0, 0.34306],
      "94": [0, 0.69444, 0, 0, 0.55],
      "95": [0.35, 0.10833, 0.03056, 0, 0.55],
      "97": [0, 0.45833, 0, 0, 0.525],
      "98": [0, 0.69444, 0, 0, 0.56111],
      "99": [0, 0.45833, 0, 0, 0.48889],
      "100": [0, 0.69444, 0, 0, 0.56111],
      "101": [0, 0.45833, 0, 0, 0.51111],
      "102": [0, 0.69444, 0.07639, 0, 0.33611],
      "103": [0.19444, 0.45833, 0.01528, 0, 0.55],
      "104": [0, 0.69444, 0, 0, 0.56111],
      "105": [0, 0.69444, 0, 0, 0.25556],
      "106": [0.19444, 0.69444, 0, 0, 0.28611],
      "107": [0, 0.69444, 0, 0, 0.53056],
      "108": [0, 0.69444, 0, 0, 0.25556],
      "109": [0, 0.45833, 0, 0, 0.86667],
      "110": [0, 0.45833, 0, 0, 0.56111],
      "111": [0, 0.45833, 0, 0, 0.55],
      "112": [0.19444, 0.45833, 0, 0, 0.56111],
      "113": [0.19444, 0.45833, 0, 0, 0.56111],
      "114": [0, 0.45833, 0.01528, 0, 0.37222],
      "115": [0, 0.45833, 0, 0, 0.42167],
      "116": [0, 0.58929, 0, 0, 0.40417],
      "117": [0, 0.45833, 0, 0, 0.56111],
      "118": [0, 0.45833, 0.01528, 0, 0.5],
      "119": [0, 0.45833, 0.01528, 0, 0.74445],
      "120": [0, 0.45833, 0, 0, 0.5],
      "121": [0.19444, 0.45833, 0.01528, 0, 0.5],
      "122": [0, 0.45833, 0, 0, 0.47639],
      "126": [0.35, 0.34444, 0, 0, 0.55],
      "160": [0, 0, 0, 0, 0.25],
      "168": [0, 0.69444, 0, 0, 0.55],
      "176": [0, 0.69444, 0, 0, 0.73334],
      "180": [0, 0.69444, 0, 0, 0.55],
      "184": [0.17014, 0, 0, 0, 0.48889],
      "305": [0, 0.45833, 0, 0, 0.25556],
      "567": [0.19444, 0.45833, 0, 0, 0.28611],
      "710": [0, 0.69444, 0, 0, 0.55],
      "711": [0, 0.63542, 0, 0, 0.55],
      "713": [0, 0.63778, 0, 0, 0.55],
      "728": [0, 0.69444, 0, 0, 0.55],
      "729": [0, 0.69444, 0, 0, 0.30556],
      "730": [0, 0.69444, 0, 0, 0.73334],
      "732": [0, 0.69444, 0, 0, 0.55],
      "733": [0, 0.69444, 0, 0, 0.55],
      "915": [0, 0.69444, 0, 0, 0.58056],
      "916": [0, 0.69444, 0, 0, 0.91667],
      "920": [0, 0.69444, 0, 0, 0.85556],
      "923": [0, 0.69444, 0, 0, 0.67223],
      "926": [0, 0.69444, 0, 0, 0.73334],
      "928": [0, 0.69444, 0, 0, 0.79445],
      "931": [0, 0.69444, 0, 0, 0.79445],
      "933": [0, 0.69444, 0, 0, 0.85556],
      "934": [0, 0.69444, 0, 0, 0.79445],
      "936": [0, 0.69444, 0, 0, 0.85556],
      "937": [0, 0.69444, 0, 0, 0.79445],
      "8211": [0, 0.45833, 0.03056, 0, 0.55],
      "8212": [0, 0.45833, 0.03056, 0, 1.10001],
      "8216": [0, 0.69444, 0, 0, 0.30556],
      "8217": [0, 0.69444, 0, 0, 0.30556],
      "8220": [0, 0.69444, 0, 0, 0.55834],
      "8221": [0, 0.69444, 0, 0, 0.55834]
    },
    "SansSerif-Italic": {
      "32": [0, 0, 0, 0, 0.25],
      "33": [0, 0.69444, 0.05733, 0, 0.31945],
      "34": [0, 0.69444, 316e-5, 0, 0.5],
      "35": [0.19444, 0.69444, 0.05087, 0, 0.83334],
      "36": [0.05556, 0.75, 0.11156, 0, 0.5],
      "37": [0.05556, 0.75, 0.03126, 0, 0.83334],
      "38": [0, 0.69444, 0.03058, 0, 0.75834],
      "39": [0, 0.69444, 0.07816, 0, 0.27778],
      "40": [0.25, 0.75, 0.13164, 0, 0.38889],
      "41": [0.25, 0.75, 0.02536, 0, 0.38889],
      "42": [0, 0.75, 0.11775, 0, 0.5],
      "43": [0.08333, 0.58333, 0.02536, 0, 0.77778],
      "44": [0.125, 0.08333, 0, 0, 0.27778],
      "45": [0, 0.44444, 0.01946, 0, 0.33333],
      "46": [0, 0.08333, 0, 0, 0.27778],
      "47": [0.25, 0.75, 0.13164, 0, 0.5],
      "48": [0, 0.65556, 0.11156, 0, 0.5],
      "49": [0, 0.65556, 0.11156, 0, 0.5],
      "50": [0, 0.65556, 0.11156, 0, 0.5],
      "51": [0, 0.65556, 0.11156, 0, 0.5],
      "52": [0, 0.65556, 0.11156, 0, 0.5],
      "53": [0, 0.65556, 0.11156, 0, 0.5],
      "54": [0, 0.65556, 0.11156, 0, 0.5],
      "55": [0, 0.65556, 0.11156, 0, 0.5],
      "56": [0, 0.65556, 0.11156, 0, 0.5],
      "57": [0, 0.65556, 0.11156, 0, 0.5],
      "58": [0, 0.44444, 0.02502, 0, 0.27778],
      "59": [0.125, 0.44444, 0.02502, 0, 0.27778],
      "61": [-0.13, 0.37, 0.05087, 0, 0.77778],
      "63": [0, 0.69444, 0.11809, 0, 0.47222],
      "64": [0, 0.69444, 0.07555, 0, 0.66667],
      "65": [0, 0.69444, 0, 0, 0.66667],
      "66": [0, 0.69444, 0.08293, 0, 0.66667],
      "67": [0, 0.69444, 0.11983, 0, 0.63889],
      "68": [0, 0.69444, 0.07555, 0, 0.72223],
      "69": [0, 0.69444, 0.11983, 0, 0.59722],
      "70": [0, 0.69444, 0.13372, 0, 0.56945],
      "71": [0, 0.69444, 0.11983, 0, 0.66667],
      "72": [0, 0.69444, 0.08094, 0, 0.70834],
      "73": [0, 0.69444, 0.13372, 0, 0.27778],
      "74": [0, 0.69444, 0.08094, 0, 0.47222],
      "75": [0, 0.69444, 0.11983, 0, 0.69445],
      "76": [0, 0.69444, 0, 0, 0.54167],
      "77": [0, 0.69444, 0.08094, 0, 0.875],
      "78": [0, 0.69444, 0.08094, 0, 0.70834],
      "79": [0, 0.69444, 0.07555, 0, 0.73611],
      "80": [0, 0.69444, 0.08293, 0, 0.63889],
      "81": [0.125, 0.69444, 0.07555, 0, 0.73611],
      "82": [0, 0.69444, 0.08293, 0, 0.64584],
      "83": [0, 0.69444, 0.09205, 0, 0.55556],
      "84": [0, 0.69444, 0.13372, 0, 0.68056],
      "85": [0, 0.69444, 0.08094, 0, 0.6875],
      "86": [0, 0.69444, 0.1615, 0, 0.66667],
      "87": [0, 0.69444, 0.1615, 0, 0.94445],
      "88": [0, 0.69444, 0.13372, 0, 0.66667],
      "89": [0, 0.69444, 0.17261, 0, 0.66667],
      "90": [0, 0.69444, 0.11983, 0, 0.61111],
      "91": [0.25, 0.75, 0.15942, 0, 0.28889],
      "93": [0.25, 0.75, 0.08719, 0, 0.28889],
      "94": [0, 0.69444, 0.0799, 0, 0.5],
      "95": [0.35, 0.09444, 0.08616, 0, 0.5],
      "97": [0, 0.44444, 981e-5, 0, 0.48056],
      "98": [0, 0.69444, 0.03057, 0, 0.51667],
      "99": [0, 0.44444, 0.08336, 0, 0.44445],
      "100": [0, 0.69444, 0.09483, 0, 0.51667],
      "101": [0, 0.44444, 0.06778, 0, 0.44445],
      "102": [0, 0.69444, 0.21705, 0, 0.30556],
      "103": [0.19444, 0.44444, 0.10836, 0, 0.5],
      "104": [0, 0.69444, 0.01778, 0, 0.51667],
      "105": [0, 0.67937, 0.09718, 0, 0.23889],
      "106": [0.19444, 0.67937, 0.09162, 0, 0.26667],
      "107": [0, 0.69444, 0.08336, 0, 0.48889],
      "108": [0, 0.69444, 0.09483, 0, 0.23889],
      "109": [0, 0.44444, 0.01778, 0, 0.79445],
      "110": [0, 0.44444, 0.01778, 0, 0.51667],
      "111": [0, 0.44444, 0.06613, 0, 0.5],
      "112": [0.19444, 0.44444, 0.0389, 0, 0.51667],
      "113": [0.19444, 0.44444, 0.04169, 0, 0.51667],
      "114": [0, 0.44444, 0.10836, 0, 0.34167],
      "115": [0, 0.44444, 0.0778, 0, 0.38333],
      "116": [0, 0.57143, 0.07225, 0, 0.36111],
      "117": [0, 0.44444, 0.04169, 0, 0.51667],
      "118": [0, 0.44444, 0.10836, 0, 0.46111],
      "119": [0, 0.44444, 0.10836, 0, 0.68334],
      "120": [0, 0.44444, 0.09169, 0, 0.46111],
      "121": [0.19444, 0.44444, 0.10836, 0, 0.46111],
      "122": [0, 0.44444, 0.08752, 0, 0.43472],
      "126": [0.35, 0.32659, 0.08826, 0, 0.5],
      "160": [0, 0, 0, 0, 0.25],
      "168": [0, 0.67937, 0.06385, 0, 0.5],
      "176": [0, 0.69444, 0, 0, 0.73752],
      "184": [0.17014, 0, 0, 0, 0.44445],
      "305": [0, 0.44444, 0.04169, 0, 0.23889],
      "567": [0.19444, 0.44444, 0.04169, 0, 0.26667],
      "710": [0, 0.69444, 0.0799, 0, 0.5],
      "711": [0, 0.63194, 0.08432, 0, 0.5],
      "713": [0, 0.60889, 0.08776, 0, 0.5],
      "714": [0, 0.69444, 0.09205, 0, 0.5],
      "715": [0, 0.69444, 0, 0, 0.5],
      "728": [0, 0.69444, 0.09483, 0, 0.5],
      "729": [0, 0.67937, 0.07774, 0, 0.27778],
      "730": [0, 0.69444, 0, 0, 0.73752],
      "732": [0, 0.67659, 0.08826, 0, 0.5],
      "733": [0, 0.69444, 0.09205, 0, 0.5],
      "915": [0, 0.69444, 0.13372, 0, 0.54167],
      "916": [0, 0.69444, 0, 0, 0.83334],
      "920": [0, 0.69444, 0.07555, 0, 0.77778],
      "923": [0, 0.69444, 0, 0, 0.61111],
      "926": [0, 0.69444, 0.12816, 0, 0.66667],
      "928": [0, 0.69444, 0.08094, 0, 0.70834],
      "931": [0, 0.69444, 0.11983, 0, 0.72222],
      "933": [0, 0.69444, 0.09031, 0, 0.77778],
      "934": [0, 0.69444, 0.04603, 0, 0.72222],
      "936": [0, 0.69444, 0.09031, 0, 0.77778],
      "937": [0, 0.69444, 0.08293, 0, 0.72222],
      "8211": [0, 0.44444, 0.08616, 0, 0.5],
      "8212": [0, 0.44444, 0.08616, 0, 1],
      "8216": [0, 0.69444, 0.07816, 0, 0.27778],
      "8217": [0, 0.69444, 0.07816, 0, 0.27778],
      "8220": [0, 0.69444, 0.14205, 0, 0.5],
      "8221": [0, 0.69444, 316e-5, 0, 0.5]
    },
    "SansSerif-Regular": {
      "32": [0, 0, 0, 0, 0.25],
      "33": [0, 0.69444, 0, 0, 0.31945],
      "34": [0, 0.69444, 0, 0, 0.5],
      "35": [0.19444, 0.69444, 0, 0, 0.83334],
      "36": [0.05556, 0.75, 0, 0, 0.5],
      "37": [0.05556, 0.75, 0, 0, 0.83334],
      "38": [0, 0.69444, 0, 0, 0.75834],
      "39": [0, 0.69444, 0, 0, 0.27778],
      "40": [0.25, 0.75, 0, 0, 0.38889],
      "41": [0.25, 0.75, 0, 0, 0.38889],
      "42": [0, 0.75, 0, 0, 0.5],
      "43": [0.08333, 0.58333, 0, 0, 0.77778],
      "44": [0.125, 0.08333, 0, 0, 0.27778],
      "45": [0, 0.44444, 0, 0, 0.33333],
      "46": [0, 0.08333, 0, 0, 0.27778],
      "47": [0.25, 0.75, 0, 0, 0.5],
      "48": [0, 0.65556, 0, 0, 0.5],
      "49": [0, 0.65556, 0, 0, 0.5],
      "50": [0, 0.65556, 0, 0, 0.5],
      "51": [0, 0.65556, 0, 0, 0.5],
      "52": [0, 0.65556, 0, 0, 0.5],
      "53": [0, 0.65556, 0, 0, 0.5],
      "54": [0, 0.65556, 0, 0, 0.5],
      "55": [0, 0.65556, 0, 0, 0.5],
      "56": [0, 0.65556, 0, 0, 0.5],
      "57": [0, 0.65556, 0, 0, 0.5],
      "58": [0, 0.44444, 0, 0, 0.27778],
      "59": [0.125, 0.44444, 0, 0, 0.27778],
      "61": [-0.13, 0.37, 0, 0, 0.77778],
      "63": [0, 0.69444, 0, 0, 0.47222],
      "64": [0, 0.69444, 0, 0, 0.66667],
      "65": [0, 0.69444, 0, 0, 0.66667],
      "66": [0, 0.69444, 0, 0, 0.66667],
      "67": [0, 0.69444, 0, 0, 0.63889],
      "68": [0, 0.69444, 0, 0, 0.72223],
      "69": [0, 0.69444, 0, 0, 0.59722],
      "70": [0, 0.69444, 0, 0, 0.56945],
      "71": [0, 0.69444, 0, 0, 0.66667],
      "72": [0, 0.69444, 0, 0, 0.70834],
      "73": [0, 0.69444, 0, 0, 0.27778],
      "74": [0, 0.69444, 0, 0, 0.47222],
      "75": [0, 0.69444, 0, 0, 0.69445],
      "76": [0, 0.69444, 0, 0, 0.54167],
      "77": [0, 0.69444, 0, 0, 0.875],
      "78": [0, 0.69444, 0, 0, 0.70834],
      "79": [0, 0.69444, 0, 0, 0.73611],
      "80": [0, 0.69444, 0, 0, 0.63889],
      "81": [0.125, 0.69444, 0, 0, 0.73611],
      "82": [0, 0.69444, 0, 0, 0.64584],
      "83": [0, 0.69444, 0, 0, 0.55556],
      "84": [0, 0.69444, 0, 0, 0.68056],
      "85": [0, 0.69444, 0, 0, 0.6875],
      "86": [0, 0.69444, 0.01389, 0, 0.66667],
      "87": [0, 0.69444, 0.01389, 0, 0.94445],
      "88": [0, 0.69444, 0, 0, 0.66667],
      "89": [0, 0.69444, 0.025, 0, 0.66667],
      "90": [0, 0.69444, 0, 0, 0.61111],
      "91": [0.25, 0.75, 0, 0, 0.28889],
      "93": [0.25, 0.75, 0, 0, 0.28889],
      "94": [0, 0.69444, 0, 0, 0.5],
      "95": [0.35, 0.09444, 0.02778, 0, 0.5],
      "97": [0, 0.44444, 0, 0, 0.48056],
      "98": [0, 0.69444, 0, 0, 0.51667],
      "99": [0, 0.44444, 0, 0, 0.44445],
      "100": [0, 0.69444, 0, 0, 0.51667],
      "101": [0, 0.44444, 0, 0, 0.44445],
      "102": [0, 0.69444, 0.06944, 0, 0.30556],
      "103": [0.19444, 0.44444, 0.01389, 0, 0.5],
      "104": [0, 0.69444, 0, 0, 0.51667],
      "105": [0, 0.67937, 0, 0, 0.23889],
      "106": [0.19444, 0.67937, 0, 0, 0.26667],
      "107": [0, 0.69444, 0, 0, 0.48889],
      "108": [0, 0.69444, 0, 0, 0.23889],
      "109": [0, 0.44444, 0, 0, 0.79445],
      "110": [0, 0.44444, 0, 0, 0.51667],
      "111": [0, 0.44444, 0, 0, 0.5],
      "112": [0.19444, 0.44444, 0, 0, 0.51667],
      "113": [0.19444, 0.44444, 0, 0, 0.51667],
      "114": [0, 0.44444, 0.01389, 0, 0.34167],
      "115": [0, 0.44444, 0, 0, 0.38333],
      "116": [0, 0.57143, 0, 0, 0.36111],
      "117": [0, 0.44444, 0, 0, 0.51667],
      "118": [0, 0.44444, 0.01389, 0, 0.46111],
      "119": [0, 0.44444, 0.01389, 0, 0.68334],
      "120": [0, 0.44444, 0, 0, 0.46111],
      "121": [0.19444, 0.44444, 0.01389, 0, 0.46111],
      "122": [0, 0.44444, 0, 0, 0.43472],
      "126": [0.35, 0.32659, 0, 0, 0.5],
      "160": [0, 0, 0, 0, 0.25],
      "168": [0, 0.67937, 0, 0, 0.5],
      "176": [0, 0.69444, 0, 0, 0.66667],
      "184": [0.17014, 0, 0, 0, 0.44445],
      "305": [0, 0.44444, 0, 0, 0.23889],
      "567": [0.19444, 0.44444, 0, 0, 0.26667],
      "710": [0, 0.69444, 0, 0, 0.5],
      "711": [0, 0.63194, 0, 0, 0.5],
      "713": [0, 0.60889, 0, 0, 0.5],
      "714": [0, 0.69444, 0, 0, 0.5],
      "715": [0, 0.69444, 0, 0, 0.5],
      "728": [0, 0.69444, 0, 0, 0.5],
      "729": [0, 0.67937, 0, 0, 0.27778],
      "730": [0, 0.69444, 0, 0, 0.66667],
      "732": [0, 0.67659, 0, 0, 0.5],
      "733": [0, 0.69444, 0, 0, 0.5],
      "915": [0, 0.69444, 0, 0, 0.54167],
      "916": [0, 0.69444, 0, 0, 0.83334],
      "920": [0, 0.69444, 0, 0, 0.77778],
      "923": [0, 0.69444, 0, 0, 0.61111],
      "926": [0, 0.69444, 0, 0, 0.66667],
      "928": [0, 0.69444, 0, 0, 0.70834],
      "931": [0, 0.69444, 0, 0, 0.72222],
      "933": [0, 0.69444, 0, 0, 0.77778],
      "934": [0, 0.69444, 0, 0, 0.72222],
      "936": [0, 0.69444, 0, 0, 0.77778],
      "937": [0, 0.69444, 0, 0, 0.72222],
      "8211": [0, 0.44444, 0.02778, 0, 0.5],
      "8212": [0, 0.44444, 0.02778, 0, 1],
      "8216": [0, 0.69444, 0, 0, 0.27778],
      "8217": [0, 0.69444, 0, 0, 0.27778],
      "8220": [0, 0.69444, 0, 0, 0.5],
      "8221": [0, 0.69444, 0, 0, 0.5]
    },
    "Script-Regular": {
      "32": [0, 0, 0, 0, 0.25],
      "65": [0, 0.7, 0.22925, 0, 0.80253],
      "66": [0, 0.7, 0.04087, 0, 0.90757],
      "67": [0, 0.7, 0.1689, 0, 0.66619],
      "68": [0, 0.7, 0.09371, 0, 0.77443],
      "69": [0, 0.7, 0.18583, 0, 0.56162],
      "70": [0, 0.7, 0.13634, 0, 0.89544],
      "71": [0, 0.7, 0.17322, 0, 0.60961],
      "72": [0, 0.7, 0.29694, 0, 0.96919],
      "73": [0, 0.7, 0.19189, 0, 0.80907],
      "74": [0.27778, 0.7, 0.19189, 0, 1.05159],
      "75": [0, 0.7, 0.31259, 0, 0.91364],
      "76": [0, 0.7, 0.19189, 0, 0.87373],
      "77": [0, 0.7, 0.15981, 0, 1.08031],
      "78": [0, 0.7, 0.3525, 0, 0.9015],
      "79": [0, 0.7, 0.08078, 0, 0.73787],
      "80": [0, 0.7, 0.08078, 0, 1.01262],
      "81": [0, 0.7, 0.03305, 0, 0.88282],
      "82": [0, 0.7, 0.06259, 0, 0.85],
      "83": [0, 0.7, 0.19189, 0, 0.86767],
      "84": [0, 0.7, 0.29087, 0, 0.74697],
      "85": [0, 0.7, 0.25815, 0, 0.79996],
      "86": [0, 0.7, 0.27523, 0, 0.62204],
      "87": [0, 0.7, 0.27523, 0, 0.80532],
      "88": [0, 0.7, 0.26006, 0, 0.94445],
      "89": [0, 0.7, 0.2939, 0, 0.70961],
      "90": [0, 0.7, 0.24037, 0, 0.8212],
      "160": [0, 0, 0, 0, 0.25]
    },
    "Size1-Regular": {
      "32": [0, 0, 0, 0, 0.25],
      "40": [0.35001, 0.85, 0, 0, 0.45834],
      "41": [0.35001, 0.85, 0, 0, 0.45834],
      "47": [0.35001, 0.85, 0, 0, 0.57778],
      "91": [0.35001, 0.85, 0, 0, 0.41667],
      "92": [0.35001, 0.85, 0, 0, 0.57778],
      "93": [0.35001, 0.85, 0, 0, 0.41667],
      "123": [0.35001, 0.85, 0, 0, 0.58334],
      "125": [0.35001, 0.85, 0, 0, 0.58334],
      "160": [0, 0, 0, 0, 0.25],
      "710": [0, 0.72222, 0, 0, 0.55556],
      "732": [0, 0.72222, 0, 0, 0.55556],
      "770": [0, 0.72222, 0, 0, 0.55556],
      "771": [0, 0.72222, 0, 0, 0.55556],
      "8214": [-99e-5, 0.601, 0, 0, 0.77778],
      "8593": [1e-5, 0.6, 0, 0, 0.66667],
      "8595": [1e-5, 0.6, 0, 0, 0.66667],
      "8657": [1e-5, 0.6, 0, 0, 0.77778],
      "8659": [1e-5, 0.6, 0, 0, 0.77778],
      "8719": [0.25001, 0.75, 0, 0, 0.94445],
      "8720": [0.25001, 0.75, 0, 0, 0.94445],
      "8721": [0.25001, 0.75, 0, 0, 1.05556],
      "8730": [0.35001, 0.85, 0, 0, 1],
      "8739": [-599e-5, 0.606, 0, 0, 0.33333],
      "8741": [-599e-5, 0.606, 0, 0, 0.55556],
      "8747": [0.30612, 0.805, 0.19445, 0, 0.47222],
      "8748": [0.306, 0.805, 0.19445, 0, 0.47222],
      "8749": [0.306, 0.805, 0.19445, 0, 0.47222],
      "8750": [0.30612, 0.805, 0.19445, 0, 0.47222],
      "8896": [0.25001, 0.75, 0, 0, 0.83334],
      "8897": [0.25001, 0.75, 0, 0, 0.83334],
      "8898": [0.25001, 0.75, 0, 0, 0.83334],
      "8899": [0.25001, 0.75, 0, 0, 0.83334],
      "8968": [0.35001, 0.85, 0, 0, 0.47222],
      "8969": [0.35001, 0.85, 0, 0, 0.47222],
      "8970": [0.35001, 0.85, 0, 0, 0.47222],
      "8971": [0.35001, 0.85, 0, 0, 0.47222],
      "9168": [-99e-5, 0.601, 0, 0, 0.66667],
      "10216": [0.35001, 0.85, 0, 0, 0.47222],
      "10217": [0.35001, 0.85, 0, 0, 0.47222],
      "10752": [0.25001, 0.75, 0, 0, 1.11111],
      "10753": [0.25001, 0.75, 0, 0, 1.11111],
      "10754": [0.25001, 0.75, 0, 0, 1.11111],
      "10756": [0.25001, 0.75, 0, 0, 0.83334],
      "10758": [0.25001, 0.75, 0, 0, 0.83334]
    },
    "Size2-Regular": {
      "32": [0, 0, 0, 0, 0.25],
      "40": [0.65002, 1.15, 0, 0, 0.59722],
      "41": [0.65002, 1.15, 0, 0, 0.59722],
      "47": [0.65002, 1.15, 0, 0, 0.81111],
      "91": [0.65002, 1.15, 0, 0, 0.47222],
      "92": [0.65002, 1.15, 0, 0, 0.81111],
      "93": [0.65002, 1.15, 0, 0, 0.47222],
      "123": [0.65002, 1.15, 0, 0, 0.66667],
      "125": [0.65002, 1.15, 0, 0, 0.66667],
      "160": [0, 0, 0, 0, 0.25],
      "710": [0, 0.75, 0, 0, 1],
      "732": [0, 0.75, 0, 0, 1],
      "770": [0, 0.75, 0, 0, 1],
      "771": [0, 0.75, 0, 0, 1],
      "8719": [0.55001, 1.05, 0, 0, 1.27778],
      "8720": [0.55001, 1.05, 0, 0, 1.27778],
      "8721": [0.55001, 1.05, 0, 0, 1.44445],
      "8730": [0.65002, 1.15, 0, 0, 1],
      "8747": [0.86225, 1.36, 0.44445, 0, 0.55556],
      "8748": [0.862, 1.36, 0.44445, 0, 0.55556],
      "8749": [0.862, 1.36, 0.44445, 0, 0.55556],
      "8750": [0.86225, 1.36, 0.44445, 0, 0.55556],
      "8896": [0.55001, 1.05, 0, 0, 1.11111],
      "8897": [0.55001, 1.05, 0, 0, 1.11111],
      "8898": [0.55001, 1.05, 0, 0, 1.11111],
      "8899": [0.55001, 1.05, 0, 0, 1.11111],
      "8968": [0.65002, 1.15, 0, 0, 0.52778],
      "8969": [0.65002, 1.15, 0, 0, 0.52778],
      "8970": [0.65002, 1.15, 0, 0, 0.52778],
      "8971": [0.65002, 1.15, 0, 0, 0.52778],
      "10216": [0.65002, 1.15, 0, 0, 0.61111],
      "10217": [0.65002, 1.15, 0, 0, 0.61111],
      "10752": [0.55001, 1.05, 0, 0, 1.51112],
      "10753": [0.55001, 1.05, 0, 0, 1.51112],
      "10754": [0.55001, 1.05, 0, 0, 1.51112],
      "10756": [0.55001, 1.05, 0, 0, 1.11111],
      "10758": [0.55001, 1.05, 0, 0, 1.11111]
    },
    "Size3-Regular": {
      "32": [0, 0, 0, 0, 0.25],
      "40": [0.95003, 1.45, 0, 0, 0.73611],
      "41": [0.95003, 1.45, 0, 0, 0.73611],
      "47": [0.95003, 1.45, 0, 0, 1.04445],
      "91": [0.95003, 1.45, 0, 0, 0.52778],
      "92": [0.95003, 1.45, 0, 0, 1.04445],
      "93": [0.95003, 1.45, 0, 0, 0.52778],
      "123": [0.95003, 1.45, 0, 0, 0.75],
      "125": [0.95003, 1.45, 0, 0, 0.75],
      "160": [0, 0, 0, 0, 0.25],
      "710": [0, 0.75, 0, 0, 1.44445],
      "732": [0, 0.75, 0, 0, 1.44445],
      "770": [0, 0.75, 0, 0, 1.44445],
      "771": [0, 0.75, 0, 0, 1.44445],
      "8730": [0.95003, 1.45, 0, 0, 1],
      "8968": [0.95003, 1.45, 0, 0, 0.58334],
      "8969": [0.95003, 1.45, 0, 0, 0.58334],
      "8970": [0.95003, 1.45, 0, 0, 0.58334],
      "8971": [0.95003, 1.45, 0, 0, 0.58334],
      "10216": [0.95003, 1.45, 0, 0, 0.75],
      "10217": [0.95003, 1.45, 0, 0, 0.75]
    },
    "Size4-Regular": {
      "32": [0, 0, 0, 0, 0.25],
      "40": [1.25003, 1.75, 0, 0, 0.79167],
      "41": [1.25003, 1.75, 0, 0, 0.79167],
      "47": [1.25003, 1.75, 0, 0, 1.27778],
      "91": [1.25003, 1.75, 0, 0, 0.58334],
      "92": [1.25003, 1.75, 0, 0, 1.27778],
      "93": [1.25003, 1.75, 0, 0, 0.58334],
      "123": [1.25003, 1.75, 0, 0, 0.80556],
      "125": [1.25003, 1.75, 0, 0, 0.80556],
      "160": [0, 0, 0, 0, 0.25],
      "710": [0, 0.825, 0, 0, 1.8889],
      "732": [0, 0.825, 0, 0, 1.8889],
      "770": [0, 0.825, 0, 0, 1.8889],
      "771": [0, 0.825, 0, 0, 1.8889],
      "8730": [1.25003, 1.75, 0, 0, 1],
      "8968": [1.25003, 1.75, 0, 0, 0.63889],
      "8969": [1.25003, 1.75, 0, 0, 0.63889],
      "8970": [1.25003, 1.75, 0, 0, 0.63889],
      "8971": [1.25003, 1.75, 0, 0, 0.63889],
      "9115": [0.64502, 1.155, 0, 0, 0.875],
      "9116": [1e-5, 0.6, 0, 0, 0.875],
      "9117": [0.64502, 1.155, 0, 0, 0.875],
      "9118": [0.64502, 1.155, 0, 0, 0.875],
      "9119": [1e-5, 0.6, 0, 0, 0.875],
      "9120": [0.64502, 1.155, 0, 0, 0.875],
      "9121": [0.64502, 1.155, 0, 0, 0.66667],
      "9122": [-99e-5, 0.601, 0, 0, 0.66667],
      "9123": [0.64502, 1.155, 0, 0, 0.66667],
      "9124": [0.64502, 1.155, 0, 0, 0.66667],
      "9125": [-99e-5, 0.601, 0, 0, 0.66667],
      "9126": [0.64502, 1.155, 0, 0, 0.66667],
      "9127": [1e-5, 0.9, 0, 0, 0.88889],
      "9128": [0.65002, 1.15, 0, 0, 0.88889],
      "9129": [0.90001, 0, 0, 0, 0.88889],
      "9130": [0, 0.3, 0, 0, 0.88889],
      "9131": [1e-5, 0.9, 0, 0, 0.88889],
      "9132": [0.65002, 1.15, 0, 0, 0.88889],
      "9133": [0.90001, 0, 0, 0, 0.88889],
      "9143": [0.88502, 0.915, 0, 0, 1.05556],
      "10216": [1.25003, 1.75, 0, 0, 0.80556],
      "10217": [1.25003, 1.75, 0, 0, 0.80556],
      "57344": [-499e-5, 0.605, 0, 0, 1.05556],
      "57345": [-499e-5, 0.605, 0, 0, 1.05556],
      "57680": [0, 0.12, 0, 0, 0.45],
      "57681": [0, 0.12, 0, 0, 0.45],
      "57682": [0, 0.12, 0, 0, 0.45],
      "57683": [0, 0.12, 0, 0, 0.45]
    },
    "Typewriter-Regular": {
      "32": [0, 0, 0, 0, 0.525],
      "33": [0, 0.61111, 0, 0, 0.525],
      "34": [0, 0.61111, 0, 0, 0.525],
      "35": [0, 0.61111, 0, 0, 0.525],
      "36": [0.08333, 0.69444, 0, 0, 0.525],
      "37": [0.08333, 0.69444, 0, 0, 0.525],
      "38": [0, 0.61111, 0, 0, 0.525],
      "39": [0, 0.61111, 0, 0, 0.525],
      "40": [0.08333, 0.69444, 0, 0, 0.525],
      "41": [0.08333, 0.69444, 0, 0, 0.525],
      "42": [0, 0.52083, 0, 0, 0.525],
      "43": [-0.08056, 0.53055, 0, 0, 0.525],
      "44": [0.13889, 0.125, 0, 0, 0.525],
      "45": [-0.08056, 0.53055, 0, 0, 0.525],
      "46": [0, 0.125, 0, 0, 0.525],
      "47": [0.08333, 0.69444, 0, 0, 0.525],
      "48": [0, 0.61111, 0, 0, 0.525],
      "49": [0, 0.61111, 0, 0, 0.525],
      "50": [0, 0.61111, 0, 0, 0.525],
      "51": [0, 0.61111, 0, 0, 0.525],
      "52": [0, 0.61111, 0, 0, 0.525],
      "53": [0, 0.61111, 0, 0, 0.525],
      "54": [0, 0.61111, 0, 0, 0.525],
      "55": [0, 0.61111, 0, 0, 0.525],
      "56": [0, 0.61111, 0, 0, 0.525],
      "57": [0, 0.61111, 0, 0, 0.525],
      "58": [0, 0.43056, 0, 0, 0.525],
      "59": [0.13889, 0.43056, 0, 0, 0.525],
      "60": [-0.05556, 0.55556, 0, 0, 0.525],
      "61": [-0.19549, 0.41562, 0, 0, 0.525],
      "62": [-0.05556, 0.55556, 0, 0, 0.525],
      "63": [0, 0.61111, 0, 0, 0.525],
      "64": [0, 0.61111, 0, 0, 0.525],
      "65": [0, 0.61111, 0, 0, 0.525],
      "66": [0, 0.61111, 0, 0, 0.525],
      "67": [0, 0.61111, 0, 0, 0.525],
      "68": [0, 0.61111, 0, 0, 0.525],
      "69": [0, 0.61111, 0, 0, 0.525],
      "70": [0, 0.61111, 0, 0, 0.525],
      "71": [0, 0.61111, 0, 0, 0.525],
      "72": [0, 0.61111, 0, 0, 0.525],
      "73": [0, 0.61111, 0, 0, 0.525],
      "74": [0, 0.61111, 0, 0, 0.525],
      "75": [0, 0.61111, 0, 0, 0.525],
      "76": [0, 0.61111, 0, 0, 0.525],
      "77": [0, 0.61111, 0, 0, 0.525],
      "78": [0, 0.61111, 0, 0, 0.525],
      "79": [0, 0.61111, 0, 0, 0.525],
      "80": [0, 0.61111, 0, 0, 0.525],
      "81": [0.13889, 0.61111, 0, 0, 0.525],
      "82": [0, 0.61111, 0, 0, 0.525],
      "83": [0, 0.61111, 0, 0, 0.525],
      "84": [0, 0.61111, 0, 0, 0.525],
      "85": [0, 0.61111, 0, 0, 0.525],
      "86": [0, 0.61111, 0, 0, 0.525],
      "87": [0, 0.61111, 0, 0, 0.525],
      "88": [0, 0.61111, 0, 0, 0.525],
      "89": [0, 0.61111, 0, 0, 0.525],
      "90": [0, 0.61111, 0, 0, 0.525],
      "91": [0.08333, 0.69444, 0, 0, 0.525],
      "92": [0.08333, 0.69444, 0, 0, 0.525],
      "93": [0.08333, 0.69444, 0, 0, 0.525],
      "94": [0, 0.61111, 0, 0, 0.525],
      "95": [0.09514, 0, 0, 0, 0.525],
      "96": [0, 0.61111, 0, 0, 0.525],
      "97": [0, 0.43056, 0, 0, 0.525],
      "98": [0, 0.61111, 0, 0, 0.525],
      "99": [0, 0.43056, 0, 0, 0.525],
      "100": [0, 0.61111, 0, 0, 0.525],
      "101": [0, 0.43056, 0, 0, 0.525],
      "102": [0, 0.61111, 0, 0, 0.525],
      "103": [0.22222, 0.43056, 0, 0, 0.525],
      "104": [0, 0.61111, 0, 0, 0.525],
      "105": [0, 0.61111, 0, 0, 0.525],
      "106": [0.22222, 0.61111, 0, 0, 0.525],
      "107": [0, 0.61111, 0, 0, 0.525],
      "108": [0, 0.61111, 0, 0, 0.525],
      "109": [0, 0.43056, 0, 0, 0.525],
      "110": [0, 0.43056, 0, 0, 0.525],
      "111": [0, 0.43056, 0, 0, 0.525],
      "112": [0.22222, 0.43056, 0, 0, 0.525],
      "113": [0.22222, 0.43056, 0, 0, 0.525],
      "114": [0, 0.43056, 0, 0, 0.525],
      "115": [0, 0.43056, 0, 0, 0.525],
      "116": [0, 0.55358, 0, 0, 0.525],
      "117": [0, 0.43056, 0, 0, 0.525],
      "118": [0, 0.43056, 0, 0, 0.525],
      "119": [0, 0.43056, 0, 0, 0.525],
      "120": [0, 0.43056, 0, 0, 0.525],
      "121": [0.22222, 0.43056, 0, 0, 0.525],
      "122": [0, 0.43056, 0, 0, 0.525],
      "123": [0.08333, 0.69444, 0, 0, 0.525],
      "124": [0.08333, 0.69444, 0, 0, 0.525],
      "125": [0.08333, 0.69444, 0, 0, 0.525],
      "126": [0, 0.61111, 0, 0, 0.525],
      "127": [0, 0.61111, 0, 0, 0.525],
      "160": [0, 0, 0, 0, 0.525],
      "176": [0, 0.61111, 0, 0, 0.525],
      "184": [0.19445, 0, 0, 0, 0.525],
      "305": [0, 0.43056, 0, 0, 0.525],
      "567": [0.22222, 0.43056, 0, 0, 0.525],
      "711": [0, 0.56597, 0, 0, 0.525],
      "713": [0, 0.56555, 0, 0, 0.525],
      "714": [0, 0.61111, 0, 0, 0.525],
      "715": [0, 0.61111, 0, 0, 0.525],
      "728": [0, 0.61111, 0, 0, 0.525],
      "730": [0, 0.61111, 0, 0, 0.525],
      "770": [0, 0.61111, 0, 0, 0.525],
      "771": [0, 0.61111, 0, 0, 0.525],
      "776": [0, 0.61111, 0, 0, 0.525],
      "915": [0, 0.61111, 0, 0, 0.525],
      "916": [0, 0.61111, 0, 0, 0.525],
      "920": [0, 0.61111, 0, 0, 0.525],
      "923": [0, 0.61111, 0, 0, 0.525],
      "926": [0, 0.61111, 0, 0, 0.525],
      "928": [0, 0.61111, 0, 0, 0.525],
      "931": [0, 0.61111, 0, 0, 0.525],
      "933": [0, 0.61111, 0, 0, 0.525],
      "934": [0, 0.61111, 0, 0, 0.525],
      "936": [0, 0.61111, 0, 0, 0.525],
      "937": [0, 0.61111, 0, 0, 0.525],
      "8216": [0, 0.61111, 0, 0, 0.525],
      "8217": [0, 0.61111, 0, 0, 0.525],
      "8242": [0, 0.61111, 0, 0, 0.525],
      "9251": [0.11111, 0.21944, 0, 0, 0.525]
    }
  };
  var sigmasAndXis = {
    slant: [0.25, 0.25, 0.25],
    // sigma1
    space: [0, 0, 0],
    // sigma2
    stretch: [0, 0, 0],
    // sigma3
    shrink: [0, 0, 0],
    // sigma4
    xHeight: [0.431, 0.431, 0.431],
    // sigma5
    quad: [1, 1.171, 1.472],
    // sigma6
    extraSpace: [0, 0, 0],
    // sigma7
    num1: [0.677, 0.732, 0.925],
    // sigma8
    num2: [0.394, 0.384, 0.387],
    // sigma9
    num3: [0.444, 0.471, 0.504],
    // sigma10
    denom1: [0.686, 0.752, 1.025],
    // sigma11
    denom2: [0.345, 0.344, 0.532],
    // sigma12
    sup1: [0.413, 0.503, 0.504],
    // sigma13
    sup2: [0.363, 0.431, 0.404],
    // sigma14
    sup3: [0.289, 0.286, 0.294],
    // sigma15
    sub1: [0.15, 0.143, 0.2],
    // sigma16
    sub2: [0.247, 0.286, 0.4],
    // sigma17
    supDrop: [0.386, 0.353, 0.494],
    // sigma18
    subDrop: [0.05, 0.071, 0.1],
    // sigma19
    delim1: [2.39, 1.7, 1.98],
    // sigma20
    delim2: [1.01, 1.157, 1.42],
    // sigma21
    axisHeight: [0.25, 0.25, 0.25],
    // sigma22
    // These font metrics are extracted from TeX by using tftopl on cmex10.tfm;
    // they correspond to the font parameters of the extension fonts (family 3).
    // See the TeXbook, page 441. In AMSTeX, the extension fonts scale; to
    // match cmex7, we'd use cmex7.tfm values for script and scriptscript
    // values.
    defaultRuleThickness: [0.04, 0.049, 0.049],
    // xi8; cmex7: 0.049
    bigOpSpacing1: [0.111, 0.111, 0.111],
    // xi9
    bigOpSpacing2: [0.166, 0.166, 0.166],
    // xi10
    bigOpSpacing3: [0.2, 0.2, 0.2],
    // xi11
    bigOpSpacing4: [0.6, 0.611, 0.611],
    // xi12; cmex7: 0.611
    bigOpSpacing5: [0.1, 0.143, 0.143],
    // xi13; cmex7: 0.143
    // The \sqrt rule width is taken from the height of the surd character.
    // Since we use the same font at all sizes, this thickness doesn't scale.
    sqrtRuleThickness: [0.04, 0.04, 0.04],
    // This value determines how large a pt is, for metrics which are defined
    // in terms of pts.
    // This value is also used in katex.scss; if you change it make sure the
    // values match.
    ptPerEm: [10, 10, 10],
    // The space between adjacent `|` columns in an array definition. From
    // `\showthe\doublerulesep` in LaTeX. Equals 2.0 / ptPerEm.
    doubleRuleSep: [0.2, 0.2, 0.2],
    // The width of separator lines in {array} environments. From
    // `\showthe\arrayrulewidth` in LaTeX. Equals 0.4 / ptPerEm.
    arrayRuleWidth: [0.04, 0.04, 0.04],
    // Two values from LaTeX source2e:
    fboxsep: [0.3, 0.3, 0.3],
    //        3 pt / ptPerEm
    fboxrule: [0.04, 0.04, 0.04]
    // 0.4 pt / ptPerEm
  };
  var extraCharacterMap = {
    // Latin-1
    "\xC5": "A",
    "\xD0": "D",
    "\xDE": "o",
    "\xE5": "a",
    "\xF0": "d",
    "\xFE": "o",
    // Cyrillic
    "\u0410": "A",
    "\u0411": "B",
    "\u0412": "B",
    "\u0413": "F",
    "\u0414": "A",
    "\u0415": "E",
    "\u0416": "K",
    "\u0417": "3",
    "\u0418": "N",
    "\u0419": "N",
    "\u041A": "K",
    "\u041B": "N",
    "\u041C": "M",
    "\u041D": "H",
    "\u041E": "O",
    "\u041F": "N",
    "\u0420": "P",
    "\u0421": "C",
    "\u0422": "T",
    "\u0423": "y",
    "\u0424": "O",
    "\u0425": "X",
    "\u0426": "U",
    "\u0427": "h",
    "\u0428": "W",
    "\u0429": "W",
    "\u042A": "B",
    "\u042B": "X",
    "\u042C": "B",
    "\u042D": "3",
    "\u042E": "X",
    "\u042F": "R",
    "\u0430": "a",
    "\u0431": "b",
    "\u0432": "a",
    "\u0433": "r",
    "\u0434": "y",
    "\u0435": "e",
    "\u0436": "m",
    "\u0437": "e",
    "\u0438": "n",
    "\u0439": "n",
    "\u043A": "n",
    "\u043B": "n",
    "\u043C": "m",
    "\u043D": "n",
    "\u043E": "o",
    "\u043F": "n",
    "\u0440": "p",
    "\u0441": "c",
    "\u0442": "o",
    "\u0443": "y",
    "\u0444": "b",
    "\u0445": "x",
    "\u0446": "n",
    "\u0447": "n",
    "\u0448": "w",
    "\u0449": "w",
    "\u044A": "a",
    "\u044B": "m",
    "\u044C": "a",
    "\u044D": "e",
    "\u044E": "m",
    "\u044F": "r"
  };
  function setFontMetrics(fontName, metrics) {
    fontMetricsData[fontName] = metrics;
  }
  function getCharacterMetrics(character, font, mode) {
    if (!fontMetricsData[font]) {
      throw new Error("Font metrics not found for font: " + font + ".");
    }
    var ch2 = character.charCodeAt(0);
    var metrics = fontMetricsData[font][ch2];
    if (!metrics && character[0] in extraCharacterMap) {
      ch2 = extraCharacterMap[character[0]].charCodeAt(0);
      metrics = fontMetricsData[font][ch2];
    }
    if (!metrics && mode === "text") {
      if (supportedCodepoint(ch2)) {
        metrics = fontMetricsData[font][77];
      }
    }
    if (metrics) {
      return {
        depth: metrics[0],
        height: metrics[1],
        italic: metrics[2],
        skew: metrics[3],
        width: metrics[4]
      };
    }
  }
  var fontMetricsBySizeIndex = {};
  function getGlobalMetrics(size) {
    var sizeIndex;
    if (size >= 5) {
      sizeIndex = 0;
    } else if (size >= 3) {
      sizeIndex = 1;
    } else {
      sizeIndex = 2;
    }
    if (!fontMetricsBySizeIndex[sizeIndex]) {
      var metrics = fontMetricsBySizeIndex[sizeIndex] = {
        cssEmPerMu: sigmasAndXis.quad[sizeIndex] / 18
      };
      for (var key in sigmasAndXis) {
        if (sigmasAndXis.hasOwnProperty(key)) {
          metrics[key] = sigmasAndXis[key][sizeIndex];
        }
      }
    }
    return fontMetricsBySizeIndex[sizeIndex];
  }
  var symbols = {
    "math": {},
    "text": {}
  };
  function defineSymbol(mode, font, group, replace, name, acceptUnicodeChar) {
    symbols[mode][name] = {
      font,
      group,
      replace
    };
    if (acceptUnicodeChar && replace) {
      symbols[mode][replace] = symbols[mode][name];
    }
  }
  var math = "math";
  var text = "text";
  var main = "main";
  var ams = "ams";
  var accent = "accent-token";
  var bin = "bin";
  var close = "close";
  var inner = "inner";
  var mathord = "mathord";
  var op = "op-token";
  var open = "open";
  var punct = "punct";
  var rel = "rel";
  var spacing = "spacing";
  var textord = "textord";
  defineSymbol(math, main, rel, "\u2261", "\\equiv", true);
  defineSymbol(math, main, rel, "\u227A", "\\prec", true);
  defineSymbol(math, main, rel, "\u227B", "\\succ", true);
  defineSymbol(math, main, rel, "\u223C", "\\sim", true);
  defineSymbol(math, main, rel, "\u22A5", "\\perp");
  defineSymbol(math, main, rel, "\u2AAF", "\\preceq", true);
  defineSymbol(math, main, rel, "\u2AB0", "\\succeq", true);
  defineSymbol(math, main, rel, "\u2243", "\\simeq", true);
  defineSymbol(math, main, rel, "\u2223", "\\mid", true);
  defineSymbol(math, main, rel, "\u226A", "\\ll", true);
  defineSymbol(math, main, rel, "\u226B", "\\gg", true);
  defineSymbol(math, main, rel, "\u224D", "\\asymp", true);
  defineSymbol(math, main, rel, "\u2225", "\\parallel");
  defineSymbol(math, main, rel, "\u22C8", "\\bowtie", true);
  defineSymbol(math, main, rel, "\u2323", "\\smile", true);
  defineSymbol(math, main, rel, "\u2291", "\\sqsubseteq", true);
  defineSymbol(math, main, rel, "\u2292", "\\sqsupseteq", true);
  defineSymbol(math, main, rel, "\u2250", "\\doteq", true);
  defineSymbol(math, main, rel, "\u2322", "\\frown", true);
  defineSymbol(math, main, rel, "\u220B", "\\ni", true);
  defineSymbol(math, main, rel, "\u221D", "\\propto", true);
  defineSymbol(math, main, rel, "\u22A2", "\\vdash", true);
  defineSymbol(math, main, rel, "\u22A3", "\\dashv", true);
  defineSymbol(math, main, rel, "\u220B", "\\owns");
  defineSymbol(math, main, punct, ".", "\\ldotp");
  defineSymbol(math, main, punct, "\u22C5", "\\cdotp");
  defineSymbol(math, main, punct, "\u22C5", "\xB7");
  defineSymbol(text, main, textord, "\u22C5", "\xB7");
  defineSymbol(math, main, textord, "#", "\\#");
  defineSymbol(text, main, textord, "#", "\\#");
  defineSymbol(math, main, textord, "&", "\\&");
  defineSymbol(text, main, textord, "&", "\\&");
  defineSymbol(math, main, textord, "\u2135", "\\aleph", true);
  defineSymbol(math, main, textord, "\u2200", "\\forall", true);
  defineSymbol(math, main, textord, "\u210F", "\\hbar", true);
  defineSymbol(math, main, textord, "\u2203", "\\exists", true);
  defineSymbol(math, main, textord, "\u2207", "\\nabla", true);
  defineSymbol(math, main, textord, "\u266D", "\\flat", true);
  defineSymbol(math, main, textord, "\u2113", "\\ell", true);
  defineSymbol(math, main, textord, "\u266E", "\\natural", true);
  defineSymbol(math, main, textord, "\u2663", "\\clubsuit", true);
  defineSymbol(math, main, textord, "\u2118", "\\wp", true);
  defineSymbol(math, main, textord, "\u266F", "\\sharp", true);
  defineSymbol(math, main, textord, "\u2662", "\\diamondsuit", true);
  defineSymbol(math, main, textord, "\u211C", "\\Re", true);
  defineSymbol(math, main, textord, "\u2661", "\\heartsuit", true);
  defineSymbol(math, main, textord, "\u2111", "\\Im", true);
  defineSymbol(math, main, textord, "\u2660", "\\spadesuit", true);
  defineSymbol(math, main, textord, "\xA7", "\\S", true);
  defineSymbol(text, main, textord, "\xA7", "\\S");
  defineSymbol(math, main, textord, "\xB6", "\\P", true);
  defineSymbol(text, main, textord, "\xB6", "\\P");
  defineSymbol(math, main, textord, "\u2020", "\\dag");
  defineSymbol(text, main, textord, "\u2020", "\\dag");
  defineSymbol(text, main, textord, "\u2020", "\\textdagger");
  defineSymbol(math, main, textord, "\u2021", "\\ddag");
  defineSymbol(text, main, textord, "\u2021", "\\ddag");
  defineSymbol(text, main, textord, "\u2021", "\\textdaggerdbl");
  defineSymbol(math, main, close, "\u23B1", "\\rmoustache", true);
  defineSymbol(math, main, open, "\u23B0", "\\lmoustache", true);
  defineSymbol(math, main, close, "\u27EF", "\\rgroup", true);
  defineSymbol(math, main, open, "\u27EE", "\\lgroup", true);
  defineSymbol(math, main, bin, "\u2213", "\\mp", true);
  defineSymbol(math, main, bin, "\u2296", "\\ominus", true);
  defineSymbol(math, main, bin, "\u228E", "\\uplus", true);
  defineSymbol(math, main, bin, "\u2293", "\\sqcap", true);
  defineSymbol(math, main, bin, "\u2217", "\\ast");
  defineSymbol(math, main, bin, "\u2294", "\\sqcup", true);
  defineSymbol(math, main, bin, "\u25EF", "\\bigcirc", true);
  defineSymbol(math, main, bin, "\u2219", "\\bullet", true);
  defineSymbol(math, main, bin, "\u2021", "\\ddagger");
  defineSymbol(math, main, bin, "\u2240", "\\wr", true);
  defineSymbol(math, main, bin, "\u2A3F", "\\amalg");
  defineSymbol(math, main, bin, "&", "\\And");
  defineSymbol(math, main, rel, "\u27F5", "\\longleftarrow", true);
  defineSymbol(math, main, rel, "\u21D0", "\\Leftarrow", true);
  defineSymbol(math, main, rel, "\u27F8", "\\Longleftarrow", true);
  defineSymbol(math, main, rel, "\u27F6", "\\longrightarrow", true);
  defineSymbol(math, main, rel, "\u21D2", "\\Rightarrow", true);
  defineSymbol(math, main, rel, "\u27F9", "\\Longrightarrow", true);
  defineSymbol(math, main, rel, "\u2194", "\\leftrightarrow", true);
  defineSymbol(math, main, rel, "\u27F7", "\\longleftrightarrow", true);
  defineSymbol(math, main, rel, "\u21D4", "\\Leftrightarrow", true);
  defineSymbol(math, main, rel, "\u27FA", "\\Longleftrightarrow", true);
  defineSymbol(math, main, rel, "\u21A6", "\\mapsto", true);
  defineSymbol(math, main, rel, "\u27FC", "\\longmapsto", true);
  defineSymbol(math, main, rel, "\u2197", "\\nearrow", true);
  defineSymbol(math, main, rel, "\u21A9", "\\hookleftarrow", true);
  defineSymbol(math, main, rel, "\u21AA", "\\hookrightarrow", true);
  defineSymbol(math, main, rel, "\u2198", "\\searrow", true);
  defineSymbol(math, main, rel, "\u21BC", "\\leftharpoonup", true);
  defineSymbol(math, main, rel, "\u21C0", "\\rightharpoonup", true);
  defineSymbol(math, main, rel, "\u2199", "\\swarrow", true);
  defineSymbol(math, main, rel, "\u21BD", "\\leftharpoondown", true);
  defineSymbol(math, main, rel, "\u21C1", "\\rightharpoondown", true);
  defineSymbol(math, main, rel, "\u2196", "\\nwarrow", true);
  defineSymbol(math, main, rel, "\u21CC", "\\rightleftharpoons", true);
  defineSymbol(math, ams, rel, "\u226E", "\\nless", true);
  defineSymbol(math, ams, rel, "\uE010", "\\@nleqslant");
  defineSymbol(math, ams, rel, "\uE011", "\\@nleqq");
  defineSymbol(math, ams, rel, "\u2A87", "\\lneq", true);
  defineSymbol(math, ams, rel, "\u2268", "\\lneqq", true);
  defineSymbol(math, ams, rel, "\uE00C", "\\@lvertneqq");
  defineSymbol(math, ams, rel, "\u22E6", "\\lnsim", true);
  defineSymbol(math, ams, rel, "\u2A89", "\\lnapprox", true);
  defineSymbol(math, ams, rel, "\u2280", "\\nprec", true);
  defineSymbol(math, ams, rel, "\u22E0", "\\npreceq", true);
  defineSymbol(math, ams, rel, "\u22E8", "\\precnsim", true);
  defineSymbol(math, ams, rel, "\u2AB9", "\\precnapprox", true);
  defineSymbol(math, ams, rel, "\u2241", "\\nsim", true);
  defineSymbol(math, ams, rel, "\uE006", "\\@nshortmid");
  defineSymbol(math, ams, rel, "\u2224", "\\nmid", true);
  defineSymbol(math, ams, rel, "\u22AC", "\\nvdash", true);
  defineSymbol(math, ams, rel, "\u22AD", "\\nvDash", true);
  defineSymbol(math, ams, rel, "\u22EA", "\\ntriangleleft");
  defineSymbol(math, ams, rel, "\u22EC", "\\ntrianglelefteq", true);
  defineSymbol(math, ams, rel, "\u228A", "\\subsetneq", true);
  defineSymbol(math, ams, rel, "\uE01A", "\\@varsubsetneq");
  defineSymbol(math, ams, rel, "\u2ACB", "\\subsetneqq", true);
  defineSymbol(math, ams, rel, "\uE017", "\\@varsubsetneqq");
  defineSymbol(math, ams, rel, "\u226F", "\\ngtr", true);
  defineSymbol(math, ams, rel, "\uE00F", "\\@ngeqslant");
  defineSymbol(math, ams, rel, "\uE00E", "\\@ngeqq");
  defineSymbol(math, ams, rel, "\u2A88", "\\gneq", true);
  defineSymbol(math, ams, rel, "\u2269", "\\gneqq", true);
  defineSymbol(math, ams, rel, "\uE00D", "\\@gvertneqq");
  defineSymbol(math, ams, rel, "\u22E7", "\\gnsim", true);
  defineSymbol(math, ams, rel, "\u2A8A", "\\gnapprox", true);
  defineSymbol(math, ams, rel, "\u2281", "\\nsucc", true);
  defineSymbol(math, ams, rel, "\u22E1", "\\nsucceq", true);
  defineSymbol(math, ams, rel, "\u22E9", "\\succnsim", true);
  defineSymbol(math, ams, rel, "\u2ABA", "\\succnapprox", true);
  defineSymbol(math, ams, rel, "\u2246", "\\ncong", true);
  defineSymbol(math, ams, rel, "\uE007", "\\@nshortparallel");
  defineSymbol(math, ams, rel, "\u2226", "\\nparallel", true);
  defineSymbol(math, ams, rel, "\u22AF", "\\nVDash", true);
  defineSymbol(math, ams, rel, "\u22EB", "\\ntriangleright");
  defineSymbol(math, ams, rel, "\u22ED", "\\ntrianglerighteq", true);
  defineSymbol(math, ams, rel, "\uE018", "\\@nsupseteqq");
  defineSymbol(math, ams, rel, "\u228B", "\\supsetneq", true);
  defineSymbol(math, ams, rel, "\uE01B", "\\@varsupsetneq");
  defineSymbol(math, ams, rel, "\u2ACC", "\\supsetneqq", true);
  defineSymbol(math, ams, rel, "\uE019", "\\@varsupsetneqq");
  defineSymbol(math, ams, rel, "\u22AE", "\\nVdash", true);
  defineSymbol(math, ams, rel, "\u2AB5", "\\precneqq", true);
  defineSymbol(math, ams, rel, "\u2AB6", "\\succneqq", true);
  defineSymbol(math, ams, rel, "\uE016", "\\@nsubseteqq");
  defineSymbol(math, ams, bin, "\u22B4", "\\unlhd");
  defineSymbol(math, ams, bin, "\u22B5", "\\unrhd");
  defineSymbol(math, ams, rel, "\u219A", "\\nleftarrow", true);
  defineSymbol(math, ams, rel, "\u219B", "\\nrightarrow", true);
  defineSymbol(math, ams, rel, "\u21CD", "\\nLeftarrow", true);
  defineSymbol(math, ams, rel, "\u21CF", "\\nRightarrow", true);
  defineSymbol(math, ams, rel, "\u21AE", "\\nleftrightarrow", true);
  defineSymbol(math, ams, rel, "\u21CE", "\\nLeftrightarrow", true);
  defineSymbol(math, ams, rel, "\u25B3", "\\vartriangle");
  defineSymbol(math, ams, textord, "\u210F", "\\hslash");
  defineSymbol(math, ams, textord, "\u25BD", "\\triangledown");
  defineSymbol(math, ams, textord, "\u25CA", "\\lozenge");
  defineSymbol(math, ams, textord, "\u24C8", "\\circledS");
  defineSymbol(math, ams, textord, "\xAE", "\\circledR");
  defineSymbol(text, ams, textord, "\xAE", "\\circledR");
  defineSymbol(math, ams, textord, "\u2221", "\\measuredangle", true);
  defineSymbol(math, ams, textord, "\u2204", "\\nexists");
  defineSymbol(math, ams, textord, "\u2127", "\\mho");
  defineSymbol(math, ams, textord, "\u2132", "\\Finv", true);
  defineSymbol(math, ams, textord, "\u2141", "\\Game", true);
  defineSymbol(math, ams, textord, "\u2035", "\\backprime");
  defineSymbol(math, ams, textord, "\u25B2", "\\blacktriangle");
  defineSymbol(math, ams, textord, "\u25BC", "\\blacktriangledown");
  defineSymbol(math, ams, textord, "\u25A0", "\\blacksquare");
  defineSymbol(math, ams, textord, "\u29EB", "\\blacklozenge");
  defineSymbol(math, ams, textord, "\u2605", "\\bigstar");
  defineSymbol(math, ams, textord, "\u2222", "\\sphericalangle", true);
  defineSymbol(math, ams, textord, "\u2201", "\\complement", true);
  defineSymbol(math, ams, textord, "\xF0", "\\eth", true);
  defineSymbol(text, main, textord, "\xF0", "\xF0");
  defineSymbol(math, ams, textord, "\u2571", "\\diagup");
  defineSymbol(math, ams, textord, "\u2572", "\\diagdown");
  defineSymbol(math, ams, textord, "\u25A1", "\\square");
  defineSymbol(math, ams, textord, "\u25A1", "\\Box");
  defineSymbol(math, ams, textord, "\u25CA", "\\Diamond");
  defineSymbol(math, ams, textord, "\xA5", "\\yen", true);
  defineSymbol(text, ams, textord, "\xA5", "\\yen", true);
  defineSymbol(math, ams, textord, "\u2713", "\\checkmark", true);
  defineSymbol(text, ams, textord, "\u2713", "\\checkmark");
  defineSymbol(math, ams, textord, "\u2136", "\\beth", true);
  defineSymbol(math, ams, textord, "\u2138", "\\daleth", true);
  defineSymbol(math, ams, textord, "\u2137", "\\gimel", true);
  defineSymbol(math, ams, textord, "\u03DD", "\\digamma", true);
  defineSymbol(math, ams, textord, "\u03F0", "\\varkappa");
  defineSymbol(math, ams, open, "\u250C", "\\@ulcorner", true);
  defineSymbol(math, ams, close, "\u2510", "\\@urcorner", true);
  defineSymbol(math, ams, open, "\u2514", "\\@llcorner", true);
  defineSymbol(math, ams, close, "\u2518", "\\@lrcorner", true);
  defineSymbol(math, ams, rel, "\u2266", "\\leqq", true);
  defineSymbol(math, ams, rel, "\u2A7D", "\\leqslant", true);
  defineSymbol(math, ams, rel, "\u2A95", "\\eqslantless", true);
  defineSymbol(math, ams, rel, "\u2272", "\\lesssim", true);
  defineSymbol(math, ams, rel, "\u2A85", "\\lessapprox", true);
  defineSymbol(math, ams, rel, "\u224A", "\\approxeq", true);
  defineSymbol(math, ams, bin, "\u22D6", "\\lessdot");
  defineSymbol(math, ams, rel, "\u22D8", "\\lll", true);
  defineSymbol(math, ams, rel, "\u2276", "\\lessgtr", true);
  defineSymbol(math, ams, rel, "\u22DA", "\\lesseqgtr", true);
  defineSymbol(math, ams, rel, "\u2A8B", "\\lesseqqgtr", true);
  defineSymbol(math, ams, rel, "\u2251", "\\doteqdot");
  defineSymbol(math, ams, rel, "\u2253", "\\risingdotseq", true);
  defineSymbol(math, ams, rel, "\u2252", "\\fallingdotseq", true);
  defineSymbol(math, ams, rel, "\u223D", "\\backsim", true);
  defineSymbol(math, ams, rel, "\u22CD", "\\backsimeq", true);
  defineSymbol(math, ams, rel, "\u2AC5", "\\subseteqq", true);
  defineSymbol(math, ams, rel, "\u22D0", "\\Subset", true);
  defineSymbol(math, ams, rel, "\u228F", "\\sqsubset", true);
  defineSymbol(math, ams, rel, "\u227C", "\\preccurlyeq", true);
  defineSymbol(math, ams, rel, "\u22DE", "\\curlyeqprec", true);
  defineSymbol(math, ams, rel, "\u227E", "\\precsim", true);
  defineSymbol(math, ams, rel, "\u2AB7", "\\precapprox", true);
  defineSymbol(math, ams, rel, "\u22B2", "\\vartriangleleft");
  defineSymbol(math, ams, rel, "\u22B4", "\\trianglelefteq");
  defineSymbol(math, ams, rel, "\u22A8", "\\vDash", true);
  defineSymbol(math, ams, rel, "\u22AA", "\\Vvdash", true);
  defineSymbol(math, ams, rel, "\u2323", "\\smallsmile");
  defineSymbol(math, ams, rel, "\u2322", "\\smallfrown");
  defineSymbol(math, ams, rel, "\u224F", "\\bumpeq", true);
  defineSymbol(math, ams, rel, "\u224E", "\\Bumpeq", true);
  defineSymbol(math, ams, rel, "\u2267", "\\geqq", true);
  defineSymbol(math, ams, rel, "\u2A7E", "\\geqslant", true);
  defineSymbol(math, ams, rel, "\u2A96", "\\eqslantgtr", true);
  defineSymbol(math, ams, rel, "\u2273", "\\gtrsim", true);
  defineSymbol(math, ams, rel, "\u2A86", "\\gtrapprox", true);
  defineSymbol(math, ams, bin, "\u22D7", "\\gtrdot");
  defineSymbol(math, ams, rel, "\u22D9", "\\ggg", true);
  defineSymbol(math, ams, rel, "\u2277", "\\gtrless", true);
  defineSymbol(math, ams, rel, "\u22DB", "\\gtreqless", true);
  defineSymbol(math, ams, rel, "\u2A8C", "\\gtreqqless", true);
  defineSymbol(math, ams, rel, "\u2256", "\\eqcirc", true);
  defineSymbol(math, ams, rel, "\u2257", "\\circeq", true);
  defineSymbol(math, ams, rel, "\u225C", "\\triangleq", true);
  defineSymbol(math, ams, rel, "\u223C", "\\thicksim");
  defineSymbol(math, ams, rel, "\u2248", "\\thickapprox");
  defineSymbol(math, ams, rel, "\u2AC6", "\\supseteqq", true);
  defineSymbol(math, ams, rel, "\u22D1", "\\Supset", true);
  defineSymbol(math, ams, rel, "\u2290", "\\sqsupset", true);
  defineSymbol(math, ams, rel, "\u227D", "\\succcurlyeq", true);
  defineSymbol(math, ams, rel, "\u22DF", "\\curlyeqsucc", true);
  defineSymbol(math, ams, rel, "\u227F", "\\succsim", true);
  defineSymbol(math, ams, rel, "\u2AB8", "\\succapprox", true);
  defineSymbol(math, ams, rel, "\u22B3", "\\vartriangleright");
  defineSymbol(math, ams, rel, "\u22B5", "\\trianglerighteq");
  defineSymbol(math, ams, rel, "\u22A9", "\\Vdash", true);
  defineSymbol(math, ams, rel, "\u2223", "\\shortmid");
  defineSymbol(math, ams, rel, "\u2225", "\\shortparallel");
  defineSymbol(math, ams, rel, "\u226C", "\\between", true);
  defineSymbol(math, ams, rel, "\u22D4", "\\pitchfork", true);
  defineSymbol(math, ams, rel, "\u221D", "\\varpropto");
  defineSymbol(math, ams, rel, "\u25C0", "\\blacktriangleleft");
  defineSymbol(math, ams, rel, "\u2234", "\\therefore", true);
  defineSymbol(math, ams, rel, "\u220D", "\\backepsilon");
  defineSymbol(math, ams, rel, "\u25B6", "\\blacktriangleright");
  defineSymbol(math, ams, rel, "\u2235", "\\because", true);
  defineSymbol(math, ams, rel, "\u22D8", "\\llless");
  defineSymbol(math, ams, rel, "\u22D9", "\\gggtr");
  defineSymbol(math, ams, bin, "\u22B2", "\\lhd");
  defineSymbol(math, ams, bin, "\u22B3", "\\rhd");
  defineSymbol(math, ams, rel, "\u2242", "\\eqsim", true);
  defineSymbol(math, main, rel, "\u22C8", "\\Join");
  defineSymbol(math, ams, rel, "\u2251", "\\Doteq", true);
  defineSymbol(math, ams, bin, "\u2214", "\\dotplus", true);
  defineSymbol(math, ams, bin, "\u2216", "\\smallsetminus");
  defineSymbol(math, ams, bin, "\u22D2", "\\Cap", true);
  defineSymbol(math, ams, bin, "\u22D3", "\\Cup", true);
  defineSymbol(math, ams, bin, "\u2A5E", "\\doublebarwedge", true);
  defineSymbol(math, ams, bin, "\u229F", "\\boxminus", true);
  defineSymbol(math, ams, bin, "\u229E", "\\boxplus", true);
  defineSymbol(math, ams, bin, "\u22C7", "\\divideontimes", true);
  defineSymbol(math, ams, bin, "\u22C9", "\\ltimes", true);
  defineSymbol(math, ams, bin, "\u22CA", "\\rtimes", true);
  defineSymbol(math, ams, bin, "\u22CB", "\\leftthreetimes", true);
  defineSymbol(math, ams, bin, "\u22CC", "\\rightthreetimes", true);
  defineSymbol(math, ams, bin, "\u22CF", "\\curlywedge", true);
  defineSymbol(math, ams, bin, "\u22CE", "\\curlyvee", true);
  defineSymbol(math, ams, bin, "\u229D", "\\circleddash", true);
  defineSymbol(math, ams, bin, "\u229B", "\\circledast", true);
  defineSymbol(math, ams, bin, "\u22C5", "\\centerdot");
  defineSymbol(math, ams, bin, "\u22BA", "\\intercal", true);
  defineSymbol(math, ams, bin, "\u22D2", "\\doublecap");
  defineSymbol(math, ams, bin, "\u22D3", "\\doublecup");
  defineSymbol(math, ams, bin, "\u22A0", "\\boxtimes", true);
  defineSymbol(math, ams, rel, "\u21E2", "\\dashrightarrow", true);
  defineSymbol(math, ams, rel, "\u21E0", "\\dashleftarrow", true);
  defineSymbol(math, ams, rel, "\u21C7", "\\leftleftarrows", true);
  defineSymbol(math, ams, rel, "\u21C6", "\\leftrightarrows", true);
  defineSymbol(math, ams, rel, "\u21DA", "\\Lleftarrow", true);
  defineSymbol(math, ams, rel, "\u219E", "\\twoheadleftarrow", true);
  defineSymbol(math, ams, rel, "\u21A2", "\\leftarrowtail", true);
  defineSymbol(math, ams, rel, "\u21AB", "\\looparrowleft", true);
  defineSymbol(math, ams, rel, "\u21CB", "\\leftrightharpoons", true);
  defineSymbol(math, ams, rel, "\u21B6", "\\curvearrowleft", true);
  defineSymbol(math, ams, rel, "\u21BA", "\\circlearrowleft", true);
  defineSymbol(math, ams, rel, "\u21B0", "\\Lsh", true);
  defineSymbol(math, ams, rel, "\u21C8", "\\upuparrows", true);
  defineSymbol(math, ams, rel, "\u21BF", "\\upharpoonleft", true);
  defineSymbol(math, ams, rel, "\u21C3", "\\downharpoonleft", true);
  defineSymbol(math, main, rel, "\u22B6", "\\origof", true);
  defineSymbol(math, main, rel, "\u22B7", "\\imageof", true);
  defineSymbol(math, ams, rel, "\u22B8", "\\multimap", true);
  defineSymbol(math, ams, rel, "\u21AD", "\\leftrightsquigarrow", true);
  defineSymbol(math, ams, rel, "\u21C9", "\\rightrightarrows", true);
  defineSymbol(math, ams, rel, "\u21C4", "\\rightleftarrows", true);
  defineSymbol(math, ams, rel, "\u21A0", "\\twoheadrightarrow", true);
  defineSymbol(math, ams, rel, "\u21A3", "\\rightarrowtail", true);
  defineSymbol(math, ams, rel, "\u21AC", "\\looparrowright", true);
  defineSymbol(math, ams, rel, "\u21B7", "\\curvearrowright", true);
  defineSymbol(math, ams, rel, "\u21BB", "\\circlearrowright", true);
  defineSymbol(math, ams, rel, "\u21B1", "\\Rsh", true);
  defineSymbol(math, ams, rel, "\u21CA", "\\downdownarrows", true);
  defineSymbol(math, ams, rel, "\u21BE", "\\upharpoonright", true);
  defineSymbol(math, ams, rel, "\u21C2", "\\downharpoonright", true);
  defineSymbol(math, ams, rel, "\u21DD", "\\rightsquigarrow", true);
  defineSymbol(math, ams, rel, "\u21DD", "\\leadsto");
  defineSymbol(math, ams, rel, "\u21DB", "\\Rrightarrow", true);
  defineSymbol(math, ams, rel, "\u21BE", "\\restriction");
  defineSymbol(math, main, textord, "\u2018", "`");
  defineSymbol(math, main, textord, "$", "\\$");
  defineSymbol(text, main, textord, "$", "\\$");
  defineSymbol(text, main, textord, "$", "\\textdollar");
  defineSymbol(math, main, textord, "%", "\\%");
  defineSymbol(text, main, textord, "%", "\\%");
  defineSymbol(math, main, textord, "_", "\\_");
  defineSymbol(text, main, textord, "_", "\\_");
  defineSymbol(text, main, textord, "_", "\\textunderscore");
  defineSymbol(math, main, textord, "\u2220", "\\angle", true);
  defineSymbol(math, main, textord, "\u221E", "\\infty", true);
  defineSymbol(math, main, textord, "\u2032", "\\prime");
  defineSymbol(math, main, textord, "\u25B3", "\\triangle");
  defineSymbol(math, main, textord, "\u0393", "\\Gamma", true);
  defineSymbol(math, main, textord, "\u0394", "\\Delta", true);
  defineSymbol(math, main, textord, "\u0398", "\\Theta", true);
  defineSymbol(math, main, textord, "\u039B", "\\Lambda", true);
  defineSymbol(math, main, textord, "\u039E", "\\Xi", true);
  defineSymbol(math, main, textord, "\u03A0", "\\Pi", true);
  defineSymbol(math, main, textord, "\u03A3", "\\Sigma", true);
  defineSymbol(math, main, textord, "\u03A5", "\\Upsilon", true);
  defineSymbol(math, main, textord, "\u03A6", "\\Phi", true);
  defineSymbol(math, main, textord, "\u03A8", "\\Psi", true);
  defineSymbol(math, main, textord, "\u03A9", "\\Omega", true);
  defineSymbol(math, main, textord, "A", "\u0391");
  defineSymbol(math, main, textord, "B", "\u0392");
  defineSymbol(math, main, textord, "E", "\u0395");
  defineSymbol(math, main, textord, "Z", "\u0396");
  defineSymbol(math, main, textord, "H", "\u0397");
  defineSymbol(math, main, textord, "I", "\u0399");
  defineSymbol(math, main, textord, "K", "\u039A");
  defineSymbol(math, main, textord, "M", "\u039C");
  defineSymbol(math, main, textord, "N", "\u039D");
  defineSymbol(math, main, textord, "O", "\u039F");
  defineSymbol(math, main, textord, "P", "\u03A1");
  defineSymbol(math, main, textord, "T", "\u03A4");
  defineSymbol(math, main, textord, "X", "\u03A7");
  defineSymbol(math, main, textord, "\xAC", "\\neg", true);
  defineSymbol(math, main, textord, "\xAC", "\\lnot");
  defineSymbol(math, main, textord, "\u22A4", "\\top");
  defineSymbol(math, main, textord, "\u22A5", "\\bot");
  defineSymbol(math, main, textord, "\u2205", "\\emptyset");
  defineSymbol(math, ams, textord, "\u2205", "\\varnothing");
  defineSymbol(math, main, mathord, "\u03B1", "\\alpha", true);
  defineSymbol(math, main, mathord, "\u03B2", "\\beta", true);
  defineSymbol(math, main, mathord, "\u03B3", "\\gamma", true);
  defineSymbol(math, main, mathord, "\u03B4", "\\delta", true);
  defineSymbol(math, main, mathord, "\u03F5", "\\epsilon", true);
  defineSymbol(math, main, mathord, "\u03B6", "\\zeta", true);
  defineSymbol(math, main, mathord, "\u03B7", "\\eta", true);
  defineSymbol(math, main, mathord, "\u03B8", "\\theta", true);
  defineSymbol(math, main, mathord, "\u03B9", "\\iota", true);
  defineSymbol(math, main, mathord, "\u03BA", "\\kappa", true);
  defineSymbol(math, main, mathord, "\u03BB", "\\lambda", true);
  defineSymbol(math, main, mathord, "\u03BC", "\\mu", true);
  defineSymbol(math, main, mathord, "\u03BD", "\\nu", true);
  defineSymbol(math, main, mathord, "\u03BE", "\\xi", true);
  defineSymbol(math, main, mathord, "\u03BF", "\\omicron", true);
  defineSymbol(math, main, mathord, "\u03C0", "\\pi", true);
  defineSymbol(math, main, mathord, "\u03C1", "\\rho", true);
  defineSymbol(math, main, mathord, "\u03C3", "\\sigma", true);
  defineSymbol(math, main, mathord, "\u03C4", "\\tau", true);
  defineSymbol(math, main, mathord, "\u03C5", "\\upsilon", true);
  defineSymbol(math, main, mathord, "\u03D5", "\\phi", true);
  defineSymbol(math, main, mathord, "\u03C7", "\\chi", true);
  defineSymbol(math, main, mathord, "\u03C8", "\\psi", true);
  defineSymbol(math, main, mathord, "\u03C9", "\\omega", true);
  defineSymbol(math, main, mathord, "\u03B5", "\\varepsilon", true);
  defineSymbol(math, main, mathord, "\u03D1", "\\vartheta", true);
  defineSymbol(math, main, mathord, "\u03D6", "\\varpi", true);
  defineSymbol(math, main, mathord, "\u03F1", "\\varrho", true);
  defineSymbol(math, main, mathord, "\u03C2", "\\varsigma", true);
  defineSymbol(math, main, mathord, "\u03C6", "\\varphi", true);
  defineSymbol(math, main, bin, "\u2217", "*", true);
  defineSymbol(math, main, bin, "+", "+");
  defineSymbol(math, main, bin, "\u2212", "-", true);
  defineSymbol(math, main, bin, "\u22C5", "\\cdot", true);
  defineSymbol(math, main, bin, "\u2218", "\\circ", true);
  defineSymbol(math, main, bin, "\xF7", "\\div", true);
  defineSymbol(math, main, bin, "\xB1", "\\pm", true);
  defineSymbol(math, main, bin, "\xD7", "\\times", true);
  defineSymbol(math, main, bin, "\u2229", "\\cap", true);
  defineSymbol(math, main, bin, "\u222A", "\\cup", true);
  defineSymbol(math, main, bin, "\u2216", "\\setminus", true);
  defineSymbol(math, main, bin, "\u2227", "\\land");
  defineSymbol(math, main, bin, "\u2228", "\\lor");
  defineSymbol(math, main, bin, "\u2227", "\\wedge", true);
  defineSymbol(math, main, bin, "\u2228", "\\vee", true);
  defineSymbol(math, main, textord, "\u221A", "\\surd");
  defineSymbol(math, main, open, "\u27E8", "\\langle", true);
  defineSymbol(math, main, open, "\u2223", "\\lvert");
  defineSymbol(math, main, open, "\u2225", "\\lVert");
  defineSymbol(math, main, close, "?", "?");
  defineSymbol(math, main, close, "!", "!");
  defineSymbol(math, main, close, "\u27E9", "\\rangle", true);
  defineSymbol(math, main, close, "\u2223", "\\rvert");
  defineSymbol(math, main, close, "\u2225", "\\rVert");
  defineSymbol(math, main, rel, "=", "=");
  defineSymbol(math, main, rel, ":", ":");
  defineSymbol(math, main, rel, "\u2248", "\\approx", true);
  defineSymbol(math, main, rel, "\u2245", "\\cong", true);
  defineSymbol(math, main, rel, "\u2265", "\\ge");
  defineSymbol(math, main, rel, "\u2265", "\\geq", true);
  defineSymbol(math, main, rel, "\u2190", "\\gets");
  defineSymbol(math, main, rel, ">", "\\gt", true);
  defineSymbol(math, main, rel, "\u2208", "\\in", true);
  defineSymbol(math, main, rel, "\uE020", "\\@not");
  defineSymbol(math, main, rel, "\u2282", "\\subset", true);
  defineSymbol(math, main, rel, "\u2283", "\\supset", true);
  defineSymbol(math, main, rel, "\u2286", "\\subseteq", true);
  defineSymbol(math, main, rel, "\u2287", "\\supseteq", true);
  defineSymbol(math, ams, rel, "\u2288", "\\nsubseteq", true);
  defineSymbol(math, ams, rel, "\u2289", "\\nsupseteq", true);
  defineSymbol(math, main, rel, "\u22A8", "\\models");
  defineSymbol(math, main, rel, "\u2190", "\\leftarrow", true);
  defineSymbol(math, main, rel, "\u2264", "\\le");
  defineSymbol(math, main, rel, "\u2264", "\\leq", true);
  defineSymbol(math, main, rel, "<", "\\lt", true);
  defineSymbol(math, main, rel, "\u2192", "\\rightarrow", true);
  defineSymbol(math, main, rel, "\u2192", "\\to");
  defineSymbol(math, ams, rel, "\u2271", "\\ngeq", true);
  defineSymbol(math, ams, rel, "\u2270", "\\nleq", true);
  defineSymbol(math, main, spacing, "\xA0", "\\ ");
  defineSymbol(math, main, spacing, "\xA0", "\\space");
  defineSymbol(math, main, spacing, "\xA0", "\\nobreakspace");
  defineSymbol(text, main, spacing, "\xA0", "\\ ");
  defineSymbol(text, main, spacing, "\xA0", " ");
  defineSymbol(text, main, spacing, "\xA0", "\\space");
  defineSymbol(text, main, spacing, "\xA0", "\\nobreakspace");
  defineSymbol(math, main, spacing, "", "\\nobreak");
  defineSymbol(math, main, spacing, "", "\\allowbreak");
  defineSymbol(math, main, punct, ",", ",");
  defineSymbol(math, main, punct, ";", ";");
  defineSymbol(math, ams, bin, "\u22BC", "\\barwedge", true);
  defineSymbol(math, ams, bin, "\u22BB", "\\veebar", true);
  defineSymbol(math, main, bin, "\u2299", "\\odot", true);
  defineSymbol(math, main, bin, "\u2295", "\\oplus", true);
  defineSymbol(math, main, bin, "\u2297", "\\otimes", true);
  defineSymbol(math, main, textord, "\u2202", "\\partial", true);
  defineSymbol(math, main, bin, "\u2298", "\\oslash", true);
  defineSymbol(math, ams, bin, "\u229A", "\\circledcirc", true);
  defineSymbol(math, ams, bin, "\u22A1", "\\boxdot", true);
  defineSymbol(math, main, bin, "\u25B3", "\\bigtriangleup");
  defineSymbol(math, main, bin, "\u25BD", "\\bigtriangledown");
  defineSymbol(math, main, bin, "\u2020", "\\dagger");
  defineSymbol(math, main, bin, "\u22C4", "\\diamond");
  defineSymbol(math, main, bin, "\u22C6", "\\star");
  defineSymbol(math, main, bin, "\u25C3", "\\triangleleft");
  defineSymbol(math, main, bin, "\u25B9", "\\triangleright");
  defineSymbol(math, main, open, "{", "\\{");
  defineSymbol(text, main, textord, "{", "\\{");
  defineSymbol(text, main, textord, "{", "\\textbraceleft");
  defineSymbol(math, main, close, "}", "\\}");
  defineSymbol(text, main, textord, "}", "\\}");
  defineSymbol(text, main, textord, "}", "\\textbraceright");
  defineSymbol(math, main, open, "{", "\\lbrace");
  defineSymbol(math, main, close, "}", "\\rbrace");
  defineSymbol(math, main, open, "[", "\\lbrack", true);
  defineSymbol(text, main, textord, "[", "\\lbrack", true);
  defineSymbol(math, main, close, "]", "\\rbrack", true);
  defineSymbol(text, main, textord, "]", "\\rbrack", true);
  defineSymbol(math, main, open, "(", "\\lparen", true);
  defineSymbol(math, main, close, ")", "\\rparen", true);
  defineSymbol(text, main, textord, "<", "\\textless", true);
  defineSymbol(text, main, textord, ">", "\\textgreater", true);
  defineSymbol(math, main, open, "\u230A", "\\lfloor", true);
  defineSymbol(math, main, close, "\u230B", "\\rfloor", true);
  defineSymbol(math, main, open, "\u2308", "\\lceil", true);
  defineSymbol(math, main, close, "\u2309", "\\rceil", true);
  defineSymbol(math, main, textord, "\\", "\\backslash");
  defineSymbol(math, main, textord, "\u2223", "|");
  defineSymbol(math, main, textord, "\u2223", "\\vert");
  defineSymbol(text, main, textord, "|", "\\textbar", true);
  defineSymbol(math, main, textord, "\u2225", "\\|");
  defineSymbol(math, main, textord, "\u2225", "\\Vert");
  defineSymbol(text, main, textord, "\u2225", "\\textbardbl");
  defineSymbol(text, main, textord, "~", "\\textasciitilde");
  defineSymbol(text, main, textord, "\\", "\\textbackslash");
  defineSymbol(text, main, textord, "^", "\\textasciicircum");
  defineSymbol(math, main, rel, "\u2191", "\\uparrow", true);
  defineSymbol(math, main, rel, "\u21D1", "\\Uparrow", true);
  defineSymbol(math, main, rel, "\u2193", "\\downarrow", true);
  defineSymbol(math, main, rel, "\u21D3", "\\Downarrow", true);
  defineSymbol(math, main, rel, "\u2195", "\\updownarrow", true);
  defineSymbol(math, main, rel, "\u21D5", "\\Updownarrow", true);
  defineSymbol(math, main, op, "\u2210", "\\coprod");
  defineSymbol(math, main, op, "\u22C1", "\\bigvee");
  defineSymbol(math, main, op, "\u22C0", "\\bigwedge");
  defineSymbol(math, main, op, "\u2A04", "\\biguplus");
  defineSymbol(math, main, op, "\u22C2", "\\bigcap");
  defineSymbol(math, main, op, "\u22C3", "\\bigcup");
  defineSymbol(math, main, op, "\u222B", "\\int");
  defineSymbol(math, main, op, "\u222B", "\\intop");
  defineSymbol(math, main, op, "\u222C", "\\iint");
  defineSymbol(math, main, op, "\u222D", "\\iiint");
  defineSymbol(math, main, op, "\u220F", "\\prod");
  defineSymbol(math, main, op, "\u2211", "\\sum");
  defineSymbol(math, main, op, "\u2A02", "\\bigotimes");
  defineSymbol(math, main, op, "\u2A01", "\\bigoplus");
  defineSymbol(math, main, op, "\u2A00", "\\bigodot");
  defineSymbol(math, main, op, "\u222E", "\\oint");
  defineSymbol(math, main, op, "\u222F", "\\oiint");
  defineSymbol(math, main, op, "\u2230", "\\oiiint");
  defineSymbol(math, main, op, "\u2A06", "\\bigsqcup");
  defineSymbol(math, main, op, "\u222B", "\\smallint");
  defineSymbol(text, main, inner, "\u2026", "\\textellipsis");
  defineSymbol(math, main, inner, "\u2026", "\\mathellipsis");
  defineSymbol(text, main, inner, "\u2026", "\\ldots", true);
  defineSymbol(math, main, inner, "\u2026", "\\ldots", true);
  defineSymbol(math, main, inner, "\u22EF", "\\@cdots", true);
  defineSymbol(math, main, inner, "\u22F1", "\\ddots", true);
  defineSymbol(math, main, textord, "\u22EE", "\\varvdots");
  defineSymbol(text, main, textord, "\u22EE", "\\varvdots");
  defineSymbol(math, main, accent, "\u02CA", "\\acute");
  defineSymbol(math, main, accent, "\u02CB", "\\grave");
  defineSymbol(math, main, accent, "\xA8", "\\ddot");
  defineSymbol(math, main, accent, "~", "\\tilde");
  defineSymbol(math, main, accent, "\u02C9", "\\bar");
  defineSymbol(math, main, accent, "\u02D8", "\\breve");
  defineSymbol(math, main, accent, "\u02C7", "\\check");
  defineSymbol(math, main, accent, "^", "\\hat");
  defineSymbol(math, main, accent, "\u20D7", "\\vec");
  defineSymbol(math, main, accent, "\u02D9", "\\dot");
  defineSymbol(math, main, accent, "\u02DA", "\\mathring");
  defineSymbol(math, main, mathord, "\uE131", "\\@imath");
  defineSymbol(math, main, mathord, "\uE237", "\\@jmath");
  defineSymbol(math, main, textord, "\u0131", "\u0131");
  defineSymbol(math, main, textord, "\u0237", "\u0237");
  defineSymbol(text, main, textord, "\u0131", "\\i", true);
  defineSymbol(text, main, textord, "\u0237", "\\j", true);
  defineSymbol(text, main, textord, "\xDF", "\\ss", true);
  defineSymbol(text, main, textord, "\xE6", "\\ae", true);
  defineSymbol(text, main, textord, "\u0153", "\\oe", true);
  defineSymbol(text, main, textord, "\xF8", "\\o", true);
  defineSymbol(text, main, textord, "\xC6", "\\AE", true);
  defineSymbol(text, main, textord, "\u0152", "\\OE", true);
  defineSymbol(text, main, textord, "\xD8", "\\O", true);
  defineSymbol(text, main, accent, "\u02CA", "\\'");
  defineSymbol(text, main, accent, "\u02CB", "\\`");
  defineSymbol(text, main, accent, "\u02C6", "\\^");
  defineSymbol(text, main, accent, "\u02DC", "\\~");
  defineSymbol(text, main, accent, "\u02C9", "\\=");
  defineSymbol(text, main, accent, "\u02D8", "\\u");
  defineSymbol(text, main, accent, "\u02D9", "\\.");
  defineSymbol(text, main, accent, "\xB8", "\\c");
  defineSymbol(text, main, accent, "\u02DA", "\\r");
  defineSymbol(text, main, accent, "\u02C7", "\\v");
  defineSymbol(text, main, accent, "\xA8", '\\"');
  defineSymbol(text, main, accent, "\u02DD", "\\H");
  defineSymbol(text, main, accent, "\u25EF", "\\textcircled");
  var ligatures = {
    "--": true,
    "---": true,
    "``": true,
    "''": true
  };
  defineSymbol(text, main, textord, "\u2013", "--", true);
  defineSymbol(text, main, textord, "\u2013", "\\textendash");
  defineSymbol(text, main, textord, "\u2014", "---", true);
  defineSymbol(text, main, textord, "\u2014", "\\textemdash");
  defineSymbol(text, main, textord, "\u2018", "`", true);
  defineSymbol(text, main, textord, "\u2018", "\\textquoteleft");
  defineSymbol(text, main, textord, "\u2019", "'", true);
  defineSymbol(text, main, textord, "\u2019", "\\textquoteright");
  defineSymbol(text, main, textord, "\u201C", "``", true);
  defineSymbol(text, main, textord, "\u201C", "\\textquotedblleft");
  defineSymbol(text, main, textord, "\u201D", "''", true);
  defineSymbol(text, main, textord, "\u201D", "\\textquotedblright");
  defineSymbol(math, main, textord, "\xB0", "\\degree", true);
  defineSymbol(text, main, textord, "\xB0", "\\degree");
  defineSymbol(text, main, textord, "\xB0", "\\textdegree", true);
  defineSymbol(math, main, textord, "\xA3", "\\pounds");
  defineSymbol(math, main, textord, "\xA3", "\\mathsterling", true);
  defineSymbol(text, main, textord, "\xA3", "\\pounds");
  defineSymbol(text, main, textord, "\xA3", "\\textsterling", true);
  defineSymbol(math, ams, textord, "\u2720", "\\maltese");
  defineSymbol(text, ams, textord, "\u2720", "\\maltese");
  var mathTextSymbols = '0123456789/@."';
  for (i2 = 0; i2 < mathTextSymbols.length; i2++) {
    ch = mathTextSymbols.charAt(i2);
    defineSymbol(math, main, textord, ch, ch);
  }
  var ch;
  var i2;
  var textSymbols = '0123456789!@*()-=+";:?/.,';
  for (_i = 0; _i < textSymbols.length; _i++) {
    _ch = textSymbols.charAt(_i);
    defineSymbol(text, main, textord, _ch, _ch);
  }
  var _ch;
  var _i;
  var letters = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz";
  for (_i2 = 0; _i2 < letters.length; _i2++) {
    _ch2 = letters.charAt(_i2);
    defineSymbol(math, main, mathord, _ch2, _ch2);
    defineSymbol(text, main, textord, _ch2, _ch2);
  }
  var _ch2;
  var _i2;
  defineSymbol(math, ams, textord, "C", "\u2102");
  defineSymbol(text, ams, textord, "C", "\u2102");
  defineSymbol(math, ams, textord, "H", "\u210D");
  defineSymbol(text, ams, textord, "H", "\u210D");
  defineSymbol(math, ams, textord, "N", "\u2115");
  defineSymbol(text, ams, textord, "N", "\u2115");
  defineSymbol(math, ams, textord, "P", "\u2119");
  defineSymbol(text, ams, textord, "P", "\u2119");
  defineSymbol(math, ams, textord, "Q", "\u211A");
  defineSymbol(text, ams, textord, "Q", "\u211A");
  defineSymbol(math, ams, textord, "R", "\u211D");
  defineSymbol(text, ams, textord, "R", "\u211D");
  defineSymbol(math, ams, textord, "Z", "\u2124");
  defineSymbol(text, ams, textord, "Z", "\u2124");
  defineSymbol(math, main, mathord, "h", "\u210E");
  defineSymbol(text, main, mathord, "h", "\u210E");
  var wideChar;
  for (_i3 = 0; _i3 < letters.length; _i3++) {
    _ch3 = letters.charAt(_i3);
    wideChar = String.fromCharCode(55349, 56320 + _i3);
    defineSymbol(math, main, mathord, _ch3, wideChar);
    defineSymbol(text, main, textord, _ch3, wideChar);
    wideChar = String.fromCharCode(55349, 56372 + _i3);
    defineSymbol(math, main, mathord, _ch3, wideChar);
    defineSymbol(text, main, textord, _ch3, wideChar);
    wideChar = String.fromCharCode(55349, 56424 + _i3);
    defineSymbol(math, main, mathord, _ch3, wideChar);
    defineSymbol(text, main, textord, _ch3, wideChar);
    wideChar = String.fromCharCode(55349, 56580 + _i3);
    defineSymbol(math, main, mathord, _ch3, wideChar);
    defineSymbol(text, main, textord, _ch3, wideChar);
    wideChar = String.fromCharCode(55349, 56684 + _i3);
    defineSymbol(math, main, mathord, _ch3, wideChar);
    defineSymbol(text, main, textord, _ch3, wideChar);
    wideChar = String.fromCharCode(55349, 56736 + _i3);
    defineSymbol(math, main, mathord, _ch3, wideChar);
    defineSymbol(text, main, textord, _ch3, wideChar);
    wideChar = String.fromCharCode(55349, 56788 + _i3);
    defineSymbol(math, main, mathord, _ch3, wideChar);
    defineSymbol(text, main, textord, _ch3, wideChar);
    wideChar = String.fromCharCode(55349, 56840 + _i3);
    defineSymbol(math, main, mathord, _ch3, wideChar);
    defineSymbol(text, main, textord, _ch3, wideChar);
    wideChar = String.fromCharCode(55349, 56944 + _i3);
    defineSymbol(math, main, mathord, _ch3, wideChar);
    defineSymbol(text, main, textord, _ch3, wideChar);
    if (_i3 < 26) {
      wideChar = String.fromCharCode(55349, 56632 + _i3);
      defineSymbol(math, main, mathord, _ch3, wideChar);
      defineSymbol(text, main, textord, _ch3, wideChar);
      wideChar = String.fromCharCode(55349, 56476 + _i3);
      defineSymbol(math, main, mathord, _ch3, wideChar);
      defineSymbol(text, main, textord, _ch3, wideChar);
    }
  }
  var _ch3;
  var _i3;
  wideChar = String.fromCharCode(55349, 56668);
  defineSymbol(math, main, mathord, "k", wideChar);
  defineSymbol(text, main, textord, "k", wideChar);
  for (_i4 = 0; _i4 < 10; _i4++) {
    _ch4 = _i4.toString();
    wideChar = String.fromCharCode(55349, 57294 + _i4);
    defineSymbol(math, main, mathord, _ch4, wideChar);
    defineSymbol(text, main, textord, _ch4, wideChar);
    wideChar = String.fromCharCode(55349, 57314 + _i4);
    defineSymbol(math, main, mathord, _ch4, wideChar);
    defineSymbol(text, main, textord, _ch4, wideChar);
    wideChar = String.fromCharCode(55349, 57324 + _i4);
    defineSymbol(math, main, mathord, _ch4, wideChar);
    defineSymbol(text, main, textord, _ch4, wideChar);
    wideChar = String.fromCharCode(55349, 57334 + _i4);
    defineSymbol(math, main, mathord, _ch4, wideChar);
    defineSymbol(text, main, textord, _ch4, wideChar);
  }
  var _ch4;
  var _i4;
  var extraLatin = "\xD0\xDE\xFE";
  for (_i5 = 0; _i5 < extraLatin.length; _i5++) {
    _ch5 = extraLatin.charAt(_i5);
    defineSymbol(math, main, mathord, _ch5, _ch5);
    defineSymbol(text, main, textord, _ch5, _ch5);
  }
  var _ch5;
  var _i5;
  var boldUpright = {
    mathClass: "mathbf",
    textClass: "textbf",
    font: "Main-Bold"
  };
  var italic = {
    mathClass: "mathnormal",
    textClass: "textit",
    font: "Math-Italic"
  };
  var boldItalic = {
    mathClass: "boldsymbol",
    textClass: "boldsymbol",
    font: "Main-BoldItalic"
  };
  var script = {
    mathClass: "mathscr",
    textClass: "textscr",
    font: "Script-Regular"
  };
  var noFont = {
    mathClass: "",
    textClass: "",
    font: ""
  };
  var fraktur = {
    mathClass: "mathfrak",
    textClass: "textfrak",
    font: "Fraktur-Regular"
  };
  var doubleStruck = {
    mathClass: "mathbb",
    textClass: "textbb",
    font: "AMS-Regular"
  };
  var boldFraktur = {
    mathClass: "mathboldfrak",
    textClass: "textboldfrak",
    font: "Fraktur-Regular"
  };
  var sansSerif = {
    mathClass: "mathsf",
    textClass: "textsf",
    font: "SansSerif-Regular"
  };
  var boldSansSerif = {
    mathClass: "mathboldsf",
    textClass: "textboldsf",
    font: "SansSerif-Bold"
  };
  var italicSansSerif = {
    mathClass: "mathitsf",
    textClass: "textitsf",
    font: "SansSerif-Italic"
  };
  var monospace = {
    mathClass: "mathtt",
    textClass: "texttt",
    font: "Typewriter-Regular"
  };
  var wideLatinLetterData = [
    boldUpright,
    boldUpright,
    // A-Z, a-z
    italic,
    italic,
    // A-Z, a-z
    boldItalic,
    boldItalic,
    // A-Z, a-z
    // Map fancy A-Z letters to script, not calligraphic.
    // This aligns with unicode-math and math fonts (except Cambria Math).
    script,
    noFont,
    // A-Z script, a-z — no font
    noFont,
    noFont,
    // A-Z bold script, a-z bold script — no font
    fraktur,
    fraktur,
    // A-Z, a-z
    doubleStruck,
    doubleStruck,
    // A-Z double-struck, k double-struck
    // Note that we are using a bold font, but font metrics for regular Fraktur.
    boldFraktur,
    boldFraktur,
    // A-Z, a-z
    sansSerif,
    sansSerif,
    // A-Z, a-z
    boldSansSerif,
    boldSansSerif,
    // A-Z, a-z
    italicSansSerif,
    italicSansSerif,
    // A-Z, a-z
    noFont,
    noFont,
    // A-Z bold italic sans, a-z bold italic sans - no font
    monospace,
    monospace
    // A-Z, a-z
  ];
  var wideNumeralData = [
    boldUpright,
    // 0-9
    noFont,
    // 0-9 double-struck. No KaTeX font.
    sansSerif,
    // 0-9
    boldSansSerif,
    // 0-9
    monospace
    // 0-9
  ];
  var wideCharacterFont = (wideChar2) => {
    var H2 = wideChar2.charCodeAt(0);
    var L2 = wideChar2.charCodeAt(1);
    var codePoint = (H2 - 55296) * 1024 + (L2 - 56320) + 65536;
    if (119808 <= codePoint && codePoint < 120484) {
      var i3 = Math.floor((codePoint - 119808) / 26);
      return wideLatinLetterData[i3];
    } else if (120782 <= codePoint && codePoint <= 120831) {
      var _i6 = Math.floor((codePoint - 120782) / 10);
      return wideNumeralData[_i6];
    } else if (codePoint === 120485 || codePoint === 120486) {
      return wideLatinLetterData[0];
    } else if (120486 < codePoint && codePoint < 120782) {
      return noFont;
    } else {
      throw new ParseError("Unsupported character: " + wideChar2);
    }
  };
  var lookupSymbol = function lookupSymbol2(value, fontName, mode) {
    if (symbols[mode][value]) {
      var replacement = symbols[mode][value].replace;
      if (replacement) {
        value = replacement;
      }
    }
    return {
      value,
      metrics: getCharacterMetrics(value, fontName, mode)
    };
  };
  var makeSymbol = function makeSymbol2(value, fontName, mode, options, classes) {
    var lookup = lookupSymbol(value, fontName, mode);
    var metrics = lookup.metrics;
    value = lookup.value;
    var symbolNode;
    if (metrics) {
      var italic2 = metrics.italic;
      if (mode === "text" || options && options.font === "mathit") {
        italic2 = 0;
      }
      symbolNode = new SymbolNode(value, metrics.height, metrics.depth, italic2, metrics.skew, metrics.width, classes);
    } else {
      typeof console !== "undefined" && console.warn("No character metrics " + ("for '" + value + "' in style '" + fontName + "' and mode '" + mode + "'"));
      symbolNode = new SymbolNode(value, 0, 0, 0, 0, 0, classes);
    }
    if (options) {
      symbolNode.maxFontSize = options.sizeMultiplier;
      if (options.style.isTight()) {
        symbolNode.classes.push("mtight");
      }
      var color = options.getColor();
      if (color) {
        symbolNode.style.color = color;
      }
    }
    return symbolNode;
  };
  var mathsym = function mathsym2(value, mode, options, classes) {
    if (classes === void 0) {
      classes = [];
    }
    if (options.font === "boldsymbol" && lookupSymbol(value, "Main-Bold", mode).metrics) {
      return makeSymbol(value, "Main-Bold", mode, options, classes.concat(["mathbf"]));
    } else if (value === "\\" || symbols[mode][value].font === "main") {
      return makeSymbol(value, "Main-Regular", mode, options, classes);
    } else {
      return makeSymbol(value, "AMS-Regular", mode, options, classes.concat(["amsrm"]));
    }
  };
  var boldSymbol = function boldSymbol2(value, mode, type) {
    if (type !== "textord" && lookupSymbol(value, "Math-BoldItalic", mode).metrics) {
      return {
        fontName: "Math-BoldItalic",
        fontClass: "boldsymbol"
      };
    } else {
      return {
        fontName: "Main-Bold",
        fontClass: "mathbf"
      };
    }
  };
  var makeOrd = function makeOrd2(group, options, type) {
    var mode = group.mode;
    var text2 = group.text;
    var classes = ["mord"];
    var {
      font,
      fontFamily,
      fontWeight,
      fontShape
    } = options;
    var useFont = mode === "math" || mode === "text" && !!font;
    var fontOrFamily = useFont ? font : fontFamily;
    var wideFontName = "";
    var wideFontClass = "";
    if (text2.charCodeAt(0) === 55349) {
      var wideCharData = wideCharacterFont(text2);
      wideFontName = wideCharData.font;
      wideFontClass = wideCharData[mode + "Class"];
    }
    if (wideFontName) {
      return makeSymbol(text2, wideFontName, mode, options, classes.concat(wideFontClass));
    } else if (fontOrFamily) {
      var fontName;
      var fontClasses;
      if (fontOrFamily === "boldsymbol") {
        var fontData = boldSymbol(text2, mode, type);
        fontName = fontData.fontName;
        fontClasses = [fontData.fontClass];
      } else if (useFont) {
        fontName = fontMap[font].fontName;
        fontClasses = [font];
      } else {
        fontName = retrieveTextFontName(fontFamily, fontWeight, fontShape);
        fontClasses = [fontFamily, fontWeight, fontShape];
      }
      if (lookupSymbol(text2, fontName, mode).metrics) {
        return makeSymbol(text2, fontName, mode, options, classes.concat(fontClasses));
      } else if (ligatures.hasOwnProperty(text2) && fontName.slice(0, 10) === "Typewriter") {
        var parts = [];
        for (var i3 = 0; i3 < text2.length; i3++) {
          parts.push(makeSymbol(text2[i3], fontName, mode, options, classes.concat(fontClasses)));
        }
        return makeFragment(parts);
      }
    }
    if (type === "mathord") {
      return makeSymbol(text2, "Math-Italic", mode, options, classes.concat(["mathnormal"]));
    } else if (type === "textord") {
      var _font = symbols[mode][text2] && symbols[mode][text2].font;
      if (_font === "ams") {
        var _fontName = retrieveTextFontName("amsrm", fontWeight, fontShape);
        return makeSymbol(text2, _fontName, mode, options, classes.concat("amsrm", fontWeight, fontShape));
      } else if (_font === "main" || !_font) {
        var _fontName2 = retrieveTextFontName("textrm", fontWeight, fontShape);
        return makeSymbol(text2, _fontName2, mode, options, classes.concat(fontWeight, fontShape));
      } else {
        var _fontName3 = retrieveTextFontName(_font, fontWeight, fontShape);
        return makeSymbol(text2, _fontName3, mode, options, classes.concat(_fontName3, fontWeight, fontShape));
      }
    } else {
      throw new Error("unexpected type: " + type + " in makeOrd");
    }
  };
  var canCombine = (prev, next) => {
    if (createClass(prev.classes) !== createClass(next.classes) || prev.skew !== next.skew || prev.maxFontSize !== next.maxFontSize || prev.italic !== 0 && prev.hasClass("mathnormal")) {
      return false;
    }
    if (prev.classes.length === 1) {
      var cls = prev.classes[0];
      if (cls === "mbin" || cls === "mord") {
        return false;
      }
    }
    for (var key of Object.keys(prev.style)) {
      if (prev.style[key] !== next.style[key]) {
        return false;
      }
    }
    for (var _key of Object.keys(next.style)) {
      if (prev.style[_key] !== next.style[_key]) {
        return false;
      }
    }
    return true;
  };
  var tryCombineChars = (chars) => {
    for (var i3 = 0; i3 < chars.length - 1; i3++) {
      var prev = chars[i3];
      var next = chars[i3 + 1];
      if (prev instanceof SymbolNode && next instanceof SymbolNode && canCombine(prev, next)) {
        prev.text += next.text;
        prev.height = Math.max(prev.height, next.height);
        prev.depth = Math.max(prev.depth, next.depth);
        prev.italic = next.italic;
        chars.splice(i3 + 1, 1);
        i3--;
      }
    }
    return chars;
  };
  var sizeElementFromChildren = function sizeElementFromChildren2(elem) {
    var height = 0;
    var depth = 0;
    var maxFontSize = 0;
    for (var i3 = 0; i3 < elem.children.length; i3++) {
      var child = elem.children[i3];
      if (child.height > height) {
        height = child.height;
      }
      if (child.depth > depth) {
        depth = child.depth;
      }
      if (child.maxFontSize > maxFontSize) {
        maxFontSize = child.maxFontSize;
      }
    }
    elem.height = height;
    elem.depth = depth;
    elem.maxFontSize = maxFontSize;
  };
  var makeSpan = function makeSpan2(classes, children, options, style) {
    var span = new Span(classes, children, options, style);
    sizeElementFromChildren(span);
    return span;
  };
  var makeSvgSpan = (classes, children, options, style) => new Span(classes, children, options, style);
  var makeLineSpan = function makeLineSpan2(className, options, thickness) {
    var line = makeSpan([className], [], options);
    line.height = Math.max(thickness || options.fontMetrics().defaultRuleThickness, options.minRuleThickness);
    line.style.borderBottomWidth = makeEm(line.height);
    line.maxFontSize = 1;
    return line;
  };
  var makeAnchor = function makeAnchor2(href, classes, children, options) {
    var anchor = new Anchor(href, classes, children, options);
    sizeElementFromChildren(anchor);
    return anchor;
  };
  var makeFragment = function makeFragment2(children) {
    var fragment = new DocumentFragment(children);
    sizeElementFromChildren(fragment);
    return fragment;
  };
  var wrapFragment = function wrapFragment2(group, options) {
    if (group instanceof DocumentFragment) {
      return makeSpan([], [group], options);
    }
    return group;
  };
  var getVListChildrenAndDepth = function getVListChildrenAndDepth2(params) {
    if (params.positionType === "individualShift") {
      var oldChildren = params.children;
      var children = [oldChildren[0]];
      var _depth = -oldChildren[0].shift - oldChildren[0].elem.depth;
      var currPos = _depth;
      for (var i3 = 1; i3 < oldChildren.length; i3++) {
        var diff = -oldChildren[i3].shift - currPos - oldChildren[i3].elem.depth;
        var size = diff - (oldChildren[i3 - 1].elem.height + oldChildren[i3 - 1].elem.depth);
        currPos = currPos + diff;
        children.push({
          type: "kern",
          size
        });
        children.push(oldChildren[i3]);
      }
      return {
        children,
        depth: _depth
      };
    }
    var depth;
    if (params.positionType === "top") {
      var bottom = params.positionData;
      for (var _i6 = 0; _i6 < params.children.length; _i6++) {
        var child = params.children[_i6];
        bottom -= child.type === "kern" ? child.size : child.elem.height + child.elem.depth;
      }
      depth = bottom;
    } else if (params.positionType === "bottom") {
      depth = -params.positionData;
    } else {
      var firstChild = params.children[0];
      if (firstChild.type !== "elem") {
        throw new Error('First child must have type "elem".');
      }
      if (params.positionType === "shift") {
        depth = -firstChild.elem.depth - params.positionData;
      } else if (params.positionType === "firstBaseline") {
        depth = -firstChild.elem.depth;
      } else {
        throw new Error("Invalid positionType " + params.positionType + ".");
      }
    }
    return {
      children: params.children,
      depth
    };
  };
  var makeVList = function makeVList2(params, options) {
    var {
      children,
      depth
    } = getVListChildrenAndDepth(params);
    var pstrutSize = 0;
    for (var i3 = 0; i3 < children.length; i3++) {
      var child = children[i3];
      if (child.type === "elem") {
        var elem = child.elem;
        pstrutSize = Math.max(pstrutSize, elem.maxFontSize, elem.height);
      }
    }
    pstrutSize += 2;
    var pstrut = makeSpan(["pstrut"], []);
    pstrut.style.height = makeEm(pstrutSize);
    var realChildren = [];
    var minPos = depth;
    var maxPos = depth;
    var currPos = depth;
    for (var _i22 = 0; _i22 < children.length; _i22++) {
      var _child = children[_i22];
      if (_child.type === "kern") {
        currPos += _child.size;
      } else {
        var _elem = _child.elem;
        var classes = _child.wrapperClasses || [];
        var style = _child.wrapperStyle || {};
        var childWrap = makeSpan(classes, [pstrut, _elem], void 0, style);
        childWrap.style.top = makeEm(-pstrutSize - currPos - _elem.depth);
        if (_child.marginLeft) {
          childWrap.style.marginLeft = _child.marginLeft;
        }
        if (_child.marginRight) {
          childWrap.style.marginRight = _child.marginRight;
        }
        realChildren.push(childWrap);
        currPos += _elem.height + _elem.depth;
      }
      minPos = Math.min(minPos, currPos);
      maxPos = Math.max(maxPos, currPos);
    }
    var vlist = makeSpan(["vlist"], realChildren);
    vlist.style.height = makeEm(maxPos);
    var rows;
    if (minPos < 0) {
      var emptySpan = makeSpan([], []);
      var depthStrut = makeSpan(["vlist"], [emptySpan]);
      depthStrut.style.height = makeEm(-minPos);
      var topStrut = makeSpan(["vlist-s"], [new SymbolNode("\u200B")]);
      rows = [makeSpan(["vlist-r"], [vlist, topStrut]), makeSpan(["vlist-r"], [depthStrut])];
    } else {
      rows = [makeSpan(["vlist-r"], [vlist])];
    }
    var vtable = makeSpan(["vlist-t"], rows);
    if (rows.length === 2) {
      vtable.classes.push("vlist-t2");
    }
    vtable.height = maxPos;
    vtable.depth = -minPos;
    return vtable;
  };
  var makeGlue = (measurement, options) => {
    var rule = makeSpan(["mspace"], [], options);
    var size = calculateSize(measurement, options);
    rule.style.marginRight = makeEm(size);
    return rule;
  };
  var retrieveTextFontName = (fontFamily, fontWeight, fontShape) => {
    var baseFontName;
    var fontStylesName;
    switch (fontFamily) {
      case "amsrm":
        baseFontName = "AMS";
        break;
      case "textrm":
        baseFontName = "Main";
        break;
      case "textsf":
        baseFontName = "SansSerif";
        break;
      case "texttt":
        baseFontName = "Typewriter";
        break;
      default:
        baseFontName = fontFamily;
    }
    if (fontWeight === "textbf" && fontShape === "textit") {
      fontStylesName = "BoldItalic";
    } else if (fontWeight === "textbf") {
      fontStylesName = "Bold";
    } else if (fontShape === "textit") {
      fontStylesName = "Italic";
    } else {
      fontStylesName = "Regular";
    }
    return baseFontName + "-" + fontStylesName;
  };
  var fontMap = {
    // styles
    "mathbf": {
      variant: "bold",
      fontName: "Main-Bold"
    },
    "mathrm": {
      variant: "normal",
      fontName: "Main-Regular"
    },
    "textit": {
      variant: "italic",
      fontName: "Main-Italic"
    },
    "mathit": {
      variant: "italic",
      fontName: "Main-Italic"
    },
    "mathnormal": {
      variant: "italic",
      fontName: "Math-Italic"
    },
    "mathsfit": {
      variant: "sans-serif-italic",
      fontName: "SansSerif-Italic"
    },
    // "boldsymbol" is missing because they require the use of multiple fonts:
    // Math-BoldItalic and Main-Bold.  This is handled by a special case in
    // makeOrd which ends up calling boldsymbol.
    // families
    "mathbb": {
      variant: "double-struck",
      fontName: "AMS-Regular"
    },
    "mathcal": {
      variant: "script",
      fontName: "Caligraphic-Regular"
    },
    "mathfrak": {
      variant: "fraktur",
      fontName: "Fraktur-Regular"
    },
    "mathscr": {
      variant: "script",
      fontName: "Script-Regular"
    },
    "mathsf": {
      variant: "sans-serif",
      fontName: "SansSerif-Regular"
    },
    "mathtt": {
      variant: "monospace",
      fontName: "Typewriter-Regular"
    }
  };
  var svgData = {
    //   path, width, height
    vec: ["vec", 0.471, 0.714],
    // values from the font glyph
    oiintSize1: ["oiintSize1", 0.957, 0.499],
    // oval to overlay the integrand
    oiintSize2: ["oiintSize2", 1.472, 0.659],
    oiiintSize1: ["oiiintSize1", 1.304, 0.499],
    oiiintSize2: ["oiiintSize2", 1.98, 0.659]
  };
  var staticSvg = function staticSvg2(value, options) {
    var [pathName, width, height] = svgData[value];
    var path2 = new PathNode(pathName);
    var svgNode = new SvgNode([path2], {
      "width": makeEm(width),
      "height": makeEm(height),
      // Override CSS rule `.katex svg { width: 100% }`
      "style": "width:" + makeEm(width),
      "viewBox": "0 0 " + 1e3 * width + " " + 1e3 * height,
      "preserveAspectRatio": "xMinYMin"
    });
    var span = makeSvgSpan(["overlay"], [svgNode], options);
    span.height = height;
    span.style.height = makeEm(height);
    span.style.width = makeEm(width);
    return span;
  };
  var thinspace = {
    number: 3,
    unit: "mu"
  };
  var mediumspace = {
    number: 4,
    unit: "mu"
  };
  var thickspace = {
    number: 5,
    unit: "mu"
  };
  var spacings = {
    mord: {
      mop: thinspace,
      mbin: mediumspace,
      mrel: thickspace,
      minner: thinspace
    },
    mop: {
      mord: thinspace,
      mop: thinspace,
      mrel: thickspace,
      minner: thinspace
    },
    mbin: {
      mord: mediumspace,
      mop: mediumspace,
      mopen: mediumspace,
      minner: mediumspace
    },
    mrel: {
      mord: thickspace,
      mop: thickspace,
      mopen: thickspace,
      minner: thickspace
    },
    mopen: {},
    mclose: {
      mop: thinspace,
      mbin: mediumspace,
      mrel: thickspace,
      minner: thinspace
    },
    mpunct: {
      mord: thinspace,
      mop: thinspace,
      mrel: thickspace,
      mopen: thinspace,
      mclose: thinspace,
      mpunct: thinspace,
      minner: thinspace
    },
    minner: {
      mord: thinspace,
      mop: thinspace,
      mbin: mediumspace,
      mrel: thickspace,
      mopen: thinspace,
      mpunct: thinspace,
      minner: thinspace
    }
  };
  var tightSpacings = {
    mord: {
      mop: thinspace
    },
    mop: {
      mord: thinspace,
      mop: thinspace
    },
    mbin: {},
    mrel: {},
    mopen: {},
    mclose: {
      mop: thinspace
    },
    mpunct: {},
    minner: {
      mop: thinspace
    }
  };
  var _functions = {};
  var _htmlGroupBuilders = {};
  var _mathmlGroupBuilders = {};
  function defineFunction(_ref) {
    var {
      type,
      names,
      props,
      handler,
      htmlBuilder: htmlBuilder3,
      mathmlBuilder: mathmlBuilder3
    } = _ref;
    var data = {
      type,
      numArgs: props.numArgs,
      argTypes: props.argTypes,
      allowedInArgument: !!props.allowedInArgument,
      allowedInText: !!props.allowedInText,
      allowedInMath: props.allowedInMath === void 0 ? true : props.allowedInMath,
      numOptionalArgs: props.numOptionalArgs || 0,
      infix: !!props.infix,
      primitive: !!props.primitive,
      handler
    };
    for (var i3 = 0; i3 < names.length; ++i3) {
      _functions[names[i3]] = data;
    }
    if (type) {
      if (htmlBuilder3) {
        _htmlGroupBuilders[type] = htmlBuilder3;
      }
      if (mathmlBuilder3) {
        _mathmlGroupBuilders[type] = mathmlBuilder3;
      }
    }
  }
  function defineFunctionBuilders(_ref2) {
    var {
      type,
      htmlBuilder: htmlBuilder3,
      mathmlBuilder: mathmlBuilder3
    } = _ref2;
    defineFunction({
      type,
      names: [],
      props: {
        numArgs: 0
      },
      handler() {
        throw new Error("Should never be called.");
      },
      htmlBuilder: htmlBuilder3,
      mathmlBuilder: mathmlBuilder3
    });
  }
  var normalizeArgument = function normalizeArgument2(arg) {
    return arg.type === "ordgroup" && arg.body.length === 1 ? arg.body[0] : arg;
  };
  var ordargument = function ordargument2(arg) {
    return arg.type === "ordgroup" ? arg.body : [arg];
  };
  var binLeftCanceller = /* @__PURE__ */ new Set(["leftmost", "mbin", "mopen", "mrel", "mop", "mpunct"]);
  var binRightCanceller = /* @__PURE__ */ new Set(["rightmost", "mrel", "mclose", "mpunct"]);
  var styleMap$1 = {
    "display": Style$1.DISPLAY,
    "text": Style$1.TEXT,
    "script": Style$1.SCRIPT,
    "scriptscript": Style$1.SCRIPTSCRIPT
  };
  var DomEnum = {
    mord: "mord",
    mop: "mop",
    mbin: "mbin",
    mrel: "mrel",
    mopen: "mopen",
    mclose: "mclose",
    mpunct: "mpunct",
    minner: "minner"
  };
  var buildExpression$1 = function buildExpression(expression, options, isRealGroup, surrounding) {
    if (surrounding === void 0) {
      surrounding = [null, null];
    }
    var groups = [];
    for (var i3 = 0; i3 < expression.length; i3++) {
      var output = buildGroup$1(expression[i3], options);
      if (output instanceof DocumentFragment) {
        var children = output.children;
        groups.push(...children);
      } else {
        groups.push(output);
      }
    }
    tryCombineChars(groups);
    if (!isRealGroup) {
      return groups;
    }
    var glueOptions = options;
    if (expression.length === 1) {
      var node = expression[0];
      if (node.type === "sizing") {
        glueOptions = options.havingSize(node.size);
      } else if (node.type === "styling") {
        glueOptions = options.havingStyle(styleMap$1[node.style]);
      }
    }
    var dummyPrev = makeSpan([surrounding[0] || "leftmost"], [], options);
    var dummyNext = makeSpan([surrounding[1] || "rightmost"], [], options);
    var isRoot = isRealGroup === "root";
    _traverseNonSpaceNodes(groups, (node2, prev) => {
      var prevType = prev.classes[0];
      var type = node2.classes[0];
      if (prevType === "mbin" && binRightCanceller.has(type)) {
        prev.classes[0] = "mord";
      } else if (type === "mbin" && binLeftCanceller.has(prevType)) {
        node2.classes[0] = "mord";
      }
    }, {
      node: dummyPrev
    }, dummyNext, isRoot);
    _traverseNonSpaceNodes(groups, (node2, prev) => {
      var _tightSpacings$prevTy, _spacings$prevType;
      var prevType = getTypeOfDomTree(prev);
      var type = getTypeOfDomTree(node2);
      var space = prevType && type ? node2.hasClass("mtight") ? (_tightSpacings$prevTy = tightSpacings[prevType]) == null ? void 0 : _tightSpacings$prevTy[type] : (_spacings$prevType = spacings[prevType]) == null ? void 0 : _spacings$prevType[type] : null;
      if (space) {
        return makeGlue(space, glueOptions);
      }
    }, {
      node: dummyPrev
    }, dummyNext, isRoot);
    return groups;
  };
  var _traverseNonSpaceNodes = function traverseNonSpaceNodes(nodes, callback, prev, next, isRoot) {
    if (next) {
      nodes.push(next);
    }
    var i3 = 0;
    for (; i3 < nodes.length; i3++) {
      var node = nodes[i3];
      var partialGroup = checkPartialGroup(node);
      if (partialGroup) {
        _traverseNonSpaceNodes(partialGroup.children, callback, prev, null, isRoot);
        continue;
      }
      var nonspace = !node.hasClass("mspace");
      if (nonspace) {
        var result = callback(node, prev.node);
        if (result) {
          if (prev.insertAfter) {
            prev.insertAfter(result);
          } else {
            nodes.unshift(result);
            i3++;
          }
        }
      }
      if (nonspace) {
        prev.node = node;
      } else if (isRoot && node.hasClass("newline")) {
        prev.node = makeSpan(["leftmost"]);
      }
      prev.insertAfter = /* @__PURE__ */ ((index) => (n) => {
        nodes.splice(index + 1, 0, n);
        i3++;
      })(i3);
    }
    if (next) {
      nodes.pop();
    }
  };
  var checkPartialGroup = function checkPartialGroup2(node) {
    if (node instanceof DocumentFragment || node instanceof Anchor || node instanceof Span && node.hasClass("enclosing")) {
      return node;
    }
    return null;
  };
  var _getOutermostNode = function getOutermostNode(node, side) {
    var partialGroup = checkPartialGroup(node);
    if (partialGroup) {
      var children = partialGroup.children;
      if (children.length) {
        if (side === "right") {
          return _getOutermostNode(children[children.length - 1], "right");
        } else if (side === "left") {
          return _getOutermostNode(children[0], "left");
        }
      }
    }
    return node;
  };
  var getTypeOfDomTree = function getTypeOfDomTree2(node, side) {
    if (!node) {
      return null;
    }
    if (side) {
      node = _getOutermostNode(node, side);
    }
    var className = node.classes[0];
    return DomEnum[className] || null;
  };
  var makeNullDelimiter = function makeNullDelimiter2(options, classes) {
    var moreClasses = ["nulldelimiter"].concat(options.baseSizingClasses());
    return makeSpan(classes.concat(moreClasses));
  };
  var buildGroup$1 = function buildGroup(group, options, baseOptions) {
    if (!group) {
      return makeSpan();
    }
    if (_htmlGroupBuilders[group.type]) {
      var groupNode = _htmlGroupBuilders[group.type](group, options);
      if (baseOptions && options.size !== baseOptions.size) {
        groupNode = makeSpan(options.sizingClasses(baseOptions), [groupNode], options);
        var multiplier = options.sizeMultiplier / baseOptions.sizeMultiplier;
        groupNode.height *= multiplier;
        groupNode.depth *= multiplier;
      }
      return groupNode;
    } else {
      throw new ParseError("Got group of unknown type: '" + group.type + "'");
    }
  };
  function buildHTMLUnbreakable(children, options) {
    var body = makeSpan(["base"], children, options);
    var strut = makeSpan(["strut"]);
    strut.style.height = makeEm(body.height + body.depth);
    if (body.depth) {
      strut.style.verticalAlign = makeEm(-body.depth);
    }
    body.children.unshift(strut);
    return body;
  }
  function buildHTML(tree, options) {
    var tag = null;
    if (tree.length === 1 && tree[0].type === "tag") {
      tag = tree[0].tag;
      tree = tree[0].body;
    }
    var expression = buildExpression$1(tree, options, "root");
    var eqnNum;
    if (expression.length === 2 && expression[1].hasClass("tag")) {
      eqnNum = expression.pop();
    }
    var children = [];
    var parts = [];
    for (var i3 = 0; i3 < expression.length; i3++) {
      parts.push(expression[i3]);
      if (expression[i3].hasClass("mbin") || expression[i3].hasClass("mrel") || expression[i3].hasClass("allowbreak")) {
        var nobreak = false;
        while (i3 < expression.length - 1 && expression[i3 + 1].hasClass("mspace") && !expression[i3 + 1].hasClass("newline")) {
          i3++;
          parts.push(expression[i3]);
          if (expression[i3].hasClass("nobreak")) {
            nobreak = true;
          }
        }
        if (!nobreak) {
          children.push(buildHTMLUnbreakable(parts, options));
          parts = [];
        }
      } else if (expression[i3].hasClass("newline")) {
        parts.pop();
        if (parts.length > 0) {
          children.push(buildHTMLUnbreakable(parts, options));
          parts = [];
        }
        children.push(expression[i3]);
      }
    }
    if (parts.length > 0) {
      children.push(buildHTMLUnbreakable(parts, options));
    }
    var tagChild;
    if (tag) {
      tagChild = buildHTMLUnbreakable(buildExpression$1(tag, options, true), options);
      tagChild.classes = ["tag"];
      children.push(tagChild);
    } else if (eqnNum) {
      children.push(eqnNum);
    }
    var htmlNode = makeSpan(["katex-html"], children);
    htmlNode.setAttribute("aria-hidden", "true");
    if (tagChild) {
      var strut = tagChild.children[0];
      strut.style.height = makeEm(htmlNode.height + htmlNode.depth);
      if (htmlNode.depth) {
        strut.style.verticalAlign = makeEm(-htmlNode.depth);
      }
    }
    return htmlNode;
  }
  function newDocumentFragment(children) {
    return new DocumentFragment(children);
  }
  var MathNode = class {
    constructor(type, children, classes) {
      this.type = void 0;
      this.attributes = void 0;
      this.children = void 0;
      this.classes = void 0;
      this.type = type;
      this.attributes = {};
      this.children = children || [];
      this.classes = classes || [];
    }
    /**
     * Sets an attribute on a MathML node. MathML depends on attributes to convey a
     * semantic content, so this is used heavily.
     */
    setAttribute(name, value) {
      this.attributes[name] = value;
    }
    /**
     * Gets an attribute on a MathML node.
     */
    getAttribute(name) {
      return this.attributes[name];
    }
    /**
     * Converts the math node into a MathML-namespaced DOM element.
     */
    toNode() {
      var node = document.createElementNS("http://www.w3.org/1998/Math/MathML", this.type);
      for (var attr in this.attributes) {
        if (Object.prototype.hasOwnProperty.call(this.attributes, attr)) {
          node.setAttribute(attr, this.attributes[attr]);
        }
      }
      if (this.classes.length > 0) {
        node.className = createClass(this.classes);
      }
      for (var i3 = 0; i3 < this.children.length; i3++) {
        if (this.children[i3] instanceof TextNode && this.children[i3 + 1] instanceof TextNode) {
          var text2 = this.children[i3].toText() + this.children[++i3].toText();
          while (this.children[i3 + 1] instanceof TextNode) {
            text2 += this.children[++i3].toText();
          }
          node.appendChild(new TextNode(text2).toNode());
        } else {
          node.appendChild(this.children[i3].toNode());
        }
      }
      return node;
    }
    /**
     * Converts the math node into an HTML markup string.
     */
    toMarkup() {
      var markup = "<" + this.type;
      for (var attr in this.attributes) {
        if (Object.prototype.hasOwnProperty.call(this.attributes, attr)) {
          markup += " " + attr + '="';
          markup += escape(this.attributes[attr]);
          markup += '"';
        }
      }
      if (this.classes.length > 0) {
        markup += ' class ="' + escape(createClass(this.classes)) + '"';
      }
      markup += ">";
      for (var i3 = 0; i3 < this.children.length; i3++) {
        markup += this.children[i3].toMarkup();
      }
      markup += "</" + this.type + ">";
      return markup;
    }
    /**
     * Converts the math node into a string, similar to innerText, but escaped.
     */
    toText() {
      return this.children.map((child) => child.toText()).join("");
    }
  };
  var TextNode = class {
    constructor(text2) {
      this.text = void 0;
      this.text = text2;
    }
    /**
     * Converts the text node into a DOM text node.
     */
    toNode() {
      return document.createTextNode(this.text);
    }
    /**
     * Converts the text node into escaped HTML markup
     * (representing the text itself).
     */
    toMarkup() {
      return escape(this.toText());
    }
    /**
     * Converts the text node into a string
     * (representing the text itself).
     */
    toText() {
      return this.text;
    }
  };
  var SpaceNode = class {
    /**
     * Create a Space node with width given in CSS ems.
     */
    constructor(width) {
      this.width = void 0;
      this.character = void 0;
      this.width = width;
      if (width >= 0.05555 && width <= 0.05556) {
        this.character = "\u200A";
      } else if (width >= 0.1666 && width <= 0.1667) {
        this.character = "\u2009";
      } else if (width >= 0.2222 && width <= 0.2223) {
        this.character = "\u2005";
      } else if (width >= 0.2777 && width <= 0.2778) {
        this.character = "\u2005\u200A";
      } else if (width >= -0.05556 && width <= -0.05555) {
        this.character = "\u200A\u2063";
      } else if (width >= -0.1667 && width <= -0.1666) {
        this.character = "\u2009\u2063";
      } else if (width >= -0.2223 && width <= -0.2222) {
        this.character = "\u205F\u2063";
      } else if (width >= -0.2778 && width <= -0.2777) {
        this.character = "\u2005\u2063";
      } else {
        this.character = null;
      }
    }
    /**
     * Converts the math node into a MathML-namespaced DOM element.
     */
    toNode() {
      if (this.character) {
        return document.createTextNode(this.character);
      } else {
        var node = document.createElementNS("http://www.w3.org/1998/Math/MathML", "mspace");
        node.setAttribute("width", makeEm(this.width));
        return node;
      }
    }
    /**
     * Converts the math node into an HTML markup string.
     */
    toMarkup() {
      if (this.character) {
        return "<mtext>" + this.character + "</mtext>";
      } else {
        return '<mspace width="' + makeEm(this.width) + '"/>';
      }
    }
    /**
     * Converts the math node into a string, similar to innerText.
     */
    toText() {
      if (this.character) {
        return this.character;
      } else {
        return " ";
      }
    }
  };
  var noVariantSymbols = /* @__PURE__ */ new Set(["\\imath", "\\jmath"]);
  var rowLikeTypes = /* @__PURE__ */ new Set(["mrow", "mtable"]);
  var makeText = function makeText2(text2, mode, options) {
    if (symbols[mode][text2] && symbols[mode][text2].replace && text2.charCodeAt(0) !== 55349 && !(ligatures.hasOwnProperty(text2) && options && (options.fontFamily && options.fontFamily.slice(4, 6) === "tt" || options.font && options.font.slice(4, 6) === "tt"))) {
      text2 = symbols[mode][text2].replace;
    }
    return new TextNode(text2);
  };
  var makeRow = function makeRow2(body) {
    if (body.length === 1) {
      return body[0];
    } else {
      return new MathNode("mrow", body);
    }
  };
  var mathFontVariants = {
    mathit: "italic",
    boldsymbol: (group) => group.type === "textord" ? "bold" : "bold-italic",
    mathbf: "bold",
    mathbb: "double-struck",
    mathsfit: "sans-serif-italic",
    mathfrak: "fraktur",
    mathscr: "script",
    mathcal: "script",
    mathsf: "sans-serif",
    mathtt: "monospace"
  };
  var getVariant = (group, options) => {
    if (group.mode === "text") {
      if (options.fontFamily === "texttt") {
        return "monospace";
      } else if (options.fontFamily === "textsf") {
        if (options.fontShape === "textit" && options.fontWeight === "textbf") {
          return "sans-serif-bold-italic";
        } else if (options.fontShape === "textit") {
          return "sans-serif-italic";
        } else if (options.fontWeight === "textbf") {
          return "bold-sans-serif";
        } else {
          return "sans-serif";
        }
      } else if (options.fontShape === "textit" && options.fontWeight === "textbf") {
        return "bold-italic";
      } else if (options.fontShape === "textit") {
        return "italic";
      } else if (options.fontWeight === "textbf") {
        return "bold";
      }
    }
    var font = options.font;
    if (!font || font === "mathnormal") {
      return null;
    }
    var mode = group.mode;
    var mathVariant = mathFontVariants[font];
    if (mathVariant) {
      return typeof mathVariant === "function" ? mathVariant(group) : mathVariant;
    }
    var text2 = group.text;
    if (noVariantSymbols.has(text2)) {
      return null;
    }
    if (symbols[mode][text2]) {
      var replacement = symbols[mode][text2].replace;
      if (replacement) {
        text2 = replacement;
      }
    }
    var fontName = fontMap[font].fontName;
    if (getCharacterMetrics(text2, fontName, mode)) {
      return fontMap[font].variant;
    }
    return null;
  };
  function isNumberPunctuation(group) {
    if (!group) {
      return false;
    }
    if (group.type === "mi" && group.children.length === 1) {
      var child = group.children[0];
      return child instanceof TextNode && child.text === ".";
    } else if (group.type === "mo" && group.children.length === 1 && group.getAttribute("separator") === "true" && group.getAttribute("lspace") === "0em" && group.getAttribute("rspace") === "0em") {
      var _child = group.children[0];
      return _child instanceof TextNode && _child.text === ",";
    } else {
      return false;
    }
  }
  var buildExpression2 = function buildExpression3(expression, options, isOrdgroup) {
    if (expression.length === 1) {
      var group = buildGroup2(expression[0], options);
      if (isOrdgroup && group instanceof MathNode && group.type === "mo") {
        group.setAttribute("lspace", "0em");
        group.setAttribute("rspace", "0em");
      }
      return [group];
    }
    var groups = [];
    var lastGroup;
    for (var i3 = 0; i3 < expression.length; i3++) {
      var _group = buildGroup2(expression[i3], options);
      if (_group instanceof MathNode && lastGroup instanceof MathNode) {
        if (_group.type === "mtext" && lastGroup.type === "mtext" && _group.getAttribute("mathvariant") === lastGroup.getAttribute("mathvariant")) {
          lastGroup.children.push(..._group.children);
          continue;
        } else if (_group.type === "mn" && lastGroup.type === "mn") {
          lastGroup.children.push(..._group.children);
          continue;
        } else if (isNumberPunctuation(_group) && lastGroup.type === "mn") {
          lastGroup.children.push(..._group.children);
          continue;
        } else if (_group.type === "mn" && isNumberPunctuation(lastGroup)) {
          _group.children = [...lastGroup.children, ..._group.children];
          groups.pop();
        } else if ((_group.type === "msup" || _group.type === "msub") && _group.children.length >= 1 && (lastGroup.type === "mn" || isNumberPunctuation(lastGroup))) {
          var base = _group.children[0];
          if (base instanceof MathNode && base.type === "mn") {
            base.children = [...lastGroup.children, ...base.children];
            groups.pop();
          }
        } else if (lastGroup.type === "mi" && lastGroup.children.length === 1) {
          var lastChild = lastGroup.children[0];
          if (lastChild instanceof TextNode && lastChild.text === "\u0338" && (_group.type === "mo" || _group.type === "mi" || _group.type === "mn")) {
            var child = _group.children[0];
            if (child instanceof TextNode && child.text.length > 0) {
              child.text = child.text.slice(0, 1) + "\u0338" + child.text.slice(1);
              groups.pop();
            }
          }
        }
      }
      groups.push(_group);
      lastGroup = _group;
    }
    return groups;
  };
  var buildExpressionRow = function buildExpressionRow2(expression, options, isOrdgroup) {
    return makeRow(buildExpression2(expression, options, isOrdgroup));
  };
  var buildGroup2 = function buildGroup3(group, options) {
    if (!group) {
      return new MathNode("mrow");
    }
    if (_mathmlGroupBuilders[group.type]) {
      return _mathmlGroupBuilders[group.type](group, options);
    } else {
      throw new ParseError("Got group of unknown type: '" + group.type + "'");
    }
  };
  function buildMathML(tree, texExpression, options, isDisplayMode, forMathmlOnly) {
    var expression = buildExpression2(tree, options);
    var wrapper;
    if (expression.length === 1 && expression[0] instanceof MathNode && rowLikeTypes.has(expression[0].type)) {
      wrapper = expression[0];
    } else {
      wrapper = new MathNode("mrow", expression);
    }
    var annotation = new MathNode("annotation", [new TextNode(texExpression)]);
    annotation.setAttribute("encoding", "application/x-tex");
    var semantics = new MathNode("semantics", [wrapper, annotation]);
    var math2 = new MathNode("math", [semantics]);
    math2.setAttribute("xmlns", "http://www.w3.org/1998/Math/MathML");
    if (isDisplayMode) {
      math2.setAttribute("display", "block");
    }
    var wrapperClass = forMathmlOnly ? "katex" : "katex-mathml";
    return makeSpan([wrapperClass], [math2]);
  }
  var sizeStyleMap = [
    // Each element contains [textsize, scriptsize, scriptscriptsize].
    // The size mappings are taken from TeX with \normalsize=10pt.
    [1, 1, 1],
    // size1: [5, 5, 5]              \tiny
    [2, 1, 1],
    // size2: [6, 5, 5]
    [3, 1, 1],
    // size3: [7, 5, 5]              \scriptsize
    [4, 2, 1],
    // size4: [8, 6, 5]              \footnotesize
    [5, 2, 1],
    // size5: [9, 6, 5]              \small
    [6, 3, 1],
    // size6: [10, 7, 5]             \normalsize
    [7, 4, 2],
    // size7: [12, 8, 6]             \large
    [8, 6, 3],
    // size8: [14.4, 10, 7]          \Large
    [9, 7, 6],
    // size9: [17.28, 12, 10]        \LARGE
    [10, 8, 7],
    // size10: [20.74, 14.4, 12]     \huge
    [11, 10, 9]
    // size11: [24.88, 20.74, 17.28] \HUGE
  ];
  var sizeMultipliers = [
    // fontMetrics.js:getGlobalMetrics also uses size indexes, so if
    // you change size indexes, change that function.
    0.5,
    0.6,
    0.7,
    0.8,
    0.9,
    1,
    1.2,
    1.44,
    1.728,
    2.074,
    2.488
  ];
  var sizeAtStyle = function sizeAtStyle2(size, style) {
    return style.size < 2 ? size : sizeStyleMap[size - 1][style.size - 1];
  };
  var Options = class _Options {
    constructor(data) {
      this.style = void 0;
      this.color = void 0;
      this.size = void 0;
      this.textSize = void 0;
      this.phantom = void 0;
      this.font = void 0;
      this.fontFamily = void 0;
      this.fontWeight = void 0;
      this.fontShape = void 0;
      this.sizeMultiplier = void 0;
      this.maxSize = void 0;
      this.minRuleThickness = void 0;
      this._fontMetrics = void 0;
      this.style = data.style;
      this.color = data.color;
      this.size = data.size || _Options.BASESIZE;
      this.textSize = data.textSize || this.size;
      this.phantom = !!data.phantom;
      this.font = data.font || "";
      this.fontFamily = data.fontFamily || "";
      this.fontWeight = data.fontWeight || "";
      this.fontShape = data.fontShape || "";
      this.sizeMultiplier = sizeMultipliers[this.size - 1];
      this.maxSize = data.maxSize;
      this.minRuleThickness = data.minRuleThickness;
      this._fontMetrics = void 0;
    }
    /**
     * Returns a new options object with the same properties as "this".  Properties
     * from "extension" will be copied to the new options object.
     */
    extend(extension) {
      var data = {
        style: this.style,
        size: this.size,
        textSize: this.textSize,
        color: this.color,
        phantom: this.phantom,
        font: this.font,
        fontFamily: this.fontFamily,
        fontWeight: this.fontWeight,
        fontShape: this.fontShape,
        maxSize: this.maxSize,
        minRuleThickness: this.minRuleThickness
      };
      Object.assign(data, extension);
      return new _Options(data);
    }
    /**
     * Return an options object with the given style. If `this.style === style`,
     * returns `this`.
     */
    havingStyle(style) {
      if (this.style === style) {
        return this;
      } else {
        return this.extend({
          style,
          size: sizeAtStyle(this.textSize, style)
        });
      }
    }
    /**
     * Return an options object with a cramped version of the current style. If
     * the current style is cramped, returns `this`.
     */
    havingCrampedStyle() {
      return this.havingStyle(this.style.cramp());
    }
    /**
     * Return an options object with the given size and in at least `\textstyle`.
     * Returns `this` if appropriate.
     */
    havingSize(size) {
      if (this.size === size && this.textSize === size) {
        return this;
      } else {
        return this.extend({
          style: this.style.text(),
          size,
          textSize: size,
          sizeMultiplier: sizeMultipliers[size - 1]
        });
      }
    }
    /**
     * Like `this.havingSize(BASESIZE).havingStyle(style)`. If `style` is omitted,
     * changes to at least `\textstyle`.
     */
    havingBaseStyle(style) {
      style = style || this.style.text();
      var wantSize = sizeAtStyle(_Options.BASESIZE, style);
      if (this.size === wantSize && this.textSize === _Options.BASESIZE && this.style === style) {
        return this;
      } else {
        return this.extend({
          style,
          size: wantSize
        });
      }
    }
    /**
     * Remove the effect of sizing changes such as \Huge.
     * Keep the effect of the current style, such as \scriptstyle.
     */
    havingBaseSizing() {
      var size;
      switch (this.style.id) {
        case 4:
        case 5:
          size = 3;
          break;
        case 6:
        case 7:
          size = 1;
          break;
        default:
          size = 6;
      }
      return this.extend({
        style: this.style.text(),
        size
      });
    }
    /**
     * Create a new options object with the given color.
     */
    withColor(color) {
      return this.extend({
        color
      });
    }
    /**
     * Create a new options object with "phantom" set to true.
     */
    withPhantom() {
      return this.extend({
        phantom: true
      });
    }
    /**
     * Creates a new options object with the given math font or old text font.
     * @type {[type]}
     */
    withFont(font) {
      return this.extend({
        font
      });
    }
    /**
     * Create a new options objects with the given fontFamily.
     */
    withTextFontFamily(fontFamily) {
      return this.extend({
        fontFamily,
        font: ""
      });
    }
    /**
     * Creates a new options object with the given font weight
     */
    withTextFontWeight(fontWeight) {
      return this.extend({
        fontWeight,
        font: ""
      });
    }
    /**
     * Creates a new options object with the given font weight
     */
    withTextFontShape(fontShape) {
      return this.extend({
        fontShape,
        font: ""
      });
    }
    /**
     * Return the CSS sizing classes required to switch from enclosing options
     * `oldOptions` to `this`. Returns an array of classes.
     */
    sizingClasses(oldOptions) {
      if (oldOptions.size !== this.size) {
        return ["sizing", "reset-size" + oldOptions.size, "size" + this.size];
      } else {
        return [];
      }
    }
    /**
     * Return the CSS sizing classes required to switch to the base size. Like
     * `this.havingSize(BASESIZE).sizingClasses(this)`.
     */
    baseSizingClasses() {
      if (this.size !== _Options.BASESIZE) {
        return ["sizing", "reset-size" + this.size, "size" + _Options.BASESIZE];
      } else {
        return [];
      }
    }
    /**
     * Return the font metrics for this size.
     */
    fontMetrics() {
      if (!this._fontMetrics) {
        this._fontMetrics = getGlobalMetrics(this.size);
      }
      return this._fontMetrics;
    }
    /**
     * Gets the CSS color of the current options object
     */
    getColor() {
      if (this.phantom) {
        return "transparent";
      } else {
        return this.color;
      }
    }
  };
  Options.BASESIZE = 6;
  var optionsFromSettings = function optionsFromSettings2(settings) {
    return new Options({
      style: settings.displayMode ? Style$1.DISPLAY : Style$1.TEXT,
      maxSize: settings.maxSize,
      minRuleThickness: settings.minRuleThickness
    });
  };
  var displayWrap = function displayWrap2(node, settings) {
    if (settings.displayMode) {
      var classes = ["katex-display"];
      if (settings.leqno) {
        classes.push("leqno");
      }
      if (settings.fleqn) {
        classes.push("fleqn");
      }
      node = makeSpan(classes, [node]);
    }
    return node;
  };
  var buildTree = function buildTree2(tree, expression, settings) {
    var options = optionsFromSettings(settings);
    var katexNode;
    if (settings.output === "mathml") {
      return buildMathML(tree, expression, options, settings.displayMode, true);
    } else if (settings.output === "html") {
      var htmlNode = buildHTML(tree, options);
      katexNode = makeSpan(["katex"], [htmlNode]);
    } else {
      var mathMLNode = buildMathML(tree, expression, options, settings.displayMode, false);
      var _htmlNode = buildHTML(tree, options);
      katexNode = makeSpan(["katex"], [mathMLNode, _htmlNode]);
    }
    return displayWrap(katexNode, settings);
  };
  var buildHTMLTree = function buildHTMLTree2(tree, expression, settings) {
    var options = optionsFromSettings(settings);
    var htmlNode = buildHTML(tree, options);
    var katexNode = makeSpan(["katex"], [htmlNode]);
    return displayWrap(katexNode, settings);
  };
  var stretchyCodePoint = {
    widehat: "^",
    widecheck: "\u02C7",
    widetilde: "~",
    utilde: "~",
    overleftarrow: "\u2190",
    underleftarrow: "\u2190",
    xleftarrow: "\u2190",
    overrightarrow: "\u2192",
    underrightarrow: "\u2192",
    xrightarrow: "\u2192",
    underbrace: "\u23DF",
    overbrace: "\u23DE",
    underbracket: "\u23B5",
    overbracket: "\u23B4",
    overgroup: "\u23E0",
    undergroup: "\u23E1",
    overleftrightarrow: "\u2194",
    underleftrightarrow: "\u2194",
    xleftrightarrow: "\u2194",
    Overrightarrow: "\u21D2",
    xRightarrow: "\u21D2",
    overleftharpoon: "\u21BC",
    xleftharpoonup: "\u21BC",
    overrightharpoon: "\u21C0",
    xrightharpoonup: "\u21C0",
    xLeftarrow: "\u21D0",
    xLeftrightarrow: "\u21D4",
    xhookleftarrow: "\u21A9",
    xhookrightarrow: "\u21AA",
    xmapsto: "\u21A6",
    xrightharpoondown: "\u21C1",
    xleftharpoondown: "\u21BD",
    xrightleftharpoons: "\u21CC",
    xleftrightharpoons: "\u21CB",
    xtwoheadleftarrow: "\u219E",
    xtwoheadrightarrow: "\u21A0",
    xlongequal: "=",
    xtofrom: "\u21C4",
    xrightleftarrows: "\u21C4",
    xrightequilibrium: "\u21CC",
    // Not a perfect match.
    xleftequilibrium: "\u21CB",
    // None better available.
    "\\cdrightarrow": "\u2192",
    "\\cdleftarrow": "\u2190",
    "\\cdlongequal": "="
  };
  var stretchyMathML = function stretchyMathML2(label) {
    var node = new MathNode("mo", [new TextNode(stretchyCodePoint[label.replace(/^\\/, "")])]);
    node.setAttribute("stretchy", "true");
    return node;
  };
  var katexImagesData = {
    //   path(s), minWidth, height, align
    overrightarrow: [["rightarrow"], 0.888, 522, "xMaxYMin"],
    overleftarrow: [["leftarrow"], 0.888, 522, "xMinYMin"],
    underrightarrow: [["rightarrow"], 0.888, 522, "xMaxYMin"],
    underleftarrow: [["leftarrow"], 0.888, 522, "xMinYMin"],
    xrightarrow: [["rightarrow"], 1.469, 522, "xMaxYMin"],
    "\\cdrightarrow": [["rightarrow"], 3, 522, "xMaxYMin"],
    // CD minwwidth2.5pc
    xleftarrow: [["leftarrow"], 1.469, 522, "xMinYMin"],
    "\\cdleftarrow": [["leftarrow"], 3, 522, "xMinYMin"],
    Overrightarrow: [["doublerightarrow"], 0.888, 560, "xMaxYMin"],
    xRightarrow: [["doublerightarrow"], 1.526, 560, "xMaxYMin"],
    xLeftarrow: [["doubleleftarrow"], 1.526, 560, "xMinYMin"],
    overleftharpoon: [["leftharpoon"], 0.888, 522, "xMinYMin"],
    xleftharpoonup: [["leftharpoon"], 0.888, 522, "xMinYMin"],
    xleftharpoondown: [["leftharpoondown"], 0.888, 522, "xMinYMin"],
    overrightharpoon: [["rightharpoon"], 0.888, 522, "xMaxYMin"],
    xrightharpoonup: [["rightharpoon"], 0.888, 522, "xMaxYMin"],
    xrightharpoondown: [["rightharpoondown"], 0.888, 522, "xMaxYMin"],
    xlongequal: [["longequal"], 0.888, 334, "xMinYMin"],
    "\\cdlongequal": [["longequal"], 3, 334, "xMinYMin"],
    xtwoheadleftarrow: [["twoheadleftarrow"], 0.888, 334, "xMinYMin"],
    xtwoheadrightarrow: [["twoheadrightarrow"], 0.888, 334, "xMaxYMin"],
    overleftrightarrow: [["leftarrow", "rightarrow"], 0.888, 522],
    overbrace: [["leftbrace", "midbrace", "rightbrace"], 1.6, 548],
    underbrace: [["leftbraceunder", "midbraceunder", "rightbraceunder"], 1.6, 548],
    underleftrightarrow: [["leftarrow", "rightarrow"], 0.888, 522],
    xleftrightarrow: [["leftarrow", "rightarrow"], 1.75, 522],
    xLeftrightarrow: [["doubleleftarrow", "doublerightarrow"], 1.75, 560],
    xrightleftharpoons: [["leftharpoondownplus", "rightharpoonplus"], 1.75, 716],
    xleftrightharpoons: [["leftharpoonplus", "rightharpoondownplus"], 1.75, 716],
    xhookleftarrow: [["leftarrow", "righthook"], 1.08, 522],
    xhookrightarrow: [["lefthook", "rightarrow"], 1.08, 522],
    overlinesegment: [["leftlinesegment", "rightlinesegment"], 0.888, 522],
    underlinesegment: [["leftlinesegment", "rightlinesegment"], 0.888, 522],
    overbracket: [["leftbracketover", "rightbracketover"], 1.6, 440],
    underbracket: [["leftbracketunder", "rightbracketunder"], 1.6, 410],
    overgroup: [["leftgroup", "rightgroup"], 0.888, 342],
    undergroup: [["leftgroupunder", "rightgroupunder"], 0.888, 342],
    xmapsto: [["leftmapsto", "rightarrow"], 1.5, 522],
    xtofrom: [["leftToFrom", "rightToFrom"], 1.75, 528],
    // The next three arrows are from the mhchem package.
    // In mhchem.sty, min-length is 2.0em. But these arrows might appear in the
    // document as \xrightarrow or \xrightleftharpoons. Those have
    // min-length = 1.75em, so we set min-length on these next three to match.
    xrightleftarrows: [["baraboveleftarrow", "rightarrowabovebar"], 1.75, 901],
    xrightequilibrium: [["baraboveshortleftharpoon", "rightharpoonaboveshortbar"], 1.75, 716],
    xleftequilibrium: [["shortbaraboveleftharpoon", "shortrightharpoonabovebar"], 1.75, 716]
  };
  var wideAccentLabels = /* @__PURE__ */ new Set(["widehat", "widecheck", "widetilde", "utilde"]);
  var stretchySvg = function stretchySvg2(group, options) {
    function buildSvgSpan_() {
      var viewBoxWidth = 4e5;
      var label = group.label.slice(1);
      if (wideAccentLabels.has(label) && "base" in group) {
        var numChars = group.base.type === "ordgroup" ? group.base.body.length : 1;
        var viewBoxHeight;
        var pathName;
        var _height;
        if (numChars > 5) {
          if (label === "widehat" || label === "widecheck") {
            viewBoxHeight = 420;
            viewBoxWidth = 2364;
            _height = 0.42;
            pathName = label + "4";
          } else {
            viewBoxHeight = 312;
            viewBoxWidth = 2340;
            _height = 0.34;
            pathName = "tilde4";
          }
        } else {
          var imgIndex = [1, 1, 2, 2, 3, 3][numChars];
          if (label === "widehat" || label === "widecheck") {
            viewBoxWidth = [0, 1062, 2364, 2364, 2364][imgIndex];
            viewBoxHeight = [0, 239, 300, 360, 420][imgIndex];
            _height = [0, 0.24, 0.3, 0.3, 0.36, 0.42][imgIndex];
            pathName = label + imgIndex;
          } else {
            viewBoxWidth = [0, 600, 1033, 2339, 2340][imgIndex];
            viewBoxHeight = [0, 260, 286, 306, 312][imgIndex];
            _height = [0, 0.26, 0.286, 0.3, 0.306, 0.34][imgIndex];
            pathName = "tilde" + imgIndex;
          }
        }
        var path2 = new PathNode(pathName);
        var svgNode = new SvgNode([path2], {
          "width": "100%",
          "height": makeEm(_height),
          "viewBox": "0 0 " + viewBoxWidth + " " + viewBoxHeight,
          "preserveAspectRatio": "none"
        });
        return {
          span: makeSvgSpan([], [svgNode], options),
          minWidth: 0,
          height: _height
        };
      } else {
        var spans = [];
        var data = katexImagesData[label];
        if (!data) {
          throw new Error('No SVG data for "' + label + '".');
        }
        var [paths, _minWidth, _viewBoxHeight] = data;
        var _height2 = _viewBoxHeight / 1e3;
        var numSvgChildren = paths.length;
        var widthClasses;
        var aligns;
        if (numSvgChildren === 1) {
          if (data.length !== 4) {
            throw new Error('Expected 4-tuple for single-path SVG data "' + label + '".');
          }
          widthClasses = ["hide-tail"];
          aligns = [data[3]];
        } else if (numSvgChildren === 2) {
          widthClasses = ["halfarrow-left", "halfarrow-right"];
          aligns = ["xMinYMin", "xMaxYMin"];
        } else if (numSvgChildren === 3) {
          widthClasses = ["brace-left", "brace-center", "brace-right"];
          aligns = ["xMinYMin", "xMidYMin", "xMaxYMin"];
        } else {
          throw new Error("Correct katexImagesData or update code here to support\n                    " + numSvgChildren + " children.");
        }
        for (var i3 = 0; i3 < numSvgChildren; i3++) {
          var _path = new PathNode(paths[i3]);
          var _svgNode = new SvgNode([_path], {
            "width": "400em",
            "height": makeEm(_height2),
            "viewBox": "0 0 " + viewBoxWidth + " " + _viewBoxHeight,
            "preserveAspectRatio": aligns[i3] + " slice"
          });
          var _span = makeSvgSpan([widthClasses[i3]], [_svgNode], options);
          if (numSvgChildren === 1) {
            return {
              span: _span,
              minWidth: _minWidth,
              height: _height2
            };
          } else {
            _span.style.height = makeEm(_height2);
            spans.push(_span);
          }
        }
        return {
          span: makeSpan(["stretchy"], spans, options),
          minWidth: _minWidth,
          height: _height2
        };
      }
    }
    var {
      span,
      minWidth,
      height
    } = buildSvgSpan_();
    span.height = height;
    span.style.height = makeEm(height);
    if (minWidth > 0) {
      span.style.minWidth = makeEm(minWidth);
    }
    return span;
  };
  var stretchyEnclose = function stretchyEnclose2(inner2, label, topPad, bottomPad, options) {
    var img;
    var totalHeight = inner2.height + inner2.depth + topPad + bottomPad;
    if (/fbox|color|angl/.test(label)) {
      img = makeSpan(["stretchy", label], [], options);
      if (label === "fbox") {
        var color = options.color && options.getColor();
        if (color) {
          img.style.borderColor = color;
        }
      }
    } else {
      var lines = [];
      if (/^[bx]cancel$/.test(label)) {
        lines.push(new LineNode({
          "x1": "0",
          "y1": "0",
          "x2": "100%",
          "y2": "100%",
          "stroke-width": "0.046em"
        }));
      }
      if (/^x?cancel$/.test(label)) {
        lines.push(new LineNode({
          "x1": "0",
          "y1": "100%",
          "x2": "100%",
          "y2": "0",
          "stroke-width": "0.046em"
        }));
      }
      var svgNode = new SvgNode(lines, {
        "width": "100%",
        "height": makeEm(totalHeight)
      });
      img = makeSvgSpan([], [svgNode], options);
    }
    img.height = totalHeight;
    img.style.height = makeEm(totalHeight);
    return img;
  };
  var ATOMS = {
    "bin": 1,
    "close": 1,
    "inner": 1,
    "open": 1,
    "punct": 1,
    "rel": 1
  };
  var NON_ATOMS = {
    "accent-token": 1,
    "mathord": 1,
    "op-token": 1,
    "spacing": 1,
    "textord": 1
  };
  function isAtom(value) {
    return value in ATOMS;
  }
  function assertNodeType(node, type) {
    if (!node || node.type !== type) {
      throw new Error("Expected node of type " + type + ", but got " + (node ? "node of type " + node.type : String(node)));
    }
    return node;
  }
  function assertSymbolNodeType(node) {
    var typedNode = checkSymbolNodeType(node);
    if (!typedNode) {
      throw new Error("Expected node of symbol group type, but got " + (node ? "node of type " + node.type : String(node)));
    }
    return typedNode;
  }
  function checkSymbolNodeType(node) {
    if (node && (node.type === "atom" || NON_ATOMS.hasOwnProperty(node.type))) {
      return node;
    }
    return null;
  }
  var getBaseSymbol = (group) => {
    if (group instanceof SymbolNode) {
      return group;
    }
    if (hasHtmlDomChildren(group) && group.children.length === 1) {
      return getBaseSymbol(group.children[0]);
    }
  };
  var htmlBuilder$a = (grp, options) => {
    var base;
    var group;
    var supSubGroup;
    if (grp && grp.type === "supsub") {
      group = assertNodeType(grp.base, "accent");
      base = group.base;
      grp.base = base;
      supSubGroup = assertSpan(buildGroup$1(grp, options));
      grp.base = group;
    } else {
      group = assertNodeType(grp, "accent");
      base = group.base;
    }
    var body = buildGroup$1(base, options.havingCrampedStyle());
    var mustShift = group.isShifty && isCharacterBox(base);
    var skew = 0;
    if (mustShift) {
      var _getBaseSymbol$skew, _getBaseSymbol;
      skew = (_getBaseSymbol$skew = (_getBaseSymbol = getBaseSymbol(body)) == null ? void 0 : _getBaseSymbol.skew) != null ? _getBaseSymbol$skew : 0;
    }
    var accentBelow = group.label === "\\c";
    var clearance = accentBelow ? body.height + body.depth : Math.min(body.height, options.fontMetrics().xHeight);
    var accentBody;
    if (!group.isStretchy) {
      var accent2;
      var width;
      if (group.label === "\\vec") {
        accent2 = staticSvg("vec", options);
        width = svgData.vec[1];
      } else {
        accent2 = makeOrd({
          type: "textord",
          mode: group.mode,
          text: group.label
        }, options, "textord");
        accent2 = assertSymbolDomNode(accent2);
        accent2.italic = 0;
        width = accent2.width;
        if (accentBelow) {
          clearance += accent2.depth;
        }
      }
      accentBody = makeSpan(["accent-body"], [accent2]);
      var accentFull = group.label === "\\textcircled";
      if (accentFull) {
        accentBody.classes.push("accent-full");
        clearance = body.height;
      }
      var left = skew;
      if (!accentFull) {
        left -= width / 2;
      }
      accentBody.style.left = makeEm(left);
      if (group.label === "\\textcircled") {
        accentBody.style.top = ".2em";
      }
      accentBody = makeVList({
        positionType: "firstBaseline",
        children: [{
          type: "elem",
          elem: body
        }, {
          type: "kern",
          size: -clearance
        }, {
          type: "elem",
          elem: accentBody
        }]
      });
    } else {
      accentBody = stretchySvg(group, options);
      accentBody = makeVList({
        positionType: "firstBaseline",
        children: [{
          type: "elem",
          elem: body
        }, {
          type: "elem",
          elem: accentBody,
          wrapperClasses: ["svg-align"],
          wrapperStyle: skew > 0 ? {
            width: "calc(100% - " + makeEm(2 * skew) + ")",
            marginLeft: makeEm(2 * skew)
          } : void 0
        }]
      });
    }
    var accentWrap = makeSpan(["mord", "accent"], [accentBody], options);
    if (supSubGroup) {
      supSubGroup.children[0] = accentWrap;
      supSubGroup.height = Math.max(accentWrap.height, supSubGroup.height);
      supSubGroup.classes[0] = "mord";
      return supSubGroup;
    } else {
      return accentWrap;
    }
  };
  var mathmlBuilder$9 = (group, options) => {
    var accentNode = group.isStretchy ? stretchyMathML(group.label) : new MathNode("mo", [makeText(group.label, group.mode)]);
    var node = new MathNode("mover", [buildGroup2(group.base, options), accentNode]);
    node.setAttribute("accent", "true");
    return node;
  };
  var NON_STRETCHY_ACCENT_REGEX = new RegExp(["\\acute", "\\grave", "\\ddot", "\\tilde", "\\bar", "\\breve", "\\check", "\\hat", "\\vec", "\\dot", "\\mathring"].map((accent2) => "\\" + accent2).join("|"));
  defineFunction({
    type: "accent",
    names: ["\\acute", "\\grave", "\\ddot", "\\tilde", "\\bar", "\\breve", "\\check", "\\hat", "\\vec", "\\dot", "\\mathring", "\\widecheck", "\\widehat", "\\widetilde", "\\overrightarrow", "\\overleftarrow", "\\Overrightarrow", "\\overleftrightarrow", "\\overgroup", "\\overlinesegment", "\\overleftharpoon", "\\overrightharpoon"],
    props: {
      numArgs: 1
    },
    handler: (context, args) => {
      var base = normalizeArgument(args[0]);
      var isStretchy = !NON_STRETCHY_ACCENT_REGEX.test(context.funcName);
      var isShifty = !isStretchy || context.funcName === "\\widehat" || context.funcName === "\\widetilde" || context.funcName === "\\widecheck";
      return {
        type: "accent",
        mode: context.parser.mode,
        label: context.funcName,
        isStretchy,
        isShifty,
        base
      };
    },
    htmlBuilder: htmlBuilder$a,
    mathmlBuilder: mathmlBuilder$9
  });
  defineFunction({
    type: "accent",
    names: ["\\'", "\\`", "\\^", "\\~", "\\=", "\\u", "\\.", '\\"', "\\c", "\\r", "\\H", "\\v", "\\textcircled"],
    props: {
      numArgs: 1,
      allowedInText: true,
      allowedInMath: true,
      // unless in strict mode
      argTypes: ["primitive"]
    },
    handler: (context, args) => {
      var base = args[0];
      var mode = context.parser.mode;
      if (mode === "math") {
        context.parser.settings.reportNonstrict("mathVsTextAccents", "LaTeX's accent " + context.funcName + " works only in text mode");
        mode = "text";
      }
      return {
        type: "accent",
        mode,
        label: context.funcName,
        isStretchy: false,
        isShifty: true,
        base
      };
    },
    htmlBuilder: htmlBuilder$a,
    mathmlBuilder: mathmlBuilder$9
  });
  defineFunction({
    type: "accentUnder",
    names: ["\\underleftarrow", "\\underrightarrow", "\\underleftrightarrow", "\\undergroup", "\\underlinesegment", "\\utilde"],
    props: {
      numArgs: 1
    },
    handler: (_ref, args) => {
      var {
        parser,
        funcName
      } = _ref;
      var base = args[0];
      return {
        type: "accentUnder",
        mode: parser.mode,
        label: funcName,
        base
      };
    },
    htmlBuilder: (group, options) => {
      var innerGroup = buildGroup$1(group.base, options);
      var accentBody = stretchySvg(group, options);
      var kern = group.label === "\\utilde" ? 0.12 : 0;
      var vlist = makeVList({
        positionType: "top",
        positionData: innerGroup.height,
        children: [{
          type: "elem",
          elem: accentBody,
          wrapperClasses: ["svg-align"]
        }, {
          type: "kern",
          size: kern
        }, {
          type: "elem",
          elem: innerGroup
        }]
      });
      return makeSpan(["mord", "accentunder"], [vlist], options);
    },
    mathmlBuilder: (group, options) => {
      var accentNode = stretchyMathML(group.label);
      var node = new MathNode("munder", [buildGroup2(group.base, options), accentNode]);
      node.setAttribute("accentunder", "true");
      return node;
    }
  });
  var paddedNode = (group) => {
    var node = new MathNode("mpadded", group ? [group] : []);
    node.setAttribute("width", "+0.6em");
    node.setAttribute("lspace", "0.3em");
    return node;
  };
  defineFunction({
    type: "xArrow",
    names: [
      "\\xleftarrow",
      "\\xrightarrow",
      "\\xLeftarrow",
      "\\xRightarrow",
      "\\xleftrightarrow",
      "\\xLeftrightarrow",
      "\\xhookleftarrow",
      "\\xhookrightarrow",
      "\\xmapsto",
      "\\xrightharpoondown",
      "\\xrightharpoonup",
      "\\xleftharpoondown",
      "\\xleftharpoonup",
      "\\xrightleftharpoons",
      "\\xleftrightharpoons",
      "\\xlongequal",
      "\\xtwoheadrightarrow",
      "\\xtwoheadleftarrow",
      "\\xtofrom",
      // The next 3 functions are here to support the mhchem extension.
      // Direct use of these functions is discouraged and may break someday.
      "\\xrightleftarrows",
      "\\xrightequilibrium",
      "\\xleftequilibrium",
      // The next 3 functions are here only to support the {CD} environment.
      "\\\\cdrightarrow",
      "\\\\cdleftarrow",
      "\\\\cdlongequal"
    ],
    props: {
      numArgs: 1,
      numOptionalArgs: 1
    },
    handler(_ref, args, optArgs) {
      var {
        parser,
        funcName
      } = _ref;
      return {
        type: "xArrow",
        mode: parser.mode,
        label: funcName,
        body: args[0],
        below: optArgs[0]
      };
    },
    htmlBuilder(group, options) {
      var style = options.style;
      var newOptions = options.havingStyle(style.sup());
      var upperGroup = wrapFragment(buildGroup$1(group.body, newOptions, options), options);
      var arrowPrefix = group.label.slice(0, 2) === "\\x" ? "x" : "cd";
      upperGroup.classes.push(arrowPrefix + "-arrow-pad");
      var lowerGroup;
      if (group.below) {
        newOptions = options.havingStyle(style.sub());
        lowerGroup = wrapFragment(buildGroup$1(group.below, newOptions, options), options);
        lowerGroup.classes.push(arrowPrefix + "-arrow-pad");
      }
      var arrowBody = stretchySvg(group, options);
      var arrowShift = -options.fontMetrics().axisHeight + 0.5 * arrowBody.height;
      var upperShift = -options.fontMetrics().axisHeight - 0.5 * arrowBody.height - 0.111;
      if (upperGroup.depth > 0.25 || group.label === "\\xleftequilibrium") {
        upperShift -= upperGroup.depth;
      }
      var vlist;
      if (lowerGroup) {
        var lowerShift = -options.fontMetrics().axisHeight + lowerGroup.height + 0.5 * arrowBody.height + 0.111;
        vlist = makeVList({
          positionType: "individualShift",
          children: [{
            type: "elem",
            elem: upperGroup,
            shift: upperShift
          }, {
            type: "elem",
            elem: arrowBody,
            shift: arrowShift,
            wrapperClasses: ["svg-align"]
          }, {
            type: "elem",
            elem: lowerGroup,
            shift: lowerShift
          }]
        });
      } else {
        vlist = makeVList({
          positionType: "individualShift",
          children: [{
            type: "elem",
            elem: upperGroup,
            shift: upperShift
          }, {
            type: "elem",
            elem: arrowBody,
            shift: arrowShift,
            wrapperClasses: ["svg-align"]
          }]
        });
      }
      return makeSpan(["mrel", "x-arrow"], [vlist], options);
    },
    mathmlBuilder(group, options) {
      var arrowNode = stretchyMathML(group.label);
      arrowNode.setAttribute("minsize", group.label.charAt(0) === "x" ? "1.75em" : "3.0em");
      var node;
      if (group.body) {
        var upperNode = paddedNode(buildGroup2(group.body, options));
        if (group.below) {
          var lowerNode = paddedNode(buildGroup2(group.below, options));
          node = new MathNode("munderover", [arrowNode, lowerNode, upperNode]);
        } else {
          node = new MathNode("mover", [arrowNode, upperNode]);
        }
      } else if (group.below) {
        var _lowerNode = paddedNode(buildGroup2(group.below, options));
        node = new MathNode("munder", [arrowNode, _lowerNode]);
      } else {
        node = paddedNode();
        node = new MathNode("mover", [arrowNode, node]);
      }
      return node;
    }
  });
  function htmlBuilder$9(group, options) {
    var elements = buildExpression$1(group.body, options, true);
    return makeSpan([group.mclass], elements, options);
  }
  function mathmlBuilder$8(group, options) {
    var node;
    var inner2 = buildExpression2(group.body, options);
    if (group.mclass === "minner") {
      node = new MathNode("mpadded", inner2);
    } else if (group.mclass === "mord") {
      if (group.isCharacterBox) {
        node = inner2[0];
        node.type = "mi";
      } else {
        node = new MathNode("mi", inner2);
      }
    } else {
      if (group.isCharacterBox) {
        node = inner2[0];
        node.type = "mo";
      } else {
        node = new MathNode("mo", inner2);
      }
      if (group.mclass === "mbin") {
        node.attributes.lspace = "0.22em";
        node.attributes.rspace = "0.22em";
      } else if (group.mclass === "mpunct") {
        node.attributes.lspace = "0em";
        node.attributes.rspace = "0.17em";
      } else if (group.mclass === "mopen" || group.mclass === "mclose") {
        node.attributes.lspace = "0em";
        node.attributes.rspace = "0em";
      } else if (group.mclass === "minner") {
        node.attributes.lspace = "0.0556em";
        node.attributes.width = "+0.1111em";
      }
    }
    return node;
  }
  defineFunction({
    type: "mclass",
    names: ["\\mathord", "\\mathbin", "\\mathrel", "\\mathopen", "\\mathclose", "\\mathpunct", "\\mathinner"],
    props: {
      numArgs: 1,
      primitive: true
    },
    handler(_ref, args) {
      var {
        parser,
        funcName
      } = _ref;
      var body = args[0];
      return {
        type: "mclass",
        mode: parser.mode,
        mclass: "m" + funcName.slice(5),
        // TODO(kevinb): don't prefix with 'm'
        body: ordargument(body),
        isCharacterBox: isCharacterBox(body)
      };
    },
    htmlBuilder: htmlBuilder$9,
    mathmlBuilder: mathmlBuilder$8
  });
  var binrelClass = (arg) => {
    var atom = arg.type === "ordgroup" && arg.body.length ? arg.body[0] : arg;
    if (atom.type === "atom" && (atom.family === "bin" || atom.family === "rel")) {
      return "m" + atom.family;
    } else {
      return "mord";
    }
  };
  defineFunction({
    type: "mclass",
    names: ["\\@binrel"],
    props: {
      numArgs: 2
    },
    handler(_ref2, args) {
      var {
        parser
      } = _ref2;
      return {
        type: "mclass",
        mode: parser.mode,
        mclass: binrelClass(args[0]),
        body: ordargument(args[1]),
        isCharacterBox: isCharacterBox(args[1])
      };
    }
  });
  defineFunction({
    type: "mclass",
    names: ["\\stackrel", "\\overset", "\\underset"],
    props: {
      numArgs: 2
    },
    handler(_ref3, args) {
      var {
        parser,
        funcName
      } = _ref3;
      var baseArg = args[1];
      var shiftedArg = args[0];
      var mclass;
      if (funcName !== "\\stackrel") {
        mclass = binrelClass(baseArg);
      } else {
        mclass = "mrel";
      }
      var baseOp = {
        type: "op",
        mode: baseArg.mode,
        limits: true,
        alwaysHandleSupSub: true,
        parentIsSupSub: false,
        symbol: false,
        suppressBaseShift: funcName !== "\\stackrel",
        body: ordargument(baseArg)
      };
      var supsub = {
        type: "supsub",
        mode: shiftedArg.mode,
        base: baseOp,
        sup: funcName === "\\underset" ? null : shiftedArg,
        sub: funcName === "\\underset" ? shiftedArg : null
      };
      return {
        type: "mclass",
        mode: parser.mode,
        mclass,
        body: [supsub],
        isCharacterBox: isCharacterBox(supsub)
      };
    },
    htmlBuilder: htmlBuilder$9,
    mathmlBuilder: mathmlBuilder$8
  });
  defineFunction({
    type: "pmb",
    names: ["\\pmb"],
    props: {
      numArgs: 1,
      allowedInText: true
    },
    handler(_ref, args) {
      var {
        parser
      } = _ref;
      return {
        type: "pmb",
        mode: parser.mode,
        mclass: binrelClass(args[0]),
        body: ordargument(args[0])
      };
    },
    htmlBuilder(group, options) {
      var elements = buildExpression$1(group.body, options, true);
      var node = makeSpan([group.mclass], elements, options);
      node.style.textShadow = "0.02em 0.01em 0.04px";
      return node;
    },
    mathmlBuilder(group, style) {
      var inner2 = buildExpression2(group.body, style);
      var node = new MathNode("mstyle", inner2);
      node.setAttribute("style", "text-shadow: 0.02em 0.01em 0.04px");
      return node;
    }
  });
  var cdArrowFunctionName = {
    ">": "\\\\cdrightarrow",
    "<": "\\\\cdleftarrow",
    "=": "\\\\cdlongequal",
    "A": "\\uparrow",
    "V": "\\downarrow",
    "|": "\\Vert",
    ".": "no arrow"
  };
  var newCell = () => {
    return {
      type: "styling",
      body: [],
      mode: "math",
      style: "display",
      resetFont: true
    };
  };
  var isStartOfArrow = (node) => {
    return node.type === "textord" && node.text === "@";
  };
  var isLabelEnd = (node, endChar) => {
    return (node.type === "mathord" || node.type === "atom") && node.text === endChar;
  };
  function cdArrow(arrowChar, labels, parser) {
    var funcName = cdArrowFunctionName[arrowChar];
    switch (funcName) {
      case "\\\\cdrightarrow":
      case "\\\\cdleftarrow":
        return parser.callFunction(funcName, [labels[0]], [labels[1]]);
      case "\\uparrow":
      case "\\downarrow": {
        var leftLabel = parser.callFunction("\\\\cdleft", [labels[0]], []);
        var bareArrow = {
          type: "atom",
          text: funcName,
          mode: "math",
          family: "rel"
        };
        var sizedArrow = parser.callFunction("\\Big", [bareArrow], []);
        var rightLabel = parser.callFunction("\\\\cdright", [labels[1]], []);
        var arrowGroup = {
          type: "ordgroup",
          mode: "math",
          body: [leftLabel, sizedArrow, rightLabel]
        };
        return parser.callFunction("\\\\cdparent", [arrowGroup], []);
      }
      case "\\\\cdlongequal":
        return parser.callFunction("\\\\cdlongequal", [], []);
      case "\\Vert": {
        var arrow = {
          type: "textord",
          text: "\\Vert",
          mode: "math"
        };
        return parser.callFunction("\\Big", [arrow], []);
      }
      default:
        return {
          type: "textord",
          text: " ",
          mode: "math"
        };
    }
  }
  function parseCD(parser) {
    var parsedRows = [];
    parser.gullet.beginGroup();
    parser.gullet.macros.set("\\cr", "\\\\\\relax");
    parser.gullet.beginGroup();
    while (true) {
      parsedRows.push(parser.parseExpression(false, "\\\\"));
      parser.gullet.endGroup();
      parser.gullet.beginGroup();
      var next = parser.fetch().text;
      if (next === "&" || next === "\\\\") {
        parser.consume();
      } else if (next === "\\end") {
        if (parsedRows[parsedRows.length - 1].length === 0) {
          parsedRows.pop();
        }
        break;
      } else {
        throw new ParseError("Expected \\\\ or \\cr or \\end", parser.nextToken);
      }
    }
    var row = [];
    var body = [row];
    for (var i3 = 0; i3 < parsedRows.length; i3++) {
      var rowNodes = parsedRows[i3];
      var cell = newCell();
      for (var j2 = 0; j2 < rowNodes.length; j2++) {
        if (!isStartOfArrow(rowNodes[j2])) {
          cell.body.push(rowNodes[j2]);
        } else {
          row.push(cell);
          j2 += 1;
          var arrowChar = assertSymbolNodeType(rowNodes[j2]).text;
          var labels = new Array(2);
          labels[0] = {
            type: "ordgroup",
            mode: "math",
            body: []
          };
          labels[1] = {
            type: "ordgroup",
            mode: "math",
            body: []
          };
          if ("=|.".includes(arrowChar)) ;
          else if ("<>AV".includes(arrowChar)) {
            for (var labelNum = 0; labelNum < 2; labelNum++) {
              var inLabel = true;
              for (var k = j2 + 1; k < rowNodes.length; k++) {
                if (isLabelEnd(rowNodes[k], arrowChar)) {
                  inLabel = false;
                  j2 = k;
                  break;
                }
                if (isStartOfArrow(rowNodes[k])) {
                  throw new ParseError("Missing a " + arrowChar + " character to complete a CD arrow.", rowNodes[k]);
                }
                labels[labelNum].body.push(rowNodes[k]);
              }
              if (inLabel) {
                throw new ParseError("Missing a " + arrowChar + " character to complete a CD arrow.", rowNodes[j2]);
              }
            }
          } else {
            throw new ParseError('Expected one of "<>AV=|." after @', rowNodes[j2]);
          }
          var arrow = cdArrow(arrowChar, labels, parser);
          var wrappedArrow = {
            type: "styling",
            body: [arrow],
            mode: "math",
            style: "display",
            // CD is always displaystyle.
            resetFont: true
          };
          row.push(wrappedArrow);
          cell = newCell();
        }
      }
      if (i3 % 2 === 0) {
        row.push(cell);
      } else {
        row.shift();
      }
      row = [];
      body.push(row);
    }
    parser.gullet.endGroup();
    parser.gullet.endGroup();
    var cols = new Array(body[0].length).fill({
      type: "align",
      align: "c",
      pregap: 0.25,
      // CD package sets \enskip between columns.
      postgap: 0.25
      // So pre and post each get half an \enskip, i.e. 0.25em.
    });
    return {
      type: "array",
      mode: "math",
      body,
      arraystretch: 1,
      addJot: true,
      rowGaps: [null],
      cols,
      colSeparationType: "CD",
      hLinesBeforeRow: new Array(body.length + 1).fill([])
    };
  }
  defineFunction({
    type: "cdlabel",
    names: ["\\\\cdleft", "\\\\cdright"],
    props: {
      numArgs: 1
    },
    handler(_ref, args) {
      var {
        parser,
        funcName
      } = _ref;
      return {
        type: "cdlabel",
        mode: parser.mode,
        side: funcName.slice(4),
        label: args[0]
      };
    },
    htmlBuilder(group, options) {
      var newOptions = options.havingStyle(options.style.sup());
      var label = wrapFragment(buildGroup$1(group.label, newOptions, options), options);
      label.classes.push("cd-label-" + group.side);
      label.style.bottom = makeEm(0.8 - label.depth);
      label.height = 0;
      label.depth = 0;
      return label;
    },
    mathmlBuilder(group, options) {
      var label = new MathNode("mrow", [buildGroup2(group.label, options)]);
      label = new MathNode("mpadded", [label]);
      label.setAttribute("width", "0");
      if (group.side === "left") {
        label.setAttribute("lspace", "-1width");
      }
      label.setAttribute("voffset", "0.7em");
      label = new MathNode("mstyle", [label]);
      label.setAttribute("displaystyle", "false");
      label.setAttribute("scriptlevel", "1");
      return label;
    }
  });
  defineFunction({
    type: "cdlabelparent",
    names: ["\\\\cdparent"],
    props: {
      numArgs: 1
    },
    handler(_ref2, args) {
      var {
        parser
      } = _ref2;
      return {
        type: "cdlabelparent",
        mode: parser.mode,
        fragment: args[0]
      };
    },
    htmlBuilder(group, options) {
      var parent = wrapFragment(buildGroup$1(group.fragment, options), options);
      parent.classes.push("cd-vert-arrow");
      return parent;
    },
    mathmlBuilder(group, options) {
      return new MathNode("mrow", [buildGroup2(group.fragment, options)]);
    }
  });
  defineFunction({
    type: "textord",
    names: ["\\@char"],
    props: {
      numArgs: 1,
      allowedInText: true
    },
    handler(_ref, args) {
      var {
        parser
      } = _ref;
      var arg = assertNodeType(args[0], "ordgroup");
      var group = arg.body;
      var number = "";
      for (var i3 = 0; i3 < group.length; i3++) {
        var node = assertNodeType(group[i3], "textord");
        number += node.text;
      }
      var code2 = parseInt(number);
      var text2;
      if (isNaN(code2)) {
        throw new ParseError("\\@char has non-numeric argument " + number);
      } else if (code2 < 0 || code2 >= 1114111) {
        throw new ParseError("\\@char with invalid code point " + number);
      } else if (code2 <= 65535) {
        text2 = String.fromCharCode(code2);
      } else {
        code2 -= 65536;
        text2 = String.fromCharCode((code2 >> 10) + 55296, (code2 & 1023) + 56320);
      }
      return {
        type: "textord",
        mode: parser.mode,
        text: text2
      };
    }
  });
  var htmlBuilder$8 = (group, options) => {
    var elements = buildExpression$1(group.body, options.withColor(group.color), false);
    return makeFragment(elements);
  };
  var mathmlBuilder$7 = (group, options) => {
    var inner2 = buildExpression2(group.body, options.withColor(group.color));
    var node = new MathNode("mstyle", inner2);
    node.setAttribute("mathcolor", group.color);
    return node;
  };
  defineFunction({
    type: "color",
    names: ["\\textcolor"],
    props: {
      numArgs: 2,
      allowedInText: true,
      argTypes: ["color", "original"]
    },
    handler(_ref, args) {
      var {
        parser
      } = _ref;
      var color = assertNodeType(args[0], "color-token").color;
      var body = args[1];
      return {
        type: "color",
        mode: parser.mode,
        color,
        body: ordargument(body)
      };
    },
    htmlBuilder: htmlBuilder$8,
    mathmlBuilder: mathmlBuilder$7
  });
  defineFunction({
    type: "color",
    names: ["\\color"],
    props: {
      numArgs: 1,
      allowedInText: true,
      argTypes: ["color"]
    },
    handler(_ref2, args) {
      var {
        parser,
        breakOnTokenText
      } = _ref2;
      var color = assertNodeType(args[0], "color-token").color;
      parser.gullet.macros.set("\\current@color", color);
      var body = parser.parseExpression(true, breakOnTokenText);
      return {
        type: "color",
        mode: parser.mode,
        color,
        body
      };
    },
    htmlBuilder: htmlBuilder$8,
    mathmlBuilder: mathmlBuilder$7
  });
  defineFunction({
    type: "cr",
    names: ["\\\\"],
    props: {
      numArgs: 0,
      numOptionalArgs: 0,
      allowedInText: true
    },
    handler(_ref, args, optArgs) {
      var {
        parser
      } = _ref;
      var size = parser.gullet.future().text === "[" ? parser.parseSizeGroup(true) : null;
      var newLine = !parser.settings.displayMode || !parser.settings.useStrictBehavior("newLineInDisplayMode", "In LaTeX, \\\\ or \\newline does nothing in display mode");
      return {
        type: "cr",
        mode: parser.mode,
        newLine,
        size: size && assertNodeType(size, "size").value
      };
    },
    // The following builders are called only at the top level,
    // not within tabular/array environments.
    htmlBuilder(group, options) {
      var span = makeSpan(["mspace"], [], options);
      if (group.newLine) {
        span.classes.push("newline");
        if (group.size) {
          span.style.marginTop = makeEm(calculateSize(group.size, options));
        }
      }
      return span;
    },
    mathmlBuilder(group, options) {
      var node = new MathNode("mspace");
      if (group.newLine) {
        node.setAttribute("linebreak", "newline");
        if (group.size) {
          node.setAttribute("height", makeEm(calculateSize(group.size, options)));
        }
      }
      return node;
    }
  });
  var globalMap = {
    "\\global": "\\global",
    "\\long": "\\\\globallong",
    "\\\\globallong": "\\\\globallong",
    "\\def": "\\gdef",
    "\\gdef": "\\gdef",
    "\\edef": "\\xdef",
    "\\xdef": "\\xdef",
    "\\let": "\\\\globallet",
    "\\futurelet": "\\\\globalfuture"
  };
  var checkControlSequence = (tok) => {
    var name = tok.text;
    if (/^(?:[\\{}$&#^_]|EOF)$/.test(name)) {
      throw new ParseError("Expected a control sequence", tok);
    }
    return name;
  };
  var getRHS = (parser) => {
    var tok = parser.gullet.popToken();
    if (tok.text === "=") {
      tok = parser.gullet.popToken();
      if (tok.text === " ") {
        tok = parser.gullet.popToken();
      }
    }
    return tok;
  };
  var letCommand = (parser, name, tok, global) => {
    var macro = parser.gullet.macros.get(tok.text);
    if (macro == null) {
      tok.noexpand = true;
      macro = {
        tokens: [tok],
        numArgs: 0,
        // reproduce the same behavior in expansion
        unexpandable: !parser.gullet.isExpandable(tok.text)
      };
    }
    parser.gullet.macros.set(name, macro, global);
  };
  defineFunction({
    type: "internal",
    names: [
      "\\global",
      "\\long",
      "\\\\globallong"
      // can’t be entered directly
    ],
    props: {
      numArgs: 0,
      allowedInText: true
    },
    handler(_ref) {
      var {
        parser,
        funcName
      } = _ref;
      parser.consumeSpaces();
      var token = parser.fetch();
      if (globalMap[token.text]) {
        if (funcName === "\\global" || funcName === "\\\\globallong") {
          token.text = globalMap[token.text];
        }
        return assertNodeType(parser.parseFunction(), "internal");
      }
      throw new ParseError("Invalid token after macro prefix", token);
    }
  });
  defineFunction({
    type: "internal",
    names: ["\\def", "\\gdef", "\\edef", "\\xdef"],
    props: {
      numArgs: 0,
      allowedInText: true,
      primitive: true
    },
    handler(_ref2) {
      var {
        parser,
        funcName
      } = _ref2;
      var tok = parser.gullet.popToken();
      var name = tok.text;
      if (/^(?:[\\{}$&#^_]|EOF)$/.test(name)) {
        throw new ParseError("Expected a control sequence", tok);
      }
      var numArgs = 0;
      var insert;
      var delimiters2 = [[]];
      while (parser.gullet.future().text !== "{") {
        tok = parser.gullet.popToken();
        if (tok.text === "#") {
          if (parser.gullet.future().text === "{") {
            insert = parser.gullet.future();
            delimiters2[numArgs].push("{");
            break;
          }
          tok = parser.gullet.popToken();
          if (!/^[1-9]$/.test(tok.text)) {
            throw new ParseError('Invalid argument number "' + tok.text + '"');
          }
          if (parseInt(tok.text) !== numArgs + 1) {
            throw new ParseError('Argument number "' + tok.text + '" out of order');
          }
          numArgs++;
          delimiters2.push([]);
        } else if (tok.text === "EOF") {
          throw new ParseError("Expected a macro definition");
        } else {
          delimiters2[numArgs].push(tok.text);
        }
      }
      var {
        tokens
      } = parser.gullet.consumeArg();
      if (insert) {
        tokens.unshift(insert);
      }
      if (funcName === "\\edef" || funcName === "\\xdef") {
        tokens = parser.gullet.expandTokens(tokens);
        tokens.reverse();
      }
      parser.gullet.macros.set(name, {
        tokens,
        numArgs,
        delimiters: delimiters2
      }, funcName === globalMap[funcName]);
      return {
        type: "internal",
        mode: parser.mode
      };
    }
  });
  defineFunction({
    type: "internal",
    names: [
      "\\let",
      "\\\\globallet"
      // can’t be entered directly
    ],
    props: {
      numArgs: 0,
      allowedInText: true,
      primitive: true
    },
    handler(_ref3) {
      var {
        parser,
        funcName
      } = _ref3;
      var name = checkControlSequence(parser.gullet.popToken());
      parser.gullet.consumeSpaces();
      var tok = getRHS(parser);
      letCommand(parser, name, tok, funcName === "\\\\globallet");
      return {
        type: "internal",
        mode: parser.mode
      };
    }
  });
  defineFunction({
    type: "internal",
    names: [
      "\\futurelet",
      "\\\\globalfuture"
      // can’t be entered directly
    ],
    props: {
      numArgs: 0,
      allowedInText: true,
      primitive: true
    },
    handler(_ref4) {
      var {
        parser,
        funcName
      } = _ref4;
      var name = checkControlSequence(parser.gullet.popToken());
      var middle = parser.gullet.popToken();
      var tok = parser.gullet.popToken();
      letCommand(parser, name, tok, funcName === "\\\\globalfuture");
      parser.gullet.pushToken(tok);
      parser.gullet.pushToken(middle);
      return {
        type: "internal",
        mode: parser.mode
      };
    }
  });
  var getMetrics = function getMetrics2(symbol, font, mode) {
    var replace = symbols.math[symbol] && symbols.math[symbol].replace;
    var metrics = getCharacterMetrics(replace || symbol, font, mode);
    if (!metrics) {
      throw new Error("Unsupported symbol " + symbol + " and font size " + font + ".");
    }
    return metrics;
  };
  var styleWrap = function styleWrap2(delim, toStyle, options, classes) {
    var newOptions = options.havingBaseStyle(toStyle);
    var span = makeSpan(classes.concat(newOptions.sizingClasses(options)), [delim], options);
    var delimSizeMultiplier = newOptions.sizeMultiplier / options.sizeMultiplier;
    span.height *= delimSizeMultiplier;
    span.depth *= delimSizeMultiplier;
    span.maxFontSize = newOptions.sizeMultiplier;
    return span;
  };
  var centerSpan = function centerSpan2(span, options, style) {
    var newOptions = options.havingBaseStyle(style);
    var shift = (1 - options.sizeMultiplier / newOptions.sizeMultiplier) * options.fontMetrics().axisHeight;
    span.classes.push("delimcenter");
    span.style.top = makeEm(shift);
    span.height -= shift;
    span.depth += shift;
  };
  var makeSmallDelim = function makeSmallDelim2(delim, style, center, options, mode, classes) {
    var text2 = makeSymbol(delim, "Main-Regular", mode, options);
    var span = styleWrap(text2, style, options, classes);
    if (center) {
      centerSpan(span, options, style);
    }
    return span;
  };
  var mathrmSize = function mathrmSize2(value, size, mode, options) {
    return makeSymbol(value, "Size" + size + "-Regular", mode, options);
  };
  var makeLargeDelim = function makeLargeDelim2(delim, size, center, options, mode, classes) {
    var inner2 = mathrmSize(delim, size, mode, options);
    var span = styleWrap(makeSpan(["delimsizing", "size" + size], [inner2], options), Style$1.TEXT, options, classes);
    if (center) {
      centerSpan(span, options, Style$1.TEXT);
    }
    return span;
  };
  var makeGlyphSpan = function makeGlyphSpan2(symbol, font, mode) {
    var sizeClass;
    if (font === "Size1-Regular") {
      sizeClass = "delim-size1";
    } else {
      sizeClass = "delim-size4";
    }
    var corner = makeSpan(["delimsizinginner", sizeClass], [makeSpan([], [makeSymbol(symbol, font, mode)])]);
    return {
      type: "elem",
      elem: corner
    };
  };
  var makeInner = function makeInner2(ch2, height, options) {
    var width = fontMetricsData["Size4-Regular"][ch2.charCodeAt(0)] ? fontMetricsData["Size4-Regular"][ch2.charCodeAt(0)][4] : fontMetricsData["Size1-Regular"][ch2.charCodeAt(0)][4];
    var path2 = new PathNode("inner", innerPath(ch2, Math.round(1e3 * height)));
    var svgNode = new SvgNode([path2], {
      "width": makeEm(width),
      "height": makeEm(height),
      // Override CSS rule `.katex svg { width: 100% }`
      "style": "width:" + makeEm(width),
      "viewBox": "0 0 " + 1e3 * width + " " + Math.round(1e3 * height),
      "preserveAspectRatio": "xMinYMin"
    });
    var span = makeSvgSpan([], [svgNode], options);
    span.height = height;
    span.style.height = makeEm(height);
    span.style.width = makeEm(width);
    return {
      type: "elem",
      elem: span
    };
  };
  var lapInEms = 8e-3;
  var lap = {
    type: "kern",
    size: -1 * lapInEms
  };
  var verts = /* @__PURE__ */ new Set(["|", "\\lvert", "\\rvert", "\\vert"]);
  var doubleVerts = /* @__PURE__ */ new Set(["\\|", "\\lVert", "\\rVert", "\\Vert"]);
  var makeStackedDelim = function makeStackedDelim2(delim, heightTotal, center, options, mode, classes) {
    var top;
    var middle;
    var repeat;
    var bottom;
    var svgLabel = "";
    var viewBoxWidth = 0;
    top = repeat = bottom = delim;
    middle = null;
    var font = "Size1-Regular";
    if (delim === "\\uparrow") {
      repeat = bottom = "\u23D0";
    } else if (delim === "\\Uparrow") {
      repeat = bottom = "\u2016";
    } else if (delim === "\\downarrow") {
      top = repeat = "\u23D0";
    } else if (delim === "\\Downarrow") {
      top = repeat = "\u2016";
    } else if (delim === "\\updownarrow") {
      top = "\\uparrow";
      repeat = "\u23D0";
      bottom = "\\downarrow";
    } else if (delim === "\\Updownarrow") {
      top = "\\Uparrow";
      repeat = "\u2016";
      bottom = "\\Downarrow";
    } else if (verts.has(delim)) {
      repeat = "\u2223";
      svgLabel = "vert";
      viewBoxWidth = 333;
    } else if (doubleVerts.has(delim)) {
      repeat = "\u2225";
      svgLabel = "doublevert";
      viewBoxWidth = 556;
    } else if (delim === "[" || delim === "\\lbrack") {
      top = "\u23A1";
      repeat = "\u23A2";
      bottom = "\u23A3";
      font = "Size4-Regular";
      svgLabel = "lbrack";
      viewBoxWidth = 667;
    } else if (delim === "]" || delim === "\\rbrack") {
      top = "\u23A4";
      repeat = "\u23A5";
      bottom = "\u23A6";
      font = "Size4-Regular";
      svgLabel = "rbrack";
      viewBoxWidth = 667;
    } else if (delim === "\\lfloor" || delim === "\u230A") {
      repeat = top = "\u23A2";
      bottom = "\u23A3";
      font = "Size4-Regular";
      svgLabel = "lfloor";
      viewBoxWidth = 667;
    } else if (delim === "\\lceil" || delim === "\u2308") {
      top = "\u23A1";
      repeat = bottom = "\u23A2";
      font = "Size4-Regular";
      svgLabel = "lceil";
      viewBoxWidth = 667;
    } else if (delim === "\\rfloor" || delim === "\u230B") {
      repeat = top = "\u23A5";
      bottom = "\u23A6";
      font = "Size4-Regular";
      svgLabel = "rfloor";
      viewBoxWidth = 667;
    } else if (delim === "\\rceil" || delim === "\u2309") {
      top = "\u23A4";
      repeat = bottom = "\u23A5";
      font = "Size4-Regular";
      svgLabel = "rceil";
      viewBoxWidth = 667;
    } else if (delim === "(" || delim === "\\lparen") {
      top = "\u239B";
      repeat = "\u239C";
      bottom = "\u239D";
      font = "Size4-Regular";
      svgLabel = "lparen";
      viewBoxWidth = 875;
    } else if (delim === ")" || delim === "\\rparen") {
      top = "\u239E";
      repeat = "\u239F";
      bottom = "\u23A0";
      font = "Size4-Regular";
      svgLabel = "rparen";
      viewBoxWidth = 875;
    } else if (delim === "\\{" || delim === "\\lbrace") {
      top = "\u23A7";
      middle = "\u23A8";
      bottom = "\u23A9";
      repeat = "\u23AA";
      font = "Size4-Regular";
    } else if (delim === "\\}" || delim === "\\rbrace") {
      top = "\u23AB";
      middle = "\u23AC";
      bottom = "\u23AD";
      repeat = "\u23AA";
      font = "Size4-Regular";
    } else if (delim === "\\lgroup" || delim === "\u27EE") {
      top = "\u23A7";
      bottom = "\u23A9";
      repeat = "\u23AA";
      font = "Size4-Regular";
    } else if (delim === "\\rgroup" || delim === "\u27EF") {
      top = "\u23AB";
      bottom = "\u23AD";
      repeat = "\u23AA";
      font = "Size4-Regular";
    } else if (delim === "\\lmoustache" || delim === "\u23B0") {
      top = "\u23A7";
      bottom = "\u23AD";
      repeat = "\u23AA";
      font = "Size4-Regular";
    } else if (delim === "\\rmoustache" || delim === "\u23B1") {
      top = "\u23AB";
      bottom = "\u23A9";
      repeat = "\u23AA";
      font = "Size4-Regular";
    }
    var topMetrics = getMetrics(top, font, mode);
    var topHeightTotal = topMetrics.height + topMetrics.depth;
    var repeatMetrics = getMetrics(repeat, font, mode);
    var repeatHeightTotal = repeatMetrics.height + repeatMetrics.depth;
    var bottomMetrics = getMetrics(bottom, font, mode);
    var bottomHeightTotal = bottomMetrics.height + bottomMetrics.depth;
    var middleHeightTotal = 0;
    var middleFactor = 1;
    if (middle !== null) {
      var middleMetrics = getMetrics(middle, font, mode);
      middleHeightTotal = middleMetrics.height + middleMetrics.depth;
      middleFactor = 2;
    }
    var minHeight = topHeightTotal + bottomHeightTotal + middleHeightTotal;
    var repeatCount = Math.max(0, Math.ceil((heightTotal - minHeight) / (middleFactor * repeatHeightTotal)));
    var realHeightTotal = minHeight + repeatCount * middleFactor * repeatHeightTotal;
    var axisHeight = options.fontMetrics().axisHeight;
    if (center) {
      axisHeight *= options.sizeMultiplier;
    }
    var depth = realHeightTotal / 2 - axisHeight;
    var stack = [];
    if (svgLabel.length > 0) {
      var midHeight = realHeightTotal - topHeightTotal - bottomHeightTotal;
      var viewBoxHeight = Math.round(realHeightTotal * 1e3);
      var pathStr = tallDelim(svgLabel, Math.round(midHeight * 1e3));
      var path2 = new PathNode(svgLabel, pathStr);
      var width = makeEm(viewBoxWidth / 1e3);
      var height = makeEm(viewBoxHeight / 1e3);
      var svg = new SvgNode([path2], {
        "width": width,
        "height": height,
        "viewBox": "0 0 " + viewBoxWidth + " " + viewBoxHeight
      });
      var wrapper = makeSvgSpan([], [svg], options);
      wrapper.height = viewBoxHeight / 1e3;
      wrapper.style.width = width;
      wrapper.style.height = height;
      stack.push({
        type: "elem",
        elem: wrapper
      });
    } else {
      stack.push(makeGlyphSpan(bottom, font, mode));
      stack.push(lap);
      if (middle === null) {
        var innerHeight = realHeightTotal - topHeightTotal - bottomHeightTotal + 2 * lapInEms;
        stack.push(makeInner(repeat, innerHeight, options));
      } else {
        var _innerHeight = (realHeightTotal - topHeightTotal - bottomHeightTotal - middleHeightTotal) / 2 + 2 * lapInEms;
        stack.push(makeInner(repeat, _innerHeight, options));
        stack.push(lap);
        stack.push(makeGlyphSpan(middle, font, mode));
        stack.push(lap);
        stack.push(makeInner(repeat, _innerHeight, options));
      }
      stack.push(lap);
      stack.push(makeGlyphSpan(top, font, mode));
    }
    var newOptions = options.havingBaseStyle(Style$1.TEXT);
    var inner2 = makeVList({
      positionType: "bottom",
      positionData: depth,
      children: stack
    });
    return styleWrap(makeSpan(["delimsizing", "mult"], [inner2], newOptions), Style$1.TEXT, options, classes);
  };
  var vbPad = 80;
  var emPad = 0.08;
  var sqrtSvg = function sqrtSvg2(sqrtName, height, viewBoxHeight, extraVinculum, options) {
    var path2 = sqrtPath(sqrtName, extraVinculum, viewBoxHeight);
    var pathNode = new PathNode(sqrtName, path2);
    var svg = new SvgNode([pathNode], {
      // Note: 1000:1 ratio of viewBox to document em width.
      "width": "400em",
      "height": makeEm(height),
      "viewBox": "0 0 400000 " + viewBoxHeight,
      "preserveAspectRatio": "xMinYMin slice"
    });
    return makeSvgSpan(["hide-tail"], [svg], options);
  };
  var makeSqrtImage = function makeSqrtImage2(height, options) {
    var newOptions = options.havingBaseSizing();
    var delim = traverseSequence("\\surd", height * newOptions.sizeMultiplier, stackLargeDelimiterSequence, newOptions);
    var sizeMultiplier = newOptions.sizeMultiplier;
    var extraVinculum = Math.max(0, options.minRuleThickness - options.fontMetrics().sqrtRuleThickness);
    var span;
    var spanHeight;
    var texHeight;
    var viewBoxHeight;
    var advanceWidth;
    if (delim.type === "small") {
      viewBoxHeight = 1e3 + 1e3 * extraVinculum + vbPad;
      if (height < 1) {
        sizeMultiplier = 1;
      } else if (height < 1.4) {
        sizeMultiplier = 0.7;
      }
      spanHeight = (1 + extraVinculum + emPad) / sizeMultiplier;
      texHeight = (1 + extraVinculum) / sizeMultiplier;
      span = sqrtSvg("sqrtMain", spanHeight, viewBoxHeight, extraVinculum, options);
      span.style.minWidth = "0.853em";
      advanceWidth = 0.833 / sizeMultiplier;
    } else if (delim.type === "large") {
      viewBoxHeight = (1e3 + vbPad) * sizeToMaxHeight[delim.size];
      texHeight = (sizeToMaxHeight[delim.size] + extraVinculum) / sizeMultiplier;
      spanHeight = (sizeToMaxHeight[delim.size] + extraVinculum + emPad) / sizeMultiplier;
      span = sqrtSvg("sqrtSize" + delim.size, spanHeight, viewBoxHeight, extraVinculum, options);
      span.style.minWidth = "1.02em";
      advanceWidth = 1 / sizeMultiplier;
    } else {
      spanHeight = height + extraVinculum + emPad;
      texHeight = height + extraVinculum;
      viewBoxHeight = Math.floor(1e3 * height + extraVinculum) + vbPad;
      span = sqrtSvg("sqrtTall", spanHeight, viewBoxHeight, extraVinculum, options);
      span.style.minWidth = "0.742em";
      advanceWidth = 1.056;
    }
    span.height = texHeight;
    span.style.height = makeEm(spanHeight);
    return {
      span,
      advanceWidth,
      // Calculate the actual line width.
      // This actually should depend on the chosen font -- e.g. \boldmath
      // should use the thicker surd symbols from e.g. KaTeX_Main-Bold, and
      // have thicker rules.
      ruleWidth: (options.fontMetrics().sqrtRuleThickness + extraVinculum) * sizeMultiplier
    };
  };
  var stackLargeDelimiters = /* @__PURE__ */ new Set(["(", "\\lparen", ")", "\\rparen", "[", "\\lbrack", "]", "\\rbrack", "\\{", "\\lbrace", "\\}", "\\rbrace", "\\lfloor", "\\rfloor", "\u230A", "\u230B", "\\lceil", "\\rceil", "\u2308", "\u2309", "\\surd"]);
  var stackAlwaysDelimiters = /* @__PURE__ */ new Set(["\\uparrow", "\\downarrow", "\\updownarrow", "\\Uparrow", "\\Downarrow", "\\Updownarrow", "|", "\\|", "\\vert", "\\Vert", "\\lvert", "\\rvert", "\\lVert", "\\rVert", "\\lgroup", "\\rgroup", "\u27EE", "\u27EF", "\\lmoustache", "\\rmoustache", "\u23B0", "\u23B1"]);
  var stackNeverDelimiters = /* @__PURE__ */ new Set(["<", ">", "\\langle", "\\rangle", "/", "\\backslash", "\\lt", "\\gt"]);
  var sizeToMaxHeight = [0, 1.2, 1.8, 2.4, 3];
  var makeSizedDelim = function makeSizedDelim2(delim, size, options, mode, classes) {
    if (delim === "<" || delim === "\\lt" || delim === "\u27E8") {
      delim = "\\langle";
    } else if (delim === ">" || delim === "\\gt" || delim === "\u27E9") {
      delim = "\\rangle";
    }
    if (stackLargeDelimiters.has(delim) || stackNeverDelimiters.has(delim)) {
      return makeLargeDelim(delim, size, false, options, mode, classes);
    } else if (stackAlwaysDelimiters.has(delim)) {
      return makeStackedDelim(delim, sizeToMaxHeight[size], false, options, mode, classes);
    } else {
      throw new ParseError("Illegal delimiter: '" + delim + "'");
    }
  };
  var stackNeverDelimiterSequence = [{
    type: "small",
    style: Style$1.SCRIPTSCRIPT
  }, {
    type: "small",
    style: Style$1.SCRIPT
  }, {
    type: "small",
    style: Style$1.TEXT
  }, {
    type: "large",
    size: 1
  }, {
    type: "large",
    size: 2
  }, {
    type: "large",
    size: 3
  }, {
    type: "large",
    size: 4
  }];
  var stackAlwaysDelimiterSequence = [{
    type: "small",
    style: Style$1.SCRIPTSCRIPT
  }, {
    type: "small",
    style: Style$1.SCRIPT
  }, {
    type: "small",
    style: Style$1.TEXT
  }, {
    type: "stack"
  }];
  var stackLargeDelimiterSequence = [{
    type: "small",
    style: Style$1.SCRIPTSCRIPT
  }, {
    type: "small",
    style: Style$1.SCRIPT
  }, {
    type: "small",
    style: Style$1.TEXT
  }, {
    type: "large",
    size: 1
  }, {
    type: "large",
    size: 2
  }, {
    type: "large",
    size: 3
  }, {
    type: "large",
    size: 4
  }, {
    type: "stack"
  }];
  var delimTypeToFont = function delimTypeToFont2(type) {
    if (type.type === "small") {
      return "Main-Regular";
    } else if (type.type === "large") {
      return "Size" + type.size + "-Regular";
    } else if (type.type === "stack") {
      return "Size4-Regular";
    } else {
      var delimKind = type.type;
      throw new Error("Add support for delim type '" + delimKind + "' here.");
    }
  };
  var traverseSequence = function traverseSequence2(delim, height, sequence, options) {
    var start = Math.min(2, 3 - options.style.size);
    for (var i3 = start; i3 < sequence.length; i3++) {
      var delimType = sequence[i3];
      if (delimType.type === "stack") {
        break;
      }
      var metrics = getMetrics(delim, delimTypeToFont(delimType), "math");
      var heightDepth = metrics.height + metrics.depth;
      if (delimType.type === "small") {
        var newOptions = options.havingBaseStyle(delimType.style);
        heightDepth *= newOptions.sizeMultiplier;
      }
      if (heightDepth > height) {
        return delimType;
      }
    }
    return sequence[sequence.length - 1];
  };
  var makeCustomSizedDelim = function makeCustomSizedDelim2(delim, height, center, options, mode, classes) {
    if (delim === "<" || delim === "\\lt" || delim === "\u27E8") {
      delim = "\\langle";
    } else if (delim === ">" || delim === "\\gt" || delim === "\u27E9") {
      delim = "\\rangle";
    }
    var sequence;
    if (stackNeverDelimiters.has(delim)) {
      sequence = stackNeverDelimiterSequence;
    } else if (stackLargeDelimiters.has(delim)) {
      sequence = stackLargeDelimiterSequence;
    } else {
      sequence = stackAlwaysDelimiterSequence;
    }
    var delimType = traverseSequence(delim, height, sequence, options);
    if (delimType.type === "small") {
      return makeSmallDelim(delim, delimType.style, center, options, mode, classes);
    } else if (delimType.type === "large") {
      return makeLargeDelim(delim, delimType.size, center, options, mode, classes);
    } else {
      return makeStackedDelim(delim, height, center, options, mode, classes);
    }
  };
  var makeLeftRightDelim = function makeLeftRightDelim2(delim, height, depth, options, mode, classes) {
    var axisHeight = options.fontMetrics().axisHeight * options.sizeMultiplier;
    var delimiterFactor = 901;
    var delimiterExtend = 5 / options.fontMetrics().ptPerEm;
    var maxDistFromAxis = Math.max(height - axisHeight, depth + axisHeight);
    var totalHeight = Math.max(
      // In real TeX, calculations are done using integral values which are
      // 65536 per pt, or 655360 per em. So, the division here truncates in
      // TeX but doesn't here, producing different results. If we wanted to
      // exactly match TeX's calculation, we could do
      //   Math.floor(655360 * maxDistFromAxis / 500) *
      //    delimiterFactor / 655360
      // (To see the difference, compare
      //    x^{x^{\left(\rule{0.1em}{0.68em}\right)}}
      // in TeX and KaTeX)
      maxDistFromAxis / 500 * delimiterFactor,
      2 * maxDistFromAxis - delimiterExtend
    );
    return makeCustomSizedDelim(delim, totalHeight, true, options, mode, classes);
  };
  var delimiterSizes = {
    "\\bigl": {
      mclass: "mopen",
      size: 1
    },
    "\\Bigl": {
      mclass: "mopen",
      size: 2
    },
    "\\biggl": {
      mclass: "mopen",
      size: 3
    },
    "\\Biggl": {
      mclass: "mopen",
      size: 4
    },
    "\\bigr": {
      mclass: "mclose",
      size: 1
    },
    "\\Bigr": {
      mclass: "mclose",
      size: 2
    },
    "\\biggr": {
      mclass: "mclose",
      size: 3
    },
    "\\Biggr": {
      mclass: "mclose",
      size: 4
    },
    "\\bigm": {
      mclass: "mrel",
      size: 1
    },
    "\\Bigm": {
      mclass: "mrel",
      size: 2
    },
    "\\biggm": {
      mclass: "mrel",
      size: 3
    },
    "\\Biggm": {
      mclass: "mrel",
      size: 4
    },
    "\\big": {
      mclass: "mord",
      size: 1
    },
    "\\Big": {
      mclass: "mord",
      size: 2
    },
    "\\bigg": {
      mclass: "mord",
      size: 3
    },
    "\\Bigg": {
      mclass: "mord",
      size: 4
    }
  };
  var delimiters = /* @__PURE__ */ new Set(["(", "\\lparen", ")", "\\rparen", "[", "\\lbrack", "]", "\\rbrack", "\\{", "\\lbrace", "\\}", "\\rbrace", "\\lfloor", "\\rfloor", "\u230A", "\u230B", "\\lceil", "\\rceil", "\u2308", "\u2309", "<", ">", "\\langle", "\u27E8", "\\rangle", "\u27E9", "\\lt", "\\gt", "\\lvert", "\\rvert", "\\lVert", "\\rVert", "\\lgroup", "\\rgroup", "\u27EE", "\u27EF", "\\lmoustache", "\\rmoustache", "\u23B0", "\u23B1", "/", "\\backslash", "|", "\\vert", "\\|", "\\Vert", "\\uparrow", "\\Uparrow", "\\downarrow", "\\Downarrow", "\\updownarrow", "\\Updownarrow", "."]);
  function isMiddleDelimNode(node) {
    return "isMiddle" in node;
  }
  function checkDelimiter(delim, context) {
    var symDelim = checkSymbolNodeType(delim);
    if (symDelim && delimiters.has(symDelim.text)) {
      return symDelim;
    } else if (symDelim) {
      throw new ParseError("Invalid delimiter '" + symDelim.text + "' after '" + context.funcName + "'", delim);
    } else {
      throw new ParseError("Invalid delimiter type '" + delim.type + "'", delim);
    }
  }
  defineFunction({
    type: "delimsizing",
    names: ["\\bigl", "\\Bigl", "\\biggl", "\\Biggl", "\\bigr", "\\Bigr", "\\biggr", "\\Biggr", "\\bigm", "\\Bigm", "\\biggm", "\\Biggm", "\\big", "\\Big", "\\bigg", "\\Bigg"],
    props: {
      numArgs: 1,
      argTypes: ["primitive"]
    },
    handler: (context, args) => {
      var delim = checkDelimiter(args[0], context);
      return {
        type: "delimsizing",
        mode: context.parser.mode,
        size: delimiterSizes[context.funcName].size,
        mclass: delimiterSizes[context.funcName].mclass,
        delim: delim.text
      };
    },
    htmlBuilder: (group, options) => {
      if (group.delim === ".") {
        return makeSpan([group.mclass]);
      }
      return makeSizedDelim(group.delim, group.size, options, group.mode, [group.mclass]);
    },
    mathmlBuilder: (group) => {
      var children = [];
      if (group.delim !== ".") {
        children.push(makeText(group.delim, group.mode));
      }
      var node = new MathNode("mo", children);
      if (group.mclass === "mopen" || group.mclass === "mclose") {
        node.setAttribute("fence", "true");
      } else {
        node.setAttribute("fence", "false");
      }
      node.setAttribute("stretchy", "true");
      var size = makeEm(sizeToMaxHeight[group.size]);
      node.setAttribute("minsize", size);
      node.setAttribute("maxsize", size);
      return node;
    }
  });
  function assertParsed(group) {
    if (!group.body) {
      throw new Error("Bug: The leftright ParseNode wasn't fully parsed.");
    }
  }
  defineFunction({
    type: "leftright-right",
    names: ["\\right"],
    props: {
      numArgs: 1,
      primitive: true
    },
    handler: (context, args) => {
      var color = context.parser.gullet.macros.get("\\current@color");
      if (color && typeof color !== "string") {
        throw new ParseError("\\current@color set to non-string in \\right");
      }
      return {
        type: "leftright-right",
        mode: context.parser.mode,
        delim: checkDelimiter(args[0], context).text,
        color
        // undefined if not set via \color
      };
    }
  });
  defineFunction({
    type: "leftright",
    names: ["\\left"],
    props: {
      numArgs: 1,
      primitive: true
    },
    handler: (context, args) => {
      var delim = checkDelimiter(args[0], context);
      var parser = context.parser;
      ++parser.leftrightDepth;
      var body = parser.parseExpression(false);
      --parser.leftrightDepth;
      parser.expect("\\right", false);
      var right = assertNodeType(parser.parseFunction(), "leftright-right");
      return {
        type: "leftright",
        mode: parser.mode,
        body,
        left: delim.text,
        right: right.delim,
        rightColor: right.color
      };
    },
    htmlBuilder: (group, options) => {
      assertParsed(group);
      var inner2 = buildExpression$1(group.body, options, true, ["mopen", "mclose"]);
      var innerHeight = 0;
      var innerDepth = 0;
      var hadMiddle = false;
      for (var i3 = 0; i3 < inner2.length; i3++) {
        var node = inner2[i3];
        if (isMiddleDelimNode(node)) {
          hadMiddle = true;
        } else {
          innerHeight = Math.max(inner2[i3].height, innerHeight);
          innerDepth = Math.max(inner2[i3].depth, innerDepth);
        }
      }
      innerHeight *= options.sizeMultiplier;
      innerDepth *= options.sizeMultiplier;
      var leftDelim;
      if (group.left === ".") {
        leftDelim = makeNullDelimiter(options, ["mopen"]);
      } else {
        leftDelim = makeLeftRightDelim(group.left, innerHeight, innerDepth, options, group.mode, ["mopen"]);
      }
      inner2.unshift(leftDelim);
      if (hadMiddle) {
        for (var _i6 = 1; _i6 < inner2.length; _i6++) {
          var middleDelim = inner2[_i6];
          if (isMiddleDelimNode(middleDelim)) {
            var isMiddle = middleDelim.isMiddle;
            inner2[_i6] = makeLeftRightDelim(isMiddle.delim, innerHeight, innerDepth, isMiddle.options, group.mode, []);
          }
        }
      }
      var rightDelim;
      if (group.right === ".") {
        rightDelim = makeNullDelimiter(options, ["mclose"]);
      } else {
        var colorOptions = group.rightColor ? options.withColor(group.rightColor) : options;
        rightDelim = makeLeftRightDelim(group.right, innerHeight, innerDepth, colorOptions, group.mode, ["mclose"]);
      }
      inner2.push(rightDelim);
      return makeSpan(["minner"], inner2, options);
    },
    mathmlBuilder: (group, options) => {
      assertParsed(group);
      var inner2 = buildExpression2(group.body, options);
      if (group.left !== ".") {
        var leftNode = new MathNode("mo", [makeText(group.left, group.mode)]);
        leftNode.setAttribute("fence", "true");
        inner2.unshift(leftNode);
      }
      if (group.right !== ".") {
        var rightNode = new MathNode("mo", [makeText(group.right, group.mode)]);
        rightNode.setAttribute("fence", "true");
        if (group.rightColor) {
          rightNode.setAttribute("mathcolor", group.rightColor);
        }
        inner2.push(rightNode);
      }
      return makeRow(inner2);
    }
  });
  defineFunction({
    type: "middle",
    names: ["\\middle"],
    props: {
      numArgs: 1,
      primitive: true
    },
    handler: (context, args) => {
      var delim = checkDelimiter(args[0], context);
      if (!context.parser.leftrightDepth) {
        throw new ParseError("\\middle without preceding \\left", delim);
      }
      return {
        type: "middle",
        mode: context.parser.mode,
        delim: delim.text
      };
    },
    htmlBuilder: (group, options) => {
      var middleDelim;
      if (group.delim === ".") {
        middleDelim = makeNullDelimiter(options, []);
      } else {
        middleDelim = makeSizedDelim(group.delim, 1, options, group.mode, []);
        middleDelim.isMiddle = {
          delim: group.delim,
          options
        };
      }
      return middleDelim;
    },
    mathmlBuilder: (group, options) => {
      var textNode = group.delim === "\\vert" || group.delim === "|" ? makeText("|", "text") : makeText(group.delim, group.mode);
      var middleNode = new MathNode("mo", [textNode]);
      middleNode.setAttribute("fence", "true");
      middleNode.setAttribute("lspace", "0.05em");
      middleNode.setAttribute("rspace", "0.05em");
      return middleNode;
    }
  });
  var htmlBuilder$7 = (group, options) => {
    var inner2 = wrapFragment(buildGroup$1(group.body, options), options);
    var label = group.label.slice(1);
    var scale = options.sizeMultiplier;
    var img;
    var imgShift;
    var isSingleChar = isCharacterBox(group.body);
    if (label === "sout") {
      img = makeSpan(["stretchy", "sout"]);
      img.height = options.fontMetrics().defaultRuleThickness / scale;
      imgShift = -0.5 * options.fontMetrics().xHeight;
    } else if (label === "phase") {
      var lineWeight = calculateSize({
        number: 0.6,
        unit: "pt"
      }, options);
      var clearance = calculateSize({
        number: 0.35,
        unit: "ex"
      }, options);
      var newOptions = options.havingBaseSizing();
      scale = scale / newOptions.sizeMultiplier;
      var angleHeight = inner2.height + inner2.depth + lineWeight + clearance;
      inner2.style.paddingLeft = makeEm(angleHeight / 2 + lineWeight);
      var viewBoxHeight = Math.floor(1e3 * angleHeight * scale);
      var path2 = phasePath(viewBoxHeight);
      var svgNode = new SvgNode([new PathNode("phase", path2)], {
        "width": "400em",
        "height": makeEm(viewBoxHeight / 1e3),
        "viewBox": "0 0 400000 " + viewBoxHeight,
        "preserveAspectRatio": "xMinYMin slice"
      });
      img = makeSvgSpan(["hide-tail"], [svgNode], options);
      img.style.height = makeEm(angleHeight);
      imgShift = inner2.depth + lineWeight + clearance;
    } else {
      if (/cancel/.test(label)) {
        if (!isSingleChar) {
          inner2.classes.push("cancel-pad");
        }
      } else if (label === "angl") {
        inner2.classes.push("anglpad");
      } else {
        inner2.classes.push("boxpad");
      }
      var topPad;
      var bottomPad;
      var ruleThickness = 0;
      if (/box/.test(label)) {
        ruleThickness = Math.max(
          options.fontMetrics().fboxrule,
          // default
          options.minRuleThickness
        );
        topPad = options.fontMetrics().fboxsep + (label === "colorbox" ? 0 : ruleThickness);
        bottomPad = topPad;
      } else if (label === "angl") {
        ruleThickness = Math.max(options.fontMetrics().defaultRuleThickness, options.minRuleThickness);
        topPad = 4 * ruleThickness;
        bottomPad = Math.max(0, 0.25 - inner2.depth);
      } else {
        topPad = isSingleChar ? 0.2 : 0;
        bottomPad = topPad;
      }
      img = stretchyEnclose(inner2, label, topPad, bottomPad, options);
      if (/fbox|boxed|fcolorbox/.test(label)) {
        img.style.borderStyle = "solid";
        img.style.borderWidth = makeEm(ruleThickness);
      } else if (label === "angl" && ruleThickness !== 0.049) {
        img.style.borderTopWidth = makeEm(ruleThickness);
        img.style.borderRightWidth = makeEm(ruleThickness);
      }
      imgShift = inner2.depth + bottomPad;
      if (group.backgroundColor) {
        img.style.backgroundColor = group.backgroundColor;
        if (group.borderColor) {
          img.style.borderColor = group.borderColor;
        }
      }
    }
    var vlist;
    if (group.backgroundColor) {
      vlist = makeVList({
        positionType: "individualShift",
        children: [
          // Put the color background behind inner;
          {
            type: "elem",
            elem: img,
            shift: imgShift
          },
          {
            type: "elem",
            elem: inner2,
            shift: 0
          }
        ]
      });
    } else {
      var classes = /cancel|phase/.test(label) ? ["svg-align"] : [];
      vlist = makeVList({
        positionType: "individualShift",
        children: [
          // Write the \cancel stroke on top of inner.
          {
            type: "elem",
            elem: inner2,
            shift: 0
          },
          {
            type: "elem",
            elem: img,
            shift: imgShift,
            wrapperClasses: classes
          }
        ]
      });
    }
    if (/cancel/.test(label)) {
      vlist.height = inner2.height;
      vlist.depth = inner2.depth;
    }
    if (/cancel/.test(label) && !isSingleChar) {
      return makeSpan(["mord", "cancel-lap"], [vlist], options);
    } else {
      return makeSpan(["mord"], [vlist], options);
    }
  };
  var mathmlBuilder$6 = (group, options) => {
    var fboxsep;
    var node = new MathNode(group.label.includes("colorbox") ? "mpadded" : "menclose", [buildGroup2(group.body, options)]);
    switch (group.label) {
      case "\\cancel":
        node.setAttribute("notation", "updiagonalstrike");
        break;
      case "\\bcancel":
        node.setAttribute("notation", "downdiagonalstrike");
        break;
      case "\\phase":
        node.setAttribute("notation", "phasorangle");
        break;
      case "\\sout":
        node.setAttribute("notation", "horizontalstrike");
        break;
      case "\\fbox":
        node.setAttribute("notation", "box");
        break;
      case "\\angl":
        node.setAttribute("notation", "actuarial");
        break;
      case "\\fcolorbox":
      case "\\colorbox":
        fboxsep = options.fontMetrics().fboxsep * options.fontMetrics().ptPerEm;
        node.setAttribute("width", "+" + 2 * fboxsep + "pt");
        node.setAttribute("height", "+" + 2 * fboxsep + "pt");
        node.setAttribute("lspace", fboxsep + "pt");
        node.setAttribute("voffset", fboxsep + "pt");
        if (group.label === "\\fcolorbox") {
          var thk = Math.max(
            options.fontMetrics().fboxrule,
            // default
            options.minRuleThickness
          );
          node.setAttribute("style", "border: " + makeEm(thk) + " solid " + group.borderColor);
        }
        break;
      case "\\xcancel":
        node.setAttribute("notation", "updiagonalstrike downdiagonalstrike");
        break;
    }
    if (group.backgroundColor) {
      node.setAttribute("mathbackground", group.backgroundColor);
    }
    return node;
  };
  defineFunction({
    type: "enclose",
    names: ["\\colorbox"],
    props: {
      numArgs: 2,
      allowedInText: true,
      argTypes: ["color", "hbox"]
    },
    handler(_ref, args, optArgs) {
      var {
        parser,
        funcName
      } = _ref;
      var color = assertNodeType(args[0], "color-token").color;
      var body = args[1];
      return {
        type: "enclose",
        mode: parser.mode,
        label: funcName,
        backgroundColor: color,
        body
      };
    },
    htmlBuilder: htmlBuilder$7,
    mathmlBuilder: mathmlBuilder$6
  });
  defineFunction({
    type: "enclose",
    names: ["\\fcolorbox"],
    props: {
      numArgs: 3,
      allowedInText: true,
      argTypes: ["color", "color", "hbox"]
    },
    handler(_ref2, args, optArgs) {
      var {
        parser,
        funcName
      } = _ref2;
      var borderColor = assertNodeType(args[0], "color-token").color;
      var backgroundColor = assertNodeType(args[1], "color-token").color;
      var body = args[2];
      return {
        type: "enclose",
        mode: parser.mode,
        label: funcName,
        backgroundColor,
        borderColor,
        body
      };
    },
    htmlBuilder: htmlBuilder$7,
    mathmlBuilder: mathmlBuilder$6
  });
  defineFunction({
    type: "enclose",
    names: ["\\fbox"],
    props: {
      numArgs: 1,
      argTypes: ["hbox"],
      allowedInText: true
    },
    handler(_ref3, args) {
      var {
        parser
      } = _ref3;
      return {
        type: "enclose",
        mode: parser.mode,
        label: "\\fbox",
        body: args[0]
      };
    }
  });
  defineFunction({
    type: "enclose",
    names: ["\\cancel", "\\bcancel", "\\xcancel", "\\phase"],
    props: {
      numArgs: 1
    },
    handler(_ref4, args) {
      var {
        parser,
        funcName
      } = _ref4;
      var body = args[0];
      return {
        type: "enclose",
        mode: parser.mode,
        label: funcName,
        body
      };
    },
    htmlBuilder: htmlBuilder$7,
    mathmlBuilder: mathmlBuilder$6
  });
  defineFunction({
    type: "enclose",
    names: ["\\sout"],
    props: {
      numArgs: 1,
      allowedInText: true
    },
    handler(_ref5, args) {
      var {
        parser,
        funcName
      } = _ref5;
      if (parser.mode === "math") {
        parser.settings.reportNonstrict("mathVsSout", "LaTeX's \\sout works only in text mode");
      }
      var body = args[0];
      return {
        type: "enclose",
        mode: parser.mode,
        label: funcName,
        body
      };
    },
    htmlBuilder: htmlBuilder$7,
    mathmlBuilder: mathmlBuilder$6
  });
  defineFunction({
    type: "enclose",
    names: ["\\angl"],
    props: {
      numArgs: 1,
      argTypes: ["hbox"],
      allowedInText: false
    },
    handler(_ref6, args) {
      var {
        parser
      } = _ref6;
      return {
        type: "enclose",
        mode: parser.mode,
        label: "\\angl",
        body: args[0]
      };
    }
  });
  var _environments = {};
  function defineEnvironment(_ref) {
    var {
      type,
      names,
      props,
      handler,
      htmlBuilder: htmlBuilder3,
      mathmlBuilder: mathmlBuilder3
    } = _ref;
    var data = {
      type,
      numArgs: props.numArgs || 0,
      allowedInText: false,
      numOptionalArgs: 0,
      handler
    };
    for (var i3 = 0; i3 < names.length; ++i3) {
      _environments[names[i3]] = data;
    }
    if (htmlBuilder3) {
      _htmlGroupBuilders[type] = htmlBuilder3;
    }
    if (mathmlBuilder3) {
      _mathmlGroupBuilders[type] = mathmlBuilder3;
    }
  }
  var _macros = {};
  function defineMacro(name, body) {
    _macros[name] = body;
  }
  var SourceLocation = class _SourceLocation {
    // End offset, zero-based exclusive.
    constructor(lexer, start, end) {
      this.lexer = void 0;
      this.start = void 0;
      this.end = void 0;
      this.lexer = lexer;
      this.start = start;
      this.end = end;
    }
    /**
     * Merges two `SourceLocation`s from location providers, given they are
     * provided in order of appearance.
     * - Returns the first one's location if only the first is provided.
     * - Returns a merged range of the first and the last if both are provided
     *   and their lexers match.
     * - Otherwise, returns null.
     */
    static range(first, second) {
      if (!second) {
        return first && first.loc;
      } else if (!first || !first.loc || !second.loc || first.loc.lexer !== second.loc.lexer) {
        return null;
      } else {
        return new _SourceLocation(first.loc.lexer, first.loc.start, second.loc.end);
      }
    }
  };
  var Token = class _Token {
    // used in \noexpand
    constructor(text2, loc) {
      this.text = void 0;
      this.loc = void 0;
      this.noexpand = void 0;
      this.treatAsRelax = void 0;
      this.text = text2;
      this.loc = loc;
    }
    /**
     * Given a pair of tokens (this and endToken), compute a `Token` encompassing
     * the whole input range enclosed by these two.
     */
    range(endToken, text2) {
      return new _Token(text2, SourceLocation.range(this, endToken));
    }
  };
  function getHLines(parser) {
    var hlineInfo = [];
    parser.consumeSpaces();
    var nxt = parser.fetch().text;
    if (nxt === "\\relax") {
      parser.consume();
      parser.consumeSpaces();
      nxt = parser.fetch().text;
    }
    while (nxt === "\\hline" || nxt === "\\hdashline") {
      parser.consume();
      hlineInfo.push(nxt === "\\hdashline");
      parser.consumeSpaces();
      nxt = parser.fetch().text;
    }
    return hlineInfo;
  }
  var validateAmsEnvironmentContext = (context) => {
    var settings = context.parser.settings;
    if (!settings.displayMode) {
      throw new ParseError("{" + context.envName + "} can be used only in display mode.");
    }
  };
  var gatherEnvironments = /* @__PURE__ */ new Set(["gather", "gather*"]);
  function getAutoTag(name) {
    if (!name.includes("ed")) {
      return !name.includes("*");
    }
  }
  function parseArray(parser, _ref, style) {
    var {
      hskipBeforeAndAfter,
      addJot,
      cols,
      arraystretch,
      colSeparationType,
      autoTag,
      singleRow,
      emptySingleRow,
      maxNumCols,
      leqno
    } = _ref;
    parser.gullet.beginGroup();
    if (!singleRow) {
      parser.gullet.macros.set("\\cr", "\\\\\\relax");
    }
    if (!arraystretch) {
      var stretch = parser.gullet.expandMacroAsText("\\arraystretch");
      if (stretch == null) {
        arraystretch = 1;
      } else {
        arraystretch = parseFloat(stretch);
        if (!arraystretch || arraystretch < 0) {
          throw new ParseError("Invalid \\arraystretch: " + stretch);
        }
      }
    }
    parser.gullet.beginGroup();
    var row = [];
    var body = [row];
    var rowGaps = [];
    var hLinesBeforeRow = [];
    var tags = autoTag != null ? [] : void 0;
    function beginRow() {
      if (autoTag) {
        parser.gullet.macros.set("\\@eqnsw", "1", true);
      }
    }
    function endRow() {
      if (tags) {
        if (parser.gullet.macros.get("\\df@tag")) {
          tags.push(parser.subparse([new Token("\\df@tag")]));
          parser.gullet.macros.set("\\df@tag", void 0, true);
        } else {
          tags.push(Boolean(autoTag) && parser.gullet.macros.get("\\@eqnsw") === "1");
        }
      }
    }
    beginRow();
    hLinesBeforeRow.push(getHLines(parser));
    while (true) {
      var cellBody = parser.parseExpression(false, singleRow ? "\\end" : "\\\\");
      parser.gullet.endGroup();
      parser.gullet.beginGroup();
      var cell = {
        type: "ordgroup",
        mode: parser.mode,
        body: cellBody
      };
      if (style) {
        cell = {
          type: "styling",
          mode: parser.mode,
          style,
          resetFont: true,
          body: [cell]
        };
      }
      row.push(cell);
      var next = parser.fetch().text;
      if (next === "&") {
        if (maxNumCols && row.length === maxNumCols) {
          if (singleRow || colSeparationType) {
            throw new ParseError("Too many tab characters: &", parser.nextToken);
          } else {
            parser.settings.reportNonstrict("textEnv", "Too few columns specified in the {array} column argument.");
          }
        }
        parser.consume();
      } else if (next === "\\end") {
        endRow();
        if (row.length === 1 && cell.type === "styling" && cell.body.length === 1 && cell.body[0].type === "ordgroup" && cell.body[0].body.length === 0 && (body.length > 1 || !emptySingleRow)) {
          body.pop();
        }
        if (hLinesBeforeRow.length < body.length + 1) {
          hLinesBeforeRow.push([]);
        }
        break;
      } else if (next === "\\\\") {
        parser.consume();
        var size = void 0;
        if (parser.gullet.future().text !== " ") {
          size = parser.parseSizeGroup(true);
        }
        rowGaps.push(size ? size.value : null);
        endRow();
        hLinesBeforeRow.push(getHLines(parser));
        row = [];
        body.push(row);
        beginRow();
      } else {
        throw new ParseError("Expected & or \\\\ or \\cr or \\end", parser.nextToken);
      }
    }
    parser.gullet.endGroup();
    parser.gullet.endGroup();
    return {
      type: "array",
      mode: parser.mode,
      addJot,
      arraystretch,
      body,
      cols,
      rowGaps,
      hskipBeforeAndAfter,
      hLinesBeforeRow,
      colSeparationType,
      tags,
      leqno
    };
  }
  function dCellStyle(envName) {
    if (envName.slice(0, 1) === "d") {
      return "display";
    } else {
      return "text";
    }
  }
  var htmlBuilder$6 = function htmlBuilder(group, options) {
    var r2;
    var c2;
    var nr = group.body.length;
    var hLinesBeforeRow = group.hLinesBeforeRow;
    var nc = 0;
    var body = new Array(nr);
    var hlines = [];
    var ruleThickness = Math.max(
      // From LaTeX \showthe\arrayrulewidth. Equals 0.04 em.
      options.fontMetrics().arrayRuleWidth,
      options.minRuleThickness
    );
    var pt = 1 / options.fontMetrics().ptPerEm;
    var arraycolsep = 5 * pt;
    if (group.colSeparationType && group.colSeparationType === "small") {
      var localMultiplier = options.havingStyle(Style$1.SCRIPT).sizeMultiplier;
      arraycolsep = 0.2778 * (localMultiplier / options.sizeMultiplier);
    }
    var baselineskip = group.colSeparationType === "CD" ? calculateSize({
      number: 3,
      unit: "ex"
    }, options) : 12 * pt;
    var jot = 3 * pt;
    var arrayskip = group.arraystretch * baselineskip;
    var arstrutHeight = 0.7 * arrayskip;
    var arstrutDepth = 0.3 * arrayskip;
    var totalHeight = 0;
    function setHLinePos(hlinesInGap) {
      for (var i3 = 0; i3 < hlinesInGap.length; ++i3) {
        if (i3 > 0) {
          totalHeight += 0.25;
        }
        hlines.push({
          pos: totalHeight,
          isDashed: hlinesInGap[i3]
        });
      }
    }
    setHLinePos(hLinesBeforeRow[0]);
    for (r2 = 0; r2 < group.body.length; ++r2) {
      var inrow = group.body[r2];
      var height = arstrutHeight;
      var depth = arstrutDepth;
      if (nc < inrow.length) {
        nc = inrow.length;
      }
      var outrow = {
        cells: new Array(inrow.length),
        height: 0,
        depth: 0,
        pos: 0
      };
      for (c2 = 0; c2 < inrow.length; ++c2) {
        var elt = buildGroup$1(inrow[c2], options);
        if (depth < elt.depth) {
          depth = elt.depth;
        }
        if (height < elt.height) {
          height = elt.height;
        }
        outrow.cells[c2] = elt;
      }
      var rowGap = group.rowGaps[r2];
      var gap = 0;
      if (rowGap) {
        gap = calculateSize(rowGap, options);
        if (gap > 0) {
          gap += arstrutDepth;
          if (depth < gap) {
            depth = gap;
          }
          gap = 0;
        }
      }
      if (group.addJot && r2 < group.body.length - 1) {
        depth += jot;
      }
      outrow.height = height;
      outrow.depth = depth;
      totalHeight += height;
      outrow.pos = totalHeight;
      totalHeight += depth + gap;
      body[r2] = outrow;
      setHLinePos(hLinesBeforeRow[r2 + 1]);
    }
    var offset = totalHeight / 2 + options.fontMetrics().axisHeight;
    var colDescriptions = group.cols || [];
    var cols = [];
    var colSep;
    var colDescrNum;
    var tagSpans = [];
    if (group.tags && group.tags.some((tag2) => tag2)) {
      for (r2 = 0; r2 < nr; ++r2) {
        var rw = body[r2];
        var shift = rw.pos - offset;
        var tag = group.tags[r2];
        var tagSpan = void 0;
        if (tag === true) {
          tagSpan = makeSpan(["eqn-num"], [], options);
        } else if (tag === false) {
          tagSpan = makeSpan([], [], options);
        } else {
          tagSpan = makeSpan([], buildExpression$1(tag, options, true), options);
        }
        tagSpan.depth = rw.depth;
        tagSpan.height = rw.height;
        tagSpans.push({
          type: "elem",
          elem: tagSpan,
          shift
        });
      }
    }
    for (
      c2 = 0, colDescrNum = 0;
      // Continue while either there are more columns or more column
      // descriptions, so trailing separators don't get lost.
      c2 < nc || colDescrNum < colDescriptions.length;
      ++c2, ++colDescrNum
    ) {
      var _colDescr3;
      var colDescr = colDescriptions[colDescrNum];
      var firstSeparator = true;
      while (((_colDescr = colDescr) == null ? void 0 : _colDescr.type) === "separator") {
        var _colDescr;
        if (!firstSeparator) {
          colSep = makeSpan(["arraycolsep"], []);
          colSep.style.width = makeEm(options.fontMetrics().doubleRuleSep);
          cols.push(colSep);
        }
        if (colDescr.separator === "|" || colDescr.separator === ":") {
          var lineType = colDescr.separator === "|" ? "solid" : "dashed";
          var separator = makeSpan(["vertical-separator"], [], options);
          separator.style.height = makeEm(totalHeight);
          separator.style.borderRightWidth = makeEm(ruleThickness);
          separator.style.borderRightStyle = lineType;
          separator.style.margin = "0 " + makeEm(-ruleThickness / 2);
          var _shift = totalHeight - offset;
          if (_shift) {
            separator.style.verticalAlign = makeEm(-_shift);
          }
          cols.push(separator);
        } else {
          throw new ParseError("Invalid separator type: " + colDescr.separator);
        }
        colDescrNum++;
        colDescr = colDescriptions[colDescrNum];
        firstSeparator = false;
      }
      if (c2 >= nc) {
        continue;
      }
      var sepwidth = void 0;
      if (c2 > 0 || group.hskipBeforeAndAfter) {
        var _colDescr$pregap, _colDescr2;
        sepwidth = (_colDescr$pregap = (_colDescr2 = colDescr) == null ? void 0 : _colDescr2.pregap) != null ? _colDescr$pregap : arraycolsep;
        if (sepwidth !== 0) {
          colSep = makeSpan(["arraycolsep"], []);
          colSep.style.width = makeEm(sepwidth);
          cols.push(colSep);
        }
      }
      var colElems = [];
      for (r2 = 0; r2 < nr; ++r2) {
        var row = body[r2];
        var elem = row.cells[c2];
        if (!elem) {
          continue;
        }
        var _shift2 = row.pos - offset;
        elem.depth = row.depth;
        elem.height = row.height;
        colElems.push({
          type: "elem",
          elem,
          shift: _shift2
        });
      }
      var colVList = makeVList({
        positionType: "individualShift",
        children: colElems
      });
      var colSpan = makeSpan(["col-align-" + (((_colDescr3 = colDescr) == null ? void 0 : _colDescr3.align) || "c")], [colVList]);
      cols.push(colSpan);
      if (c2 < nc - 1 || group.hskipBeforeAndAfter) {
        var _colDescr$postgap, _colDescr4;
        sepwidth = (_colDescr$postgap = (_colDescr4 = colDescr) == null ? void 0 : _colDescr4.postgap) != null ? _colDescr$postgap : arraycolsep;
        if (sepwidth !== 0) {
          colSep = makeSpan(["arraycolsep"], []);
          colSep.style.width = makeEm(sepwidth);
          cols.push(colSep);
        }
      }
    }
    var tableBody = makeSpan(["mtable"], cols);
    if (hlines.length > 0) {
      var line = makeLineSpan("hline", options, ruleThickness);
      var dashes = makeLineSpan("hdashline", options, ruleThickness);
      var vListElems = [{
        type: "elem",
        elem: tableBody,
        shift: 0
      }];
      while (hlines.length > 0) {
        var hline = hlines.pop();
        var lineShift = hline.pos - offset;
        if (hline.isDashed) {
          vListElems.push({
            type: "elem",
            elem: dashes,
            shift: lineShift
          });
        } else {
          vListElems.push({
            type: "elem",
            elem: line,
            shift: lineShift
          });
        }
      }
      tableBody = makeVList({
        positionType: "individualShift",
        children: vListElems
      });
    }
    if (tagSpans.length === 0) {
      return makeSpan(["mord"], [tableBody], options);
    } else {
      var eqnNumCol = makeVList({
        positionType: "individualShift",
        children: tagSpans
      });
      var tagCol = makeSpan(["tag"], [eqnNumCol], options);
      return makeFragment([tableBody, tagCol]);
    }
  };
  var alignMap = {
    c: "center ",
    l: "left ",
    r: "right "
  };
  var mathmlBuilder$5 = function mathmlBuilder(group, options) {
    var tbl = [];
    var glue = new MathNode("mtd", [], ["mtr-glue"]);
    var tag = new MathNode("mtd", [], ["mml-eqn-num"]);
    for (var i3 = 0; i3 < group.body.length; i3++) {
      var rw = group.body[i3];
      var row = [];
      for (var j2 = 0; j2 < rw.length; j2++) {
        row.push(new MathNode("mtd", [buildGroup2(rw[j2], options)]));
      }
      if (group.tags && group.tags[i3]) {
        row.unshift(glue);
        row.push(glue);
        if (group.leqno) {
          row.unshift(tag);
        } else {
          row.push(tag);
        }
      }
      tbl.push(new MathNode("mtr", row));
    }
    var table = new MathNode("mtable", tbl);
    var gap = group.arraystretch === 0.5 ? 0.1 : 0.16 + group.arraystretch - 1 + (group.addJot ? 0.09 : 0);
    table.setAttribute("rowspacing", makeEm(gap));
    var menclose = "";
    var align = "";
    if (group.cols && group.cols.length > 0) {
      var cols = group.cols;
      var columnLines = "";
      var prevTypeWasAlign = false;
      var iStart = 0;
      var iEnd = cols.length;
      if (cols[0].type === "separator") {
        menclose += "top ";
        iStart = 1;
      }
      if (cols[cols.length - 1].type === "separator") {
        menclose += "bottom ";
        iEnd -= 1;
      }
      for (var _i6 = iStart; _i6 < iEnd; _i6++) {
        var col = cols[_i6];
        if (col.type === "align") {
          align += alignMap[col.align];
          if (prevTypeWasAlign) {
            columnLines += "none ";
          }
          prevTypeWasAlign = true;
        } else if (col.type === "separator") {
          if (prevTypeWasAlign) {
            columnLines += col.separator === "|" ? "solid " : "dashed ";
            prevTypeWasAlign = false;
          }
        }
      }
      table.setAttribute("columnalign", align.trim());
      if (/[sd]/.test(columnLines)) {
        table.setAttribute("columnlines", columnLines.trim());
      }
    }
    if (group.colSeparationType === "align") {
      var _cols = group.cols || [];
      var spacing2 = "";
      for (var _i22 = 1; _i22 < _cols.length; _i22++) {
        spacing2 += _i22 % 2 ? "0em " : "1em ";
      }
      table.setAttribute("columnspacing", spacing2.trim());
    } else if (group.colSeparationType === "alignat" || group.colSeparationType === "gather") {
      table.setAttribute("columnspacing", "0em");
    } else if (group.colSeparationType === "small") {
      table.setAttribute("columnspacing", "0.2778em");
    } else if (group.colSeparationType === "CD") {
      table.setAttribute("columnspacing", "0.5em");
    } else {
      table.setAttribute("columnspacing", "1em");
    }
    var rowLines = "";
    var hlines = group.hLinesBeforeRow;
    menclose += hlines[0].length > 0 ? "left " : "";
    menclose += hlines[hlines.length - 1].length > 0 ? "right " : "";
    for (var _i32 = 1; _i32 < hlines.length - 1; _i32++) {
      rowLines += hlines[_i32].length === 0 ? "none " : hlines[_i32][0] ? "dashed " : "solid ";
    }
    if (/[sd]/.test(rowLines)) {
      table.setAttribute("rowlines", rowLines.trim());
    }
    if (menclose !== "") {
      table = new MathNode("menclose", [table]);
      table.setAttribute("notation", menclose.trim());
    }
    if (group.arraystretch && group.arraystretch < 1) {
      table = new MathNode("mstyle", [table]);
      table.setAttribute("scriptlevel", "1");
    }
    return table;
  };
  var alignedHandler = function alignedHandler2(context, args) {
    if (!context.envName.includes("ed")) {
      validateAmsEnvironmentContext(context);
    }
    var cols = [];
    var separationType = context.envName.includes("at") ? "alignat" : "align";
    var isSplit = context.envName === "split";
    var res = parseArray(context.parser, {
      cols,
      addJot: true,
      autoTag: isSplit ? void 0 : getAutoTag(context.envName),
      emptySingleRow: true,
      colSeparationType: separationType,
      maxNumCols: isSplit ? 2 : void 0,
      leqno: context.parser.settings.leqno
    }, "display");
    var numMaths = 0;
    var numCols = 0;
    var emptyGroup = {
      type: "ordgroup",
      mode: context.mode,
      body: []
    };
    if (args[0] && args[0].type === "ordgroup") {
      var arg0 = "";
      for (var i3 = 0; i3 < args[0].body.length; i3++) {
        var textord2 = assertNodeType(args[0].body[i3], "textord");
        arg0 += textord2.text;
      }
      numMaths = Number(arg0);
      numCols = numMaths * 2;
    }
    var isAligned = !numCols;
    res.body.forEach(function(row) {
      for (var _i42 = 1; _i42 < row.length; _i42 += 2) {
        var styling = assertNodeType(row[_i42], "styling");
        var ordgroup = assertNodeType(styling.body[0], "ordgroup");
        ordgroup.body.unshift(emptyGroup);
      }
      if (!isAligned) {
        var curMaths = row.length / 2;
        if (numMaths < curMaths) {
          throw new ParseError("Too many math in a row: " + ("expected " + numMaths + ", but got " + curMaths), row[0]);
        }
      } else if (numCols < row.length) {
        numCols = row.length;
      }
    });
    for (var _i52 = 0; _i52 < numCols; ++_i52) {
      var align = "r";
      var pregap = 0;
      if (_i52 % 2 === 1) {
        align = "l";
      } else if (_i52 > 0 && isAligned) {
        pregap = 1;
      }
      cols[_i52] = {
        type: "align",
        align,
        pregap,
        postgap: 0
      };
    }
    res.colSeparationType = isAligned ? "align" : "alignat";
    return res;
  };
  defineEnvironment({
    type: "array",
    names: ["array", "darray"],
    props: {
      numArgs: 1
    },
    handler(context, args) {
      var symNode = checkSymbolNodeType(args[0]);
      var colalign = symNode ? [args[0]] : assertNodeType(args[0], "ordgroup").body;
      var cols = colalign.map(function(nde) {
        var node = assertSymbolNodeType(nde);
        var ca = node.text;
        if ("lcr".includes(ca)) {
          return {
            type: "align",
            align: ca
          };
        } else if (ca === "|") {
          return {
            type: "separator",
            separator: "|"
          };
        } else if (ca === ":") {
          return {
            type: "separator",
            separator: ":"
          };
        }
        throw new ParseError("Unknown column alignment: " + ca, nde);
      });
      var res = {
        cols,
        hskipBeforeAndAfter: true,
        // \@preamble in lttab.dtx
        maxNumCols: cols.length
      };
      return parseArray(context.parser, res, dCellStyle(context.envName));
    },
    htmlBuilder: htmlBuilder$6,
    mathmlBuilder: mathmlBuilder$5
  });
  defineEnvironment({
    type: "array",
    names: ["matrix", "pmatrix", "bmatrix", "Bmatrix", "vmatrix", "Vmatrix", "matrix*", "pmatrix*", "bmatrix*", "Bmatrix*", "vmatrix*", "Vmatrix*"],
    props: {
      numArgs: 0
    },
    handler(context) {
      var delimiters2 = {
        "matrix": null,
        "pmatrix": ["(", ")"],
        "bmatrix": ["[", "]"],
        "Bmatrix": ["\\{", "\\}"],
        "vmatrix": ["|", "|"],
        "Vmatrix": ["\\Vert", "\\Vert"]
      }[context.envName.replace("*", "")];
      var colAlign = "c";
      var payload = {
        hskipBeforeAndAfter: false,
        cols: [{
          type: "align",
          align: colAlign
        }]
      };
      if (context.envName.charAt(context.envName.length - 1) === "*") {
        var parser = context.parser;
        parser.consumeSpaces();
        if (parser.fetch().text === "[") {
          parser.consume();
          parser.consumeSpaces();
          colAlign = parser.fetch().text;
          if (!"lcr".includes(colAlign)) {
            throw new ParseError("Expected l or c or r", parser.nextToken);
          }
          parser.consume();
          parser.consumeSpaces();
          parser.expect("]");
          parser.consume();
          payload.cols = [{
            type: "align",
            align: colAlign
          }];
        }
      }
      var res = parseArray(context.parser, payload, dCellStyle(context.envName));
      var numCols = Math.max(0, ...res.body.map((row) => row.length));
      res.cols = new Array(numCols).fill({
        type: "align",
        align: colAlign
      });
      return delimiters2 ? {
        type: "leftright",
        mode: context.mode,
        body: [res],
        left: delimiters2[0],
        right: delimiters2[1],
        rightColor: void 0
        // \right uninfluenced by \color in array
      } : res;
    },
    htmlBuilder: htmlBuilder$6,
    mathmlBuilder: mathmlBuilder$5
  });
  defineEnvironment({
    type: "array",
    names: ["smallmatrix"],
    props: {
      numArgs: 0
    },
    handler(context) {
      var payload = {
        arraystretch: 0.5
      };
      var res = parseArray(context.parser, payload, "script");
      res.colSeparationType = "small";
      return res;
    },
    htmlBuilder: htmlBuilder$6,
    mathmlBuilder: mathmlBuilder$5
  });
  defineEnvironment({
    type: "array",
    names: ["subarray"],
    props: {
      numArgs: 1
    },
    handler(context, args) {
      var symNode = checkSymbolNodeType(args[0]);
      var colalign = symNode ? [args[0]] : assertNodeType(args[0], "ordgroup").body;
      var cols = colalign.map(function(nde) {
        var node = assertSymbolNodeType(nde);
        var ca = node.text;
        if ("lc".includes(ca)) {
          return {
            type: "align",
            align: ca
          };
        }
        throw new ParseError("Unknown column alignment: " + ca, nde);
      });
      if (cols.length > 1) {
        throw new ParseError("{subarray} can contain only one column");
      }
      var payload = {
        cols,
        hskipBeforeAndAfter: false,
        arraystretch: 0.5
      };
      var res = parseArray(context.parser, payload, "script");
      if (res.body.length > 0 && res.body[0].length > 1) {
        throw new ParseError("{subarray} can contain only one column");
      }
      return res;
    },
    htmlBuilder: htmlBuilder$6,
    mathmlBuilder: mathmlBuilder$5
  });
  defineEnvironment({
    type: "array",
    names: ["cases", "dcases", "rcases", "drcases"],
    props: {
      numArgs: 0
    },
    handler(context) {
      var payload = {
        arraystretch: 1.2,
        cols: [{
          type: "align",
          align: "l",
          pregap: 0,
          // TODO(kevinb) get the current style.
          // For now we use the metrics for TEXT style which is what we were
          // doing before.  Before attempting to get the current style we
          // should look at TeX's behavior especially for \over and matrices.
          postgap: 1
          /* 1em quad */
        }, {
          type: "align",
          align: "l",
          pregap: 0,
          postgap: 0
        }]
      };
      var res = parseArray(context.parser, payload, dCellStyle(context.envName));
      return {
        type: "leftright",
        mode: context.mode,
        body: [res],
        left: context.envName.includes("r") ? "." : "\\{",
        right: context.envName.includes("r") ? "\\}" : ".",
        rightColor: void 0
      };
    },
    htmlBuilder: htmlBuilder$6,
    mathmlBuilder: mathmlBuilder$5
  });
  defineEnvironment({
    type: "array",
    names: ["align", "align*", "aligned", "split"],
    props: {
      numArgs: 0
    },
    handler: alignedHandler,
    htmlBuilder: htmlBuilder$6,
    mathmlBuilder: mathmlBuilder$5
  });
  defineEnvironment({
    type: "array",
    names: ["gathered", "gather", "gather*"],
    props: {
      numArgs: 0
    },
    handler(context) {
      if (gatherEnvironments.has(context.envName)) {
        validateAmsEnvironmentContext(context);
      }
      var res = {
        cols: [{
          type: "align",
          align: "c"
        }],
        addJot: true,
        colSeparationType: "gather",
        autoTag: getAutoTag(context.envName),
        emptySingleRow: true,
        leqno: context.parser.settings.leqno
      };
      return parseArray(context.parser, res, "display");
    },
    htmlBuilder: htmlBuilder$6,
    mathmlBuilder: mathmlBuilder$5
  });
  defineEnvironment({
    type: "array",
    names: ["alignat", "alignat*", "alignedat"],
    props: {
      numArgs: 1
    },
    handler: alignedHandler,
    htmlBuilder: htmlBuilder$6,
    mathmlBuilder: mathmlBuilder$5
  });
  defineEnvironment({
    type: "array",
    names: ["equation", "equation*"],
    props: {
      numArgs: 0
    },
    handler(context) {
      validateAmsEnvironmentContext(context);
      var res = {
        autoTag: getAutoTag(context.envName),
        emptySingleRow: true,
        singleRow: true,
        maxNumCols: 1,
        leqno: context.parser.settings.leqno
      };
      return parseArray(context.parser, res, "display");
    },
    htmlBuilder: htmlBuilder$6,
    mathmlBuilder: mathmlBuilder$5
  });
  defineEnvironment({
    type: "array",
    names: ["CD"],
    props: {
      numArgs: 0
    },
    handler(context) {
      validateAmsEnvironmentContext(context);
      return parseCD(context.parser);
    },
    htmlBuilder: htmlBuilder$6,
    mathmlBuilder: mathmlBuilder$5
  });
  defineMacro("\\nonumber", "\\gdef\\@eqnsw{0}");
  defineMacro("\\notag", "\\nonumber");
  defineFunction({
    type: "text",
    // Doesn't matter what this is.
    names: ["\\hline", "\\hdashline"],
    props: {
      numArgs: 0,
      allowedInText: true,
      allowedInMath: true
    },
    handler(context, args) {
      throw new ParseError(context.funcName + " valid only within array environment");
    }
  });
  var environments = _environments;
  defineFunction({
    type: "environment",
    names: ["\\begin", "\\end"],
    props: {
      numArgs: 1,
      argTypes: ["text"]
    },
    handler(_ref, args) {
      var {
        parser,
        funcName
      } = _ref;
      var nameGroup = args[0];
      if (nameGroup.type !== "ordgroup") {
        throw new ParseError("Invalid environment name", nameGroup);
      }
      var envName = "";
      for (var i3 = 0; i3 < nameGroup.body.length; ++i3) {
        envName += assertNodeType(nameGroup.body[i3], "textord").text;
      }
      if (funcName === "\\begin") {
        if (!environments.hasOwnProperty(envName)) {
          throw new ParseError("No such environment: " + envName, nameGroup);
        }
        var env = environments[envName];
        var {
          args: _args,
          optArgs
        } = parser.parseArguments("\\begin{" + envName + "}", env);
        var context = {
          mode: parser.mode,
          envName,
          parser
        };
        var result = env.handler(context, _args, optArgs);
        parser.expect("\\end", false);
        var endNameToken = parser.nextToken;
        var end = assertNodeType(parser.parseFunction(), "environment");
        if (end.name !== envName) {
          throw new ParseError("Mismatch: \\begin{" + envName + "} matched by \\end{" + end.name + "}", endNameToken);
        }
        return result;
      }
      return {
        type: "environment",
        mode: parser.mode,
        name: envName,
        nameGroup
      };
    }
  });
  var htmlBuilder$5 = (group, options) => {
    var font = group.font;
    var newOptions = options.withFont(font);
    return buildGroup$1(group.body, newOptions);
  };
  var mathmlBuilder$4 = (group, options) => {
    var font = group.font;
    var newOptions = options.withFont(font);
    return buildGroup2(group.body, newOptions);
  };
  var fontAliases = {
    "\\Bbb": "\\mathbb",
    "\\bold": "\\mathbf",
    "\\frak": "\\mathfrak"
  };
  defineFunction({
    type: "font",
    names: [
      // styles, except \boldsymbol defined below
      "\\mathrm",
      "\\mathit",
      "\\mathbf",
      "\\mathnormal",
      "\\mathsfit",
      // families
      "\\mathbb",
      "\\mathcal",
      "\\mathfrak",
      "\\mathscr",
      "\\mathsf",
      "\\mathtt",
      // aliases, except \bm defined below
      "\\Bbb",
      "\\bold",
      "\\frak"
    ],
    props: {
      numArgs: 1,
      allowedInArgument: true
    },
    handler: (_ref, args) => {
      var {
        parser,
        funcName
      } = _ref;
      var body = normalizeArgument(args[0]);
      var func = funcName;
      if (func in fontAliases) {
        func = fontAliases[func];
      }
      return {
        type: "font",
        mode: parser.mode,
        font: func.slice(1),
        body
      };
    },
    htmlBuilder: htmlBuilder$5,
    mathmlBuilder: mathmlBuilder$4
  });
  defineFunction({
    type: "mclass",
    names: ["\\boldsymbol", "\\bm"],
    props: {
      numArgs: 1
    },
    handler: (_ref2, args) => {
      var {
        parser
      } = _ref2;
      var body = args[0];
      return {
        type: "mclass",
        mode: parser.mode,
        mclass: binrelClass(body),
        body: [{
          type: "font",
          mode: parser.mode,
          font: "boldsymbol",
          body
        }],
        isCharacterBox: isCharacterBox(body)
      };
    }
  });
  defineFunction({
    type: "font",
    names: ["\\rm", "\\sf", "\\tt", "\\bf", "\\it", "\\cal"],
    props: {
      numArgs: 0,
      allowedInText: true
    },
    handler: (_ref3, args) => {
      var {
        parser,
        funcName,
        breakOnTokenText
      } = _ref3;
      var {
        mode
      } = parser;
      var body = parser.parseExpression(true, breakOnTokenText);
      return {
        type: "font",
        mode,
        font: "math" + funcName.slice(1),
        body: {
          type: "ordgroup",
          mode: parser.mode,
          body
        }
      };
    },
    htmlBuilder: htmlBuilder$5,
    mathmlBuilder: mathmlBuilder$4
  });
  var htmlBuilder$4 = (group, options) => {
    var style = options.style;
    var nstyle = style.fracNum();
    var dstyle = style.fracDen();
    var newOptions;
    newOptions = options.havingStyle(nstyle);
    var numerm = buildGroup$1(group.numer, newOptions, options);
    if (group.continued) {
      var hStrut = 8.5 / options.fontMetrics().ptPerEm;
      var dStrut = 3.5 / options.fontMetrics().ptPerEm;
      numerm.height = numerm.height < hStrut ? hStrut : numerm.height;
      numerm.depth = numerm.depth < dStrut ? dStrut : numerm.depth;
    }
    newOptions = options.havingStyle(dstyle);
    var denomm = buildGroup$1(group.denom, newOptions, options);
    var rule;
    var ruleWidth;
    var ruleSpacing;
    if (group.hasBarLine) {
      if (group.barSize) {
        ruleWidth = calculateSize(group.barSize, options);
        rule = makeLineSpan("frac-line", options, ruleWidth);
      } else {
        rule = makeLineSpan("frac-line", options);
      }
      ruleWidth = rule.height;
      ruleSpacing = rule.height;
    } else {
      rule = null;
      ruleWidth = 0;
      ruleSpacing = options.fontMetrics().defaultRuleThickness;
    }
    var numShift;
    var clearance;
    var denomShift;
    if (style.size === Style$1.DISPLAY.size) {
      numShift = options.fontMetrics().num1;
      if (ruleWidth > 0) {
        clearance = 3 * ruleSpacing;
      } else {
        clearance = 7 * ruleSpacing;
      }
      denomShift = options.fontMetrics().denom1;
    } else {
      if (ruleWidth > 0) {
        numShift = options.fontMetrics().num2;
        clearance = ruleSpacing;
      } else {
        numShift = options.fontMetrics().num3;
        clearance = 3 * ruleSpacing;
      }
      denomShift = options.fontMetrics().denom2;
    }
    var frac;
    if (!rule) {
      var candidateClearance = numShift - numerm.depth - (denomm.height - denomShift);
      if (candidateClearance < clearance) {
        numShift += 0.5 * (clearance - candidateClearance);
        denomShift += 0.5 * (clearance - candidateClearance);
      }
      frac = makeVList({
        positionType: "individualShift",
        children: [{
          type: "elem",
          elem: denomm,
          shift: denomShift
        }, {
          type: "elem",
          elem: numerm,
          shift: -numShift
        }]
      });
    } else {
      var axisHeight = options.fontMetrics().axisHeight;
      if (numShift - numerm.depth - (axisHeight + 0.5 * ruleWidth) < clearance) {
        numShift += clearance - (numShift - numerm.depth - (axisHeight + 0.5 * ruleWidth));
      }
      if (axisHeight - 0.5 * ruleWidth - (denomm.height - denomShift) < clearance) {
        denomShift += clearance - (axisHeight - 0.5 * ruleWidth - (denomm.height - denomShift));
      }
      var midShift = -(axisHeight - 0.5 * ruleWidth);
      frac = makeVList({
        positionType: "individualShift",
        children: [{
          type: "elem",
          elem: denomm,
          shift: denomShift
        }, {
          type: "elem",
          elem: rule,
          shift: midShift
        }, {
          type: "elem",
          elem: numerm,
          shift: -numShift
        }]
      });
    }
    newOptions = options.havingStyle(style);
    frac.height *= newOptions.sizeMultiplier / options.sizeMultiplier;
    frac.depth *= newOptions.sizeMultiplier / options.sizeMultiplier;
    var delimSize;
    if (style.size === Style$1.DISPLAY.size) {
      delimSize = options.fontMetrics().delim1;
    } else if (style.size === Style$1.SCRIPTSCRIPT.size) {
      delimSize = options.havingStyle(Style$1.SCRIPT).fontMetrics().delim2;
    } else {
      delimSize = options.fontMetrics().delim2;
    }
    var leftDelim;
    var rightDelim;
    if (group.leftDelim == null) {
      leftDelim = makeNullDelimiter(options, ["mopen"]);
    } else {
      leftDelim = makeCustomSizedDelim(group.leftDelim, delimSize, true, options.havingStyle(style), group.mode, ["mopen"]);
    }
    if (group.continued) {
      rightDelim = makeSpan([]);
    } else if (group.rightDelim == null) {
      rightDelim = makeNullDelimiter(options, ["mclose"]);
    } else {
      rightDelim = makeCustomSizedDelim(group.rightDelim, delimSize, true, options.havingStyle(style), group.mode, ["mclose"]);
    }
    return makeSpan(["mord"].concat(newOptions.sizingClasses(options)), [leftDelim, makeSpan(["mfrac"], [frac]), rightDelim], options);
  };
  var mathmlBuilder$3 = (group, options) => {
    var node = new MathNode("mfrac", [buildGroup2(group.numer, options), buildGroup2(group.denom, options)]);
    if (!group.hasBarLine) {
      node.setAttribute("linethickness", "0px");
    } else if (group.barSize) {
      var ruleWidth = calculateSize(group.barSize, options);
      node.setAttribute("linethickness", makeEm(ruleWidth));
    }
    if (group.leftDelim != null || group.rightDelim != null) {
      var withDelims = [];
      if (group.leftDelim != null) {
        var leftOp = new MathNode("mo", [new TextNode(group.leftDelim.replace("\\", ""))]);
        leftOp.setAttribute("fence", "true");
        withDelims.push(leftOp);
      }
      withDelims.push(node);
      if (group.rightDelim != null) {
        var rightOp = new MathNode("mo", [new TextNode(group.rightDelim.replace("\\", ""))]);
        rightOp.setAttribute("fence", "true");
        withDelims.push(rightOp);
      }
      return makeRow(withDelims);
    }
    return node;
  };
  var wrapWithStyle = (frac, style) => {
    if (!style) {
      return frac;
    }
    var wrapper = {
      type: "styling",
      mode: frac.mode,
      style,
      body: [frac]
    };
    return wrapper;
  };
  defineFunction({
    type: "genfrac",
    names: [
      "\\cfrac",
      "\\dfrac",
      "\\frac",
      "\\tfrac",
      "\\dbinom",
      "\\binom",
      "\\tbinom",
      "\\\\atopfrac",
      // can’t be entered directly
      "\\\\bracefrac",
      "\\\\brackfrac"
      // ditto
    ],
    props: {
      numArgs: 2,
      allowedInArgument: true
    },
    handler: (_ref, args) => {
      var {
        parser,
        funcName
      } = _ref;
      var numer = args[0];
      var denom = args[1];
      var hasBarLine;
      var leftDelim = null;
      var rightDelim = null;
      switch (funcName) {
        case "\\cfrac":
        case "\\dfrac":
        case "\\frac":
        case "\\tfrac":
          hasBarLine = true;
          break;
        case "\\\\atopfrac":
          hasBarLine = false;
          break;
        case "\\dbinom":
        case "\\binom":
        case "\\tbinom":
          hasBarLine = false;
          leftDelim = "(";
          rightDelim = ")";
          break;
        case "\\\\bracefrac":
          hasBarLine = false;
          leftDelim = "\\{";
          rightDelim = "\\}";
          break;
        case "\\\\brackfrac":
          hasBarLine = false;
          leftDelim = "[";
          rightDelim = "]";
          break;
        default:
          throw new Error("Unrecognized genfrac command");
      }
      var continued = funcName === "\\cfrac";
      var style = null;
      if (continued || funcName.startsWith("\\d")) {
        style = "display";
      } else if (funcName.startsWith("\\t")) {
        style = "text";
      }
      return wrapWithStyle({
        type: "genfrac",
        mode: parser.mode,
        numer,
        denom,
        continued,
        hasBarLine,
        leftDelim,
        rightDelim,
        barSize: null
      }, style);
    },
    htmlBuilder: htmlBuilder$4,
    mathmlBuilder: mathmlBuilder$3
  });
  defineFunction({
    type: "infix",
    names: ["\\over", "\\choose", "\\atop", "\\brace", "\\brack"],
    props: {
      numArgs: 0,
      infix: true
    },
    handler(_ref2) {
      var {
        parser,
        funcName,
        token
      } = _ref2;
      var replaceWith;
      switch (funcName) {
        case "\\over":
          replaceWith = "\\frac";
          break;
        case "\\choose":
          replaceWith = "\\binom";
          break;
        case "\\atop":
          replaceWith = "\\\\atopfrac";
          break;
        case "\\brace":
          replaceWith = "\\\\bracefrac";
          break;
        case "\\brack":
          replaceWith = "\\\\brackfrac";
          break;
        default:
          throw new Error("Unrecognized infix genfrac command");
      }
      return {
        type: "infix",
        mode: parser.mode,
        replaceWith,
        token
      };
    }
  });
  var stylArray = ["display", "text", "script", "scriptscript"];
  var delimFromValue = function delimFromValue2(delimString) {
    var delim = null;
    if (delimString.length > 0) {
      delim = delimString;
      delim = delim === "." ? null : delim;
    }
    return delim;
  };
  defineFunction({
    type: "genfrac",
    names: ["\\genfrac"],
    props: {
      numArgs: 6,
      allowedInArgument: true,
      argTypes: ["math", "math", "size", "text", "math", "math"]
    },
    handler(_ref3, args) {
      var {
        parser
      } = _ref3;
      var numer = args[4];
      var denom = args[5];
      var leftNode = normalizeArgument(args[0]);
      var leftDelim = leftNode.type === "atom" && leftNode.family === "open" ? delimFromValue(leftNode.text) : null;
      var rightNode = normalizeArgument(args[1]);
      var rightDelim = rightNode.type === "atom" && rightNode.family === "close" ? delimFromValue(rightNode.text) : null;
      var barNode = assertNodeType(args[2], "size");
      var hasBarLine;
      var barSize = null;
      if (barNode.isBlank) {
        hasBarLine = true;
      } else {
        barSize = barNode.value;
        hasBarLine = barSize.number > 0;
      }
      var size = null;
      var styl = args[3];
      if (styl.type === "ordgroup") {
        if (styl.body.length > 0) {
          var textOrd = assertNodeType(styl.body[0], "textord");
          size = stylArray[Number(textOrd.text)];
        }
      } else {
        styl = assertNodeType(styl, "textord");
        size = stylArray[Number(styl.text)];
      }
      return wrapWithStyle({
        type: "genfrac",
        mode: parser.mode,
        numer,
        denom,
        continued: false,
        hasBarLine,
        barSize,
        leftDelim,
        rightDelim
      }, size);
    }
  });
  defineFunction({
    type: "infix",
    names: ["\\above"],
    props: {
      numArgs: 1,
      argTypes: ["size"],
      infix: true
    },
    handler(_ref4, args) {
      var {
        parser,
        funcName,
        token
      } = _ref4;
      return {
        type: "infix",
        mode: parser.mode,
        replaceWith: "\\\\abovefrac",
        size: assertNodeType(args[0], "size").value,
        token
      };
    }
  });
  defineFunction({
    type: "genfrac",
    names: ["\\\\abovefrac"],
    props: {
      numArgs: 3,
      argTypes: ["math", "size", "math"]
    },
    handler: (_ref5, args) => {
      var {
        parser,
        funcName
      } = _ref5;
      var numer = args[0];
      var barSize = assertNodeType(args[1], "infix").size;
      if (!barSize) {
        throw new Error("\\\\abovefrac expected size, but got " + String(barSize));
      }
      var denom = args[2];
      var hasBarLine = barSize.number > 0;
      return {
        type: "genfrac",
        mode: parser.mode,
        numer,
        denom,
        continued: false,
        hasBarLine,
        barSize,
        leftDelim: null,
        rightDelim: null
      };
    }
  });
  var htmlBuilder$3 = (grp, options) => {
    var style = options.style;
    var supSubGroup;
    var group;
    if (grp.type === "supsub") {
      supSubGroup = grp.sup ? buildGroup$1(grp.sup, options.havingStyle(style.sup()), options) : buildGroup$1(grp.sub, options.havingStyle(style.sub()), options);
      group = assertNodeType(grp.base, "horizBrace");
    } else {
      group = assertNodeType(grp, "horizBrace");
    }
    var body = buildGroup$1(group.base, options.havingBaseStyle(Style$1.DISPLAY));
    var braceBody = stretchySvg(group, options);
    var vlist;
    if (group.isOver) {
      vlist = makeVList({
        positionType: "firstBaseline",
        children: [{
          type: "elem",
          elem: body
        }, {
          type: "kern",
          size: 0.1
        }, {
          type: "elem",
          elem: braceBody,
          wrapperClasses: ["svg-align"]
        }]
      });
    } else {
      vlist = makeVList({
        positionType: "bottom",
        positionData: body.depth + 0.1 + braceBody.height,
        children: [{
          type: "elem",
          elem: braceBody,
          wrapperClasses: ["svg-align"]
        }, {
          type: "kern",
          size: 0.1
        }, {
          type: "elem",
          elem: body
        }]
      });
    }
    if (supSubGroup) {
      var vSpan = makeSpan(["minner", group.isOver ? "mover" : "munder"], [vlist], options);
      if (group.isOver) {
        vlist = makeVList({
          positionType: "firstBaseline",
          children: [{
            type: "elem",
            elem: vSpan
          }, {
            type: "kern",
            size: 0.2
          }, {
            type: "elem",
            elem: supSubGroup
          }]
        });
      } else {
        vlist = makeVList({
          positionType: "bottom",
          positionData: vSpan.depth + 0.2 + supSubGroup.height + supSubGroup.depth,
          children: [{
            type: "elem",
            elem: supSubGroup
          }, {
            type: "kern",
            size: 0.2
          }, {
            type: "elem",
            elem: vSpan
          }]
        });
      }
    }
    return makeSpan(["minner", group.isOver ? "mover" : "munder"], [vlist], options);
  };
  var mathmlBuilder$2 = (group, options) => {
    var accentNode = stretchyMathML(group.label);
    return new MathNode(group.isOver ? "mover" : "munder", [buildGroup2(group.base, options), accentNode]);
  };
  defineFunction({
    type: "horizBrace",
    names: ["\\overbrace", "\\underbrace", "\\overbracket", "\\underbracket"],
    props: {
      numArgs: 1
    },
    handler(_ref, args) {
      var {
        parser,
        funcName
      } = _ref;
      return {
        type: "horizBrace",
        mode: parser.mode,
        label: funcName,
        isOver: funcName.includes("\\over"),
        base: args[0]
      };
    },
    htmlBuilder: htmlBuilder$3,
    mathmlBuilder: mathmlBuilder$2
  });
  defineFunction({
    type: "href",
    names: ["\\href"],
    props: {
      numArgs: 2,
      argTypes: ["url", "original"],
      allowedInText: true
    },
    handler: (_ref, args) => {
      var {
        parser
      } = _ref;
      var body = args[1];
      var href = assertNodeType(args[0], "url").url;
      if (!parser.settings.isTrusted({
        command: "\\href",
        url: href
      })) {
        return parser.formatUnsupportedCmd("\\href");
      }
      return {
        type: "href",
        mode: parser.mode,
        href,
        body: ordargument(body)
      };
    },
    htmlBuilder: (group, options) => {
      var elements = buildExpression$1(group.body, options, false);
      return makeAnchor(group.href, [], elements, options);
    },
    mathmlBuilder: (group, options) => {
      var math2 = buildExpressionRow(group.body, options);
      if (!(math2 instanceof MathNode)) {
        math2 = new MathNode("mrow", [math2]);
      }
      math2.setAttribute("href", group.href);
      return math2;
    }
  });
  defineFunction({
    type: "href",
    names: ["\\url"],
    props: {
      numArgs: 1,
      argTypes: ["url"],
      allowedInText: true
    },
    handler: (_ref2, args) => {
      var {
        parser
      } = _ref2;
      var href = assertNodeType(args[0], "url").url;
      if (!parser.settings.isTrusted({
        command: "\\url",
        url: href
      })) {
        return parser.formatUnsupportedCmd("\\url");
      }
      var chars = [];
      for (var i3 = 0; i3 < href.length; i3++) {
        var c2 = href[i3];
        if (c2 === "~") {
          c2 = "\\textasciitilde";
        }
        chars.push({
          type: "textord",
          mode: "text",
          text: c2
        });
      }
      var body = {
        type: "text",
        mode: parser.mode,
        font: "\\texttt",
        body: chars
      };
      return {
        type: "href",
        mode: parser.mode,
        href,
        body: ordargument(body)
      };
    }
  });
  defineFunction({
    type: "hbox",
    names: ["\\hbox"],
    props: {
      numArgs: 1,
      argTypes: ["text"],
      allowedInText: true,
      primitive: true
    },
    handler(_ref, args) {
      var {
        parser
      } = _ref;
      return {
        type: "hbox",
        mode: parser.mode,
        body: ordargument(args[0])
      };
    },
    htmlBuilder(group, options) {
      var elements = buildExpression$1(group.body, options.withFont(""), false);
      return makeFragment(elements);
    },
    mathmlBuilder(group, options) {
      return new MathNode("mrow", buildExpression2(group.body, options.withFont("")));
    }
  });
  defineFunction({
    type: "html",
    names: ["\\htmlClass", "\\htmlId", "\\htmlStyle", "\\htmlData"],
    props: {
      numArgs: 2,
      argTypes: ["raw", "original"],
      allowedInText: true
    },
    handler: (_ref, args) => {
      var {
        parser,
        funcName,
        token
      } = _ref;
      var value = assertNodeType(args[0], "raw").string;
      var body = args[1];
      if (parser.settings.strict) {
        parser.settings.reportNonstrict("htmlExtension", "HTML extension is disabled on strict mode");
      }
      var trustContext;
      var attributes = {};
      switch (funcName) {
        case "\\htmlClass":
          attributes.class = value;
          trustContext = {
            command: "\\htmlClass",
            class: value
          };
          break;
        case "\\htmlId":
          attributes.id = value;
          trustContext = {
            command: "\\htmlId",
            id: value
          };
          break;
        case "\\htmlStyle":
          attributes.style = value;
          trustContext = {
            command: "\\htmlStyle",
            style: value
          };
          break;
        case "\\htmlData": {
          var data = value.split(",");
          for (var i3 = 0; i3 < data.length; i3++) {
            var item = data[i3];
            var firstEquals = item.indexOf("=");
            if (firstEquals < 0) {
              throw new ParseError("\\htmlData key/value '" + item + "' missing equals sign");
            }
            var key = item.slice(0, firstEquals);
            var _value = item.slice(firstEquals + 1);
            attributes["data-" + key.trim()] = _value;
          }
          trustContext = {
            command: "\\htmlData",
            attributes
          };
          break;
        }
        default:
          throw new Error("Unrecognized html command");
      }
      if (!parser.settings.isTrusted(trustContext)) {
        return parser.formatUnsupportedCmd(funcName);
      }
      return {
        type: "html",
        mode: parser.mode,
        attributes,
        body: ordargument(body)
      };
    },
    htmlBuilder: (group, options) => {
      var elements = buildExpression$1(group.body, options, false);
      var classes = ["enclosing"];
      if (group.attributes.class) {
        classes.push(...group.attributes.class.trim().split(/\s+/));
      }
      var span = makeSpan(classes, elements, options);
      for (var attr in group.attributes) {
        if (attr !== "class" && group.attributes.hasOwnProperty(attr)) {
          span.setAttribute(attr, group.attributes[attr]);
        }
      }
      return span;
    },
    mathmlBuilder: (group, options) => {
      return buildExpressionRow(group.body, options);
    }
  });
  defineFunction({
    type: "htmlmathml",
    names: ["\\html@mathml"],
    props: {
      numArgs: 2,
      allowedInArgument: true,
      allowedInText: true
    },
    handler: (_ref, args) => {
      var {
        parser
      } = _ref;
      return {
        type: "htmlmathml",
        mode: parser.mode,
        html: ordargument(args[0]),
        mathml: ordargument(args[1])
      };
    },
    htmlBuilder: (group, options) => {
      var elements = buildExpression$1(group.html, options, false);
      return makeFragment(elements);
    },
    mathmlBuilder: (group, options) => {
      return buildExpressionRow(group.mathml, options);
    }
  });
  var sizeData = function sizeData2(str) {
    if (/^[-+]? *(\d+(\.\d*)?|\.\d+)$/.test(str)) {
      return {
        number: +str,
        unit: "bp"
      };
    } else {
      var match = /([-+]?) *(\d+(?:\.\d*)?|\.\d+) *([a-z]{2})/.exec(str);
      if (!match) {
        throw new ParseError("Invalid size: '" + str + "' in \\includegraphics");
      }
      var data = {
        number: +(match[1] + match[2]),
        // sign + magnitude, cast to number
        unit: match[3]
      };
      if (!validUnit(data)) {
        throw new ParseError("Invalid unit: '" + data.unit + "' in \\includegraphics.");
      }
      return data;
    }
  };
  defineFunction({
    type: "includegraphics",
    names: ["\\includegraphics"],
    props: {
      numArgs: 1,
      numOptionalArgs: 1,
      argTypes: ["raw", "url"],
      allowedInText: false
    },
    handler: (_ref, args, optArgs) => {
      var {
        parser
      } = _ref;
      var width = {
        number: 0,
        unit: "em"
      };
      var height = {
        number: 0.9,
        unit: "em"
      };
      var totalheight = {
        number: 0,
        unit: "em"
      };
      var alt = "";
      if (optArgs[0]) {
        var attributeStr = assertNodeType(optArgs[0], "raw").string;
        var attributes = attributeStr.split(",");
        for (var i3 = 0; i3 < attributes.length; i3++) {
          var keyVal = attributes[i3].split("=");
          if (keyVal.length === 2) {
            var str = keyVal[1].trim();
            switch (keyVal[0].trim()) {
              case "alt":
                alt = str;
                break;
              case "width":
                width = sizeData(str);
                break;
              case "height":
                height = sizeData(str);
                break;
              case "totalheight":
                totalheight = sizeData(str);
                break;
              default:
                throw new ParseError("Invalid key: '" + keyVal[0] + "' in \\includegraphics.");
            }
          }
        }
      }
      var src = assertNodeType(args[0], "url").url;
      if (alt === "") {
        alt = src;
        alt = alt.replace(/^.*[\\/]/, "");
        alt = alt.substring(0, alt.lastIndexOf("."));
      }
      if (!parser.settings.isTrusted({
        command: "\\includegraphics",
        url: src
      })) {
        return parser.formatUnsupportedCmd("\\includegraphics");
      }
      return {
        type: "includegraphics",
        mode: parser.mode,
        alt,
        width,
        height,
        totalheight,
        src
      };
    },
    htmlBuilder: (group, options) => {
      var height = calculateSize(group.height, options);
      var depth = 0;
      if (group.totalheight.number > 0) {
        depth = calculateSize(group.totalheight, options) - height;
      }
      var width = 0;
      if (group.width.number > 0) {
        width = calculateSize(group.width, options);
      }
      var style = {
        height: makeEm(height + depth)
      };
      if (width > 0) {
        style.width = makeEm(width);
      }
      if (depth > 0) {
        style.verticalAlign = makeEm(-depth);
      }
      var node = new Img(group.src, group.alt, style);
      node.height = height;
      node.depth = depth;
      return node;
    },
    mathmlBuilder: (group, options) => {
      var node = new MathNode("mglyph", []);
      node.setAttribute("alt", group.alt);
      var height = calculateSize(group.height, options);
      var depth = 0;
      if (group.totalheight.number > 0) {
        depth = calculateSize(group.totalheight, options) - height;
        node.setAttribute("valign", makeEm(-depth));
      }
      node.setAttribute("height", makeEm(height + depth));
      if (group.width.number > 0) {
        var width = calculateSize(group.width, options);
        node.setAttribute("width", makeEm(width));
      }
      node.setAttribute("src", group.src);
      return node;
    }
  });
  defineFunction({
    type: "kern",
    names: ["\\kern", "\\mkern", "\\hskip", "\\mskip"],
    props: {
      numArgs: 1,
      argTypes: ["size"],
      primitive: true,
      allowedInText: true
    },
    handler(_ref, args) {
      var {
        parser,
        funcName
      } = _ref;
      var size = assertNodeType(args[0], "size");
      if (parser.settings.strict) {
        var mathFunction = funcName[1] === "m";
        var muUnit = size.value.unit === "mu";
        if (mathFunction) {
          if (!muUnit) {
            parser.settings.reportNonstrict("mathVsTextUnits", "LaTeX's " + funcName + " supports only mu units, " + ("not " + size.value.unit + " units"));
          }
          if (parser.mode !== "math") {
            parser.settings.reportNonstrict("mathVsTextUnits", "LaTeX's " + funcName + " works only in math mode");
          }
        } else {
          if (muUnit) {
            parser.settings.reportNonstrict("mathVsTextUnits", "LaTeX's " + funcName + " doesn't support mu units");
          }
        }
      }
      return {
        type: "kern",
        mode: parser.mode,
        dimension: size.value
      };
    },
    htmlBuilder(group, options) {
      return makeGlue(group.dimension, options);
    },
    mathmlBuilder(group, options) {
      var dimension = calculateSize(group.dimension, options);
      return new SpaceNode(dimension);
    }
  });
  defineFunction({
    type: "lap",
    names: ["\\mathllap", "\\mathrlap", "\\mathclap"],
    props: {
      numArgs: 1,
      allowedInText: true
    },
    handler: (_ref, args) => {
      var {
        parser,
        funcName
      } = _ref;
      var body = args[0];
      return {
        type: "lap",
        mode: parser.mode,
        alignment: funcName.slice(5),
        body
      };
    },
    htmlBuilder: (group, options) => {
      var inner2;
      if (group.alignment === "clap") {
        inner2 = makeSpan([], [buildGroup$1(group.body, options)]);
        inner2 = makeSpan(["inner"], [inner2], options);
      } else {
        inner2 = makeSpan(["inner"], [buildGroup$1(group.body, options)]);
      }
      var fix = makeSpan(["fix"], []);
      var node = makeSpan([group.alignment], [inner2, fix], options);
      var strut = makeSpan(["strut"]);
      strut.style.height = makeEm(node.height + node.depth);
      if (node.depth) {
        strut.style.verticalAlign = makeEm(-node.depth);
      }
      node.children.unshift(strut);
      node = makeSpan(["thinbox"], [node], options);
      return makeSpan(["mord", "vbox"], [node], options);
    },
    mathmlBuilder: (group, options) => {
      var node = new MathNode("mpadded", [buildGroup2(group.body, options)]);
      if (group.alignment !== "rlap") {
        var offset = group.alignment === "llap" ? "-1" : "-0.5";
        node.setAttribute("lspace", offset + "width");
      }
      node.setAttribute("width", "0px");
      return node;
    }
  });
  defineFunction({
    type: "styling",
    names: ["\\(", "$"],
    props: {
      numArgs: 0,
      allowedInText: true,
      allowedInMath: false
    },
    handler(_ref, args) {
      var {
        funcName,
        parser
      } = _ref;
      var outerMode = parser.mode;
      parser.switchMode("math");
      var close2 = funcName === "\\(" ? "\\)" : "$";
      var body = parser.parseExpression(false, close2);
      parser.expect(close2);
      parser.switchMode(outerMode);
      return {
        type: "styling",
        mode: parser.mode,
        style: "text",
        resetFont: true,
        body
      };
    }
  });
  defineFunction({
    type: "text",
    // Doesn't matter what this is.
    names: ["\\)", "\\]"],
    props: {
      numArgs: 0,
      allowedInText: true,
      allowedInMath: false
    },
    handler(context, args) {
      throw new ParseError("Mismatched " + context.funcName);
    }
  });
  var chooseMathStyle = (group, options) => {
    switch (options.style.size) {
      case Style$1.DISPLAY.size:
        return group.display;
      case Style$1.TEXT.size:
        return group.text;
      case Style$1.SCRIPT.size:
        return group.script;
      case Style$1.SCRIPTSCRIPT.size:
        return group.scriptscript;
      default:
        return group.text;
    }
  };
  defineFunction({
    type: "mathchoice",
    names: ["\\mathchoice"],
    props: {
      numArgs: 4,
      primitive: true
    },
    handler: (_ref, args) => {
      var {
        parser
      } = _ref;
      return {
        type: "mathchoice",
        mode: parser.mode,
        display: ordargument(args[0]),
        text: ordargument(args[1]),
        script: ordargument(args[2]),
        scriptscript: ordargument(args[3])
      };
    },
    htmlBuilder: (group, options) => {
      var body = chooseMathStyle(group, options);
      var elements = buildExpression$1(body, options, false);
      return makeFragment(elements);
    },
    mathmlBuilder: (group, options) => {
      var body = chooseMathStyle(group, options);
      return buildExpressionRow(body, options);
    }
  });
  var assembleSupSub = (base, supGroup, subGroup, options, style, slant, baseShift) => {
    base = makeSpan([], [base]);
    var subIsSingleCharacter = subGroup && isCharacterBox(subGroup);
    var sub2;
    var sup2;
    if (supGroup) {
      var elem = buildGroup$1(supGroup, options.havingStyle(style.sup()), options);
      sup2 = {
        elem,
        kern: Math.max(options.fontMetrics().bigOpSpacing1, options.fontMetrics().bigOpSpacing3 - elem.depth)
      };
    }
    if (subGroup) {
      var _elem = buildGroup$1(subGroup, options.havingStyle(style.sub()), options);
      sub2 = {
        elem: _elem,
        kern: Math.max(options.fontMetrics().bigOpSpacing2, options.fontMetrics().bigOpSpacing4 - _elem.height)
      };
    }
    var finalGroup;
    if (sup2 && sub2) {
      var bottom = options.fontMetrics().bigOpSpacing5 + sub2.elem.height + sub2.elem.depth + sub2.kern + base.depth + baseShift;
      finalGroup = makeVList({
        positionType: "bottom",
        positionData: bottom,
        children: [{
          type: "kern",
          size: options.fontMetrics().bigOpSpacing5
        }, {
          type: "elem",
          elem: sub2.elem,
          marginLeft: makeEm(-slant)
        }, {
          type: "kern",
          size: sub2.kern
        }, {
          type: "elem",
          elem: base
        }, {
          type: "kern",
          size: sup2.kern
        }, {
          type: "elem",
          elem: sup2.elem,
          marginLeft: makeEm(slant)
        }, {
          type: "kern",
          size: options.fontMetrics().bigOpSpacing5
        }]
      });
    } else if (sub2) {
      var top = base.height - baseShift;
      finalGroup = makeVList({
        positionType: "top",
        positionData: top,
        children: [{
          type: "kern",
          size: options.fontMetrics().bigOpSpacing5
        }, {
          type: "elem",
          elem: sub2.elem,
          marginLeft: makeEm(-slant)
        }, {
          type: "kern",
          size: sub2.kern
        }, {
          type: "elem",
          elem: base
        }]
      });
    } else if (sup2) {
      var _bottom = base.depth + baseShift;
      finalGroup = makeVList({
        positionType: "bottom",
        positionData: _bottom,
        children: [{
          type: "elem",
          elem: base
        }, {
          type: "kern",
          size: sup2.kern
        }, {
          type: "elem",
          elem: sup2.elem,
          marginLeft: makeEm(slant)
        }, {
          type: "kern",
          size: options.fontMetrics().bigOpSpacing5
        }]
      });
    } else {
      return base;
    }
    var parts = [finalGroup];
    if (sub2 && slant !== 0 && !subIsSingleCharacter) {
      var spacer = makeSpan(["mspace"], [], options);
      spacer.style.marginRight = makeEm(slant);
      parts.unshift(spacer);
    }
    return makeSpan(["mop", "op-limits"], parts, options);
  };
  var noSuccessor = /* @__PURE__ */ new Set(["\\smallint"]);
  var htmlBuilder$2 = (grp, options) => {
    var supGroup;
    var subGroup;
    var hasLimits = false;
    var group;
    if (grp.type === "supsub") {
      supGroup = grp.sup;
      subGroup = grp.sub;
      group = assertNodeType(grp.base, "op");
      hasLimits = true;
    } else {
      group = assertNodeType(grp, "op");
    }
    var style = options.style;
    var large = false;
    if (style.size === Style$1.DISPLAY.size && group.symbol && !noSuccessor.has(group.name)) {
      large = true;
    }
    var base;
    var symbolItalic;
    if (group.symbol) {
      var fontName = large ? "Size2-Regular" : "Size1-Regular";
      var stash = "";
      if (group.name === "\\oiint" || group.name === "\\oiiint") {
        stash = group.name.slice(1);
        group.name = stash === "oiint" ? "\\iint" : "\\iiint";
      }
      base = makeSymbol(group.name, fontName, "math", options, ["mop", "op-symbol", large ? "large-op" : "small-op"]);
      symbolItalic = base.italic;
      if (stash.length > 0) {
        var oval = staticSvg(stash + "Size" + (large ? "2" : "1"), options);
        base = makeVList({
          positionType: "individualShift",
          children: [{
            type: "elem",
            elem: base,
            shift: 0
          }, {
            type: "elem",
            elem: oval,
            shift: large ? 0.08 : 0
          }]
        });
        group.name = "\\" + stash;
        base.classes.unshift("mop");
        base.italic = symbolItalic;
      }
    } else if (group.body) {
      var inner2 = buildExpression$1(group.body, options, true);
      if (inner2.length === 1 && inner2[0] instanceof SymbolNode) {
        base = inner2[0];
        base.classes[0] = "mop";
      } else {
        base = makeSpan(["mop"], inner2, options);
      }
    } else {
      var output = [];
      for (var i3 = 1; i3 < group.name.length; i3++) {
        output.push(mathsym(group.name[i3], group.mode, options));
      }
      base = makeSpan(["mop"], output, options);
    }
    var baseShift = 0;
    var slant = 0;
    if ((base instanceof SymbolNode || group.name === "\\oiint" || group.name === "\\oiiint") && !group.suppressBaseShift) {
      var _base$italic;
      baseShift = (base.height - base.depth) / 2 - options.fontMetrics().axisHeight;
      slant = (_base$italic = base.italic) != null ? _base$italic : 0;
    }
    if (hasLimits) {
      return assembleSupSub(base, supGroup, subGroup, options, style, slant, baseShift);
    } else {
      if (baseShift) {
        base.style.position = "relative";
        base.style.top = makeEm(baseShift);
      }
      return base;
    }
  };
  var mathmlBuilder$1 = (group, options) => {
    var node;
    if (group.symbol) {
      node = new MathNode("mo", [makeText(group.name, group.mode)]);
      if (noSuccessor.has(group.name)) {
        node.setAttribute("largeop", "false");
      }
    } else if (group.body) {
      node = new MathNode("mo", buildExpression2(group.body, options));
    } else {
      node = new MathNode("mi", [new TextNode(group.name.slice(1))]);
      var operator = new MathNode("mo", [makeText("\u2061", "text")]);
      if (group.parentIsSupSub) {
        node = new MathNode("mrow", [node, operator]);
      } else {
        node = newDocumentFragment([node, operator]);
      }
    }
    return node;
  };
  var singleCharBigOps = {
    "\u220F": "\\prod",
    "\u2210": "\\coprod",
    "\u2211": "\\sum",
    "\u22C0": "\\bigwedge",
    "\u22C1": "\\bigvee",
    "\u22C2": "\\bigcap",
    "\u22C3": "\\bigcup",
    "\u2A00": "\\bigodot",
    "\u2A01": "\\bigoplus",
    "\u2A02": "\\bigotimes",
    "\u2A04": "\\biguplus",
    "\u2A06": "\\bigsqcup"
  };
  defineFunction({
    type: "op",
    names: ["\\coprod", "\\bigvee", "\\bigwedge", "\\biguplus", "\\bigcap", "\\bigcup", "\\intop", "\\prod", "\\sum", "\\bigotimes", "\\bigoplus", "\\bigodot", "\\bigsqcup", "\\smallint", "\u220F", "\u2210", "\u2211", "\u22C0", "\u22C1", "\u22C2", "\u22C3", "\u2A00", "\u2A01", "\u2A02", "\u2A04", "\u2A06"],
    props: {
      numArgs: 0
    },
    handler: (_ref, args) => {
      var {
        parser,
        funcName
      } = _ref;
      var fName = funcName;
      if (fName.length === 1) {
        fName = singleCharBigOps[fName];
      }
      return {
        type: "op",
        mode: parser.mode,
        limits: true,
        parentIsSupSub: false,
        symbol: true,
        name: fName
      };
    },
    htmlBuilder: htmlBuilder$2,
    mathmlBuilder: mathmlBuilder$1
  });
  defineFunction({
    type: "op",
    names: ["\\mathop"],
    props: {
      numArgs: 1,
      primitive: true
    },
    handler: (_ref2, args) => {
      var {
        parser
      } = _ref2;
      var body = args[0];
      return {
        type: "op",
        mode: parser.mode,
        limits: false,
        parentIsSupSub: false,
        symbol: false,
        body: ordargument(body)
      };
    },
    htmlBuilder: htmlBuilder$2,
    mathmlBuilder: mathmlBuilder$1
  });
  var singleCharIntegrals = {
    "\u222B": "\\int",
    "\u222C": "\\iint",
    "\u222D": "\\iiint",
    "\u222E": "\\oint",
    "\u222F": "\\oiint",
    "\u2230": "\\oiiint"
  };
  defineFunction({
    type: "op",
    names: ["\\arcsin", "\\arccos", "\\arctan", "\\arctg", "\\arcctg", "\\arg", "\\ch", "\\cos", "\\cosec", "\\cosh", "\\cot", "\\cotg", "\\coth", "\\csc", "\\ctg", "\\cth", "\\deg", "\\dim", "\\exp", "\\hom", "\\ker", "\\lg", "\\ln", "\\log", "\\sec", "\\sin", "\\sinh", "\\sh", "\\tan", "\\tanh", "\\tg", "\\th"],
    props: {
      numArgs: 0
    },
    handler(_ref3) {
      var {
        parser,
        funcName
      } = _ref3;
      return {
        type: "op",
        mode: parser.mode,
        limits: false,
        parentIsSupSub: false,
        symbol: false,
        name: funcName
      };
    },
    htmlBuilder: htmlBuilder$2,
    mathmlBuilder: mathmlBuilder$1
  });
  defineFunction({
    type: "op",
    names: ["\\det", "\\gcd", "\\inf", "\\lim", "\\max", "\\min", "\\Pr", "\\sup"],
    props: {
      numArgs: 0
    },
    handler(_ref4) {
      var {
        parser,
        funcName
      } = _ref4;
      return {
        type: "op",
        mode: parser.mode,
        limits: true,
        parentIsSupSub: false,
        symbol: false,
        name: funcName
      };
    },
    htmlBuilder: htmlBuilder$2,
    mathmlBuilder: mathmlBuilder$1
  });
  defineFunction({
    type: "op",
    names: ["\\int", "\\iint", "\\iiint", "\\oint", "\\oiint", "\\oiiint", "\u222B", "\u222C", "\u222D", "\u222E", "\u222F", "\u2230"],
    props: {
      numArgs: 0,
      allowedInArgument: true
    },
    handler(_ref5) {
      var {
        parser,
        funcName
      } = _ref5;
      var fName = funcName;
      if (fName.length === 1) {
        fName = singleCharIntegrals[fName];
      }
      return {
        type: "op",
        mode: parser.mode,
        limits: false,
        parentIsSupSub: false,
        symbol: true,
        name: fName
      };
    },
    htmlBuilder: htmlBuilder$2,
    mathmlBuilder: mathmlBuilder$1
  });
  var htmlBuilder$1 = (grp, options) => {
    var supGroup;
    var subGroup;
    var hasLimits = false;
    var group;
    if (grp.type === "supsub") {
      supGroup = grp.sup;
      subGroup = grp.sub;
      group = assertNodeType(grp.base, "operatorname");
      hasLimits = true;
    } else {
      group = assertNodeType(grp, "operatorname");
    }
    var base;
    if (group.body.length > 0) {
      var body = group.body.map((child2) => {
        var childText = "text" in child2 ? child2.text : void 0;
        if (typeof childText === "string") {
          return {
            type: "textord",
            mode: child2.mode,
            text: childText
          };
        } else {
          return child2;
        }
      });
      var expression = buildExpression$1(body, options.withFont("mathrm"), true);
      for (var i3 = 0; i3 < expression.length; i3++) {
        var child = expression[i3];
        if (child instanceof SymbolNode) {
          child.text = child.text.replace(/\u2212/, "-").replace(/\u2217/, "*");
        }
      }
      base = makeSpan(["mop"], expression, options);
    } else {
      base = makeSpan(["mop"], [], options);
    }
    if (hasLimits) {
      return assembleSupSub(base, supGroup, subGroup, options, options.style, 0, 0);
    } else {
      return base;
    }
  };
  var mathmlBuilder2 = (group, options) => {
    var expression = buildExpression2(group.body, options.withFont("mathrm"));
    var isAllString = true;
    for (var i3 = 0; i3 < expression.length; i3++) {
      var node = expression[i3];
      if (node instanceof SpaceNode) ;
      else if (node instanceof MathNode) {
        switch (node.type) {
          case "mi":
          case "mn":
          case "mspace":
          case "mtext":
            break;
          // Do nothing yet.
          case "mo": {
            var child = node.children[0];
            if (node.children.length === 1 && child instanceof TextNode) {
              child.text = child.text.replace(/\u2212/, "-").replace(/\u2217/, "*");
            } else {
              isAllString = false;
            }
            break;
          }
          default:
            isAllString = false;
        }
      } else {
        isAllString = false;
      }
    }
    if (isAllString) {
      var word = expression.map((node2) => node2.toText()).join("");
      expression = [new TextNode(word)];
    }
    var identifier = new MathNode("mi", expression);
    identifier.setAttribute("mathvariant", "normal");
    var operator = new MathNode("mo", [makeText("\u2061", "text")]);
    if (group.parentIsSupSub) {
      return new MathNode("mrow", [identifier, operator]);
    } else {
      return newDocumentFragment([identifier, operator]);
    }
  };
  defineFunction({
    type: "operatorname",
    names: ["\\operatorname@", "\\operatornamewithlimits"],
    props: {
      numArgs: 1
    },
    handler: (_ref, args) => {
      var {
        parser,
        funcName
      } = _ref;
      var body = args[0];
      return {
        type: "operatorname",
        mode: parser.mode,
        body: ordargument(body),
        alwaysHandleSupSub: funcName === "\\operatornamewithlimits",
        limits: false,
        parentIsSupSub: false
      };
    },
    htmlBuilder: htmlBuilder$1,
    mathmlBuilder: mathmlBuilder2
  });
  defineMacro("\\operatorname", "\\@ifstar\\operatornamewithlimits\\operatorname@");
  defineFunctionBuilders({
    type: "ordgroup",
    htmlBuilder(group, options) {
      if (group.semisimple) {
        return makeFragment(buildExpression$1(group.body, options, false));
      }
      return makeSpan(["mord"], buildExpression$1(group.body, options, true), options);
    },
    mathmlBuilder(group, options) {
      return buildExpressionRow(group.body, options, true);
    }
  });
  defineFunction({
    type: "overline",
    names: ["\\overline"],
    props: {
      numArgs: 1
    },
    handler(_ref, args) {
      var {
        parser
      } = _ref;
      var body = args[0];
      return {
        type: "overline",
        mode: parser.mode,
        body
      };
    },
    htmlBuilder(group, options) {
      var innerGroup = buildGroup$1(group.body, options.havingCrampedStyle());
      var line = makeLineSpan("overline-line", options);
      var defaultRuleThickness = options.fontMetrics().defaultRuleThickness;
      var vlist = makeVList({
        positionType: "firstBaseline",
        children: [{
          type: "elem",
          elem: innerGroup
        }, {
          type: "kern",
          size: 3 * defaultRuleThickness
        }, {
          type: "elem",
          elem: line
        }, {
          type: "kern",
          size: defaultRuleThickness
        }]
      });
      return makeSpan(["mord", "overline"], [vlist], options);
    },
    mathmlBuilder(group, options) {
      var operator = new MathNode("mo", [new TextNode("\u203E")]);
      operator.setAttribute("stretchy", "true");
      var node = new MathNode("mover", [buildGroup2(group.body, options), operator]);
      node.setAttribute("accent", "true");
      return node;
    }
  });
  defineFunction({
    type: "phantom",
    names: ["\\phantom"],
    props: {
      numArgs: 1,
      allowedInText: true
    },
    handler: (_ref, args) => {
      var {
        parser
      } = _ref;
      var body = args[0];
      return {
        type: "phantom",
        mode: parser.mode,
        body: ordargument(body)
      };
    },
    htmlBuilder: (group, options) => {
      var elements = buildExpression$1(group.body, options.withPhantom(), false);
      return makeFragment(elements);
    },
    mathmlBuilder: (group, options) => {
      var inner2 = buildExpression2(group.body, options);
      return new MathNode("mphantom", inner2);
    }
  });
  defineMacro("\\hphantom", "\\smash{\\phantom{#1}}");
  defineFunction({
    type: "vphantom",
    names: ["\\vphantom"],
    props: {
      numArgs: 1,
      allowedInText: true
    },
    handler: (_ref2, args) => {
      var {
        parser
      } = _ref2;
      var body = args[0];
      return {
        type: "vphantom",
        mode: parser.mode,
        body
      };
    },
    htmlBuilder: (group, options) => {
      var inner2 = makeSpan(["inner"], [buildGroup$1(group.body, options.withPhantom())]);
      var fix = makeSpan(["fix"], []);
      return makeSpan(["mord", "rlap"], [inner2, fix], options);
    },
    mathmlBuilder: (group, options) => {
      var inner2 = buildExpression2(ordargument(group.body), options);
      var phantom = new MathNode("mphantom", inner2);
      var node = new MathNode("mpadded", [phantom]);
      node.setAttribute("width", "0px");
      return node;
    }
  });
  defineFunction({
    type: "raisebox",
    names: ["\\raisebox"],
    props: {
      numArgs: 2,
      argTypes: ["size", "hbox"],
      allowedInText: true
    },
    handler(_ref, args) {
      var {
        parser
      } = _ref;
      var amount = assertNodeType(args[0], "size").value;
      var body = args[1];
      return {
        type: "raisebox",
        mode: parser.mode,
        dy: amount,
        body
      };
    },
    htmlBuilder(group, options) {
      var body = buildGroup$1(group.body, options);
      var dy = calculateSize(group.dy, options);
      return makeVList({
        positionType: "shift",
        positionData: -dy,
        children: [{
          type: "elem",
          elem: body
        }]
      });
    },
    mathmlBuilder(group, options) {
      var node = new MathNode("mpadded", [buildGroup2(group.body, options)]);
      var dy = group.dy.number + group.dy.unit;
      node.setAttribute("voffset", dy);
      return node;
    }
  });
  defineFunction({
    type: "internal",
    names: ["\\relax"],
    props: {
      numArgs: 0,
      allowedInText: true,
      allowedInArgument: true
    },
    handler(_ref) {
      var {
        parser
      } = _ref;
      return {
        type: "internal",
        mode: parser.mode
      };
    }
  });
  defineFunction({
    type: "rule",
    names: ["\\rule"],
    props: {
      numArgs: 2,
      numOptionalArgs: 1,
      allowedInText: true,
      allowedInMath: true,
      argTypes: ["size", "size", "size"]
    },
    handler(_ref, args, optArgs) {
      var {
        parser
      } = _ref;
      var shift = optArgs[0];
      var width = assertNodeType(args[0], "size");
      var height = assertNodeType(args[1], "size");
      return {
        type: "rule",
        mode: parser.mode,
        shift: shift && assertNodeType(shift, "size").value,
        width: width.value,
        height: height.value
      };
    },
    htmlBuilder(group, options) {
      var rule = makeSpan(["mord", "rule"], [], options);
      var width = calculateSize(group.width, options);
      var height = calculateSize(group.height, options);
      var shift = group.shift ? calculateSize(group.shift, options) : 0;
      rule.style.borderRightWidth = makeEm(width);
      rule.style.borderTopWidth = makeEm(height);
      rule.style.bottom = makeEm(shift);
      rule.width = width;
      rule.height = height + shift;
      rule.depth = -shift;
      rule.maxFontSize = height * 1.125 * options.sizeMultiplier;
      return rule;
    },
    mathmlBuilder(group, options) {
      var width = calculateSize(group.width, options);
      var height = calculateSize(group.height, options);
      var shift = group.shift ? calculateSize(group.shift, options) : 0;
      var color = options.color && options.getColor() || "black";
      var rule = new MathNode("mspace");
      rule.setAttribute("mathbackground", color);
      rule.setAttribute("width", makeEm(width));
      rule.setAttribute("height", makeEm(height));
      var wrapper = new MathNode("mpadded", [rule]);
      if (shift >= 0) {
        wrapper.setAttribute("height", makeEm(shift));
      } else {
        wrapper.setAttribute("height", makeEm(shift));
        wrapper.setAttribute("depth", makeEm(-shift));
      }
      wrapper.setAttribute("voffset", makeEm(shift));
      return wrapper;
    }
  });
  function sizingGroup(value, options, baseOptions) {
    var inner2 = buildExpression$1(value, options, false);
    var multiplier = options.sizeMultiplier / baseOptions.sizeMultiplier;
    for (var i3 = 0; i3 < inner2.length; i3++) {
      var pos = inner2[i3].classes.indexOf("sizing");
      if (pos < 0) {
        Array.prototype.push.apply(inner2[i3].classes, options.sizingClasses(baseOptions));
      } else if (inner2[i3].classes[pos + 1] === "reset-size" + options.size) {
        inner2[i3].classes[pos + 1] = "reset-size" + baseOptions.size;
      }
      inner2[i3].height *= multiplier;
      inner2[i3].depth *= multiplier;
    }
    return makeFragment(inner2);
  }
  var sizeFuncs = ["\\tiny", "\\sixptsize", "\\scriptsize", "\\footnotesize", "\\small", "\\normalsize", "\\large", "\\Large", "\\LARGE", "\\huge", "\\Huge"];
  var htmlBuilder2 = (group, options) => {
    var newOptions = options.havingSize(group.size);
    return sizingGroup(group.body, newOptions, options);
  };
  defineFunction({
    type: "sizing",
    names: sizeFuncs,
    props: {
      numArgs: 0,
      allowedInText: true
    },
    handler: (_ref, args) => {
      var {
        breakOnTokenText,
        funcName,
        parser
      } = _ref;
      var body = parser.parseExpression(false, breakOnTokenText);
      return {
        type: "sizing",
        mode: parser.mode,
        // Figure out what size to use based on the list of functions above
        size: sizeFuncs.indexOf(funcName) + 1,
        body
      };
    },
    htmlBuilder: htmlBuilder2,
    mathmlBuilder: (group, options) => {
      var newOptions = options.havingSize(group.size);
      var inner2 = buildExpression2(group.body, newOptions);
      var node = new MathNode("mstyle", inner2);
      node.setAttribute("mathsize", makeEm(newOptions.sizeMultiplier));
      return node;
    }
  });
  defineFunction({
    type: "smash",
    names: ["\\smash"],
    props: {
      numArgs: 1,
      numOptionalArgs: 1,
      allowedInText: true
    },
    handler: (_ref, args, optArgs) => {
      var {
        parser
      } = _ref;
      var smashHeight = false;
      var smashDepth = false;
      var tbArg = optArgs[0] && assertNodeType(optArgs[0], "ordgroup");
      if (tbArg) {
        var letter;
        for (var i3 = 0; i3 < tbArg.body.length; ++i3) {
          var node = tbArg.body[i3];
          letter = assertSymbolNodeType(node).text;
          if (letter === "t") {
            smashHeight = true;
          } else if (letter === "b") {
            smashDepth = true;
          } else {
            smashHeight = false;
            smashDepth = false;
            break;
          }
        }
      } else {
        smashHeight = true;
        smashDepth = true;
      }
      var body = args[0];
      return {
        type: "smash",
        mode: parser.mode,
        body,
        smashHeight,
        smashDepth
      };
    },
    htmlBuilder: (group, options) => {
      var node = makeSpan([], [buildGroup$1(group.body, options)]);
      if (!group.smashHeight && !group.smashDepth) {
        return node;
      }
      if (group.smashHeight) {
        node.height = 0;
      }
      if (group.smashDepth) {
        node.depth = 0;
      }
      if (group.smashHeight && group.smashDepth) {
        return makeSpan(["mord", "smash"], [node], options);
      }
      if (node.children) {
        for (var i3 = 0; i3 < node.children.length; i3++) {
          if (group.smashHeight) {
            node.children[i3].height = 0;
          }
          if (group.smashDepth) {
            node.children[i3].depth = 0;
          }
        }
      }
      var smashedNode = makeVList({
        positionType: "firstBaseline",
        children: [{
          type: "elem",
          elem: node
        }]
      });
      return makeSpan(["mord"], [smashedNode], options);
    },
    mathmlBuilder: (group, options) => {
      var node = new MathNode("mpadded", [buildGroup2(group.body, options)]);
      if (group.smashHeight) {
        node.setAttribute("height", "0px");
      }
      if (group.smashDepth) {
        node.setAttribute("depth", "0px");
      }
      return node;
    }
  });
  defineFunction({
    type: "sqrt",
    names: ["\\sqrt"],
    props: {
      numArgs: 1,
      numOptionalArgs: 1
    },
    handler(_ref, args, optArgs) {
      var {
        parser
      } = _ref;
      var index = optArgs[0];
      var body = args[0];
      return {
        type: "sqrt",
        mode: parser.mode,
        body,
        index
      };
    },
    htmlBuilder(group, options) {
      var inner2 = buildGroup$1(group.body, options.havingCrampedStyle());
      if (inner2.height === 0) {
        inner2.height = options.fontMetrics().xHeight;
      }
      inner2 = wrapFragment(inner2, options);
      var metrics = options.fontMetrics();
      var theta = metrics.defaultRuleThickness;
      var phi = theta;
      if (options.style.id < Style$1.TEXT.id) {
        phi = options.fontMetrics().xHeight;
      }
      var lineClearance = theta + phi / 4;
      var minDelimiterHeight = inner2.height + inner2.depth + lineClearance + theta;
      var {
        span: img,
        ruleWidth,
        advanceWidth
      } = makeSqrtImage(minDelimiterHeight, options);
      var delimDepth = img.height - ruleWidth;
      if (delimDepth > inner2.height + inner2.depth + lineClearance) {
        lineClearance = (lineClearance + delimDepth - inner2.height - inner2.depth) / 2;
      }
      var imgShift = img.height - inner2.height - lineClearance - ruleWidth;
      inner2.style.paddingLeft = makeEm(advanceWidth);
      var body = makeVList({
        positionType: "firstBaseline",
        children: [{
          type: "elem",
          elem: inner2,
          wrapperClasses: ["svg-align"]
        }, {
          type: "kern",
          size: -(inner2.height + imgShift)
        }, {
          type: "elem",
          elem: img
        }, {
          type: "kern",
          size: ruleWidth
        }]
      });
      if (!group.index) {
        return makeSpan(["mord", "sqrt"], [body], options);
      } else {
        var newOptions = options.havingStyle(Style$1.SCRIPTSCRIPT);
        var rootm = buildGroup$1(group.index, newOptions, options);
        var toShift = 0.6 * (body.height - body.depth);
        var rootVList = makeVList({
          positionType: "shift",
          positionData: -toShift,
          children: [{
            type: "elem",
            elem: rootm
          }]
        });
        var rootVListWrap = makeSpan(["root"], [rootVList]);
        return makeSpan(["mord", "sqrt"], [rootVListWrap, body], options);
      }
    },
    mathmlBuilder(group, options) {
      var {
        body,
        index
      } = group;
      return index ? new MathNode("mroot", [buildGroup2(body, options), buildGroup2(index, options)]) : new MathNode("msqrt", [buildGroup2(body, options)]);
    }
  });
  var styleMap = {
    "display": Style$1.DISPLAY,
    "text": Style$1.TEXT,
    "script": Style$1.SCRIPT,
    "scriptscript": Style$1.SCRIPTSCRIPT
  };
  function isStyleStr(s2) {
    return s2 in styleMap;
  }
  defineFunction({
    type: "styling",
    names: ["\\displaystyle", "\\textstyle", "\\scriptstyle", "\\scriptscriptstyle"],
    props: {
      numArgs: 0,
      allowedInText: true,
      primitive: true
    },
    handler(_ref, args) {
      var {
        breakOnTokenText,
        funcName,
        parser
      } = _ref;
      var body = parser.parseExpression(true, breakOnTokenText);
      var style = funcName.slice(1, funcName.length - 5);
      if (!isStyleStr(style)) {
        throw new Error("Unknown style: " + style);
      }
      return {
        type: "styling",
        mode: parser.mode,
        // Figure out what style to use by pulling out the style from
        // the function name
        style,
        body
      };
    },
    htmlBuilder(group, options) {
      var newStyle = styleMap[group.style];
      var newOptions = options.havingStyle(newStyle);
      if (group.resetFont) {
        newOptions = newOptions.withFont("");
      }
      return sizingGroup(group.body, newOptions, options);
    },
    mathmlBuilder(group, options) {
      var newStyle = styleMap[group.style];
      var newOptions = options.havingStyle(newStyle);
      if (group.resetFont) {
        newOptions = newOptions.withFont("");
      }
      var inner2 = buildExpression2(group.body, newOptions);
      var node = new MathNode("mstyle", inner2);
      var styleAttributes = {
        "display": ["0", "true"],
        "text": ["0", "false"],
        "script": ["1", "false"],
        "scriptscript": ["2", "false"]
      };
      var attr = styleAttributes[group.style];
      node.setAttribute("scriptlevel", attr[0]);
      node.setAttribute("displaystyle", attr[1]);
      return node;
    }
  });
  var htmlBuilderDelegate = function htmlBuilderDelegate2(group, options) {
    var base = group.base;
    if (!base) {
      return null;
    } else if (base.type === "op") {
      var delegate = base.limits && (options.style.size === Style$1.DISPLAY.size || base.alwaysHandleSupSub);
      return delegate ? htmlBuilder$2 : null;
    } else if (base.type === "operatorname") {
      var _delegate = base.alwaysHandleSupSub && (options.style.size === Style$1.DISPLAY.size || base.limits);
      return _delegate ? htmlBuilder$1 : null;
    } else if (base.type === "accent") {
      return isCharacterBox(base.base) ? htmlBuilder$a : null;
    } else if (base.type === "horizBrace") {
      var isSup = !group.sub;
      return isSup === base.isOver ? htmlBuilder$3 : null;
    } else {
      return null;
    }
  };
  defineFunctionBuilders({
    type: "supsub",
    htmlBuilder(group, options) {
      var builderDelegate = htmlBuilderDelegate(group, options);
      if (builderDelegate) {
        return builderDelegate(group, options);
      }
      var {
        base: valueBase,
        sup: valueSup,
        sub: valueSub
      } = group;
      var base = buildGroup$1(valueBase, options);
      var supm;
      var subm;
      var metrics = options.fontMetrics();
      var supShift = 0;
      var subShift = 0;
      var isCharBox = valueBase && isCharacterBox(valueBase);
      if (valueSup) {
        var newOptions = options.havingStyle(options.style.sup());
        supm = buildGroup$1(valueSup, newOptions, options);
        if (!isCharBox) {
          supShift = base.height - newOptions.fontMetrics().supDrop * newOptions.sizeMultiplier / options.sizeMultiplier;
        }
      }
      if (valueSub) {
        var _newOptions = options.havingStyle(options.style.sub());
        subm = buildGroup$1(valueSub, _newOptions, options);
        if (!isCharBox) {
          subShift = base.depth + _newOptions.fontMetrics().subDrop * _newOptions.sizeMultiplier / options.sizeMultiplier;
        }
      }
      var minSupShift;
      if (options.style === Style$1.DISPLAY) {
        minSupShift = metrics.sup1;
      } else if (options.style.cramped) {
        minSupShift = metrics.sup3;
      } else {
        minSupShift = metrics.sup2;
      }
      var multiplier = options.sizeMultiplier;
      var marginRight = makeEm(0.5 / metrics.ptPerEm / multiplier);
      var marginLeft = null;
      if (subm) {
        var isOiint = group.base && group.base.type === "op" && group.base.name && (group.base.name === "\\oiint" || group.base.name === "\\oiiint");
        if (base instanceof SymbolNode || isOiint) {
          var _base$italic;
          marginLeft = makeEm(-((_base$italic = base.italic) != null ? _base$italic : 0));
        }
      }
      var supsub;
      if (supm && subm) {
        supShift = Math.max(supShift, minSupShift, supm.depth + 0.25 * metrics.xHeight);
        subShift = Math.max(subShift, metrics.sub2);
        var ruleWidth = metrics.defaultRuleThickness;
        var maxWidth = 4 * ruleWidth;
        if (supShift - supm.depth - (subm.height - subShift) < maxWidth) {
          subShift = maxWidth - (supShift - supm.depth) + subm.height;
          var psi = 0.8 * metrics.xHeight - (supShift - supm.depth);
          if (psi > 0) {
            supShift += psi;
            subShift -= psi;
          }
        }
        var vlistElem = [{
          type: "elem",
          elem: subm,
          shift: subShift,
          marginRight,
          marginLeft
        }, {
          type: "elem",
          elem: supm,
          shift: -supShift,
          marginRight
        }];
        supsub = makeVList({
          positionType: "individualShift",
          children: vlistElem
        });
      } else if (subm) {
        subShift = Math.max(subShift, metrics.sub1, subm.height - 0.8 * metrics.xHeight);
        var _vlistElem = [{
          type: "elem",
          elem: subm,
          marginLeft,
          marginRight
        }];
        supsub = makeVList({
          positionType: "shift",
          positionData: subShift,
          children: _vlistElem
        });
      } else if (supm) {
        supShift = Math.max(supShift, minSupShift, supm.depth + 0.25 * metrics.xHeight);
        supsub = makeVList({
          positionType: "shift",
          positionData: -supShift,
          children: [{
            type: "elem",
            elem: supm,
            marginRight
          }]
        });
      } else {
        throw new Error("supsub must have either sup or sub.");
      }
      var mclass = getTypeOfDomTree(base, "right") || "mord";
      return makeSpan([mclass], [base, makeSpan(["msupsub"], [supsub])], options);
    },
    mathmlBuilder(group, options) {
      var isBrace = false;
      var isOver;
      var isSup;
      if (group.base && group.base.type === "horizBrace") {
        isSup = !!group.sup;
        if (isSup === group.base.isOver) {
          isBrace = true;
          isOver = group.base.isOver;
        }
      }
      if (group.base && (group.base.type === "op" || group.base.type === "operatorname")) {
        group.base.parentIsSupSub = true;
      }
      var children = [buildGroup2(group.base, options)];
      if (group.sub) {
        children.push(buildGroup2(group.sub, options));
      }
      if (group.sup) {
        children.push(buildGroup2(group.sup, options));
      }
      var nodeType;
      if (isBrace) {
        nodeType = isOver ? "mover" : "munder";
      } else if (!group.sub) {
        var base = group.base;
        if (base && base.type === "op" && base.limits && (options.style === Style$1.DISPLAY || base.alwaysHandleSupSub)) {
          nodeType = "mover";
        } else if (base && base.type === "operatorname" && base.alwaysHandleSupSub && (base.limits || options.style === Style$1.DISPLAY)) {
          nodeType = "mover";
        } else {
          nodeType = "msup";
        }
      } else if (!group.sup) {
        var _base = group.base;
        if (_base && _base.type === "op" && _base.limits && (options.style === Style$1.DISPLAY || _base.alwaysHandleSupSub)) {
          nodeType = "munder";
        } else if (_base && _base.type === "operatorname" && _base.alwaysHandleSupSub && (_base.limits || options.style === Style$1.DISPLAY)) {
          nodeType = "munder";
        } else {
          nodeType = "msub";
        }
      } else {
        var _base2 = group.base;
        if (_base2 && _base2.type === "op" && _base2.limits && options.style === Style$1.DISPLAY) {
          nodeType = "munderover";
        } else if (_base2 && _base2.type === "operatorname" && _base2.alwaysHandleSupSub && (options.style === Style$1.DISPLAY || _base2.limits)) {
          nodeType = "munderover";
        } else {
          nodeType = "msubsup";
        }
      }
      return new MathNode(nodeType, children);
    }
  });
  defineFunctionBuilders({
    type: "atom",
    htmlBuilder(group, options) {
      return mathsym(group.text, group.mode, options, ["m" + group.family]);
    },
    mathmlBuilder(group, options) {
      var node = new MathNode("mo", [makeText(group.text, group.mode)]);
      if (group.family === "bin") {
        var variant = getVariant(group, options);
        if (variant === "bold-italic") {
          node.setAttribute("mathvariant", variant);
        }
      } else if (group.family === "punct") {
        node.setAttribute("separator", "true");
      } else if (group.family === "open" || group.family === "close") {
        node.setAttribute("stretchy", "false");
      }
      return node;
    }
  });
  var defaultVariant = {
    "mi": "italic",
    "mn": "normal",
    "mtext": "normal"
  };
  defineFunctionBuilders({
    type: "mathord",
    htmlBuilder(group, options) {
      return makeOrd(group, options, "mathord");
    },
    mathmlBuilder(group, options) {
      var node = new MathNode("mi", [makeText(group.text, group.mode, options)]);
      var variant = getVariant(group, options) || "italic";
      if (variant !== defaultVariant[node.type]) {
        node.setAttribute("mathvariant", variant);
      }
      return node;
    }
  });
  defineFunctionBuilders({
    type: "textord",
    htmlBuilder(group, options) {
      return makeOrd(group, options, "textord");
    },
    mathmlBuilder(group, options) {
      var text2 = makeText(group.text, group.mode, options);
      var variant = getVariant(group, options) || "normal";
      var node;
      if (group.mode === "text") {
        node = new MathNode("mtext", [text2]);
      } else if (/[0-9]/.test(group.text)) {
        node = new MathNode("mn", [text2]);
      } else if (group.text === "\\prime") {
        node = new MathNode("mo", [text2]);
      } else {
        node = new MathNode("mi", [text2]);
      }
      if (variant !== defaultVariant[node.type]) {
        node.setAttribute("mathvariant", variant);
      }
      return node;
    }
  });
  var cssSpace = {
    "\\nobreak": "nobreak",
    "\\allowbreak": "allowbreak"
  };
  var regularSpace = {
    " ": {},
    "\\ ": {},
    "~": {
      className: "nobreak"
    },
    "\\space": {},
    "\\nobreakspace": {
      className: "nobreak"
    }
  };
  defineFunctionBuilders({
    type: "spacing",
    htmlBuilder(group, options) {
      if (regularSpace.hasOwnProperty(group.text)) {
        var className = regularSpace[group.text].className || "";
        if (group.mode === "text") {
          var ord = makeOrd(group, options, "textord");
          ord.classes.push(className);
          return ord;
        } else {
          return makeSpan(["mspace", className], [mathsym(group.text, group.mode, options)], options);
        }
      } else if (cssSpace.hasOwnProperty(group.text)) {
        return makeSpan(["mspace", cssSpace[group.text]], [], options);
      } else {
        throw new ParseError('Unknown type of space "' + group.text + '"');
      }
    },
    mathmlBuilder(group, options) {
      var node;
      if (regularSpace.hasOwnProperty(group.text)) {
        node = new MathNode("mtext", [new TextNode("\xA0")]);
      } else if (cssSpace.hasOwnProperty(group.text)) {
        return new MathNode("mspace");
      } else {
        throw new ParseError('Unknown type of space "' + group.text + '"');
      }
      return node;
    }
  });
  var pad = () => {
    var padNode = new MathNode("mtd", []);
    padNode.setAttribute("width", "50%");
    return padNode;
  };
  defineFunctionBuilders({
    type: "tag",
    mathmlBuilder(group, options) {
      var table = new MathNode("mtable", [new MathNode("mtr", [pad(), new MathNode("mtd", [buildExpressionRow(group.body, options)]), pad(), new MathNode("mtd", [buildExpressionRow(group.tag, options)])])]);
      table.setAttribute("width", "100%");
      return table;
    }
  });
  var textFontFamilies = {
    "\\text": void 0,
    "\\textrm": "textrm",
    "\\textsf": "textsf",
    "\\texttt": "texttt",
    "\\textnormal": "textrm"
  };
  var textFontWeights = {
    "\\textbf": "textbf",
    "\\textmd": "textmd"
  };
  var textFontShapes = {
    "\\textit": "textit",
    "\\textup": "textup"
  };
  var optionsWithFont = (group, options) => {
    var font = group.font;
    if (!font) {
      return options;
    } else if (textFontFamilies[font]) {
      return options.withTextFontFamily(textFontFamilies[font]);
    } else if (textFontWeights[font]) {
      return options.withTextFontWeight(textFontWeights[font]);
    } else if (font === "\\emph") {
      return options.fontShape === "textit" ? options.withTextFontShape("textup") : options.withTextFontShape("textit");
    }
    return options.withTextFontShape(textFontShapes[font]);
  };
  defineFunction({
    type: "text",
    names: [
      // Font families
      "\\text",
      "\\textrm",
      "\\textsf",
      "\\texttt",
      "\\textnormal",
      // Font weights
      "\\textbf",
      "\\textmd",
      // Font Shapes
      "\\textit",
      "\\textup",
      "\\emph"
    ],
    props: {
      numArgs: 1,
      argTypes: ["text"],
      allowedInArgument: true,
      allowedInText: true
    },
    handler(_ref, args) {
      var {
        parser,
        funcName
      } = _ref;
      var body = args[0];
      return {
        type: "text",
        mode: parser.mode,
        body: ordargument(body),
        font: funcName
      };
    },
    htmlBuilder(group, options) {
      var newOptions = optionsWithFont(group, options);
      var inner2 = buildExpression$1(group.body, newOptions, true);
      return makeSpan(["mord", "text"], inner2, newOptions);
    },
    mathmlBuilder(group, options) {
      var newOptions = optionsWithFont(group, options);
      return buildExpressionRow(group.body, newOptions);
    }
  });
  defineFunction({
    type: "underline",
    names: ["\\underline"],
    props: {
      numArgs: 1,
      allowedInText: true
    },
    handler(_ref, args) {
      var {
        parser
      } = _ref;
      return {
        type: "underline",
        mode: parser.mode,
        body: args[0]
      };
    },
    htmlBuilder(group, options) {
      var innerGroup = buildGroup$1(group.body, options);
      var line = makeLineSpan("underline-line", options);
      var defaultRuleThickness = options.fontMetrics().defaultRuleThickness;
      var vlist = makeVList({
        positionType: "top",
        positionData: innerGroup.height,
        children: [{
          type: "kern",
          size: defaultRuleThickness
        }, {
          type: "elem",
          elem: line
        }, {
          type: "kern",
          size: 3 * defaultRuleThickness
        }, {
          type: "elem",
          elem: innerGroup
        }]
      });
      return makeSpan(["mord", "underline"], [vlist], options);
    },
    mathmlBuilder(group, options) {
      var operator = new MathNode("mo", [new TextNode("\u203E")]);
      operator.setAttribute("stretchy", "true");
      var node = new MathNode("munder", [buildGroup2(group.body, options), operator]);
      node.setAttribute("accentunder", "true");
      return node;
    }
  });
  defineFunction({
    type: "vcenter",
    names: ["\\vcenter"],
    props: {
      numArgs: 1,
      argTypes: ["original"],
      // In LaTeX, \vcenter can act only on a box.
      allowedInText: false
    },
    handler(_ref, args) {
      var {
        parser
      } = _ref;
      return {
        type: "vcenter",
        mode: parser.mode,
        body: args[0]
      };
    },
    htmlBuilder(group, options) {
      var body = buildGroup$1(group.body, options);
      var axisHeight = options.fontMetrics().axisHeight;
      var dy = 0.5 * (body.height - axisHeight - (body.depth + axisHeight));
      return makeVList({
        positionType: "shift",
        positionData: dy,
        children: [{
          type: "elem",
          elem: body
        }]
      });
    },
    mathmlBuilder(group, options) {
      var mpadded = new MathNode("mpadded", [buildGroup2(group.body, options)], ["vcenter"]);
      return new MathNode("mrow", [mpadded]);
    }
  });
  defineFunction({
    type: "verb",
    names: ["\\verb"],
    props: {
      numArgs: 0,
      allowedInText: true
    },
    handler(context, args, optArgs) {
      throw new ParseError("\\verb ended by end of line instead of matching delimiter");
    },
    htmlBuilder(group, options) {
      var text2 = makeVerb(group);
      var body = [];
      var newOptions = options.havingStyle(options.style.text());
      for (var i3 = 0; i3 < text2.length; i3++) {
        var c2 = text2[i3];
        if (c2 === "~") {
          c2 = "\\textasciitilde";
        }
        body.push(makeSymbol(c2, "Typewriter-Regular", group.mode, newOptions, ["mord", "texttt"]));
      }
      return makeSpan(["mord", "text"].concat(newOptions.sizingClasses(options)), tryCombineChars(body), newOptions);
    },
    mathmlBuilder(group, options) {
      var text2 = new TextNode(makeVerb(group));
      var node = new MathNode("mtext", [text2]);
      node.setAttribute("mathvariant", "monospace");
      return node;
    }
  });
  var makeVerb = (group) => group.body.replace(/ /g, group.star ? "\u2423" : "\xA0");
  var functions = _functions;
  var spaceRegexString = "[ \r\n	]";
  var controlWordRegexString = "\\\\[a-zA-Z@]+";
  var controlSymbolRegexString = "\\\\[^\uD800-\uDFFF]";
  var controlWordWhitespaceRegexString = "(" + controlWordRegexString + ")" + spaceRegexString + "*";
  var controlSpaceRegexString = "\\\\(\n|[ \r	]+\n?)[ \r	]*";
  var combiningDiacriticalMarkString = "[\u0300-\u036F]";
  var combiningDiacriticalMarksEndRegex = new RegExp(combiningDiacriticalMarkString + "+$");
  var tokenRegexString = "(" + spaceRegexString + "+)|" + // whitespace
  (controlSpaceRegexString + "|") + // \whitespace
  "([!-\\[\\]-\u2027\u202A-\uD7FF\uF900-\uFFFF]" + // single codepoint
  (combiningDiacriticalMarkString + "*") + // ...plus accents
  "|[\uD800-\uDBFF][\uDC00-\uDFFF]" + // surrogate pair
  (combiningDiacriticalMarkString + "*") + // ...plus accents
  "|\\\\verb\\*([^]).*?\\4|\\\\verb([^*a-zA-Z]).*?\\5" + // \verb unstarred
  ("|" + controlWordWhitespaceRegexString) + // \macroName + spaces
  ("|" + controlSymbolRegexString + ")");
  var Lexer = class {
    constructor(input, settings) {
      this.input = void 0;
      this.settings = void 0;
      this.tokenRegex = void 0;
      this.catcodes = void 0;
      this.input = input;
      this.settings = settings;
      this.tokenRegex = new RegExp(tokenRegexString, "g");
      this.catcodes = {
        "%": 14,
        // comment character
        "~": 13
        // active character
      };
    }
    setCatcode(char, code2) {
      this.catcodes[char] = code2;
    }
    /**
     * This function lexes a single token.
     */
    lex() {
      var input = this.input;
      var pos = this.tokenRegex.lastIndex;
      if (pos === input.length) {
        return new Token("EOF", new SourceLocation(this, pos, pos));
      }
      var match = this.tokenRegex.exec(input);
      if (match === null || match.index !== pos) {
        throw new ParseError("Unexpected character: '" + input[pos] + "'", new Token(input[pos], new SourceLocation(this, pos, pos + 1)));
      }
      var text2 = match[6] || match[3] || (match[2] ? "\\ " : " ");
      if (this.catcodes[text2] === 14) {
        var nlIndex = input.indexOf("\n", this.tokenRegex.lastIndex);
        if (nlIndex === -1) {
          this.tokenRegex.lastIndex = input.length;
          this.settings.reportNonstrict("commentAtEnd", "% comment has no terminating newline; LaTeX would fail because of commenting the end of math mode (e.g. $)");
        } else {
          this.tokenRegex.lastIndex = nlIndex + 1;
        }
        return this.lex();
      }
      return new Token(text2, new SourceLocation(this, pos, this.tokenRegex.lastIndex));
    }
  };
  var Namespace = class {
    /**
     * Both arguments are optional.  The first argument is an object of
     * built-in mappings which never change.  The second argument is an object
     * of initial (global-level) mappings, which will constantly change
     * according to any global/top-level `set`s done.
     */
    constructor(builtins, globalMacros) {
      if (builtins === void 0) {
        builtins = {};
      }
      if (globalMacros === void 0) {
        globalMacros = {};
      }
      this.current = void 0;
      this.builtins = void 0;
      this.undefStack = void 0;
      this.current = globalMacros;
      this.builtins = builtins;
      this.undefStack = [];
    }
    /**
     * Start a new nested group, affecting future local `set`s.
     */
    beginGroup() {
      this.undefStack.push({});
    }
    /**
     * End current nested group, restoring values before the group began.
     */
    endGroup() {
      if (this.undefStack.length === 0) {
        throw new ParseError("Unbalanced namespace destruction: attempt to pop global namespace; please report this as a bug");
      }
      var undefs = this.undefStack.pop();
      for (var undef in undefs) {
        if (undefs.hasOwnProperty(undef)) {
          if (undefs[undef] == null) {
            delete this.current[undef];
          } else {
            this.current[undef] = undefs[undef];
          }
        }
      }
    }
    /**
     * Ends all currently nested groups (if any), restoring values before the
     * groups began.  Useful in case of an error in the middle of parsing.
     */
    endGroups() {
      while (this.undefStack.length > 0) {
        this.endGroup();
      }
    }
    /**
     * Detect whether `name` has a definition.  Equivalent to
     * `get(name) != null`.
     */
    has(name) {
      return this.current.hasOwnProperty(name) || this.builtins.hasOwnProperty(name);
    }
    /**
     * Get the current value of a name, or `undefined` if there is no value.
     *
     * Note: Do not use `if (namespace.get(...))` to detect whether a macro
     * is defined, as the definition may be the empty string which evaluates
     * to `false` in JavaScript.  Use `if (namespace.get(...) != null)` or
     * `if (namespace.has(...))`.
     */
    get(name) {
      if (this.current.hasOwnProperty(name)) {
        return this.current[name];
      } else {
        return this.builtins[name];
      }
    }
    /**
     * Set the current value of a name, and optionally set it globally too.
     * Local set() sets the current value and (when appropriate) adds an undo
     * operation to the undo stack.  Global set() may change the undo
     * operation at every level, so takes time linear in their number.
     * A value of undefined means to delete existing definitions.
     */
    set(name, value, global) {
      if (global === void 0) {
        global = false;
      }
      if (global) {
        for (var i3 = 0; i3 < this.undefStack.length; i3++) {
          delete this.undefStack[i3][name];
        }
        if (this.undefStack.length > 0) {
          this.undefStack[this.undefStack.length - 1][name] = value;
        }
      } else {
        var top = this.undefStack[this.undefStack.length - 1];
        if (top && !top.hasOwnProperty(name)) {
          top[name] = this.current[name];
        }
      }
      if (value == null) {
        delete this.current[name];
      } else {
        this.current[name] = value;
      }
    }
  };
  var macros = _macros;
  defineMacro("\\noexpand", function(context) {
    var t2 = context.popToken();
    if (context.isExpandable(t2.text)) {
      t2.noexpand = true;
      t2.treatAsRelax = true;
    }
    return {
      tokens: [t2],
      numArgs: 0
    };
  });
  defineMacro("\\expandafter", function(context) {
    var t2 = context.popToken();
    context.expandOnce(true);
    return {
      tokens: [t2],
      numArgs: 0
    };
  });
  defineMacro("\\@firstoftwo", function(context) {
    var args = context.consumeArgs(2);
    return {
      tokens: args[0],
      numArgs: 0
    };
  });
  defineMacro("\\@secondoftwo", function(context) {
    var args = context.consumeArgs(2);
    return {
      tokens: args[1],
      numArgs: 0
    };
  });
  defineMacro("\\@ifnextchar", function(context) {
    var args = context.consumeArgs(3);
    context.consumeSpaces();
    var nextToken = context.future();
    if (args[0].length === 1 && args[0][0].text === nextToken.text) {
      return {
        tokens: args[1],
        numArgs: 0
      };
    } else {
      return {
        tokens: args[2],
        numArgs: 0
      };
    }
  });
  defineMacro("\\@ifstar", "\\@ifnextchar *{\\@firstoftwo{#1}}");
  defineMacro("\\TextOrMath", function(context) {
    var args = context.consumeArgs(2);
    if (context.mode === "text") {
      return {
        tokens: args[0],
        numArgs: 0
      };
    } else {
      return {
        tokens: args[1],
        numArgs: 0
      };
    }
  });
  var digitToNumber = {
    "0": 0,
    "1": 1,
    "2": 2,
    "3": 3,
    "4": 4,
    "5": 5,
    "6": 6,
    "7": 7,
    "8": 8,
    "9": 9,
    "a": 10,
    "A": 10,
    "b": 11,
    "B": 11,
    "c": 12,
    "C": 12,
    "d": 13,
    "D": 13,
    "e": 14,
    "E": 14,
    "f": 15,
    "F": 15
  };
  defineMacro("\\char", function(context) {
    var token = context.popToken();
    var base;
    var number = 0;
    if (token.text === "'") {
      base = 8;
      token = context.popToken();
    } else if (token.text === '"') {
      base = 16;
      token = context.popToken();
    } else if (token.text === "`") {
      token = context.popToken();
      if (token.text[0] === "\\") {
        number = token.text.charCodeAt(1);
      } else if (token.text === "EOF") {
        throw new ParseError("\\char` missing argument");
      } else {
        number = token.text.charCodeAt(0);
      }
    } else {
      base = 10;
    }
    if (base) {
      number = digitToNumber[token.text];
      if (number == null || number >= base) {
        throw new ParseError("Invalid base-" + base + " digit " + token.text);
      }
      var digit;
      while ((digit = digitToNumber[context.future().text]) != null && digit < base) {
        number *= base;
        number += digit;
        context.popToken();
      }
    }
    return "\\@char{" + number + "}";
  });
  var newcommand = (context, existsOK, nonexistsOK, skipIfExists) => {
    var arg = context.consumeArg().tokens;
    if (arg.length !== 1) {
      throw new ParseError("\\newcommand's first argument must be a macro name");
    }
    var name = arg[0].text;
    var exists = context.isDefined(name);
    if (exists && !existsOK) {
      throw new ParseError("\\newcommand{" + name + "} attempting to redefine " + (name + "; use \\renewcommand"));
    }
    if (!exists && !nonexistsOK) {
      throw new ParseError("\\renewcommand{" + name + "} when command " + name + " does not yet exist; use \\newcommand");
    }
    var numArgs = 0;
    arg = context.consumeArg().tokens;
    if (arg.length === 1 && arg[0].text === "[") {
      var argText = "";
      var token = context.expandNextToken();
      while (token.text !== "]" && token.text !== "EOF") {
        argText += token.text;
        token = context.expandNextToken();
      }
      if (!argText.match(/^\s*[0-9]+\s*$/)) {
        throw new ParseError("Invalid number of arguments: " + argText);
      }
      numArgs = parseInt(argText);
      arg = context.consumeArg().tokens;
    }
    if (!(exists && skipIfExists)) {
      context.macros.set(name, {
        tokens: arg,
        numArgs
      });
    }
    return "";
  };
  defineMacro("\\newcommand", (context) => newcommand(context, false, true, false));
  defineMacro("\\renewcommand", (context) => newcommand(context, true, false, false));
  defineMacro("\\providecommand", (context) => newcommand(context, true, true, true));
  defineMacro("\\message", (context) => {
    var arg = context.consumeArgs(1)[0];
    console.log(arg.reverse().map((token) => token.text).join(""));
    return "";
  });
  defineMacro("\\errmessage", (context) => {
    var arg = context.consumeArgs(1)[0];
    console.error(arg.reverse().map((token) => token.text).join(""));
    return "";
  });
  defineMacro("\\show", (context) => {
    var tok = context.popToken();
    var name = tok.text;
    console.log(tok, context.macros.get(name), functions[name], symbols.math[name], symbols.text[name]);
    return "";
  });
  defineMacro("\\bgroup", "{");
  defineMacro("\\egroup", "}");
  defineMacro("~", "\\nobreakspace");
  defineMacro("\\lq", "`");
  defineMacro("\\rq", "'");
  defineMacro("\\aa", "\\r a");
  defineMacro("\\AA", "\\r A");
  defineMacro("\\textcopyright", "\\html@mathml{\\textcircled{c}}{\\char`\xA9}");
  defineMacro("\\copyright", "\\TextOrMath{\\textcopyright}{\\text{\\textcopyright}}");
  defineMacro("\\textregistered", "\\html@mathml{\\textcircled{\\scriptsize R}}{\\char`\xAE}");
  defineMacro("\u212C", "\\mathscr{B}");
  defineMacro("\u2130", "\\mathscr{E}");
  defineMacro("\u2131", "\\mathscr{F}");
  defineMacro("\u210B", "\\mathscr{H}");
  defineMacro("\u2110", "\\mathscr{I}");
  defineMacro("\u2112", "\\mathscr{L}");
  defineMacro("\u2133", "\\mathscr{M}");
  defineMacro("\u211B", "\\mathscr{R}");
  defineMacro("\u212D", "\\mathfrak{C}");
  defineMacro("\u210C", "\\mathfrak{H}");
  defineMacro("\u2128", "\\mathfrak{Z}");
  defineMacro("\\Bbbk", "\\Bbb{k}");
  defineMacro("\\llap", "\\mathllap{\\textrm{#1}}");
  defineMacro("\\rlap", "\\mathrlap{\\textrm{#1}}");
  defineMacro("\\clap", "\\mathclap{\\textrm{#1}}");
  defineMacro("\\mathstrut", "\\vphantom{(}");
  defineMacro("\\underbar", "\\underline{\\text{#1}}");
  defineMacro("\\not", '\\html@mathml{\\mathrel{\\mathrlap\\@not}\\nobreak}{\\char"338}');
  defineMacro("\\neq", "\\html@mathml{\\mathrel{\\not=}}{\\mathrel{\\char`\u2260}}");
  defineMacro("\\ne", "\\neq");
  defineMacro("\u2260", "\\neq");
  defineMacro("\\notin", "\\html@mathml{\\mathrel{{\\in}\\mathllap{/\\mskip1mu}}}{\\mathrel{\\char`\u2209}}");
  defineMacro("\u2209", "\\notin");
  defineMacro("\u2258", "\\html@mathml{\\mathrel{=\\kern{-1em}\\raisebox{0.4em}{$\\scriptsize\\frown$}}}{\\mathrel{\\char`\u2258}}");
  defineMacro("\u2259", "\\html@mathml{\\stackrel{\\tiny\\wedge}{=}}{\\mathrel{\\char`\u2258}}");
  defineMacro("\u225A", "\\html@mathml{\\stackrel{\\tiny\\vee}{=}}{\\mathrel{\\char`\u225A}}");
  defineMacro("\u225B", "\\html@mathml{\\stackrel{\\scriptsize\\star}{=}}{\\mathrel{\\char`\u225B}}");
  defineMacro("\u225D", "\\html@mathml{\\stackrel{\\tiny\\mathrm{def}}{=}}{\\mathrel{\\char`\u225D}}");
  defineMacro("\u225E", "\\html@mathml{\\stackrel{\\tiny\\mathrm{m}}{=}}{\\mathrel{\\char`\u225E}}");
  defineMacro("\u225F", "\\html@mathml{\\stackrel{\\tiny?}{=}}{\\mathrel{\\char`\u225F}}");
  defineMacro("\u27C2", "\\perp");
  defineMacro("\u203C", "\\mathclose{!\\mkern-0.8mu!}");
  defineMacro("\u220C", "\\notni");
  defineMacro("\u231C", "\\ulcorner");
  defineMacro("\u231D", "\\urcorner");
  defineMacro("\u231E", "\\llcorner");
  defineMacro("\u231F", "\\lrcorner");
  defineMacro("\xA9", "\\copyright");
  defineMacro("\xAE", "\\textregistered");
  defineMacro("\\ulcorner", '\\html@mathml{\\@ulcorner}{\\mathop{\\char"231c}}');
  defineMacro("\\urcorner", '\\html@mathml{\\@urcorner}{\\mathop{\\char"231d}}');
  defineMacro("\\llcorner", '\\html@mathml{\\@llcorner}{\\mathop{\\char"231e}}');
  defineMacro("\\lrcorner", '\\html@mathml{\\@lrcorner}{\\mathop{\\char"231f}}');
  defineMacro("\\vdots", "{\\varvdots\\rule{0pt}{15pt}}");
  defineMacro("\u22EE", "\\vdots");
  defineMacro("\\varGamma", "\\mathit{\\Gamma}");
  defineMacro("\\varDelta", "\\mathit{\\Delta}");
  defineMacro("\\varTheta", "\\mathit{\\Theta}");
  defineMacro("\\varLambda", "\\mathit{\\Lambda}");
  defineMacro("\\varXi", "\\mathit{\\Xi}");
  defineMacro("\\varPi", "\\mathit{\\Pi}");
  defineMacro("\\varSigma", "\\mathit{\\Sigma}");
  defineMacro("\\varUpsilon", "\\mathit{\\Upsilon}");
  defineMacro("\\varPhi", "\\mathit{\\Phi}");
  defineMacro("\\varPsi", "\\mathit{\\Psi}");
  defineMacro("\\varOmega", "\\mathit{\\Omega}");
  defineMacro("\\substack", "\\begin{subarray}{c}#1\\end{subarray}");
  defineMacro("\\colon", "\\nobreak\\mskip2mu\\mathpunct{}\\mathchoice{\\mkern-3mu}{\\mkern-3mu}{}{}{:}\\mskip6mu\\relax");
  defineMacro("\\boxed", "\\fbox{$\\displaystyle{#1}$}");
  defineMacro("\\iff", "\\DOTSB\\;\\Longleftrightarrow\\;");
  defineMacro("\\implies", "\\DOTSB\\;\\Longrightarrow\\;");
  defineMacro("\\impliedby", "\\DOTSB\\;\\Longleftarrow\\;");
  defineMacro("\\dddot", "{\\overset{\\raisebox{-0.1ex}{\\normalsize ...}}{#1}}");
  defineMacro("\\ddddot", "{\\overset{\\raisebox{-0.1ex}{\\normalsize ....}}{#1}}");
  var dotsByToken = {
    ",": "\\dotsc",
    "\\not": "\\dotsb",
    // \keybin@ checks for the following:
    "+": "\\dotsb",
    "=": "\\dotsb",
    "<": "\\dotsb",
    ">": "\\dotsb",
    "-": "\\dotsb",
    "*": "\\dotsb",
    ":": "\\dotsb",
    // Symbols whose definition starts with \DOTSB:
    "\\DOTSB": "\\dotsb",
    "\\coprod": "\\dotsb",
    "\\bigvee": "\\dotsb",
    "\\bigwedge": "\\dotsb",
    "\\biguplus": "\\dotsb",
    "\\bigcap": "\\dotsb",
    "\\bigcup": "\\dotsb",
    "\\prod": "\\dotsb",
    "\\sum": "\\dotsb",
    "\\bigotimes": "\\dotsb",
    "\\bigoplus": "\\dotsb",
    "\\bigodot": "\\dotsb",
    "\\bigsqcup": "\\dotsb",
    "\\And": "\\dotsb",
    "\\longrightarrow": "\\dotsb",
    "\\Longrightarrow": "\\dotsb",
    "\\longleftarrow": "\\dotsb",
    "\\Longleftarrow": "\\dotsb",
    "\\longleftrightarrow": "\\dotsb",
    "\\Longleftrightarrow": "\\dotsb",
    "\\mapsto": "\\dotsb",
    "\\longmapsto": "\\dotsb",
    "\\hookrightarrow": "\\dotsb",
    "\\doteq": "\\dotsb",
    // Symbols whose definition starts with \mathbin:
    "\\mathbin": "\\dotsb",
    // Symbols whose definition starts with \mathrel:
    "\\mathrel": "\\dotsb",
    "\\relbar": "\\dotsb",
    "\\Relbar": "\\dotsb",
    "\\xrightarrow": "\\dotsb",
    "\\xleftarrow": "\\dotsb",
    // Symbols whose definition starts with \DOTSI:
    "\\DOTSI": "\\dotsi",
    "\\int": "\\dotsi",
    "\\oint": "\\dotsi",
    "\\iint": "\\dotsi",
    "\\iiint": "\\dotsi",
    "\\iiiint": "\\dotsi",
    "\\idotsint": "\\dotsi",
    // Symbols whose definition starts with \DOTSX:
    "\\DOTSX": "\\dotsx"
  };
  var dotsbGroups = /* @__PURE__ */ new Set(["bin", "rel"]);
  defineMacro("\\dots", function(context) {
    var thedots = "\\dotso";
    var next = context.expandAfterFuture().text;
    if (next in dotsByToken) {
      thedots = dotsByToken[next];
    } else if (next.slice(0, 4) === "\\not") {
      thedots = "\\dotsb";
    } else if (next in symbols.math) {
      if (dotsbGroups.has(symbols.math[next].group)) {
        thedots = "\\dotsb";
      }
    }
    return thedots;
  });
  var spaceAfterDots = {
    // \rightdelim@ checks for the following:
    ")": true,
    "]": true,
    "\\rbrack": true,
    "\\}": true,
    "\\rbrace": true,
    "\\rangle": true,
    "\\rceil": true,
    "\\rfloor": true,
    "\\rgroup": true,
    "\\rmoustache": true,
    "\\right": true,
    "\\bigr": true,
    "\\biggr": true,
    "\\Bigr": true,
    "\\Biggr": true,
    // \extra@ also tests for the following:
    "$": true,
    // \extrap@ checks for the following:
    ";": true,
    ".": true,
    ",": true
  };
  defineMacro("\\dotso", function(context) {
    var next = context.future().text;
    if (next in spaceAfterDots) {
      return "\\ldots\\,";
    } else {
      return "\\ldots";
    }
  });
  defineMacro("\\dotsc", function(context) {
    var next = context.future().text;
    if (next in spaceAfterDots && next !== ",") {
      return "\\ldots\\,";
    } else {
      return "\\ldots";
    }
  });
  defineMacro("\\cdots", function(context) {
    var next = context.future().text;
    if (next in spaceAfterDots) {
      return "\\@cdots\\,";
    } else {
      return "\\@cdots";
    }
  });
  defineMacro("\\dotsb", "\\cdots");
  defineMacro("\\dotsm", "\\cdots");
  defineMacro("\\dotsi", "\\!\\cdots");
  defineMacro("\\dotsx", "\\ldots\\,");
  defineMacro("\\DOTSI", "\\relax");
  defineMacro("\\DOTSB", "\\relax");
  defineMacro("\\DOTSX", "\\relax");
  defineMacro("\\tmspace", "\\TextOrMath{\\kern#1#3}{\\mskip#1#2}\\relax");
  defineMacro("\\,", "\\tmspace+{3mu}{.1667em}");
  defineMacro("\\thinspace", "\\,");
  defineMacro("\\>", "\\mskip{4mu}");
  defineMacro("\\:", "\\tmspace+{4mu}{.2222em}");
  defineMacro("\\medspace", "\\:");
  defineMacro("\\;", "\\tmspace+{5mu}{.2777em}");
  defineMacro("\\thickspace", "\\;");
  defineMacro("\\!", "\\tmspace-{3mu}{.1667em}");
  defineMacro("\\negthinspace", "\\!");
  defineMacro("\\negmedspace", "\\tmspace-{4mu}{.2222em}");
  defineMacro("\\negthickspace", "\\tmspace-{5mu}{.277em}");
  defineMacro("\\enspace", "\\kern.5em ");
  defineMacro("\\enskip", "\\hskip.5em\\relax");
  defineMacro("\\quad", "\\hskip1em\\relax");
  defineMacro("\\qquad", "\\hskip2em\\relax");
  defineMacro("\\tag", "\\@ifstar\\tag@literal\\tag@paren");
  defineMacro("\\tag@paren", "\\tag@literal{({#1})}");
  defineMacro("\\tag@literal", (context) => {
    if (context.macros.get("\\df@tag")) {
      throw new ParseError("Multiple \\tag");
    }
    return "\\gdef\\df@tag{\\text{#1}}";
  });
  defineMacro("\\bmod", "\\mathchoice{\\mskip1mu}{\\mskip1mu}{\\mskip5mu}{\\mskip5mu}\\mathbin{\\rm mod}\\mathchoice{\\mskip1mu}{\\mskip1mu}{\\mskip5mu}{\\mskip5mu}");
  defineMacro("\\pod", "\\allowbreak\\mathchoice{\\mkern18mu}{\\mkern8mu}{\\mkern8mu}{\\mkern8mu}(#1)");
  defineMacro("\\pmod", "\\pod{{\\rm mod}\\mkern6mu#1}");
  defineMacro("\\mod", "\\allowbreak\\mathchoice{\\mkern18mu}{\\mkern12mu}{\\mkern12mu}{\\mkern12mu}{\\rm mod}\\,\\,#1");
  defineMacro("\\newline", "\\\\\\relax");
  defineMacro("\\TeX", "\\textrm{\\html@mathml{T\\kern-.1667em\\raisebox{-.5ex}{E}\\kern-.125emX}{TeX}}");
  var latexRaiseA = makeEm(fontMetricsData["Main-Regular"]["T".charCodeAt(0)][1] - 0.7 * fontMetricsData["Main-Regular"]["A".charCodeAt(0)][1]);
  defineMacro("\\LaTeX", "\\textrm{\\html@mathml{" + ("L\\kern-.36em\\raisebox{" + latexRaiseA + "}{\\scriptstyle A}") + "\\kern-.15em\\TeX}{LaTeX}}");
  defineMacro("\\KaTeX", "\\textrm{\\html@mathml{" + ("K\\kern-.17em\\raisebox{" + latexRaiseA + "}{\\scriptstyle A}") + "\\kern-.15em\\TeX}{KaTeX}}");
  defineMacro("\\hspace", "\\@ifstar\\@hspacer\\@hspace");
  defineMacro("\\@hspace", "\\hskip #1\\relax");
  defineMacro("\\@hspacer", "\\rule{0pt}{0pt}\\hskip #1\\relax");
  defineMacro("\\ordinarycolon", ":");
  defineMacro("\\vcentcolon", "\\mathrel{\\mathop\\ordinarycolon}");
  defineMacro("\\dblcolon", '\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-.9mu}\\vcentcolon}}{\\mathop{\\char"2237}}');
  defineMacro("\\coloneqq", '\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-1.2mu}=}}{\\mathop{\\char"2254}}');
  defineMacro("\\Coloneqq", '\\html@mathml{\\mathrel{\\dblcolon\\mathrel{\\mkern-1.2mu}=}}{\\mathop{\\char"2237\\char"3d}}');
  defineMacro("\\coloneq", '\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-1.2mu}\\mathrel{-}}}{\\mathop{\\char"3a\\char"2212}}');
  defineMacro("\\Coloneq", '\\html@mathml{\\mathrel{\\dblcolon\\mathrel{\\mkern-1.2mu}\\mathrel{-}}}{\\mathop{\\char"2237\\char"2212}}');
  defineMacro("\\eqqcolon", '\\html@mathml{\\mathrel{=\\mathrel{\\mkern-1.2mu}\\vcentcolon}}{\\mathop{\\char"2255}}');
  defineMacro("\\Eqqcolon", '\\html@mathml{\\mathrel{=\\mathrel{\\mkern-1.2mu}\\dblcolon}}{\\mathop{\\char"3d\\char"2237}}');
  defineMacro("\\eqcolon", '\\html@mathml{\\mathrel{\\mathrel{-}\\mathrel{\\mkern-1.2mu}\\vcentcolon}}{\\mathop{\\char"2239}}');
  defineMacro("\\Eqcolon", '\\html@mathml{\\mathrel{\\mathrel{-}\\mathrel{\\mkern-1.2mu}\\dblcolon}}{\\mathop{\\char"2212\\char"2237}}');
  defineMacro("\\colonapprox", '\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-1.2mu}\\approx}}{\\mathop{\\char"3a\\char"2248}}');
  defineMacro("\\Colonapprox", '\\html@mathml{\\mathrel{\\dblcolon\\mathrel{\\mkern-1.2mu}\\approx}}{\\mathop{\\char"2237\\char"2248}}');
  defineMacro("\\colonsim", '\\html@mathml{\\mathrel{\\vcentcolon\\mathrel{\\mkern-1.2mu}\\sim}}{\\mathop{\\char"3a\\char"223c}}');
  defineMacro("\\Colonsim", '\\html@mathml{\\mathrel{\\dblcolon\\mathrel{\\mkern-1.2mu}\\sim}}{\\mathop{\\char"2237\\char"223c}}');
  defineMacro("\u2237", "\\dblcolon");
  defineMacro("\u2239", "\\eqcolon");
  defineMacro("\u2254", "\\coloneqq");
  defineMacro("\u2255", "\\eqqcolon");
  defineMacro("\u2A74", "\\Coloneqq");
  defineMacro("\\ratio", "\\vcentcolon");
  defineMacro("\\coloncolon", "\\dblcolon");
  defineMacro("\\colonequals", "\\coloneqq");
  defineMacro("\\coloncolonequals", "\\Coloneqq");
  defineMacro("\\equalscolon", "\\eqqcolon");
  defineMacro("\\equalscoloncolon", "\\Eqqcolon");
  defineMacro("\\colonminus", "\\coloneq");
  defineMacro("\\coloncolonminus", "\\Coloneq");
  defineMacro("\\minuscolon", "\\eqcolon");
  defineMacro("\\minuscoloncolon", "\\Eqcolon");
  defineMacro("\\coloncolonapprox", "\\Colonapprox");
  defineMacro("\\coloncolonsim", "\\Colonsim");
  defineMacro("\\simcolon", "\\mathrel{\\sim\\mathrel{\\mkern-1.2mu}\\vcentcolon}");
  defineMacro("\\simcoloncolon", "\\mathrel{\\sim\\mathrel{\\mkern-1.2mu}\\dblcolon}");
  defineMacro("\\approxcolon", "\\mathrel{\\approx\\mathrel{\\mkern-1.2mu}\\vcentcolon}");
  defineMacro("\\approxcoloncolon", "\\mathrel{\\approx\\mathrel{\\mkern-1.2mu}\\dblcolon}");
  defineMacro("\\notni", "\\html@mathml{\\not\\ni}{\\mathrel{\\char`\u220C}}");
  defineMacro("\\limsup", "\\DOTSB\\operatorname*{lim\\,sup}");
  defineMacro("\\liminf", "\\DOTSB\\operatorname*{lim\\,inf}");
  defineMacro("\\injlim", "\\DOTSB\\operatorname*{inj\\,lim}");
  defineMacro("\\projlim", "\\DOTSB\\operatorname*{proj\\,lim}");
  defineMacro("\\varlimsup", "\\DOTSB\\operatorname*{\\overline{lim}}");
  defineMacro("\\varliminf", "\\DOTSB\\operatorname*{\\underline{lim}}");
  defineMacro("\\varinjlim", "\\DOTSB\\operatorname*{\\underrightarrow{lim}}");
  defineMacro("\\varprojlim", "\\DOTSB\\operatorname*{\\underleftarrow{lim}}");
  defineMacro("\\gvertneqq", "\\html@mathml{\\@gvertneqq}{\u2269}");
  defineMacro("\\lvertneqq", "\\html@mathml{\\@lvertneqq}{\u2268}");
  defineMacro("\\ngeqq", "\\html@mathml{\\@ngeqq}{\u2271}");
  defineMacro("\\ngeqslant", "\\html@mathml{\\@ngeqslant}{\u2271}");
  defineMacro("\\nleqq", "\\html@mathml{\\@nleqq}{\u2270}");
  defineMacro("\\nleqslant", "\\html@mathml{\\@nleqslant}{\u2270}");
  defineMacro("\\nshortmid", "\\html@mathml{\\@nshortmid}{\u2224}");
  defineMacro("\\nshortparallel", "\\html@mathml{\\@nshortparallel}{\u2226}");
  defineMacro("\\nsubseteqq", "\\html@mathml{\\@nsubseteqq}{\u2288}");
  defineMacro("\\nsupseteqq", "\\html@mathml{\\@nsupseteqq}{\u2289}");
  defineMacro("\\varsubsetneq", "\\html@mathml{\\@varsubsetneq}{\u228A}");
  defineMacro("\\varsubsetneqq", "\\html@mathml{\\@varsubsetneqq}{\u2ACB}");
  defineMacro("\\varsupsetneq", "\\html@mathml{\\@varsupsetneq}{\u228B}");
  defineMacro("\\varsupsetneqq", "\\html@mathml{\\@varsupsetneqq}{\u2ACC}");
  defineMacro("\\imath", "\\html@mathml{\\@imath}{\u0131}");
  defineMacro("\\jmath", "\\html@mathml{\\@jmath}{\u0237}");
  defineMacro("\\llbracket", "\\html@mathml{\\mathopen{[\\mkern-3.2mu[}}{\\mathopen{\\char`\u27E6}}");
  defineMacro("\\rrbracket", "\\html@mathml{\\mathclose{]\\mkern-3.2mu]}}{\\mathclose{\\char`\u27E7}}");
  defineMacro("\u27E6", "\\llbracket");
  defineMacro("\u27E7", "\\rrbracket");
  defineMacro("\\lBrace", "\\html@mathml{\\mathopen{\\{\\mkern-3.2mu[}}{\\mathopen{\\char`\u2983}}");
  defineMacro("\\rBrace", "\\html@mathml{\\mathclose{]\\mkern-3.2mu\\}}}{\\mathclose{\\char`\u2984}}");
  defineMacro("\u2983", "\\lBrace");
  defineMacro("\u2984", "\\rBrace");
  defineMacro("\\minuso", "\\mathbin{\\html@mathml{{\\mathrlap{\\mathchoice{\\kern{0.145em}}{\\kern{0.145em}}{\\kern{0.1015em}}{\\kern{0.0725em}}\\circ}{-}}}{\\char`\u29B5}}");
  defineMacro("\u29B5", "\\minuso");
  defineMacro("\\darr", "\\downarrow");
  defineMacro("\\dArr", "\\Downarrow");
  defineMacro("\\Darr", "\\Downarrow");
  defineMacro("\\lang", "\\langle");
  defineMacro("\\rang", "\\rangle");
  defineMacro("\\uarr", "\\uparrow");
  defineMacro("\\uArr", "\\Uparrow");
  defineMacro("\\Uarr", "\\Uparrow");
  defineMacro("\\N", "\\mathbb{N}");
  defineMacro("\\R", "\\mathbb{R}");
  defineMacro("\\Z", "\\mathbb{Z}");
  defineMacro("\\alef", "\\aleph");
  defineMacro("\\alefsym", "\\aleph");
  defineMacro("\\Alpha", "\\mathrm{A}");
  defineMacro("\\Beta", "\\mathrm{B}");
  defineMacro("\\bull", "\\bullet");
  defineMacro("\\Chi", "\\mathrm{X}");
  defineMacro("\\clubs", "\\clubsuit");
  defineMacro("\\cnums", "\\mathbb{C}");
  defineMacro("\\Complex", "\\mathbb{C}");
  defineMacro("\\Dagger", "\\ddagger");
  defineMacro("\\diamonds", "\\diamondsuit");
  defineMacro("\\empty", "\\emptyset");
  defineMacro("\\Epsilon", "\\mathrm{E}");
  defineMacro("\\Eta", "\\mathrm{H}");
  defineMacro("\\exist", "\\exists");
  defineMacro("\\harr", "\\leftrightarrow");
  defineMacro("\\hArr", "\\Leftrightarrow");
  defineMacro("\\Harr", "\\Leftrightarrow");
  defineMacro("\\hearts", "\\heartsuit");
  defineMacro("\\image", "\\Im");
  defineMacro("\\infin", "\\infty");
  defineMacro("\\Iota", "\\mathrm{I}");
  defineMacro("\\isin", "\\in");
  defineMacro("\\Kappa", "\\mathrm{K}");
  defineMacro("\\larr", "\\leftarrow");
  defineMacro("\\lArr", "\\Leftarrow");
  defineMacro("\\Larr", "\\Leftarrow");
  defineMacro("\\lrarr", "\\leftrightarrow");
  defineMacro("\\lrArr", "\\Leftrightarrow");
  defineMacro("\\Lrarr", "\\Leftrightarrow");
  defineMacro("\\Mu", "\\mathrm{M}");
  defineMacro("\\natnums", "\\mathbb{N}");
  defineMacro("\\Nu", "\\mathrm{N}");
  defineMacro("\\Omicron", "\\mathrm{O}");
  defineMacro("\\plusmn", "\\pm");
  defineMacro("\\rarr", "\\rightarrow");
  defineMacro("\\rArr", "\\Rightarrow");
  defineMacro("\\Rarr", "\\Rightarrow");
  defineMacro("\\real", "\\Re");
  defineMacro("\\reals", "\\mathbb{R}");
  defineMacro("\\Reals", "\\mathbb{R}");
  defineMacro("\\Rho", "\\mathrm{P}");
  defineMacro("\\sdot", "\\cdot");
  defineMacro("\\sect", "\\S");
  defineMacro("\\spades", "\\spadesuit");
  defineMacro("\\sub", "\\subset");
  defineMacro("\\sube", "\\subseteq");
  defineMacro("\\supe", "\\supseteq");
  defineMacro("\\Tau", "\\mathrm{T}");
  defineMacro("\\thetasym", "\\vartheta");
  defineMacro("\\weierp", "\\wp");
  defineMacro("\\Zeta", "\\mathrm{Z}");
  defineMacro("\\argmin", "\\DOTSB\\operatorname*{arg\\,min}");
  defineMacro("\\argmax", "\\DOTSB\\operatorname*{arg\\,max}");
  defineMacro("\\plim", "\\DOTSB\\mathop{\\operatorname{plim}}\\limits");
  defineMacro("\\bra", "\\mathinner{\\langle{#1}|}");
  defineMacro("\\ket", "\\mathinner{|{#1}\\rangle}");
  defineMacro("\\braket", "\\mathinner{\\langle{#1}\\rangle}");
  defineMacro("\\Bra", "\\left\\langle#1\\right|");
  defineMacro("\\Ket", "\\left|#1\\right\\rangle");
  var braketHelper = (one) => (context) => {
    var left = context.consumeArg().tokens;
    var middle = context.consumeArg().tokens;
    var middleDouble = context.consumeArg().tokens;
    var right = context.consumeArg().tokens;
    var oldMiddle = context.macros.get("|");
    var oldMiddleDouble = context.macros.get("\\|");
    context.macros.beginGroup();
    var midMacro = (double) => (context2) => {
      if (one) {
        context2.macros.set("|", oldMiddle);
        if (middleDouble.length) {
          context2.macros.set("\\|", oldMiddleDouble);
        }
      }
      var doubled = double;
      if (!double && middleDouble.length) {
        var nextToken = context2.future();
        if (nextToken.text === "|") {
          context2.popToken();
          doubled = true;
        }
      }
      return {
        tokens: doubled ? middleDouble : middle,
        numArgs: 0
      };
    };
    context.macros.set("|", midMacro(false));
    if (middleDouble.length) {
      context.macros.set("\\|", midMacro(true));
    }
    var arg = context.consumeArg().tokens;
    var expanded = context.expandTokens([
      ...right,
      ...arg,
      ...left
      // reversed
    ]);
    context.macros.endGroup();
    return {
      tokens: expanded.reverse(),
      numArgs: 0
    };
  };
  defineMacro("\\bra@ket", braketHelper(false));
  defineMacro("\\bra@set", braketHelper(true));
  defineMacro("\\Braket", "\\bra@ket{\\left\\langle}{\\,\\middle\\vert\\,}{\\,\\middle\\vert\\,}{\\right\\rangle}");
  defineMacro("\\Set", "\\bra@set{\\left\\{\\:}{\\;\\middle\\vert\\;}{\\;\\middle\\Vert\\;}{\\:\\right\\}}");
  defineMacro("\\set", "\\bra@set{\\{\\,}{\\mid}{}{\\,\\}}");
  defineMacro("\\angln", "{\\angl n}");
  defineMacro("\\blue", "\\textcolor{##6495ed}{#1}");
  defineMacro("\\orange", "\\textcolor{##ffa500}{#1}");
  defineMacro("\\pink", "\\textcolor{##ff00af}{#1}");
  defineMacro("\\red", "\\textcolor{##df0030}{#1}");
  defineMacro("\\green", "\\textcolor{##28ae7b}{#1}");
  defineMacro("\\gray", "\\textcolor{gray}{#1}");
  defineMacro("\\purple", "\\textcolor{##9d38bd}{#1}");
  defineMacro("\\blueA", "\\textcolor{##ccfaff}{#1}");
  defineMacro("\\blueB", "\\textcolor{##80f6ff}{#1}");
  defineMacro("\\blueC", "\\textcolor{##63d9ea}{#1}");
  defineMacro("\\blueD", "\\textcolor{##11accd}{#1}");
  defineMacro("\\blueE", "\\textcolor{##0c7f99}{#1}");
  defineMacro("\\tealA", "\\textcolor{##94fff5}{#1}");
  defineMacro("\\tealB", "\\textcolor{##26edd5}{#1}");
  defineMacro("\\tealC", "\\textcolor{##01d1c1}{#1}");
  defineMacro("\\tealD", "\\textcolor{##01a995}{#1}");
  defineMacro("\\tealE", "\\textcolor{##208170}{#1}");
  defineMacro("\\greenA", "\\textcolor{##b6ffb0}{#1}");
  defineMacro("\\greenB", "\\textcolor{##8af281}{#1}");
  defineMacro("\\greenC", "\\textcolor{##74cf70}{#1}");
  defineMacro("\\greenD", "\\textcolor{##1fab54}{#1}");
  defineMacro("\\greenE", "\\textcolor{##0d923f}{#1}");
  defineMacro("\\goldA", "\\textcolor{##ffd0a9}{#1}");
  defineMacro("\\goldB", "\\textcolor{##ffbb71}{#1}");
  defineMacro("\\goldC", "\\textcolor{##ff9c39}{#1}");
  defineMacro("\\goldD", "\\textcolor{##e07d10}{#1}");
  defineMacro("\\goldE", "\\textcolor{##a75a05}{#1}");
  defineMacro("\\redA", "\\textcolor{##fca9a9}{#1}");
  defineMacro("\\redB", "\\textcolor{##ff8482}{#1}");
  defineMacro("\\redC", "\\textcolor{##f9685d}{#1}");
  defineMacro("\\redD", "\\textcolor{##e84d39}{#1}");
  defineMacro("\\redE", "\\textcolor{##bc2612}{#1}");
  defineMacro("\\maroonA", "\\textcolor{##ffbde0}{#1}");
  defineMacro("\\maroonB", "\\textcolor{##ff92c6}{#1}");
  defineMacro("\\maroonC", "\\textcolor{##ed5fa6}{#1}");
  defineMacro("\\maroonD", "\\textcolor{##ca337c}{#1}");
  defineMacro("\\maroonE", "\\textcolor{##9e034e}{#1}");
  defineMacro("\\purpleA", "\\textcolor{##ddd7ff}{#1}");
  defineMacro("\\purpleB", "\\textcolor{##c6b9fc}{#1}");
  defineMacro("\\purpleC", "\\textcolor{##aa87ff}{#1}");
  defineMacro("\\purpleD", "\\textcolor{##7854ab}{#1}");
  defineMacro("\\purpleE", "\\textcolor{##543b78}{#1}");
  defineMacro("\\mintA", "\\textcolor{##f5f9e8}{#1}");
  defineMacro("\\mintB", "\\textcolor{##edf2df}{#1}");
  defineMacro("\\mintC", "\\textcolor{##e0e5cc}{#1}");
  defineMacro("\\grayA", "\\textcolor{##f6f7f7}{#1}");
  defineMacro("\\grayB", "\\textcolor{##f0f1f2}{#1}");
  defineMacro("\\grayC", "\\textcolor{##e3e5e6}{#1}");
  defineMacro("\\grayD", "\\textcolor{##d6d8da}{#1}");
  defineMacro("\\grayE", "\\textcolor{##babec2}{#1}");
  defineMacro("\\grayF", "\\textcolor{##888d93}{#1}");
  defineMacro("\\grayG", "\\textcolor{##626569}{#1}");
  defineMacro("\\grayH", "\\textcolor{##3b3e40}{#1}");
  defineMacro("\\grayI", "\\textcolor{##21242c}{#1}");
  defineMacro("\\kaBlue", "\\textcolor{##314453}{#1}");
  defineMacro("\\kaGreen", "\\textcolor{##71B307}{#1}");
  var implicitCommands = {
    "^": true,
    // Parser.js
    "_": true,
    // Parser.js
    "\\limits": true,
    // Parser.js
    "\\nolimits": true
    // Parser.js
  };
  var MacroExpander = class {
    constructor(input, settings, mode) {
      this.settings = void 0;
      this.expansionCount = void 0;
      this.lexer = void 0;
      this.macros = void 0;
      this.stack = void 0;
      this.mode = void 0;
      this.settings = settings;
      this.expansionCount = 0;
      this.feed(input);
      this.macros = new Namespace(macros, settings.macros);
      this.mode = mode;
      this.stack = [];
    }
    /**
     * Feed a new input string to the same MacroExpander
     * (with existing macros etc.).
     */
    feed(input) {
      this.lexer = new Lexer(input, this.settings);
    }
    /**
     * Switches between "text" and "math" modes.
     */
    switchMode(newMode) {
      this.mode = newMode;
    }
    /**
     * Start a new group nesting within all namespaces.
     */
    beginGroup() {
      this.macros.beginGroup();
    }
    /**
     * End current group nesting within all namespaces.
     */
    endGroup() {
      this.macros.endGroup();
    }
    /**
     * Ends all currently nested groups (if any), restoring values before the
     * groups began.  Useful in case of an error in the middle of parsing.
     */
    endGroups() {
      this.macros.endGroups();
    }
    /**
     * Returns the topmost token on the stack, without expanding it.
     * Similar in behavior to TeX's `\futurelet`.
     */
    future() {
      if (this.stack.length === 0) {
        this.pushToken(this.lexer.lex());
      }
      return this.stack[this.stack.length - 1];
    }
    /**
     * Remove and return the next unexpanded token.
     */
    popToken() {
      this.future();
      return this.stack.pop();
    }
    /**
     * Add a given token to the token stack.  In particular, this get be used
     * to put back a token returned from one of the other methods.
     */
    pushToken(token) {
      this.stack.push(token);
    }
    /**
     * Append an array of tokens to the token stack.
     */
    pushTokens(tokens) {
      this.stack.push(...tokens);
    }
    /**
     * Find an macro argument without expanding tokens and append the array of
     * tokens to the token stack. Uses Token as a container for the result.
     */
    scanArgument(isOptional) {
      var start;
      var end;
      var tokens;
      if (isOptional) {
        this.consumeSpaces();
        if (this.future().text !== "[") {
          return null;
        }
        start = this.popToken();
        ({
          tokens,
          end
        } = this.consumeArg(["]"]));
      } else {
        ({
          tokens,
          start,
          end
        } = this.consumeArg());
      }
      this.pushToken(new Token("EOF", end.loc));
      this.pushTokens(tokens);
      return new Token("", SourceLocation.range(start, end));
    }
    /**
     * Consume all following space tokens, without expansion.
     */
    consumeSpaces() {
      for (; ; ) {
        var token = this.future();
        if (token.text === " ") {
          this.stack.pop();
        } else {
          break;
        }
      }
    }
    /**
     * Consume an argument from the token stream, and return the resulting array
     * of tokens and start/end token.
     */
    consumeArg(delims) {
      var tokens = [];
      var isDelimited = delims && delims.length > 0;
      if (!isDelimited) {
        this.consumeSpaces();
      }
      var start = this.future();
      var tok;
      var depth = 0;
      var match = 0;
      do {
        tok = this.popToken();
        tokens.push(tok);
        if (tok.text === "{") {
          ++depth;
        } else if (tok.text === "}") {
          --depth;
          if (depth === -1) {
            throw new ParseError("Extra }", tok);
          }
        } else if (tok.text === "EOF") {
          throw new ParseError("Unexpected end of input in a macro argument, expected '" + (delims && isDelimited ? delims[match] : "}") + "'", tok);
        }
        if (delims && isDelimited) {
          if ((depth === 0 || depth === 1 && delims[match] === "{") && tok.text === delims[match]) {
            ++match;
            if (match === delims.length) {
              tokens.splice(-match, match);
              break;
            }
          } else {
            match = 0;
          }
        }
      } while (depth !== 0 || isDelimited);
      if (start.text === "{" && tokens[tokens.length - 1].text === "}") {
        tokens.pop();
        tokens.shift();
      }
      tokens.reverse();
      return {
        tokens,
        start,
        end: tok
      };
    }
    /**
     * Consume the specified number of (delimited) arguments from the token
     * stream and return the resulting array of arguments.
     */
    consumeArgs(numArgs, delimiters2) {
      if (delimiters2) {
        if (delimiters2.length !== numArgs + 1) {
          throw new ParseError("The length of delimiters doesn't match the number of args!");
        }
        var delims = delimiters2[0];
        for (var i3 = 0; i3 < delims.length; i3++) {
          var tok = this.popToken();
          if (delims[i3] !== tok.text) {
            throw new ParseError("Use of the macro doesn't match its definition", tok);
          }
        }
      }
      var args = [];
      for (var _i6 = 0; _i6 < numArgs; _i6++) {
        args.push(this.consumeArg(delimiters2 && delimiters2[_i6 + 1]).tokens);
      }
      return args;
    }
    /**
     * Increment `expansionCount` by the specified amount.
     * Throw an error if it exceeds `maxExpand`.
     */
    countExpansion(amount) {
      this.expansionCount += amount;
      if (this.expansionCount > this.settings.maxExpand) {
        throw new ParseError("Too many expansions: infinite loop or need to increase maxExpand setting");
      }
    }
    /**
     * Expand the next token only once if possible.
     *
     * If the token is expanded, the resulting tokens will be pushed onto
     * the stack in reverse order, and the number of such tokens will be
     * returned.  This number might be zero or positive.
     *
     * If not, the return value is `false`, and the next token remains at the
     * top of the stack.
     *
     * In either case, the next token will be on the top of the stack,
     * or the stack will be empty (in case of empty expansion
     * and no other tokens).
     *
     * Used to implement `expandAfterFuture` and `expandNextToken`.
     *
     * If expandableOnly, only expandable tokens are expanded and
     * an undefined control sequence results in an error.
     */
    expandOnce(expandableOnly) {
      var topToken = this.popToken();
      var name = topToken.text;
      var expansion = !topToken.noexpand ? this._getExpansion(name) : null;
      if (expansion == null || expandableOnly && expansion.unexpandable) {
        if (expandableOnly && expansion == null && name[0] === "\\" && !this.isDefined(name)) {
          throw new ParseError("Undefined control sequence: " + name);
        }
        this.pushToken(topToken);
        return false;
      }
      this.countExpansion(1);
      var tokens = expansion.tokens;
      var args = this.consumeArgs(expansion.numArgs, expansion.delimiters);
      if (expansion.numArgs) {
        tokens = tokens.slice();
        for (var i3 = tokens.length - 1; i3 >= 0; --i3) {
          var tok = tokens[i3];
          if (tok.text === "#") {
            if (i3 === 0) {
              throw new ParseError("Incomplete placeholder at end of macro body", tok);
            }
            tok = tokens[--i3];
            if (tok.text === "#") {
              tokens.splice(i3 + 1, 1);
            } else if (/^[1-9]$/.test(tok.text)) {
              tokens.splice(i3, 2, ...args[+tok.text - 1]);
            } else {
              throw new ParseError("Not a valid argument number", tok);
            }
          }
        }
      }
      this.pushTokens(tokens);
      return tokens.length;
    }
    /**
     * Expand the next token only once (if possible), and return the resulting
     * top token on the stack (without removing anything from the stack).
     * Similar in behavior to TeX's `\expandafter\futurelet`.
     * Equivalent to expandOnce() followed by future().
     */
    expandAfterFuture() {
      this.expandOnce();
      return this.future();
    }
    /**
     * Recursively expand first token, then return first non-expandable token.
     */
    expandNextToken() {
      for (; ; ) {
        if (this.expandOnce() === false) {
          var token = this.stack.pop();
          if (token.treatAsRelax) {
            token.text = "\\relax";
          }
          return token;
        }
      }
    }
    /**
     * Fully expand the given macro name and return the resulting list of
     * tokens, or return `undefined` if no such macro is defined.
     */
    expandMacro(name) {
      return this.macros.has(name) ? this.expandTokens([new Token(name)]) : void 0;
    }
    /**
     * Fully expand the given token stream and return the resulting list of
     * tokens.  Note that the input tokens are in reverse order, but the
     * output tokens are in forward order.
     */
    expandTokens(tokens) {
      var output = [];
      var oldStackLength = this.stack.length;
      this.pushTokens(tokens);
      while (this.stack.length > oldStackLength) {
        if (this.expandOnce(true) === false) {
          var token = this.stack.pop();
          if (token.treatAsRelax) {
            token.noexpand = false;
            token.treatAsRelax = false;
          }
          output.push(token);
        }
      }
      this.countExpansion(output.length);
      return output;
    }
    /**
     * Fully expand the given macro name and return the result as a string,
     * or return `undefined` if no such macro is defined.
     */
    expandMacroAsText(name) {
      var tokens = this.expandMacro(name);
      if (tokens) {
        return tokens.map((token) => token.text).join("");
      } else {
        return tokens;
      }
    }
    /**
     * Returns the expanded macro as a reversed array of tokens and a macro
     * argument count.  Or returns `null` if no such macro.
     */
    _getExpansion(name) {
      var definition = this.macros.get(name);
      if (definition == null) {
        return definition;
      }
      if (name.length === 1) {
        var catcode = this.lexer.catcodes[name];
        if (catcode != null && catcode !== 13) {
          return;
        }
      }
      var expansion = typeof definition === "function" ? definition(this) : definition;
      if (typeof expansion === "string") {
        var numArgs = 0;
        if (expansion.includes("#")) {
          var stripped = expansion.replace(/##/g, "");
          while (stripped.includes("#" + (numArgs + 1))) {
            ++numArgs;
          }
        }
        var bodyLexer = new Lexer(expansion, this.settings);
        var tokens = [];
        var tok = bodyLexer.lex();
        while (tok.text !== "EOF") {
          tokens.push(tok);
          tok = bodyLexer.lex();
        }
        tokens.reverse();
        var expanded = {
          tokens,
          numArgs
        };
        return expanded;
      }
      return expansion;
    }
    /**
     * Determine whether a command is currently "defined" (has some
     * functionality), meaning that it's a macro (in the current group),
     * a function, a symbol, or one of the special commands listed in
     * `implicitCommands`.
     */
    isDefined(name) {
      return this.macros.has(name) || functions.hasOwnProperty(name) || symbols.math.hasOwnProperty(name) || symbols.text.hasOwnProperty(name) || implicitCommands.hasOwnProperty(name);
    }
    /**
     * Determine whether a command is expandable.
     */
    isExpandable(name) {
      var macro = this.macros.get(name);
      return macro != null ? typeof macro === "string" || typeof macro === "function" || !macro.unexpandable : functions.hasOwnProperty(name) && !functions[name].primitive;
    }
  };
  var unicodeSubRegEx = /^[₊₋₌₍₎₀₁₂₃₄₅₆₇₈₉ₐₑₕᵢⱼₖₗₘₙₒₚᵣₛₜᵤᵥₓᵦᵧᵨᵩᵪ]/;
  var uSubsAndSups = Object.freeze({
    "\u208A": "+",
    "\u208B": "-",
    "\u208C": "=",
    "\u208D": "(",
    "\u208E": ")",
    "\u2080": "0",
    "\u2081": "1",
    "\u2082": "2",
    "\u2083": "3",
    "\u2084": "4",
    "\u2085": "5",
    "\u2086": "6",
    "\u2087": "7",
    "\u2088": "8",
    "\u2089": "9",
    "\u2090": "a",
    "\u2091": "e",
    "\u2095": "h",
    "\u1D62": "i",
    "\u2C7C": "j",
    "\u2096": "k",
    "\u2097": "l",
    "\u2098": "m",
    "\u2099": "n",
    "\u2092": "o",
    "\u209A": "p",
    "\u1D63": "r",
    "\u209B": "s",
    "\u209C": "t",
    "\u1D64": "u",
    "\u1D65": "v",
    "\u2093": "x",
    "\u1D66": "\u03B2",
    "\u1D67": "\u03B3",
    "\u1D68": "\u03C1",
    "\u1D69": "\u03D5",
    "\u1D6A": "\u03C7",
    "\u207A": "+",
    "\u207B": "-",
    "\u207C": "=",
    "\u207D": "(",
    "\u207E": ")",
    "\u2070": "0",
    "\xB9": "1",
    "\xB2": "2",
    "\xB3": "3",
    "\u2074": "4",
    "\u2075": "5",
    "\u2076": "6",
    "\u2077": "7",
    "\u2078": "8",
    "\u2079": "9",
    "\u1D2C": "A",
    "\u1D2E": "B",
    "\u1D30": "D",
    "\u1D31": "E",
    "\u1D33": "G",
    "\u1D34": "H",
    "\u1D35": "I",
    "\u1D36": "J",
    "\u1D37": "K",
    "\u1D38": "L",
    "\u1D39": "M",
    "\u1D3A": "N",
    "\u1D3C": "O",
    "\u1D3E": "P",
    "\u1D3F": "R",
    "\u1D40": "T",
    "\u1D41": "U",
    "\u2C7D": "V",
    "\u1D42": "W",
    "\u1D43": "a",
    "\u1D47": "b",
    "\u1D9C": "c",
    "\u1D48": "d",
    "\u1D49": "e",
    "\u1DA0": "f",
    "\u1D4D": "g",
    "\u02B0": "h",
    "\u2071": "i",
    "\u02B2": "j",
    "\u1D4F": "k",
    "\u02E1": "l",
    "\u1D50": "m",
    "\u207F": "n",
    "\u1D52": "o",
    "\u1D56": "p",
    "\u02B3": "r",
    "\u02E2": "s",
    "\u1D57": "t",
    "\u1D58": "u",
    "\u1D5B": "v",
    "\u02B7": "w",
    "\u02E3": "x",
    "\u02B8": "y",
    "\u1DBB": "z",
    "\u1D5D": "\u03B2",
    "\u1D5E": "\u03B3",
    "\u1D5F": "\u03B4",
    "\u1D60": "\u03D5",
    "\u1D61": "\u03C7",
    "\u1DBF": "\u03B8"
  });
  var unicodeAccents = {
    "\u0301": {
      "text": "\\'",
      "math": "\\acute"
    },
    "\u0300": {
      "text": "\\`",
      "math": "\\grave"
    },
    "\u0308": {
      "text": '\\"',
      "math": "\\ddot"
    },
    "\u0303": {
      "text": "\\~",
      "math": "\\tilde"
    },
    "\u0304": {
      "text": "\\=",
      "math": "\\bar"
    },
    "\u0306": {
      "text": "\\u",
      "math": "\\breve"
    },
    "\u030C": {
      "text": "\\v",
      "math": "\\check"
    },
    "\u0302": {
      "text": "\\^",
      "math": "\\hat"
    },
    "\u0307": {
      "text": "\\.",
      "math": "\\dot"
    },
    "\u030A": {
      "text": "\\r",
      "math": "\\mathring"
    },
    "\u030B": {
      "text": "\\H"
    },
    "\u0327": {
      "text": "\\c"
    }
  };
  var unicodeSymbols = {
    "\xE1": "a\u0301",
    "\xE0": "a\u0300",
    "\xE4": "a\u0308",
    "\u01DF": "a\u0308\u0304",
    "\xE3": "a\u0303",
    "\u0101": "a\u0304",
    "\u0103": "a\u0306",
    "\u1EAF": "a\u0306\u0301",
    "\u1EB1": "a\u0306\u0300",
    "\u1EB5": "a\u0306\u0303",
    "\u01CE": "a\u030C",
    "\xE2": "a\u0302",
    "\u1EA5": "a\u0302\u0301",
    "\u1EA7": "a\u0302\u0300",
    "\u1EAB": "a\u0302\u0303",
    "\u0227": "a\u0307",
    "\u01E1": "a\u0307\u0304",
    "\xE5": "a\u030A",
    "\u01FB": "a\u030A\u0301",
    "\u1E03": "b\u0307",
    "\u0107": "c\u0301",
    "\u1E09": "c\u0327\u0301",
    "\u010D": "c\u030C",
    "\u0109": "c\u0302",
    "\u010B": "c\u0307",
    "\xE7": "c\u0327",
    "\u010F": "d\u030C",
    "\u1E0B": "d\u0307",
    "\u1E11": "d\u0327",
    "\xE9": "e\u0301",
    "\xE8": "e\u0300",
    "\xEB": "e\u0308",
    "\u1EBD": "e\u0303",
    "\u0113": "e\u0304",
    "\u1E17": "e\u0304\u0301",
    "\u1E15": "e\u0304\u0300",
    "\u0115": "e\u0306",
    "\u1E1D": "e\u0327\u0306",
    "\u011B": "e\u030C",
    "\xEA": "e\u0302",
    "\u1EBF": "e\u0302\u0301",
    "\u1EC1": "e\u0302\u0300",
    "\u1EC5": "e\u0302\u0303",
    "\u0117": "e\u0307",
    "\u0229": "e\u0327",
    "\u1E1F": "f\u0307",
    "\u01F5": "g\u0301",
    "\u1E21": "g\u0304",
    "\u011F": "g\u0306",
    "\u01E7": "g\u030C",
    "\u011D": "g\u0302",
    "\u0121": "g\u0307",
    "\u0123": "g\u0327",
    "\u1E27": "h\u0308",
    "\u021F": "h\u030C",
    "\u0125": "h\u0302",
    "\u1E23": "h\u0307",
    "\u1E29": "h\u0327",
    "\xED": "i\u0301",
    "\xEC": "i\u0300",
    "\xEF": "i\u0308",
    "\u1E2F": "i\u0308\u0301",
    "\u0129": "i\u0303",
    "\u012B": "i\u0304",
    "\u012D": "i\u0306",
    "\u01D0": "i\u030C",
    "\xEE": "i\u0302",
    "\u01F0": "j\u030C",
    "\u0135": "j\u0302",
    "\u1E31": "k\u0301",
    "\u01E9": "k\u030C",
    "\u0137": "k\u0327",
    "\u013A": "l\u0301",
    "\u013E": "l\u030C",
    "\u013C": "l\u0327",
    "\u1E3F": "m\u0301",
    "\u1E41": "m\u0307",
    "\u0144": "n\u0301",
    "\u01F9": "n\u0300",
    "\xF1": "n\u0303",
    "\u0148": "n\u030C",
    "\u1E45": "n\u0307",
    "\u0146": "n\u0327",
    "\xF3": "o\u0301",
    "\xF2": "o\u0300",
    "\xF6": "o\u0308",
    "\u022B": "o\u0308\u0304",
    "\xF5": "o\u0303",
    "\u1E4D": "o\u0303\u0301",
    "\u1E4F": "o\u0303\u0308",
    "\u022D": "o\u0303\u0304",
    "\u014D": "o\u0304",
    "\u1E53": "o\u0304\u0301",
    "\u1E51": "o\u0304\u0300",
    "\u014F": "o\u0306",
    "\u01D2": "o\u030C",
    "\xF4": "o\u0302",
    "\u1ED1": "o\u0302\u0301",
    "\u1ED3": "o\u0302\u0300",
    "\u1ED7": "o\u0302\u0303",
    "\u022F": "o\u0307",
    "\u0231": "o\u0307\u0304",
    "\u0151": "o\u030B",
    "\u1E55": "p\u0301",
    "\u1E57": "p\u0307",
    "\u0155": "r\u0301",
    "\u0159": "r\u030C",
    "\u1E59": "r\u0307",
    "\u0157": "r\u0327",
    "\u015B": "s\u0301",
    "\u1E65": "s\u0301\u0307",
    "\u0161": "s\u030C",
    "\u1E67": "s\u030C\u0307",
    "\u015D": "s\u0302",
    "\u1E61": "s\u0307",
    "\u015F": "s\u0327",
    "\u1E97": "t\u0308",
    "\u0165": "t\u030C",
    "\u1E6B": "t\u0307",
    "\u0163": "t\u0327",
    "\xFA": "u\u0301",
    "\xF9": "u\u0300",
    "\xFC": "u\u0308",
    "\u01D8": "u\u0308\u0301",
    "\u01DC": "u\u0308\u0300",
    "\u01D6": "u\u0308\u0304",
    "\u01DA": "u\u0308\u030C",
    "\u0169": "u\u0303",
    "\u1E79": "u\u0303\u0301",
    "\u016B": "u\u0304",
    "\u1E7B": "u\u0304\u0308",
    "\u016D": "u\u0306",
    "\u01D4": "u\u030C",
    "\xFB": "u\u0302",
    "\u016F": "u\u030A",
    "\u0171": "u\u030B",
    "\u1E7D": "v\u0303",
    "\u1E83": "w\u0301",
    "\u1E81": "w\u0300",
    "\u1E85": "w\u0308",
    "\u0175": "w\u0302",
    "\u1E87": "w\u0307",
    "\u1E98": "w\u030A",
    "\u1E8D": "x\u0308",
    "\u1E8B": "x\u0307",
    "\xFD": "y\u0301",
    "\u1EF3": "y\u0300",
    "\xFF": "y\u0308",
    "\u1EF9": "y\u0303",
    "\u0233": "y\u0304",
    "\u0177": "y\u0302",
    "\u1E8F": "y\u0307",
    "\u1E99": "y\u030A",
    "\u017A": "z\u0301",
    "\u017E": "z\u030C",
    "\u1E91": "z\u0302",
    "\u017C": "z\u0307",
    "\xC1": "A\u0301",
    "\xC0": "A\u0300",
    "\xC4": "A\u0308",
    "\u01DE": "A\u0308\u0304",
    "\xC3": "A\u0303",
    "\u0100": "A\u0304",
    "\u0102": "A\u0306",
    "\u1EAE": "A\u0306\u0301",
    "\u1EB0": "A\u0306\u0300",
    "\u1EB4": "A\u0306\u0303",
    "\u01CD": "A\u030C",
    "\xC2": "A\u0302",
    "\u1EA4": "A\u0302\u0301",
    "\u1EA6": "A\u0302\u0300",
    "\u1EAA": "A\u0302\u0303",
    "\u0226": "A\u0307",
    "\u01E0": "A\u0307\u0304",
    "\xC5": "A\u030A",
    "\u01FA": "A\u030A\u0301",
    "\u1E02": "B\u0307",
    "\u0106": "C\u0301",
    "\u1E08": "C\u0327\u0301",
    "\u010C": "C\u030C",
    "\u0108": "C\u0302",
    "\u010A": "C\u0307",
    "\xC7": "C\u0327",
    "\u010E": "D\u030C",
    "\u1E0A": "D\u0307",
    "\u1E10": "D\u0327",
    "\xC9": "E\u0301",
    "\xC8": "E\u0300",
    "\xCB": "E\u0308",
    "\u1EBC": "E\u0303",
    "\u0112": "E\u0304",
    "\u1E16": "E\u0304\u0301",
    "\u1E14": "E\u0304\u0300",
    "\u0114": "E\u0306",
    "\u1E1C": "E\u0327\u0306",
    "\u011A": "E\u030C",
    "\xCA": "E\u0302",
    "\u1EBE": "E\u0302\u0301",
    "\u1EC0": "E\u0302\u0300",
    "\u1EC4": "E\u0302\u0303",
    "\u0116": "E\u0307",
    "\u0228": "E\u0327",
    "\u1E1E": "F\u0307",
    "\u01F4": "G\u0301",
    "\u1E20": "G\u0304",
    "\u011E": "G\u0306",
    "\u01E6": "G\u030C",
    "\u011C": "G\u0302",
    "\u0120": "G\u0307",
    "\u0122": "G\u0327",
    "\u1E26": "H\u0308",
    "\u021E": "H\u030C",
    "\u0124": "H\u0302",
    "\u1E22": "H\u0307",
    "\u1E28": "H\u0327",
    "\xCD": "I\u0301",
    "\xCC": "I\u0300",
    "\xCF": "I\u0308",
    "\u1E2E": "I\u0308\u0301",
    "\u0128": "I\u0303",
    "\u012A": "I\u0304",
    "\u012C": "I\u0306",
    "\u01CF": "I\u030C",
    "\xCE": "I\u0302",
    "\u0130": "I\u0307",
    "\u0134": "J\u0302",
    "\u1E30": "K\u0301",
    "\u01E8": "K\u030C",
    "\u0136": "K\u0327",
    "\u0139": "L\u0301",
    "\u013D": "L\u030C",
    "\u013B": "L\u0327",
    "\u1E3E": "M\u0301",
    "\u1E40": "M\u0307",
    "\u0143": "N\u0301",
    "\u01F8": "N\u0300",
    "\xD1": "N\u0303",
    "\u0147": "N\u030C",
    "\u1E44": "N\u0307",
    "\u0145": "N\u0327",
    "\xD3": "O\u0301",
    "\xD2": "O\u0300",
    "\xD6": "O\u0308",
    "\u022A": "O\u0308\u0304",
    "\xD5": "O\u0303",
    "\u1E4C": "O\u0303\u0301",
    "\u1E4E": "O\u0303\u0308",
    "\u022C": "O\u0303\u0304",
    "\u014C": "O\u0304",
    "\u1E52": "O\u0304\u0301",
    "\u1E50": "O\u0304\u0300",
    "\u014E": "O\u0306",
    "\u01D1": "O\u030C",
    "\xD4": "O\u0302",
    "\u1ED0": "O\u0302\u0301",
    "\u1ED2": "O\u0302\u0300",
    "\u1ED6": "O\u0302\u0303",
    "\u022E": "O\u0307",
    "\u0230": "O\u0307\u0304",
    "\u0150": "O\u030B",
    "\u1E54": "P\u0301",
    "\u1E56": "P\u0307",
    "\u0154": "R\u0301",
    "\u0158": "R\u030C",
    "\u1E58": "R\u0307",
    "\u0156": "R\u0327",
    "\u015A": "S\u0301",
    "\u1E64": "S\u0301\u0307",
    "\u0160": "S\u030C",
    "\u1E66": "S\u030C\u0307",
    "\u015C": "S\u0302",
    "\u1E60": "S\u0307",
    "\u015E": "S\u0327",
    "\u0164": "T\u030C",
    "\u1E6A": "T\u0307",
    "\u0162": "T\u0327",
    "\xDA": "U\u0301",
    "\xD9": "U\u0300",
    "\xDC": "U\u0308",
    "\u01D7": "U\u0308\u0301",
    "\u01DB": "U\u0308\u0300",
    "\u01D5": "U\u0308\u0304",
    "\u01D9": "U\u0308\u030C",
    "\u0168": "U\u0303",
    "\u1E78": "U\u0303\u0301",
    "\u016A": "U\u0304",
    "\u1E7A": "U\u0304\u0308",
    "\u016C": "U\u0306",
    "\u01D3": "U\u030C",
    "\xDB": "U\u0302",
    "\u016E": "U\u030A",
    "\u0170": "U\u030B",
    "\u1E7C": "V\u0303",
    "\u1E82": "W\u0301",
    "\u1E80": "W\u0300",
    "\u1E84": "W\u0308",
    "\u0174": "W\u0302",
    "\u1E86": "W\u0307",
    "\u1E8C": "X\u0308",
    "\u1E8A": "X\u0307",
    "\xDD": "Y\u0301",
    "\u1EF2": "Y\u0300",
    "\u0178": "Y\u0308",
    "\u1EF8": "Y\u0303",
    "\u0232": "Y\u0304",
    "\u0176": "Y\u0302",
    "\u1E8E": "Y\u0307",
    "\u0179": "Z\u0301",
    "\u017D": "Z\u030C",
    "\u1E90": "Z\u0302",
    "\u017B": "Z\u0307",
    "\u03AC": "\u03B1\u0301",
    "\u1F70": "\u03B1\u0300",
    "\u1FB1": "\u03B1\u0304",
    "\u1FB0": "\u03B1\u0306",
    "\u03AD": "\u03B5\u0301",
    "\u1F72": "\u03B5\u0300",
    "\u03AE": "\u03B7\u0301",
    "\u1F74": "\u03B7\u0300",
    "\u03AF": "\u03B9\u0301",
    "\u1F76": "\u03B9\u0300",
    "\u03CA": "\u03B9\u0308",
    "\u0390": "\u03B9\u0308\u0301",
    "\u1FD2": "\u03B9\u0308\u0300",
    "\u1FD1": "\u03B9\u0304",
    "\u1FD0": "\u03B9\u0306",
    "\u03CC": "\u03BF\u0301",
    "\u1F78": "\u03BF\u0300",
    "\u03CD": "\u03C5\u0301",
    "\u1F7A": "\u03C5\u0300",
    "\u03CB": "\u03C5\u0308",
    "\u03B0": "\u03C5\u0308\u0301",
    "\u1FE2": "\u03C5\u0308\u0300",
    "\u1FE1": "\u03C5\u0304",
    "\u1FE0": "\u03C5\u0306",
    "\u03CE": "\u03C9\u0301",
    "\u1F7C": "\u03C9\u0300",
    "\u038E": "\u03A5\u0301",
    "\u1FEA": "\u03A5\u0300",
    "\u03AB": "\u03A5\u0308",
    "\u1FE9": "\u03A5\u0304",
    "\u1FE8": "\u03A5\u0306",
    "\u038F": "\u03A9\u0301",
    "\u1FFA": "\u03A9\u0300"
  };
  var Parser = class _Parser {
    constructor(input, settings) {
      this.mode = void 0;
      this.gullet = void 0;
      this.settings = void 0;
      this.leftrightDepth = void 0;
      this.nextToken = void 0;
      this.mode = "math";
      this.gullet = new MacroExpander(input, settings, this.mode);
      this.settings = settings;
      this.leftrightDepth = 0;
      this.nextToken = null;
    }
    /**
     * Checks a result to make sure it has the right type, and throws an
     * appropriate error otherwise.
     */
    expect(text2, consume) {
      if (consume === void 0) {
        consume = true;
      }
      if (this.fetch().text !== text2) {
        throw new ParseError("Expected '" + text2 + "', got '" + this.fetch().text + "'", this.fetch());
      }
      if (consume) {
        this.consume();
      }
    }
    /**
     * Discards the current lookahead token, considering it consumed.
     */
    consume() {
      this.nextToken = null;
    }
    /**
     * Return the current lookahead token, or if there isn't one (at the
     * beginning, or if the previous lookahead token was consume()d),
     * fetch the next token as the new lookahead token and return it.
     */
    fetch() {
      if (this.nextToken == null) {
        this.nextToken = this.gullet.expandNextToken();
      }
      return this.nextToken;
    }
    /**
     * Switches between "text" and "math" modes.
     */
    switchMode(newMode) {
      this.mode = newMode;
      this.gullet.switchMode(newMode);
    }
    /**
     * Main parsing function, which parses an entire input.
     */
    parse() {
      if (!this.settings.globalGroup) {
        this.gullet.beginGroup();
      }
      if (this.settings.colorIsTextColor) {
        this.gullet.macros.set("\\color", "\\textcolor");
      }
      try {
        var parse = this.parseExpression(false);
        this.expect("EOF");
        if (!this.settings.globalGroup) {
          this.gullet.endGroup();
        }
        return parse;
      } finally {
        this.gullet.endGroups();
      }
    }
    /**
     * Fully parse a separate sequence of tokens as a separate job.
     * Tokens should be specified in reverse order, as in a MacroDefinition.
     */
    subparse(tokens) {
      var oldToken = this.nextToken;
      this.consume();
      this.gullet.pushToken(new Token("}"));
      this.gullet.pushTokens(tokens);
      var parse = this.parseExpression(false);
      this.expect("}");
      this.nextToken = oldToken;
      return parse;
    }
    /**
     * Parses an "expression", which is a list of atoms.
     *
     * `breakOnInfix`: Should the parsing stop when we hit infix nodes? This
     *                 happens when functions have higher precedence than infix
     *                 nodes in implicit parses.
     *
     * `breakOnTokenText`: The text of the token that the expression should end
     *                     with, or `null` if something else should end the
     *                     expression.
     */
    parseExpression(breakOnInfix, breakOnTokenText) {
      var body = [];
      while (true) {
        if (this.mode === "math") {
          this.consumeSpaces();
        }
        var lex = this.fetch();
        if (_Parser.endOfExpression.has(lex.text)) {
          break;
        }
        if (breakOnTokenText && lex.text === breakOnTokenText) {
          break;
        }
        if (breakOnInfix && functions[lex.text] && functions[lex.text].infix) {
          break;
        }
        var atom = this.parseAtom(breakOnTokenText);
        if (!atom) {
          break;
        } else if (atom.type === "internal") {
          continue;
        }
        body.push(atom);
      }
      if (this.mode === "text") {
        this.formLigatures(body);
      }
      return this.handleInfixNodes(body);
    }
    /**
     * Rewrites infix operators such as \over with corresponding commands such
     * as \frac.
     *
     * There can only be one infix operator per group.  If there's more than one
     * then the expression is ambiguous.  This can be resolved by adding {}.
     */
    handleInfixNodes(body) {
      var overIndex = -1;
      var funcName;
      for (var i3 = 0; i3 < body.length; i3++) {
        var node = body[i3];
        if (node.type === "infix") {
          if (overIndex !== -1) {
            throw new ParseError("only one infix operator per group", node.token);
          }
          overIndex = i3;
          funcName = node.replaceWith;
        }
      }
      if (overIndex !== -1 && funcName) {
        var numerNode;
        var denomNode;
        var numerBody = body.slice(0, overIndex);
        var denomBody = body.slice(overIndex + 1);
        if (numerBody.length === 1 && numerBody[0].type === "ordgroup") {
          numerNode = numerBody[0];
        } else {
          numerNode = {
            type: "ordgroup",
            mode: this.mode,
            body: numerBody
          };
        }
        if (denomBody.length === 1 && denomBody[0].type === "ordgroup") {
          denomNode = denomBody[0];
        } else {
          denomNode = {
            type: "ordgroup",
            mode: this.mode,
            body: denomBody
          };
        }
        var _node;
        if (funcName === "\\\\abovefrac") {
          _node = this.callFunction(funcName, [numerNode, body[overIndex], denomNode], []);
        } else {
          _node = this.callFunction(funcName, [numerNode, denomNode], []);
        }
        return [_node];
      } else {
        return body;
      }
    }
    /**
     * Handle a subscript or superscript with nice errors.
     */
    handleSupSubscript(name) {
      var symbolToken = this.fetch();
      var symbol = symbolToken.text;
      this.consume();
      this.consumeSpaces();
      var group;
      do {
        var _group;
        group = this.parseGroup(name);
      } while (((_group = group) == null ? void 0 : _group.type) === "internal");
      if (!group) {
        throw new ParseError("Expected group after '" + symbol + "'", symbolToken);
      }
      return group;
    }
    /**
     * Converts the textual input of an unsupported command into a text node
     * contained within a color node whose color is determined by errorColor
     */
    formatUnsupportedCmd(text2) {
      var textordArray = [];
      for (var i3 = 0; i3 < text2.length; i3++) {
        textordArray.push({
          type: "textord",
          mode: "text",
          text: text2[i3]
        });
      }
      var textNode = {
        type: "text",
        mode: this.mode,
        body: textordArray
      };
      var colorNode = {
        type: "color",
        mode: this.mode,
        color: this.settings.errorColor,
        body: [textNode]
      };
      return colorNode;
    }
    /**
     * Parses a group with optional super/subscripts.
     */
    parseAtom(breakOnTokenText) {
      var base = this.parseGroup("atom", breakOnTokenText);
      if ((base == null ? void 0 : base.type) === "internal") {
        return base;
      }
      if (this.mode === "text") {
        return base;
      }
      var superscript;
      var subscript;
      while (true) {
        this.consumeSpaces();
        var lex = this.fetch();
        if (lex.text === "\\limits" || lex.text === "\\nolimits") {
          if (base && base.type === "op") {
            var limits = lex.text === "\\limits";
            base.limits = limits;
            base.alwaysHandleSupSub = true;
          } else if (base && base.type === "operatorname") {
            if (base.alwaysHandleSupSub) {
              base.limits = lex.text === "\\limits";
            }
          } else {
            throw new ParseError("Limit controls must follow a math operator", lex);
          }
          this.consume();
        } else if (lex.text === "^") {
          if (superscript) {
            throw new ParseError("Double superscript", lex);
          }
          superscript = this.handleSupSubscript("superscript");
        } else if (lex.text === "_") {
          if (subscript) {
            throw new ParseError("Double subscript", lex);
          }
          subscript = this.handleSupSubscript("subscript");
        } else if (lex.text === "'") {
          if (superscript) {
            throw new ParseError("Double superscript", lex);
          }
          var prime = {
            type: "textord",
            mode: this.mode,
            text: "\\prime"
          };
          var primes = [prime];
          this.consume();
          while (this.fetch().text === "'") {
            primes.push(prime);
            this.consume();
          }
          if (this.fetch().text === "^") {
            primes.push(this.handleSupSubscript("superscript"));
          }
          superscript = {
            type: "ordgroup",
            mode: this.mode,
            body: primes
          };
        } else if (uSubsAndSups[lex.text]) {
          var isSub = unicodeSubRegEx.test(lex.text);
          var subsupTokens = [];
          subsupTokens.push(new Token(uSubsAndSups[lex.text]));
          this.consume();
          while (true) {
            var token = this.fetch().text;
            if (!uSubsAndSups[token]) {
              break;
            }
            if (unicodeSubRegEx.test(token) !== isSub) {
              break;
            }
            subsupTokens.unshift(new Token(uSubsAndSups[token]));
            this.consume();
          }
          var body = this.subparse(subsupTokens);
          if (isSub) {
            subscript = {
              type: "ordgroup",
              mode: "math",
              body
            };
          } else {
            superscript = {
              type: "ordgroup",
              mode: "math",
              body
            };
          }
        } else {
          break;
        }
      }
      if (superscript || subscript) {
        return {
          type: "supsub",
          mode: this.mode,
          base,
          sup: superscript,
          sub: subscript
        };
      } else {
        return base;
      }
    }
    /**
     * Parses an entire function, including its base and all of its arguments.
     */
    parseFunction(breakOnTokenText, name) {
      var token = this.fetch();
      var func = token.text;
      var funcData = functions[func];
      if (!funcData) {
        return null;
      }
      this.consume();
      if (name && name !== "atom" && !funcData.allowedInArgument) {
        throw new ParseError("Got function '" + func + "' with no arguments" + (name ? " as " + name : ""), token);
      } else if (this.mode === "text" && !funcData.allowedInText) {
        throw new ParseError("Can't use function '" + func + "' in text mode", token);
      } else if (this.mode === "math" && funcData.allowedInMath === false) {
        throw new ParseError("Can't use function '" + func + "' in math mode", token);
      }
      var {
        args,
        optArgs
      } = this.parseArguments(func, funcData);
      return this.callFunction(func, args, optArgs, token, breakOnTokenText);
    }
    /**
     * Call a function handler with a suitable context and arguments.
     */
    callFunction(name, args, optArgs, token, breakOnTokenText) {
      var context = {
        funcName: name,
        parser: this,
        token,
        breakOnTokenText
      };
      var func = functions[name];
      if (func && func.handler) {
        return func.handler(context, args, optArgs);
      } else {
        throw new ParseError("No function handler for " + name);
      }
    }
    /**
     * Parses the arguments of a function or environment
     */
    parseArguments(func, funcData) {
      var totalArgs = funcData.numArgs + funcData.numOptionalArgs;
      if (totalArgs === 0) {
        return {
          args: [],
          optArgs: []
        };
      }
      var args = [];
      var optArgs = [];
      for (var i3 = 0; i3 < totalArgs; i3++) {
        var argType = funcData.argTypes && funcData.argTypes[i3];
        var isOptional = i3 < funcData.numOptionalArgs;
        if ("primitive" in funcData && funcData.primitive && argType == null || // \sqrt expands into primitive if optional argument doesn't exist
        funcData.type === "sqrt" && i3 === 1 && optArgs[0] == null) {
          argType = "primitive";
        }
        var arg = this.parseGroupOfType("argument to '" + func + "'", argType, isOptional);
        if (isOptional) {
          optArgs.push(arg);
        } else if (arg != null) {
          args.push(arg);
        } else {
          throw new ParseError("Null argument, please report this as a bug");
        }
      }
      return {
        args,
        optArgs
      };
    }
    /**
     * Parses a group when the mode is changing.
     */
    parseGroupOfType(name, type, optional) {
      switch (type) {
        case "color":
          return this.parseColorGroup(optional);
        case "size":
          return this.parseSizeGroup(optional);
        case "url":
          return this.parseUrlGroup(optional);
        case "math":
        case "text":
          return this.parseArgumentGroup(optional, type);
        case "hbox": {
          var group = this.parseArgumentGroup(optional, "text");
          return group != null ? {
            type: "styling",
            mode: group.mode,
            body: [group],
            style: "text",
            // simulate \textstyle
            resetFont: true
          } : null;
        }
        case "raw": {
          var token = this.parseStringGroup("raw", optional);
          return token != null ? {
            type: "raw",
            mode: "text",
            string: token.text
          } : null;
        }
        case "primitive": {
          if (optional) {
            throw new ParseError("A primitive argument cannot be optional");
          }
          var _group2 = this.parseGroup(name);
          if (_group2 == null) {
            throw new ParseError("Expected group as " + name, this.fetch());
          }
          return _group2;
        }
        case "original":
        case null:
        case void 0:
          return this.parseArgumentGroup(optional);
        default:
          throw new ParseError("Unknown group type as " + name, this.fetch());
      }
    }
    /**
     * Discard any space tokens, fetching the next non-space token.
     */
    consumeSpaces() {
      while (this.fetch().text === " ") {
        this.consume();
      }
    }
    /**
     * Parses a group, essentially returning the string formed by the
     * brace-enclosed tokens plus some position information.
     */
    parseStringGroup(modeName, optional) {
      var argToken = this.gullet.scanArgument(optional);
      if (argToken == null) {
        return null;
      }
      var str = "";
      var nextToken;
      while ((nextToken = this.fetch()).text !== "EOF") {
        str += nextToken.text;
        this.consume();
      }
      this.consume();
      argToken.text = str;
      return argToken;
    }
    /**
     * Parses a regex-delimited group: the largest sequence of tokens
     * whose concatenated strings match `regex`. Returns the string
     * formed by the tokens plus some position information.
     */
    parseRegexGroup(regex, modeName) {
      var firstToken = this.fetch();
      var lastToken = firstToken;
      var str = "";
      var nextToken;
      while ((nextToken = this.fetch()).text !== "EOF" && regex.test(str + nextToken.text)) {
        lastToken = nextToken;
        str += lastToken.text;
        this.consume();
      }
      if (str === "") {
        throw new ParseError("Invalid " + modeName + ": '" + firstToken.text + "'", firstToken);
      }
      return firstToken.range(lastToken, str);
    }
    /**
     * Parses a color description.
     */
    parseColorGroup(optional) {
      var res = this.parseStringGroup("color", optional);
      if (res == null) {
        return null;
      }
      var match = /^(#[a-f0-9]{3,4}|#[a-f0-9]{6}|#[a-f0-9]{8}|[a-f0-9]{6}|[a-z]+)$/i.exec(res.text);
      if (!match) {
        throw new ParseError("Invalid color: '" + res.text + "'", res);
      }
      var color = match[0];
      if (/^[0-9a-f]{6}$/i.test(color)) {
        color = "#" + color;
      }
      return {
        type: "color-token",
        mode: this.mode,
        color
      };
    }
    /**
     * Parses a size specification, consisting of magnitude and unit.
     */
    parseSizeGroup(optional) {
      var res;
      var isBlank = false;
      this.gullet.consumeSpaces();
      if (!optional && this.gullet.future().text !== "{") {
        res = this.parseRegexGroup(/^[-+]? *(?:$|\d+|\d+\.\d*|\.\d*) *[a-z]{0,2} *$/, "size");
      } else {
        res = this.parseStringGroup("size", optional);
      }
      if (!res) {
        return null;
      }
      if (!optional && res.text.length === 0) {
        res.text = "0pt";
        isBlank = true;
      }
      var match = /([-+]?) *(\d+(?:\.\d*)?|\.\d+) *([a-z]{2})/.exec(res.text);
      if (!match) {
        throw new ParseError("Invalid size: '" + res.text + "'", res);
      }
      var data = {
        number: +(match[1] + match[2]),
        // sign + magnitude, cast to number
        unit: match[3]
      };
      if (!validUnit(data)) {
        throw new ParseError("Invalid unit: '" + data.unit + "'", res);
      }
      return {
        type: "size",
        mode: this.mode,
        value: data,
        isBlank
      };
    }
    /**
     * Parses an URL, checking escaped letters and allowed protocols,
     * and setting the catcode of % as an active character (as in \hyperref).
     */
    parseUrlGroup(optional) {
      this.gullet.lexer.setCatcode("%", 13);
      this.gullet.lexer.setCatcode("~", 12);
      var res = this.parseStringGroup("url", optional);
      this.gullet.lexer.setCatcode("%", 14);
      this.gullet.lexer.setCatcode("~", 13);
      if (res == null) {
        return null;
      }
      var url = res.text.replace(/\\([#$%&~_^{}])/g, "$1");
      return {
        type: "url",
        mode: this.mode,
        url
      };
    }
    /**
     * Parses an argument with the mode specified.
     */
    parseArgumentGroup(optional, mode) {
      var argToken = this.gullet.scanArgument(optional);
      if (argToken == null) {
        return null;
      }
      var outerMode = this.mode;
      if (mode) {
        this.switchMode(mode);
      }
      this.gullet.beginGroup();
      var expression = this.parseExpression(false, "EOF");
      this.expect("EOF");
      this.gullet.endGroup();
      var result = {
        type: "ordgroup",
        mode: this.mode,
        loc: argToken.loc,
        body: expression
      };
      if (mode) {
        this.switchMode(outerMode);
      }
      return result;
    }
    /**
     * Parses an ordinary group, which is either a single nucleus (like "x")
     * or an expression in braces (like "{x+y}") or an implicit group, a group
     * that starts at the current position, and ends right before a higher explicit
     * group ends, or at EOF.
     */
    parseGroup(name, breakOnTokenText) {
      var firstToken = this.fetch();
      var text2 = firstToken.text;
      var result;
      if (text2 === "{" || text2 === "\\begingroup") {
        this.consume();
        var groupEnd = text2 === "{" ? "}" : "\\endgroup";
        this.gullet.beginGroup();
        var expression = this.parseExpression(false, groupEnd);
        var lastToken = this.fetch();
        this.expect(groupEnd);
        this.gullet.endGroup();
        result = {
          type: "ordgroup",
          mode: this.mode,
          loc: SourceLocation.range(firstToken, lastToken),
          body: expression,
          // A group formed by \begingroup...\endgroup is a semi-simple group
          // which doesn't affect spacing in math mode, i.e., is transparent.
          // https://tex.stackexchange.com/questions/1930/when-should-one-
          // use-begingroup-instead-of-bgroup
          semisimple: text2 === "\\begingroup" || void 0
        };
      } else {
        result = this.parseFunction(breakOnTokenText, name) || this.parseSymbol();
        if (result == null && text2[0] === "\\" && !implicitCommands.hasOwnProperty(text2)) {
          if (this.settings.throwOnError) {
            throw new ParseError("Undefined control sequence: " + text2, firstToken);
          }
          result = this.formatUnsupportedCmd(text2);
          this.consume();
        }
      }
      return result;
    }
    /**
     * Form ligature-like combinations of characters for text mode.
     * This includes inputs like "--", "---", "``" and "''".
     * The result will simply replace multiple textord nodes with a single
     * character in each value by a single textord node having multiple
     * characters in its value.  The representation is still ASCII source.
     * The group will be modified in place.
     */
    formLigatures(group) {
      var n = group.length - 1;
      for (var i3 = 0; i3 < n; ++i3) {
        var a2 = group[i3];
        if (a2.type !== "textord") {
          continue;
        }
        var v2 = a2.text;
        var next = group[i3 + 1];
        if (!next || next.type !== "textord") {
          continue;
        }
        if (v2 === "-" && next.text === "-") {
          var afterNext = group[i3 + 2];
          if (i3 + 1 < n && afterNext && afterNext.type === "textord" && afterNext.text === "-") {
            group.splice(i3, 3, {
              type: "textord",
              mode: "text",
              loc: SourceLocation.range(a2, afterNext),
              text: "---"
            });
            n -= 2;
          } else {
            group.splice(i3, 2, {
              type: "textord",
              mode: "text",
              loc: SourceLocation.range(a2, next),
              text: "--"
            });
            n -= 1;
          }
        }
        if ((v2 === "'" || v2 === "`") && next.text === v2) {
          group.splice(i3, 2, {
            type: "textord",
            mode: "text",
            loc: SourceLocation.range(a2, next),
            text: v2 + v2
          });
          n -= 1;
        }
      }
    }
    /**
     * Parse a single symbol out of the string. Here, we handle single character
     * symbols and special functions like \verb.
     */
    parseSymbol() {
      var nucleus = this.fetch();
      var text2 = nucleus.text;
      if (/^\\verb[^a-zA-Z]/.test(text2)) {
        this.consume();
        var arg = text2.slice(5);
        var star = arg.charAt(0) === "*";
        if (star) {
          arg = arg.slice(1);
        }
        if (arg.length < 2 || arg.charAt(0) !== arg.slice(-1)) {
          throw new ParseError("\\verb assertion failed --\n                    please report what input caused this bug");
        }
        arg = arg.slice(1, -1);
        return {
          type: "verb",
          mode: "text",
          body: arg,
          star
        };
      }
      if (unicodeSymbols.hasOwnProperty(text2[0]) && !symbols[this.mode][text2[0]]) {
        if (this.settings.strict && this.mode === "math") {
          this.settings.reportNonstrict("unicodeTextInMathMode", 'Accented Unicode text character "' + text2[0] + '" used in math mode', nucleus);
        }
        text2 = unicodeSymbols[text2[0]] + text2.slice(1);
      }
      var match = combiningDiacriticalMarksEndRegex.exec(text2);
      if (match) {
        text2 = text2.substring(0, match.index);
        if (text2 === "i") {
          text2 = "\u0131";
        } else if (text2 === "j") {
          text2 = "\u0237";
        }
      }
      var symbol;
      if (symbols[this.mode][text2]) {
        if (this.settings.strict && this.mode === "math" && extraLatin.includes(text2)) {
          this.settings.reportNonstrict("unicodeTextInMathMode", 'Latin-1/Unicode text character "' + text2[0] + '" used in math mode', nucleus);
        }
        var group = symbols[this.mode][text2].group;
        var loc = SourceLocation.range(nucleus);
        var s2;
        if (isAtom(group)) {
          s2 = {
            type: "atom",
            mode: this.mode,
            family: group,
            loc,
            text: text2
          };
        } else {
          s2 = {
            type: group,
            mode: this.mode,
            loc,
            text: text2
          };
        }
        symbol = s2;
      } else if (text2.charCodeAt(0) >= 128) {
        if (this.settings.strict) {
          if (!supportedCodepoint(text2.charCodeAt(0))) {
            this.settings.reportNonstrict("unknownSymbol", 'Unrecognized Unicode character "' + text2[0] + '"' + (" (" + text2.charCodeAt(0) + ")"), nucleus);
          } else if (this.mode === "math") {
            this.settings.reportNonstrict("unicodeTextInMathMode", 'Unicode text character "' + text2[0] + '" used in math mode', nucleus);
          }
        }
        symbol = {
          type: "textord",
          mode: "text",
          loc: SourceLocation.range(nucleus),
          text: text2
        };
      } else {
        return null;
      }
      this.consume();
      if (match) {
        for (var i3 = 0; i3 < match[0].length; i3++) {
          var accent2 = match[0][i3];
          if (!unicodeAccents[accent2]) {
            throw new ParseError("Unknown accent ' " + accent2 + "'", nucleus);
          }
          var command = unicodeAccents[accent2][this.mode] || unicodeAccents[accent2].text;
          if (!command) {
            throw new ParseError("Accent " + accent2 + " unsupported in " + this.mode + " mode", nucleus);
          }
          symbol = {
            type: "accent",
            mode: this.mode,
            loc: SourceLocation.range(nucleus),
            label: command,
            isStretchy: false,
            isShifty: true,
            base: symbol
          };
        }
      }
      return symbol;
    }
  };
  Parser.endOfExpression = /* @__PURE__ */ new Set(["}", "\\endgroup", "\\end", "\\right", "&"]);
  var parseTree = function parseTree2(toParse, settings) {
    if (!(typeof toParse === "string" || toParse instanceof String)) {
      throw new TypeError("KaTeX can only parse string typed expression");
    }
    var parser = new Parser(toParse, settings);
    delete parser.gullet.macros.current["\\df@tag"];
    var tree = parser.parse();
    delete parser.gullet.macros.current["\\current@color"];
    delete parser.gullet.macros.current["\\color"];
    if (parser.gullet.macros.get("\\df@tag")) {
      if (!settings.displayMode) {
        throw new ParseError("\\tag works only in display equations");
      }
      tree = [{
        type: "tag",
        mode: "text",
        body: tree,
        tag: parser.subparse([new Token("\\df@tag")])
      }];
    }
    return tree;
  };
  var render = function render2(expression, baseNode, options) {
    baseNode.textContent = "";
    var node = renderToDomTree(expression, options).toNode();
    baseNode.appendChild(node);
  };
  if (typeof document !== "undefined") {
    if (document.compatMode !== "CSS1Compat") {
      typeof console !== "undefined" && console.warn("Warning: KaTeX doesn't work in quirks mode. Make sure your website has a suitable doctype.");
      render = function render4() {
        throw new ParseError("KaTeX doesn't work in quirks mode.");
      };
    }
  }
  var renderToString = function renderToString2(expression, options) {
    var markup = renderToDomTree(expression, options).toMarkup();
    return markup;
  };
  var generateParseTree = function generateParseTree2(expression, options) {
    var settings = new Settings(options);
    return parseTree(expression, settings);
  };
  var renderError = function renderError2(error, expression, options) {
    if (options.throwOnError || !(error instanceof ParseError)) {
      throw error;
    }
    var node = makeSpan(["katex-error"], [new SymbolNode(expression)]);
    node.setAttribute("title", error.toString());
    node.setAttribute("style", "color:" + options.errorColor);
    return node;
  };
  var renderToDomTree = function renderToDomTree2(expression, options) {
    var settings = new Settings(options);
    try {
      var tree = parseTree(expression, settings);
      return buildTree(tree, expression, settings);
    } catch (error) {
      return renderError(error, expression, settings);
    }
  };
  var renderToHTMLTree = function renderToHTMLTree2(expression, options) {
    var settings = new Settings(options);
    try {
      var tree = parseTree(expression, settings);
      return buildHTMLTree(tree, expression, settings);
    } catch (error) {
      return renderError(error, expression, settings);
    }
  };
  var version = "0.16.47";
  var __domTree = {
    Span,
    Anchor,
    SymbolNode,
    SvgNode,
    PathNode,
    LineNode
  };
  var katex = {
    /**
     * Current KaTeX version
     */
    version,
    /**
     * Renders the given LaTeX into an HTML+MathML combination, and adds
     * it as a child to the specified DOM node.
     */
    render,
    /**
     * Renders the given LaTeX into an HTML+MathML combination string,
     * for sending to the client.
     */
    renderToString,
    /**
     * KaTeX error, usually during parsing.
     */
    ParseError,
    /**
     * The schema of Settings
     */
    SETTINGS_SCHEMA,
    /**
     * Parses the given LaTeX into KaTeX's internal parse tree structure,
     * without rendering to HTML or MathML.
     *
     * NOTE: This method is not currently recommended for public use.
     * The internal tree representation is unstable and is very likely
     * to change. Use at your own risk.
     */
    __parse: generateParseTree,
    /**
     * Renders the given LaTeX into an HTML+MathML internal DOM tree
     * representation, without flattening that representation to a string.
     *
     * NOTE: This method is not currently recommended for public use.
     * The internal tree representation is unstable and is very likely
     * to change. Use at your own risk.
     */
    __renderToDomTree: renderToDomTree,
    /**
     * Renders the given LaTeX into an HTML internal DOM tree representation,
     * without MathML and without flattening that representation to a string.
     *
     * NOTE: This method is not currently recommended for public use.
     * The internal tree representation is unstable and is very likely
     * to change. Use at your own risk.
     */
    __renderToHTMLTree: renderToHTMLTree,
    /**
     * extends internal font metrics object with a new object
     * each key in the new object represents a font name
    */
    __setFontMetrics: setFontMetrics,
    /**
     * adds a new symbol to builtin symbols table
     */
    __defineSymbol: defineSymbol,
    /**
     * adds a new function to builtin function list,
     * which directly produce parse tree elements
     * and have their own html/mathml builders
     */
    __defineFunction: defineFunction,
    /**
     * adds a new macro to builtin macro list
     */
    __defineMacro: defineMacro,
    /**
     * Expose the dom tree node types, which can be useful for type checking nodes.
     *
     * NOTE: These methods are not currently recommended for public use.
     * The internal tree representation is unstable and is very likely
     * to change. Use at your own risk.
     */
    __domTree
  };

  // packages/player/dist/board.js
  var Board = class {
    el;
    disposers = [];
    constructor(store, items) {
      this.el = document.createElement("div");
      this.el.className = "xv-board-inner";
      for (const [id, item] of Object.entries(items)) {
        const itemEl = document.createElement("div");
        itemEl.dataset.id = id;
        const source = item.source;
        if (item.kind === "katex") {
          itemEl.innerHTML = katex.renderToString(source, { trust: true, strict: false, throwOnError: false });
        } else {
          itemEl.textContent = source;
        }
        this.el.append(itemEl);
        const stateKey = `board.${id}`;
        if (store.signals.has(stateKey)) {
          this.disposers.push(j(() => {
            itemEl.className = `xv-board-item xv-${String(store.signal(stateKey).value)}`;
          }));
        }
        for (const key of store.keys()) {
          if (!key.startsWith(`board.${id}.highlight`))
            continue;
          const tag = key.slice(`board.${id}.highlight`.length).replace(/^\./, "");
          this.disposers.push(j(() => {
            toggleHighlight(itemEl, tag, store.signal(key).value === true);
          }));
        }
      }
    }
    dispose() {
      for (const d2 of this.disposers)
        d2();
      this.el.remove();
    }
  };
  function toggleHighlight(itemEl, tag, on) {
    const targets = tag ? [...itemEl.querySelectorAll(`.${tag}`)] : [itemEl];
    for (const t2 of targets)
      t2.classList.toggle("xv-hl", on);
  }

  // packages/player/dist/captions.js
  function parseVtt(vtt) {
    const cues = [];
    const blocks = vtt.replace(/\r\n/g, "\n").split("\n\n");
    for (const block2 of blocks) {
      const lines = block2.split("\n").filter((l2) => l2.trim() !== "");
      const arrow = lines.findIndex((l2) => l2.includes("-->"));
      if (arrow === -1)
        continue;
      const [a2, b2] = lines[arrow].split("-->");
      cues.push({ start: parseStamp(a2.trim()), end: parseStamp(b2.trim()), text: lines.slice(arrow + 1).join(" ") });
    }
    return cues.sort((x2, y2) => x2.start - y2.start);
  }
  function parseStamp(s2) {
    const parts = s2.split(":").map(Number);
    if (parts.length === 3)
      return parts[0] * 3600 + parts[1] * 60 + parts[2];
    return parts[0] * 60 + parts[1];
  }
  function activeCue(cues, t2) {
    let lo = 0;
    let hi = cues.length - 1;
    while (lo <= hi) {
      const mid = lo + hi >> 1;
      const c2 = cues[mid];
      if (t2 < c2.start)
        hi = mid - 1;
      else if (t2 >= c2.end)
        lo = mid + 1;
      else
        return c2.text;
    }
    return "";
  }
  function latestCue(cues, t2) {
    let lo = 0;
    let hi = cues.length - 1;
    let latest = "";
    while (lo <= hi) {
      const mid = lo + hi >> 1;
      const cue = cues[mid];
      if (cue.start <= t2) {
        latest = cue.text;
        lo = mid + 1;
      } else {
        hi = mid - 1;
      }
    }
    return latest;
  }
  var Captions = class {
    el;
    cues;
    visible = false;
    constructor(vtt) {
      this.cues = parseVtt(vtt);
      this.el = document.createElement("div");
      this.el.className = "xv-captions";
    }
    update(t2) {
      this.el.textContent = this.visible ? activeCue(this.cues, t2) : "";
    }
    setVisible(on) {
      this.visible = on;
      if (!on)
        this.el.textContent = "";
    }
    latestText(t2) {
      return latestCue(this.cues, t2);
    }
  };

  // packages/player/dist/lesson-position.js
  function lessonPositionAt(t2, chapters, narrationJustHeard, pausePrompt = null) {
    let chapter = null;
    for (const entry of chapters) {
      if (entry.t > t2)
        break;
      chapter = entry.title;
    }
    return { chapter, narrationJustHeard: narrationJustHeard || null, pausePrompt };
  }

  // packages/player/dist/pause-gate.js
  var PAUSE_TAIL_SECONDS = 0;
  function pauseTime(pause, duration) {
    const delayed = pause.t + (pause.tail ?? PAUSE_TAIL_SECONDS);
    return duration > 0 ? Math.min(delayed, Math.max(pause.t, duration - 0.05)) : delayed;
  }
  var PauseGate = class {
    clock;
    pauses;
    satisfied = /* @__PURE__ */ new Set();
    active;
    lastT = 0;
    constructor(clock, pauses) {
      this.clock = clock;
      this.pauses = pauses;
      this.clock.on("play", () => {
        if (this.active)
          this.resolve();
      });
    }
    update(t2) {
      const seeked = Math.abs(t2 - this.lastT) >= 0.5;
      for (const p2 of this.pauses) {
        const stopT = pauseTime(p2, this.clock.duration);
        if (t2 < stopT) {
          this.satisfied.delete(p2.id);
          if (this.active?.id === p2.id)
            this.active = void 0;
        } else if (this.satisfied.has(p2.id)) {
          continue;
        } else if (seeked) {
          this.satisfied.add(p2.id);
        } else if (this.lastT <= stopT && stopT <= t2 && !this.active) {
          this.trigger(p2, stopT);
          break;
        }
      }
      this.lastT = t2;
    }
    /** Prompt for the authored gate that currently holds playback. */
    get activePrompt() {
      return this.active?.prompt ?? null;
    }
    trigger(p2, stopT) {
      this.active = p2;
      this.clock.pause();
      this.clock.seek(stopT);
    }
    resolve() {
      if (this.active)
        this.satisfied.add(this.active.id);
      this.active = void 0;
    }
  };

  // packages/player/dist/chrome.js
  function formatTime(sec) {
    const s2 = Math.max(0, Math.floor(sec));
    const h2 = Math.floor(s2 / 3600);
    const m2 = Math.floor(s2 % 3600 / 60);
    const ss = String(s2 % 60).padStart(2, "0");
    return h2 > 0 ? `${h2}:${String(m2).padStart(2, "0")}:${ss}` : `${m2}:${ss}`;
  }
  var Chrome = class {
    clock;
    tracks;
    el;
    playBtn;
    scrubber;
    elapsed;
    captionsOn = false;
    scrubbing = false;
    scrubTimer;
    constructor(clock, tracks, opts = {}) {
      this.clock = clock;
      this.tracks = tracks;
      const doc = document;
      this.el = div("xv-chrome");
      this.playBtn = doc.createElement("button");
      this.playBtn.className = "xv-play";
      this.playBtn.textContent = "\u25B6";
      this.playBtn.setAttribute("aria-label", "Play lesson");
      this.playBtn.onclick = () => this.togglePlay();
      this.scrubber = doc.createElement("input");
      this.scrubber.type = "range";
      this.scrubber.min = "0";
      this.scrubber.max = "1000";
      this.scrubber.value = "0";
      this.scrubber.className = "xv-scrubber";
      this.scrubber.setAttribute("aria-label", "Lesson position");
      const beginScrub = () => {
        this.scrubbing = true;
        clearTimeout(this.scrubTimer);
        this.scrubTimer = setTimeout(() => this.scrubbing = false, 400);
      };
      this.scrubber.addEventListener("input", () => {
        this.clock.seek(Number(this.scrubber.value) / 1e3 * this.duration());
        beginScrub();
      });
      this.scrubber.addEventListener("pointerdown", beginScrub);
      this.scrubber.addEventListener("change", () => {
        clearTimeout(this.scrubTimer);
        this.scrubbing = false;
      });
      this.elapsed = div("xv-elapsed");
      this.elapsed.textContent = "0:00 / 0:00";
      const captions = doc.createElement("button");
      captions.className = "xv-captions-toggle";
      captions.textContent = "CC";
      captions.setAttribute("aria-label", "Show captions");
      captions.setAttribute("aria-pressed", "false");
      captions.onclick = () => {
        this.captionsOn = !this.captionsOn;
        captions.setAttribute("aria-pressed", String(this.captionsOn));
        captions.setAttribute("aria-label", this.captionsOn ? "Hide captions" : "Show captions");
        opts.onCaptionsToggle?.(this.captionsOn);
      };
      const full = doc.createElement("button");
      full.className = "xv-fullscreen";
      full.textContent = "\u26F6";
      full.setAttribute("aria-label", "Enter full screen");
      full.onclick = () => this.toggleFullscreen();
      const credit = doc.createElement("a");
      credit.className = "xv-credit";
      credit.href = "https://github.com/scienceetonnante/tangible";
      credit.target = "_blank";
      credit.rel = "noopener noreferrer";
      credit.textContent = "Made with Tangible";
      credit.setAttribute("aria-label", "Made with Tangible (opens in a new tab)");
      this.el.append(this.playBtn, this.scrubber, this.elapsed, captions, full, credit);
    }
    /** Global keyboard shortcuts; returns a disposer. */
    bindKeys(target2 = window) {
      const onKey = (e2) => {
        const source = e2.target;
        if (source?.matches("input, textarea, select, button, [contenteditable=true]"))
          return;
        if (e2.key === " " || e2.key === "k") {
          e2.preventDefault();
          this.togglePlay();
        } else if (e2.key === "f")
          this.toggleFullscreen();
        else if (e2.key === "ArrowRight")
          this.clock.seek(this.clock.t + 5);
        else if (e2.key === "ArrowLeft")
          this.clock.seek(this.clock.t - 5);
      };
      target2.addEventListener("keydown", onKey);
      return () => target2.removeEventListener("keydown", onKey);
    }
    update(t2) {
      const d2 = this.duration();
      if (!this.scrubbing)
        this.scrubber.value = String(d2 > 0 ? Math.round(t2 / d2 * 1e3) : 0);
      this.elapsed.textContent = `${formatTime(t2)} / ${formatTime(d2)}`;
      this.playBtn.textContent = this.clock.playing ? "\u23F8" : "\u25B6";
      this.playBtn.setAttribute("aria-label", this.clock.playing ? "Pause lesson" : "Play lesson");
    }
    togglePlay() {
      if (this.clock.playing)
        this.clock.pause();
      else
        void this.clock.play();
    }
    toggleFullscreen() {
      if (document.fullscreenElement)
        void document.exitFullscreen();
      else
        void (this.el.closest(".xv-shell") ?? this.el.closest(".xv-player"))?.requestFullscreen?.();
    }
    duration() {
      return this.clock.duration || this.tracks.duration;
    }
  };
  function div(className) {
    const d2 = document.createElement("div");
    d2.className = className;
    return d2;
  }

  // packages/player/dist/answer-timeline.js
  function timeAnswerBeats(beats) {
    let t2 = 0;
    return beats.map((beat) => {
      const timed = { t: t2, set: beat.set, over: beat.over };
      t2 += Math.max(1.5, Math.min(5, beat.say.length / 18));
      return timed;
    });
  }
  var AnswerTimeline = class {
    schema;
    tracks = /* @__PURE__ */ new Map();
    constructor(schema2, origin, beats) {
      this.schema = schema2;
      for (const beat of beats) {
        for (const [param, target2] of Object.entries(beat.set)) {
          const spec = schema2[param];
          if (!spec)
            continue;
          const segments = this.tracks.get(param) ?? [];
          const from = this.valueFromSegments(segments, beat.t, origin[param] ?? spec.default, spec.interpolate);
          const duration = spec.interpolate === "snap" ? 0 : beat.over;
          segments.push({ start: beat.t, end: beat.t + duration, from: clone2(from), to: clone2(target2) });
          this.tracks.set(param, segments);
        }
      }
    }
    /** Writes only parameters whose first answer command has begun. */
    evaluate(t2, out = {}) {
      for (const [param, segments] of this.tracks) {
        if (t2 < segments[0].start)
          continue;
        const spec = this.schema[param];
        out[param] = this.valueFromSegments(segments, t2, spec.default, spec.interpolate);
      }
      return out;
    }
    /** Reports answer-driven transitions and a short tail after each command. */
    activity(t2, fadeSeconds = 0.55, out = {}) {
      for (const param of Object.keys(out))
        delete out[param];
      for (const [param, segments] of this.tracks) {
        let active;
        for (const segment of segments) {
          if (t2 < segment.start)
            break;
          active = segment;
        }
        if (!active)
          continue;
        if (active.end > active.start && t2 < active.end) {
          out[param] = 1;
          continue;
        }
        const age = t2 - active.end;
        if (age >= 0 && age < fadeSeconds)
          out[param] = 1 - age / fadeSeconds;
      }
      return out;
    }
    valueFromSegments(segments, t2, fallback, mode) {
      let active;
      for (const segment of segments) {
        if (t2 < segment.start)
          break;
        active = segment;
      }
      if (!active)
        return clone2(fallback);
      if (active.end > active.start && t2 < active.end) {
        return blend(mode, active.from, active.to, (t2 - active.start) / (active.end - active.start));
      }
      return clone2(active.to);
    }
  };
  function clone2(value) {
    if (Array.isArray(value))
      return value.slice();
    if (typeof value === "object" && value !== null) {
      const orbit = value;
      return { ...orbit, target: [...orbit.target] };
    }
    return value;
  }

  // packages/player/dist/assistant-panel.js
  var nextPanelId = 0;
  var AssistantPanel = class {
    el;
    history = [];
    input;
    toggle;
    body;
    askButton;
    cancelButton;
    status;
    transcript;
    onExpandedChange;
    pauseEnabled = false;
    busy = false;
    constructor(opts) {
      this.onExpandedChange = opts.onExpandedChange;
      this.el = document.createElement("section");
      this.el.className = "xv-assistant";
      this.el.setAttribute("aria-label", "Lesson assistant");
      this.toggle = button("Ask about this lesson", "xv-assistant-toggle");
      this.toggle.type = "button";
      this.toggle.setAttribute("aria-expanded", String(opts.startOpen === true));
      this.body = div2("xv-assistant-body");
      this.body.id = `xv-assistant-body-${++nextPanelId}`;
      this.body.hidden = opts.startOpen !== true;
      this.toggle.setAttribute("aria-controls", this.body.id);
      this.toggle.onclick = () => this.setExpanded(this.body.hidden);
      this.transcript = div2("xv-assistant-transcript");
      this.transcript.setAttribute("aria-live", "polite");
      const form = document.createElement("form");
      form.className = "xv-assistant-form";
      this.input = document.createElement("input");
      this.input.className = "xv-assistant-input";
      this.input.type = "text";
      this.input.placeholder = "Pause the lesson to ask a question";
      this.input.setAttribute("aria-label", "Ask a question about this lesson");
      this.input.maxLength = opts.maxQuestionCharacters ?? 1e3;
      this.askButton = button("Ask", "xv-assistant-ask");
      this.askButton.type = "submit";
      this.cancelButton = button("Cancel", "xv-assistant-cancel");
      this.cancelButton.type = "button";
      this.cancelButton.hidden = true;
      this.cancelButton.onclick = opts.onCancel;
      form.onsubmit = (event) => {
        event.preventDefault();
        const question = this.input.value.trim();
        if (!question || this.busy || !this.pauseEnabled)
          return;
        opts.onAsk(question);
      };
      form.append(this.input, this.askButton, this.cancelButton);
      this.status = div2("xv-assistant-status");
      this.status.setAttribute("role", "status");
      const clear = button("Clear conversation", "xv-assistant-clear");
      clear.type = "button";
      clear.onclick = () => {
        this.history.length = 0;
        this.transcript.replaceChildren();
      };
      const footer = div2("xv-assistant-footer");
      footer.append(this.status, clear);
      this.body.append(form, this.transcript, footer);
      this.el.append(this.toggle, this.body);
      this.updateEnabled();
    }
    setExpanded(expanded) {
      this.body.hidden = !expanded;
      this.toggle.setAttribute("aria-expanded", String(expanded));
      this.onExpandedChange?.(expanded);
      if (expanded && typeof requestAnimationFrame !== "undefined") {
        requestAnimationFrame(() => this.body.scrollIntoView?.({ block: "nearest" }));
      }
    }
    setPauseEnabled(enabled) {
      this.pauseEnabled = enabled;
      this.input.placeholder = enabled ? "Ask a question about this lesson" : "Pause the lesson to ask a question";
      this.updateEnabled();
    }
    setBusy(busy, status = "") {
      this.busy = busy;
      this.status.textContent = status;
      this.cancelButton.hidden = !busy;
      this.updateEnabled();
    }
    addTurn(question, answer, beats) {
      this.history.push({ question, answer, beats });
      const turn = div2("xv-assistant-turn");
      const q = document.createElement("p");
      q.className = "xv-assistant-question";
      q.textContent = question;
      const a2 = document.createElement("p");
      a2.className = "xv-assistant-answer";
      a2.textContent = answer;
      turn.append(q, a2);
      this.transcript.append(turn);
      this.transcript.scrollTop = this.transcript.scrollHeight;
      this.input.value = "";
    }
    finish(status = "") {
      this.busy = false;
      this.status.textContent = status;
      this.cancelButton.hidden = true;
      this.updateEnabled();
    }
    fail(message) {
      this.finish(message);
    }
    updateEnabled() {
      const enabled = this.pauseEnabled && !this.busy;
      this.input.disabled = !enabled;
      this.askButton.disabled = !enabled;
    }
  };
  function div2(className) {
    const el2 = document.createElement("div");
    el2.className = className;
    return el2;
  }
  function button(text2, className) {
    const el2 = document.createElement("button");
    el2.className = className;
    el2.textContent = text2;
    return el2;
  }

  // packages/player/dist/audio-source.js
  function mimeForAudio(src) {
    if (src.endsWith(".m4a"))
      return 'audio/mp4; codecs="mp4a.40.2"';
    if (src.endsWith(".mp3"))
      return "audio/mpeg";
    if (src.endsWith(".webm"))
      return 'audio/webm; codecs="opus"';
    if (src.endsWith(".ogg"))
      return 'audio/ogg; codecs="opus"';
    return "audio/wav";
  }
  function preferredAudioSource(sources, canPlay = (mime) => document.createElement("audio").canPlayType(mime)) {
    if (!sources.length)
      throw new Error("the lesson has no narration audio");
    const probably = sources.find((source) => canPlay(mimeForAudio(source)) === "probably");
    if (probably)
      return probably;
    const maybe = sources.find((source) => canPlay(mimeForAudio(source)) === "maybe");
    if (maybe)
      return maybe;
    throw new Error("this browser does not support the lesson audio formats");
  }

  // packages/player/dist/start-screen.js
  var StartScreen = class {
    el;
    button;
    status;
    spinner;
    action = "start";
    constructor(introduction, actions) {
      this.el = div3("xv-start-screen");
      this.el.setAttribute("aria-label", "Lesson introduction");
      const content = div3("xv-start-content");
      const title = document.createElement("h1");
      title.className = "xv-start-title";
      title.textContent = introduction.title;
      const interactive = document.createElement("p");
      interactive.className = "xv-start-interactive";
      interactive.textContent = "This scene is interactive. Change the point of view and parameters while you listen.";
      const orientation = document.createElement("p");
      orientation.className = "xv-orientation-notice";
      orientation.textContent = "For the best experience on a phone, rotate to landscape or use a larger screen.";
      const controls2 = div3("xv-start-controls");
      const live = div3("xv-start-status");
      live.setAttribute("role", "status");
      live.setAttribute("aria-live", "polite");
      this.spinner = div3("xv-loading-spinner");
      this.spinner.setAttribute("aria-hidden", "true");
      this.status = document.createElement("span");
      live.append(this.spinner, this.status);
      this.button = document.createElement("button");
      this.button.type = "button";
      this.button.className = "xv-start-button";
      this.button.onclick = () => {
        if (this.action === "retry")
          actions.onRetry();
        else
          actions.onStart();
      };
      controls2.append(live, this.button);
      content.append(title, interactive, orientation, controls2);
      this.el.append(content);
      this.setLoading();
    }
    setLoading() {
      this.el.dataset.state = "loading";
      this.action = "start";
      this.status.textContent = "Loading narration\u2026";
      this.spinner.hidden = false;
      this.button.textContent = "Loading\u2026";
      this.button.disabled = true;
    }
    setReady() {
      this.el.dataset.state = "ready";
      this.action = "start";
      this.status.textContent = "Ready";
      this.spinner.hidden = true;
      this.button.textContent = "Start lesson";
      this.button.disabled = false;
    }
    setStarting() {
      this.el.dataset.state = "starting";
      this.status.textContent = "Starting\u2026";
      this.spinner.hidden = false;
      this.button.textContent = "Starting\u2026";
      this.button.disabled = true;
    }
    setFailed(message, action = "retry") {
      this.el.dataset.state = "failed";
      this.action = action;
      this.status.textContent = message;
      this.spinner.hidden = true;
      this.button.textContent = "Try again";
      this.button.disabled = false;
    }
  };
  function div3(className) {
    const element = document.createElement("div");
    element.className = className;
    return element;
  }

  // packages/player/dist/url.js
  function parseDevParams(search) {
    const q = new URLSearchParams(search);
    const tRaw = q.get("t");
    const t2 = tRaw !== null && Number.isFinite(Number(tRaw)) ? Number(tRaw) : void 0;
    return { t: t2, nochrome: q.has("nochrome"), state: q.has("state") };
  }

  // packages/player/dist/player.js
  var AUDIO_READY_TIMEOUT_MS = 15e3;
  var AUDIO_START_TIMEOUT_MS = 5e3;
  var Player = class {
    store;
    displayStore;
    clock;
    driver;
    host;
    reconciler;
    interaction;
    board;
    captions;
    pauseGate;
    chrome;
    audio;
    assistant;
    canvas;
    container;
    shell;
    index;
    lastFrameT = 0;
    dumpState = false;
    unbindKeys;
    resizeObserver;
    activeAnswer;
    answerAbort;
    assistantFetch;
    assistantEndpoint = "/api/answer";
    assistantClientId;
    tracks;
    audioLoader;
    baseUrl;
    startScreen;
    activityTracker;
    assistantActivity = {};
    constructor(opts) {
      this.tracks = opts.tracks;
      this.audioLoader = opts.audioLoader;
      this.baseUrl = opts.baseUrl ?? "";
      const schema2 = { ...opts.scene.schema, ...boardSchema(opts.tracks.tracks) };
      this.index = buildIndex(opts.tracks.tracks, schema2);
      this.store = new StateStore(schema2);
      this.displayStore = new StateStore(schema2);
      this.activityTracker = new ParameterActivityTracker(opts.tracks.tracks);
      const assistantStartsOpen = opts.assistant?.startOpen === true && hasRoomForOpenAssistant();
      this.shell = el("div", opts.assistant ? `xv-shell xv-with-assistant${assistantStartsOpen ? " xv-assistant-expanded" : ""}` : "xv-shell");
      this.container = el("div", "xv-player");
      this.canvas = el("canvas", "");
      const overlay = el("div", "xv-overlay");
      const boardPanel = el("aside", "xv-board");
      this.audio = document.createElement("audio");
      this.audio.preload = "auto";
      this.setAudioSources(opts.audioSrc ?? []);
      this.clock = new AudioClock(this.audio);
      this.board = new Board(this.displayStore, opts.tracks.boardItems);
      boardPanel.append(this.board.el);
      this.captions = new Captions(opts.captionsVtt ?? "");
      this.pauseGate = new PauseGate(this.clock, opts.tracks.pauses);
      this.container.append(this.canvas, overlay, boardPanel, this.captions.el, this.audio);
      this.container.append(portraitMessage());
      this.shell.append(this.container);
      opts.mount.append(this.shell);
      this.resize();
      this.host = new SceneHost(opts.scene, {
        canvas: this.canvas,
        overlay,
        viewport: () => ({ width: this.canvas.width, height: this.canvas.height }),
        write: (param, value) => this.writeSceneParam(param, value, schema2),
        reset: (param) => {
          this.store.resetInteraction(param);
          this.activityTracker.noteUser(param);
          this.activeAnswer?.claimed.add(param);
        },
        pause: () => this.clock.pause()
      });
      this.reconciler = new Reconciler(this.store, this.index, schema2);
      this.driver = new TimelineDriver(this.clock, this.index, this.store, { onFrame: (t2) => this.frame(t2) }, this.reconciler);
      this.interaction = new InteractionManager(this.canvas, this.host, this.store, this.clock, () => this.displayStore.plain, (param) => {
        this.activityTracker.noteUser(param);
        this.activeAnswer?.claimed.add(param);
      });
      const dev = parseDevParams(typeof location !== "undefined" ? location.search : "");
      if (opts.chrome !== false && !dev.nochrome) {
        this.chrome = new Chrome(this.clock, opts.tracks, { onCaptionsToggle: (on) => this.captions.setVisible(on) });
        this.container.append(this.chrome.el);
        if (!opts.introduction)
          this.unbindKeys = this.chrome.bindKeys();
      }
      if (opts.assistant) {
        this.assistantFetch = opts.assistant.fetchImpl ?? ((input, init) => fetch(input, init));
        this.assistantEndpoint = opts.assistant.endpoint ?? "/api/answer";
        this.assistantClientId = persistentClientId();
        this.assistant = new AssistantPanel({
          onAsk: (question) => void this.ask(question, opts.assistant.context),
          onCancel: () => this.cancelAnswer("Cancelled"),
          onExpandedChange: (expanded) => {
            this.shell.classList.toggle("xv-assistant-expanded", expanded);
            this.resize();
            this.driver.tick();
          },
          maxQuestionCharacters: opts.assistant.context.limits.request.questionCharacters,
          startOpen: assistantStartsOpen
        });
        this.shell.append(this.assistant.el);
        let hasPlayed = false;
        this.clock.on("play", () => {
          hasPlayed = true;
          if (this.activeAnswer || this.answerAbort)
            this.cancelAnswer();
          this.assistant?.setPauseEnabled(false);
        });
        this.clock.on("pause", () => {
          if (hasPlayed)
            this.assistant?.setPauseEnabled(true);
        });
      }
      if (dev.state)
        this.dumpState = true;
      if (dev.t !== void 0) {
        this.clock.seek(dev.t);
        this.clock.pause();
      }
      if (opts.introduction) {
        this.startScreen = new StartScreen(opts.introduction, {
          onStart: () => void this.beginLesson(),
          onRetry: () => void this.loadAudio()
        });
        this.container.append(this.startScreen.el);
      }
      if (typeof ResizeObserver !== "undefined") {
        this.resizeObserver = new ResizeObserver(() => {
          this.resize();
          this.driver.tick();
        });
        this.resizeObserver.observe(this.container);
      }
    }
    start() {
      this.driver.tick();
      this.driver.start();
      if (!this.startScreen)
        return;
      if (this.audioLoader)
        void this.loadAudio();
      else
        this.startScreen.setReady();
    }
    dispose() {
      this.driver.stop();
      this.resizeObserver?.disconnect();
      this.interaction.dispose();
      this.unbindKeys?.();
      this.board.dispose();
      this.host.dispose();
      this.cancelAnswer();
      this.shell.remove();
    }
    async loadAudio() {
      if (!this.audioLoader || !this.startScreen)
        return;
      this.startScreen.setLoading();
      try {
        this.setAudioSources(await this.audioLoader());
        await this.waitForAudioReady();
        this.startScreen.setReady();
      } catch (error) {
        console.error("narration loading failed:", error);
        this.startScreen.setFailed("The narration could not load. Check your connection and try again.");
      }
    }
    async beginLesson() {
      if (!this.startScreen)
        return;
      this.startScreen.setStarting();
      if (!await withTimeout(this.clock.play(), AUDIO_START_TIMEOUT_MS)) {
        this.clock.pause();
        this.startScreen.setFailed("The narration could not start. Try again.", "start");
        return;
      }
      this.startScreen.el.remove();
      this.startScreen = void 0;
      if (this.chrome && !this.unbindKeys)
        this.unbindKeys = this.chrome.bindKeys();
    }
    waitForAudioReady() {
      return new Promise((resolve, reject) => {
        const finish = (error) => {
          clearTimeout(timeout);
          this.audio.removeEventListener("canplay", onReady);
          this.audio.removeEventListener("error", onError);
          if (error)
            reject(error);
          else
            resolve();
        };
        const onReady = () => finish();
        const onError = () => finish(new Error(this.audio.error?.message || "the browser could not decode the narration"));
        const timeout = setTimeout(() => finish(new Error("the browser did not make the narration ready in time")), AUDIO_READY_TIMEOUT_MS);
        this.audio.addEventListener("canplay", onReady);
        this.audio.addEventListener("error", onError);
        this.audio.load();
        if (this.audio.readyState >= HTMLMediaElement.HAVE_FUTURE_DATA)
          onReady();
      });
    }
    setAudioSources(sources) {
      this.audio?.replaceChildren();
      for (const src of sources) {
        const source = document.createElement("source");
        source.src = this.baseUrl + src;
        if (!source.src.startsWith("blob:"))
          source.type = mimeForAudio(src);
        this.audio.append(source);
      }
    }
    frame(t2) {
      const dt = Math.max(0, t2 - this.lastFrameT);
      this.lastFrameT = t2;
      const answerElapsed = this.activeAnswer ? (performance.now() - this.activeAnswer.startedAt) / 1e3 : void 0;
      for (const key of this.store.keys())
        this.displayStore.set(key, this.store.plain[key]);
      for (const [param, value] of Object.entries(this.temporaryAnswerState(answerElapsed)))
        this.displayStore.set(param, value);
      const assistantActivity = answerElapsed === void 0 ? {} : this.activeAnswer.timeline.activity(answerElapsed, 0.55, this.assistantActivity);
      this.host.render(this.displayStore.plain, {
        dt,
        activity: this.activityTracker.evaluate(t2, this.store.meta, assistantActivity)
      });
      this.pauseGate.update(t2);
      if (this.pauseGate.activePrompt === null)
        this.captions.update(t2);
      this.chrome?.update(t2);
      if (this.dumpState)
        window.__XV_STATE__ = { ...this.store.plain };
    }
    writeSceneParam(param, value, schema2) {
      if (!(param in schema2))
        throw new Error(`scene wrote unknown parameter: ${param}`);
      this.store.touch(param, value, this.clock.t);
      this.activityTracker.noteUser(param);
      this.activeAnswer?.claimed.add(param);
    }
    async ask(question, context) {
      const temporaryAssistantState = this.temporaryAnswerState();
      const visibleState = { ...this.store.plain, ...temporaryAssistantState };
      this.clearActiveAnswer();
      this.answerAbort = new AbortController();
      this.assistant.setBusy(true, "Thinking\u2026");
      const body = {
        lessonId: context.lessonId,
        question,
        t: this.clock.t,
        state: visibleState,
        position: lessonPositionAt(this.clock.t, this.tracks.chapters, this.captions.latestText(this.clock.t), this.pauseGate.activePrompt),
        temporaryAssistantState,
        history: this.assistant.history.slice(-context.limits.request.historyTurns)
      };
      try {
        const response2 = await this.assistantFetch(this.assistantEndpoint, {
          method: "POST",
          headers: { "content-type": "application/json", "x-tangible-client-id": this.assistantClientId },
          body: JSON.stringify(body),
          signal: this.answerAbort.signal
        });
        if (!response2.ok)
          throw new Error(await response2.text());
        const answer = await response2.json();
        if (!answer.answer || !Array.isArray(answer.beats))
          throw new Error("invalid assistant response");
        this.assistant.addTurn(question, answer.answer, answer.beats);
        this.startAnswer(answer, context);
      } catch (error) {
        if (error.name !== "AbortError")
          this.assistant.fail(`Answer failed: ${error.message}`);
      } finally {
        this.answerAbort = void 0;
      }
    }
    startAnswer(answer, context) {
      const schema2 = {};
      for (const param of context.commandable)
        schema2[param] = context.schema[param];
      this.activeAnswer = {
        timeline: new AnswerTimeline(schema2, this.displayStore.plain, timeAnswerBeats(answer.beats)),
        claimed: /* @__PURE__ */ new Set(),
        startedAt: performance.now()
      };
      this.assistant.finish();
    }
    clearActiveAnswer() {
      this.activeAnswer = void 0;
    }
    temporaryAnswerState(elapsed) {
      if (!this.activeAnswer)
        return {};
      const answer = this.activeAnswer.timeline.evaluate(elapsed ?? (performance.now() - this.activeAnswer.startedAt) / 1e3);
      for (const param of this.activeAnswer.claimed)
        delete answer[param];
      return answer;
    }
    cancelAnswer(status = "") {
      this.answerAbort?.abort();
      this.clearActiveAnswer();
      this.assistant?.finish(status);
    }
    resize() {
      const dpr = window.devicePixelRatio || 1;
      const r2 = this.container.getBoundingClientRect();
      const cssW = Math.round(r2.width) || 640;
      const cssH = Math.round(r2.height) || 360;
      this.canvas.width = Math.round(cssW * dpr);
      this.canvas.height = Math.round(cssH * dpr);
    }
  };
  function boardSchema(tracks) {
    const s2 = {};
    for (const key of Object.keys(tracks)) {
      if (!key.startsWith("board."))
        continue;
      s2[key] = key.includes(".highlight") ? { type: { kind: "boolean" }, default: false, interpolate: "snap", ownership: "script" } : { type: { kind: "boardItem" }, default: "hidden", interpolate: "snap", ownership: "script" };
    }
    return s2;
  }
  function el(tag, className) {
    const e2 = document.createElement(tag);
    if (className)
      e2.className = className;
    return e2;
  }
  function portraitMessage() {
    const notice = el("div", "xv-portrait-message");
    notice.setAttribute("role", "note");
    const title = el("strong", "xv-portrait-title");
    title.textContent = "This lesson needs a wider screen.";
    const explanation = el("span", "xv-portrait-explanation");
    explanation.textContent = "Rotate your phone to landscape, or continue on a tablet or computer.";
    notice.append(title, explanation);
    return notice;
  }
  var CLIENT_ID_KEY = "tangible.assistantClientId";
  function hasRoomForOpenAssistant() {
    if (typeof window === "undefined" || typeof window.matchMedia !== "function")
      return true;
    return !window.matchMedia("(max-width: 600px), (max-height: 500px)").matches;
  }
  function persistentClientId() {
    try {
      const stored = localStorage.getItem(CLIENT_ID_KEY);
      if (stored && /^[a-zA-Z0-9_-]{16,64}$/.test(stored))
        return stored;
      const created = randomClientId();
      localStorage.setItem(CLIENT_ID_KEY, created);
      return created;
    } catch {
      return randomClientId();
    }
  }
  function randomClientId() {
    const bytes = new Uint8Array(16);
    crypto.getRandomValues(bytes);
    return [...bytes].map((byte) => byte.toString(16).padStart(2, "0")).join("");
  }
  async function withTimeout(result, timeoutMs) {
    let timeout;
    const expired = new Promise((resolve) => {
      timeout = setTimeout(() => resolve(false), timeoutMs);
    });
    const value = await Promise.race([result, expired]);
    if (timeout !== void 0)
      clearTimeout(timeout);
    return value;
  }

  // packages/player/dist/styles.js
  var PLAYER_CSS = `
.xv-shell { width: min(100%, 177.7778vh); width: min(100%, 177.7778dvh); margin-inline: auto; }
/* Reserve the visible assistant controls below the 16:9 scene. */
.xv-shell.xv-with-assistant { width: min(100%, max(0px, calc(177.7778vh - 116px))); width: min(100%, max(0px, calc(177.7778dvh - 116px))); }
.xv-shell.xv-with-assistant.xv-assistant-expanded { width: min(100%, max(0px, calc(177.7778vh - 320px))); width: min(100%, max(0px, calc(177.7778dvh - 320px))); }
.xv-player { position: relative; width: 100%; aspect-ratio: 16 / 9; background: #fafafa; overflow: hidden; user-select: none; }
.xv-player > canvas { position: absolute; inset: 0; width: 100%; height: 100%; touch-action: none; }
.xv-overlay { position: absolute; inset: 0; pointer-events: none; }
.xv-board { position: absolute; top: 0; right: 0; width: 28%; height: 100%; padding: 12px; box-sizing: border-box; overflow: auto; pointer-events: none; }
.xv-captions { position: absolute; left: 3%; right: 30%; bottom: 60px; padding: 3px 8px; text-align: center; font: clamp(15px, 2.1vw, 18px)/1.35 sans-serif; color: #111; text-shadow: 0 1px 2px #fff; pointer-events: none; }
.xv-board-inner { display: flex; flex-direction: column; gap: 10px; }
.xv-board-item { transition: opacity 200ms ease; pointer-events: auto; }
.xv-board-item.xv-hidden { display: none; }
.xv-board-item.xv-shown { opacity: 1; }
.xv-board-item.xv-dimmed { opacity: 0.4; }
.xv-hl { background: #fff3a0; border-radius: 3px; }
.xv-start-screen { position: absolute; inset: 0; z-index: 10; display: grid; place-items: center; overflow: auto; padding: clamp(14px, 3vw, 28px); box-sizing: border-box; background: rgba(4, 8, 15, 0.46); -webkit-backdrop-filter: grayscale(0.55) brightness(0.72); backdrop-filter: grayscale(0.55) brightness(0.72); color: #fff; font-family: system-ui, sans-serif; user-select: text; }
.xv-start-content { width: min(560px, 100%); padding: clamp(22px, 3.5vw, 34px); border: 1px solid rgba(255, 255, 255, 0.18); border-radius: 20px; box-sizing: border-box; background: rgba(18, 27, 41, 0.84); box-shadow: 0 20px 60px rgba(0, 0, 0, 0.38); -webkit-backdrop-filter: blur(14px); backdrop-filter: blur(14px); }
.xv-start-title { max-width: 21ch; margin: 0; font-size: clamp(26px, 4.2vw, 40px); line-height: 1.05; letter-spacing: -0.025em; }
.xv-start-interactive { margin: 14px 0 0; color: #e5edf3; font-size: clamp(13px, 1.6vw, 15px); }
.xv-orientation-notice { display: none; margin: 12px 0 0; padding-left: 22px; color: #ffe6a6; font-size: 14px; line-height: 1.4; }
.xv-orientation-notice::before { content: "\u21BB"; display: inline-block; width: 22px; margin-left: -22px; }
.xv-start-controls { display: flex; align-items: center; gap: 18px; margin-top: clamp(18px, 3vw, 28px); }
.xv-start-status { display: flex; flex: 1; align-items: center; gap: 10px; min-width: 0; color: #cbd8e4; font-size: 14px; }
.xv-start-screen[data-state="failed"] .xv-start-status { color: #ffd0c8; }
.xv-loading-spinner { width: 16px; height: 16px; flex: 0 0 auto; border: 2px solid rgba(255,255,255,0.3); border-top-color: #fff; border-radius: 50%; animation: xv-spin 800ms linear infinite; }
.xv-loading-spinner[hidden] { display: none; }
.xv-start-button { min-width: 150px; min-height: 48px; padding: 12px 22px; border: 0; border-radius: 999px; background: #fff; color: #172033; font: 700 16px/1.2 system-ui, sans-serif; cursor: pointer; }
.xv-start-button:hover:not(:disabled) { background: #dff2ff; transform: translateY(-1px); }
.xv-start-button:focus-visible { outline: 3px solid #78c7ff; outline-offset: 3px; }
.xv-start-button:disabled { cursor: wait; opacity: 0.55; }
@keyframes xv-spin { to { transform: rotate(360deg); } }
@media (prefers-reduced-motion: reduce) { .xv-loading-spinner { animation-duration: 1600ms; } .xv-start-button { transform: none !important; } }
@media (max-width: 700px) { .xv-orientation-notice { display: block; } }
@media (max-width: 520px) { .xv-start-screen { align-items: start; padding: 12px; } .xv-start-content { padding: 20px; border-radius: 16px; } .xv-start-controls { align-items: stretch; flex-direction: column-reverse; gap: 12px; margin-top: 18px; } .xv-start-button { width: 100%; } }
@media (max-height: 500px) and (orientation: landscape) {
  .xv-start-screen { align-items: start; padding: 8px; }
  .xv-start-content { padding: 14px 18px; border-radius: 14px; }
  .xv-start-title { font-size: 28px; }
  .xv-start-interactive { margin-top: 10px; font-size: 13px; }
  .xv-start-controls { margin-top: 12px; }
  .xv-start-button { min-height: 44px; }
}
.xv-chrome { position: absolute; left: 0; right: 0; bottom: 0; height: 52px; display: flex; align-items: center; gap: 6px; padding: 0 8px; background: rgba(255,255,255,0.88); box-sizing: border-box; }
.xv-chrome button { border: none; background: none; cursor: pointer; color: #222; font-size: 18px; height: 44px; min-width: 44px; padding: 0 6px; display: inline-flex; align-items: center; justify-content: center; line-height: 1; border-radius: 6px; }
.xv-chrome button:hover { background: rgba(0,0,0,0.06); }
.xv-chrome button:focus-visible, .xv-scrubber:focus-visible, .xv-credit:focus-visible { outline: 3px solid #1677b8; outline-offset: 1px; }
.xv-scrubber { flex: 1; min-width: 44px; height: 44px; cursor: pointer; }
.xv-elapsed { font: 12px monospace; color: #333; min-width: 90px; text-align: right; }
.xv-credit { min-height: 44px; padding: 0 6px; display: inline-flex; flex: 0 0 auto; align-items: center; box-sizing: border-box; border-radius: 6px; background: rgba(255,255,255,0.82); box-shadow: inset 0 0 0 1px rgba(17,24,39,0.14); color: #364152; font: 500 11px/1.2 system-ui, sans-serif; text-decoration: none; white-space: nowrap; }
.xv-credit:hover { background: #fff; text-decoration: underline; }
.xv-portrait-message { display: none; }
.xv-assistant { border: 1px solid #cbd2da; border-top: 0; background: #f6f8fa; color: #20252c; font: 14px/1.4 system-ui, sans-serif; box-sizing: border-box; }
.xv-assistant-toggle { width: 100%; min-height: 56px; display: flex; align-items: center; justify-content: space-between; padding: 0 16px; border: 0; background: transparent; color: inherit; font: 650 15px/1.2 system-ui, sans-serif; text-align: left; cursor: pointer; }
.xv-assistant-toggle::after { content: "\u2304"; margin-left: 16px; font-size: 22px; line-height: 1; transition: transform 160ms ease; }
.xv-assistant-toggle[aria-expanded="true"]::after { transform: rotate(180deg); }
.xv-assistant-body { padding: 12px; border-top: 1px solid #cbd2da; }
.xv-assistant-body[hidden] { display: none; }
.xv-assistant-transcript:empty { display: none; }
.xv-assistant-transcript { max-height: min(240px, 30dvh); overflow: auto; margin-top: 8px; }
.xv-assistant-turn { border-left: 3px solid #ddd; padding-left: 9px; margin: 8px 0; }
.xv-assistant-question { margin: 0 0 4px; font-weight: 600; }
.xv-assistant-answer { margin: 0; }
.xv-assistant-form { display: flex; gap: 6px; }
.xv-assistant-input { flex: 1; min-width: 0; min-height: 44px; padding: 8px 10px; border: 1px solid #929ba5; border-radius: 6px; box-sizing: border-box; font: inherit; }
.xv-assistant-input:disabled { color: #777; background: #eee; }
.xv-assistant button:not(.xv-assistant-toggle) { min-height: 44px; padding: 6px 12px; border: 1px solid #929ba5; border-radius: 6px; background: #fff; color: inherit; cursor: pointer; }
.xv-assistant button:disabled { cursor: default; opacity: 0.5; }
.xv-assistant button:focus-visible, .xv-assistant-input:focus-visible { outline: 3px solid #1677b8; outline-offset: 2px; }
.xv-assistant-footer { display: flex; justify-content: space-between; align-items: center; min-height: 44px; margin-top: 4px; color: #5b626b; font-size: 12px; }
.xv-assistant-clear { border: 0 !important; background: transparent !important; color: inherit; }
@media (max-width: 520px) {
  .xv-assistant-form { display: grid; grid-template-columns: 1fr auto; }
  .xv-assistant-input { grid-column: 1 / -1; }
}
@media (max-width: 600px) and (orientation: portrait) {
  .xv-portrait-message { position: absolute; inset: 0; z-index: 30; display: flex; flex-direction: column; align-items: center; justify-content: center; gap: 8px; padding: 24px; box-sizing: border-box; background: #111827; color: #f8fafc; text-align: center; font-family: system-ui, sans-serif; user-select: text; }
  .xv-portrait-title { font-size: 19px; line-height: 1.2; }
  .xv-portrait-explanation { max-width: 34ch; color: #d9e2ec; font-size: 15px; line-height: 1.4; }
}
`;

  // ../../../Users/choxos/Documents/GitHub/mlumr-lesson/scenes/math.ts
  var logistic = (x2) => 1 / (1 + Math.exp(-x2));
  var logit = (p2) => Math.log(p2 / (1 - p2));
  var mean = (xs) => xs.reduce((a2, b2) => a2 + b2, 0) / xs.length;
  var mix = (p2, low, high) => (1 - p2) * low + p2 * high;
  var grid = (from, to, n) => Array.from({ length: n }, (_2, i3) => from + (to - from) * i3 / (n - 1));
  function binary(p2, betaB = 2.4, shift = 0) {
    const a2 = mix(p2, logistic(-1.8), logistic(-1.8 + 2.4));
    const b2 = mix(p2, logistic(-1.1 + shift), logistic(-1.1 + betaB + shift));
    return { a: a2, b: b2, rd: a2 - b2, rr: a2 / b2, lor: logit(a2) - logit(b2) };
  }
  function quadrature(width, n) {
    const count = Math.round(n);
    const points = Array.from({ length: count }, (_2, i3) => -width + 2 * width * (i3 + 0.5) / count);
    return { points, value: mean(points.map((x2) => logistic(-1.8 + 2.4 * x2))) };
  }
  function dependence(rho) {
    const weights = [(1 + rho) / 4, (1 - rho) / 4, (1 - rho) / 4, (1 + rho) / 4];
    const risks = [0, 1, 1, 2].map((x2) => logistic(-2.8 + 1.6 * x2));
    return { weights, risks, risk: weights.reduce((sum, w2, i3) => sum + w2 * risks[i3], 0) };
  }
  function identification(design, separation, target2, priorSD) {
    const xs = design === "one" ? [0] : design === "duplicate" ? [0, 0] : [-separation, separation];
    const ys = xs.map((x2) => 0.4 + 0.8 * x2);
    const precision = 1 / 0.15 ** 2;
    const a2 = 1 / priorSD ** 2 + precision * xs.length;
    const b2 = precision * xs.reduce((s2, x2) => s2 + x2, 0);
    const d2 = 1 / priorSD ** 2 + precision * xs.reduce((s2, x2) => s2 + x2 * x2, 0);
    const det = a2 * d2 - b2 * b2;
    const v00 = d2 / det, v01 = -b2 / det, v11 = a2 / det;
    const q0 = precision * ys.reduce((s2, y2) => s2 + y2, 0);
    const q1 = precision * ys.reduce((s2, y2, i3) => s2 + y2 * xs[i3], 0);
    const alpha = v00 * q0 + v01 * q1, beta = v01 * q0 + v11 * q1;
    return {
      xs,
      ys,
      alpha,
      beta,
      estimate: alpha + target2 * beta,
      sd: Math.sqrt(v00 + 2 * target2 * v01 + target2 * target2 * v11),
      rank: design === "separated" && separation > 0 ? 2 : 1,
      targetIdentified: design === "separated" && separation > 0 || target2 === 0
    };
  }
  function survival(t2, p2, beta = 1.8, hr = 0.65) {
    const ratesB = [0.06, 0.06 * Math.exp(beta)];
    const ratesA = ratesB.map((r2) => r2 * hr);
    const response2 = (rates) => {
      const ss = rates.map((r2) => Math.exp(-r2 * t2));
      const s2 = mix(p2, ss[0], ss[1]);
      const h2 = mix(p2, rates[0] * ss[0], rates[1] * ss[1]) / s2;
      const rmst = t2 === 0 ? 0 : mix(p2, -Math.expm1(-rates[0] * t2) / rates[0], -Math.expm1(-rates[1] * t2) / rates[1]);
      return { s: s2, h: h2, rmst };
    };
    const a2 = response2(ratesA), b2 = response2(ratesB);
    return { a: a2, b: b2, hr: a2.h / b2.h, rmstd: a2.rmst - b2.rmst };
  }
  function random(seed) {
    let s2 = seed >>> 0;
    const uniform = () => {
      s2 = s2 * 1664525 + 1013904223 >>> 0;
      return (s2 + 0.5) / 4294967296;
    };
    return { uniform, normal: () => Math.sqrt(-2 * Math.log(uniform())) * Math.cos(2 * Math.PI * uniform()) };
  }
  function quantile(xs, p2) {
    const sorted = [...xs].sort((a2, b2) => a2 - b2);
    const h2 = (sorted.length - 1) * p2, lo = Math.floor(h2);
    return sorted[lo] + (sorted[Math.min(lo + 1, sorted.length - 1)] - sorted[lo]) * (h2 - lo);
  }
  function splitRhat(chains) {
    const halves = chains.flatMap((c2) => {
      const n2 = Math.floor(c2.length / 2);
      return [c2.slice(0, n2), c2.slice(c2.length - n2)];
    });
    const n = halves[0].length, means = halves.map(mean), grand = mean(means);
    const within = mean(halves.map((h2, i3) => h2.reduce((s2, x2) => s2 + (x2 - means[i3]) ** 2, 0) / (n - 1)));
    const between = n * means.reduce((s2, m2) => s2 + (m2 - grand) ** 2, 0) / (halves.length - 1);
    return Math.sqrt(((n - 1) / n * within + between / n) / within);
  }

  // ../../../Users/choxos/Documents/GitHub/mlumr-lesson/scenes/content.ts
  var labs = {
    evidence: ["Two trials, no common arm", "How do you compare treatments that never met in one trial?", "The problem"],
    assumptions: ["What adjustment has to assume", "Can a hidden difference between trials look like a treatment effect?", "Assumptions"],
    response: ["Shared or separate slopes", "What does it mean for two treatments to share a covariate effect?", "Outcome models"],
    integration: ["Average the predictions", "Is the risk of an average patient the same as the average risk?", "Integration"],
    dependence: ["Rebuild the population", "Can two populations with the same summaries have different risks?", "Covariates"],
    target: ["Choose what to estimate", "Which population, and which effect scale?", "Estimands"],
    identification: ["What subgroup rows can tell you", "Can trial B's summaries pin down its own slope?", "Information"],
    workflow: ["Run mlumr in your browser", "How do these ideas become an mlumr analysis?", "In practice"],
    families: ["Pick the outcome model", "Which likelihood and effect scale fit your outcome?", "Outcome types"],
    survival: ["Survival after averaging", "Why can a population hazard ratio change over time?", "Survival"],
    priors: ["When the prior matters", "Can a strong prior stand in for missing data?", "Priors"],
    diagnostics: ["Read a fit before trusting it", "Which problem does each check actually catch?", "Checking"],
    practice: ["Report it well", "Can you defend your target, your assumptions and your checks?", "Reporting"]
  };
  var workflowSteps = [
    {
      name: "Define",
      detail: "the question",
      title: "Step 1. Define the comparison",
      code: "library(mlumr)\n# Before touching the data, write down:\n#   treatments: A (index trial) versus B (comparator trial)\n#   the outcome and its follow-up time\n#   the target population and the effect scale\n#   the covariates that affect the outcome or the effect",
      text: "Decide what you are comparing before you fit anything: the two treatments, the outcome, the population you care about, and the effect scale. When each trial studied only one treatment, the trial and the treatment always go together."
    },
    {
      name: "Prepare",
      detail: "set_agd()",
      title: "Step 2. Prepare both kinds of data",
      code: 'ipd <- set_ipd(trial_a, treatment = "trt", outcome = "event",\n               covariates = "x", family = "binomial")\nagd <- set_agd(trial_b, treatment = "trt", family = "binomial",\n               outcome_n = "n", outcome_r = "events",\n               cov_means = "x_mean", cov_sds = "x_sd",\n               cov_types = "continuous")\ndat <- combine_data(ipd, agd)',
      text: "Column names go in quotes. For a binary outcome, trial B needs the number of events and the number of patients. If trial B reports several subgroup rows, every patient must belong to exactly one row. Tables that overlap, such as one by age and another by sex, cannot be stacked."
    },
    {
      name: "Integrate",
      detail: "distr()",
      title: "Step 3. Describe trial B's covariates",
      code: 'dat <- add_integration(dat, n_int = 512,\n  x = distr(qnorm, mean = x_mean, sd = x_sd))\ncheck_integration(dat, x = distr(qnorm, mean = x_mean, sd = x_sd))\ncheck_identification(dat, link = "logit")',
      text: "add_integration() spreads quasi-random Sobol points over the covariate distribution you describe. Choose distributions that respect each covariate's range: qbern() for a yes or no covariate, qlogitnorm() for a proportion, and qgamma() for a positive value. check_integration() compares the grid with one twice as large. It checks the arithmetic, not the final effect."
    },
    {
      name: "Fit",
      detail: "mlumr()",
      title: "Step 4. Fit the model",
      code: 'fit <- mlumr(dat, model = "spfa",\n  prior_intercept = prior_normal(0, 2.5),\n  prior_beta = prior_normal(0, 1),\n  chains = 4, iter = 2000, warmup = 1000, seed = 2026)\nfit_relaxed <- mlumr(dat, model = "relaxed",\n  prior_intercept = prior_normal(0, 2.5),\n  prior_beta = prior_normal(0, 1),\n  prior_beta_comparator = prior_normal(0, 1),\n  chains = 4, iter = 2000, warmup = 1000, seed = 2026)',
      text: 'mlumr() fits one Bayesian model to both trials at once. model = "spfa" makes the treatments share their covariate slopes. model = "relaxed" gives trial B its own slopes, which only its summaries and the prior can inform. The default engine is rstan; engine = "cmdstanr" also works.'
    },
    {
      name: "Check",
      detail: "summary()",
      title: "Step 5. Check before you believe",
      code: 'summary(fit)       # chains, divergences, R-hat, ESS\nprior_summary(fit)\nplot_prior_posterior(fit_relaxed, pars = "beta_comparator[1]")\nprior_sensitivity(fit_relaxed)   # refits at several prior scales',
      text: "mlumr() checks the chains when it finishes and warns about divergences, R-hat and effective sample size. summary(fit) shows them again. Good sampling does not show that the model is right or that the trials are comparable, so also compare each posterior with its prior and refit with other prior scales. prior_sensitivity() refits the model several times, so it takes a while."
    },
    {
      name: "Report",
      detail: "predict()",
      title: "Step 6. Compare in one population",
      code: 'marginal_effects(fit, population = "both", effect = "rd")\nmarginal_effects(fit, newdata = target, effect = "lor")\nconditional_effects(fit, newdata = profiles)\npredict(fit, type = "response")\nplot(marginal_effects(fit))\nstc(dat)\nnaive(dat)',
      text: `marginal_effects() compares both treatments in one population: trial A's, trial B's, or a target you pass as newdata, where every row counts equally. effect = "lor" is a log odds ratio. For odds ratio summaries, ask for summary = FALSE and exponentiate each draw first. stc() and naive() are quick benchmarks that answer different questions.`
    }
  ];
  var families = [
    {
      name: "Binary",
      equation: "patient outcome ~ Bernoulli(p)\ntrial B events ~ Binomial(n, average p)",
      input: 'set_agd(data, treatment = "trt", family = "binomial",\n        outcome_n = "n", outcome_r = "events", cov_means = ...)',
      effects: 'marginal_effects(effect = "rd", "rr" or "lor"). No difference means RD = 0, RR = 1, log OR = 0.',
      scale: "The link is logit by default, or probit or cloglog. Risks are averaged over the population first, and every effect is built from those averages.",
      boundary: 'The "lor" effect is always a logit-scale log odds ratio of the averaged risks, even when the model uses probit or cloglog. Changing the link changes the model, including the scale on which slopes are shared.'
    },
    {
      name: "Continuous",
      equation: "patient outcome ~ Normal(mean, sigma)\ntrial B mean ~ Normal(average mean, SE)",
      input: 'set_agd(data, treatment = "trt", family = "normal",\n        outcome_mean = "y_mean", outcome_se = "y_se",\n        outcome_n = "n", cov_means = ...)',
      effects: 'marginal_effects(effect = "md"). No difference means MD = 0.',
      scale: "The link is identity by default, or log. Trial B supplies its mean outcome and the standard error of that mean, on the original scale, even with a log link.",
      boundary: "The standard error of the mean is not the standard deviation of individual outcomes. If a paper gives a standard deviation instead, divide it by the square root of the number of patients. With more than one row, outcome_n is required so the rows can be weighted by size."
    },
    {
      name: "Counts",
      equation: "patient events ~ Poisson(exposure \xD7 rate)\ntrial B events ~ Poisson(exposure \xD7 average rate)",
      input: 'set_ipd(data, ..., family = "poisson", exposure = "years")\nset_agd(data, treatment = "trt", family = "poisson",\n        outcome_r = "events", outcome_E = "years", cov_means = ...)',
      effects: 'marginal_effects(effect = "rr"), a rate ratio. No difference means RR = 1.',
      scale: "The link is log. Keep exposure in the same units in both trials.",
      boundary: "If follow-up time depends on the covariates, trial B's covariate summaries should describe person-time rather than people."
    },
    {
      name: "Survival",
      equation: "likelihood = hazard^event \xD7 survival, for each patient\ntrial B terms are averaged over its covariates",
      input: 'set_ipd(data, ..., family = "survival", time = "months", status = "event")\nset_agd_surv(data, treatment = "trt", time = "months",\n             status = "event", cov_means = ...)',
      effects: 'marginal_effects(effect = "hr" at a time, "tr", "rmstd" or "rmstr" up to a horizon). No difference means HR = 1, TR = 1, RMST difference = 0 and RMST ratio = 1. predict() gives survival, hazard, cumhaz, rmst, median and loghr.',
      scale: "Trial B enters as reconstructed event times, for example read off a published Kaplan-Meier curve, plus covariate summaries. The default distribution is Weibull.",
      boundary: "Reconstruction does not recover trial B's covariates, and its uncertainty is not carried into the fit. Only one comparator arm is supported. Right, left and interval censoring and delayed entry each need their own likelihood terms."
    }
  ];
  var survivalChoices = 'Proportional hazards: exponential, Weibull, Gompertz (positive shape only), and the flexible mspline and pexp baselines. Accelerated failure time: exponential-aft, weibull-aft, lognormal, loglogistic, gamma, and gengamma (positive Q only). By default, aux_by = ".study" gives each trial its own shape; aux_by = "none" shares it. A time ratio from marginal_effects() needs shared slopes and a shared shape; otherwise the label is EXP_DELTA_ETA. Many tied reconstructed event times can make mlumr refuse a lognormal or gengamma fit, and warn for weibull-aft, loglogistic and gamma fits.';
  var diagnosticCases = [
    {
      name: "Divergences",
      symptom: "mlumr() warns that some transitions diverged.",
      answer: "The sampler met a region of the posterior it could not explore well, so the draws may be biased. Look at covariate scaling, the priors and the parameterization. A higher adapt_delta can help, but check again after refitting. Never just delete the divergent draws.",
      tool: "summary(fit)\nfit <- mlumr(dat, ..., adapt_delta = 0.99)"
    },
    {
      name: "R-hat is Inf or missing",
      symptom: "One quantity has R-hat = Inf, and another has no R-hat at all.",
      answer: "R-hat compares the chains with each other. Inf usually means the chains got stuck, each at its own value, and a missing value means the check could not be computed, which is not a pass. Check that every chain returned draws, and look at the traces and the effective sample sizes.",
      tool: "summary(fit)"
    },
    {
      name: "Subgroup screen says NA",
      symptom: "Three binary subgroup rows, one covariate, and check_identification() reports flagged = NA.",
      answer: "For a binary outcome, each row passes through a curved link, so the spread inside a row matters as well as its mean. NA means the check will not give a verdict. It does not mean everything is fine. Look at the posterior intervals and at prior sensitivity.",
      tool: 'check_identification(dat, link = "logit")\nprior_sensitivity(fit_relaxed)'
    },
    {
      name: "Effect moves with more points",
      symptom: "The covariate summaries look stable, but the target effect moves when n_int grows.",
      answer: "The number you want to report has not settled numerically. Refit with larger grids until it stops moving. More points shrink the approximation error; they cannot repair a covariate distribution that is wrong.",
      tool: "check_integration(dat, x = distr(qnorm, mean = x_mean, sd = x_sd))\ndat <- add_integration(dat, n_int = 1024, x = ...)"
    },
    {
      name: "Prior-sensitive target",
      symptom: "Effects in trial B's population are stable, but the effect in trial A's population changes with prior_beta_comparator.",
      answer: "Trial B's summaries can inform an average in its own population while saying little about how its slopes carry over to trial A. Report both populations and the sensitivity. A tighter prior adds assumptions, not observations.",
      tool: 'prior_summary(fit_relaxed)\nplot_prior_posterior(fit_relaxed, pars = "beta_comparator[1]")\nprior_sensitivity(fit_relaxed)'
    },
    {
      name: "Incomplete summary",
      symptom: "A summary used 700 of 1,000 draws, and a survival median was not reached.",
      answer: "Report n_draws and n_draws_used, and find out why draws were dropped. A median beyond the prediction grid is a different issue: check p_not_reached and extend pred_times. Never present either one as complete.",
      tool: 'predict(fit, type = "median")\nmarginal_effects(fit)'
    },
    {
      name: "Better LOO score",
      symptom: "One model has a better LOO, WAIC or DIC score than the other.",
      answer: "A predictive score tells you how well a model predicts these observations. It says nothing about hidden differences between the trials. Compare models fitted to the same data, and check the PSIS diagnostics before trusting LOO.",
      tool: 'calculate_loo(fit)\ncalculate_waic(fit)\ncompare_models(fit, fit_relaxed, criterion = "loo")'
    }
  ];
  var questions = [
    { q: "With shared slopes on the logit scale, which statement is true?", options: ["The odds ratio for one patient is the same for every patient.", "The population odds ratio is the same in every population.", "Hidden differences between the trials are removed."], correct: 0, why: "Shared slopes cancel when you compare the two treatments for one patient. Averaging over a population is curved, so the population odds ratio can still change, and shared slopes do nothing about unmeasured differences." },
    { q: "What happens when you give newdata to marginal_effects()?", options: ["The model is refitted.", "Both treatments are averaged over your target rows.", "The rows become new outcome data."], correct: 1, why: "newdata only describes a target population, with every row counting equally. It adds no outcomes and does not refit the model, and the population argument is ignored." },
    { q: "Can one aggregate row with a normal outcome pin down a target mean?", options: ["Never, because the slope is unknown.", "Always, whatever the target.", "Yes, if the target sits exactly where the row's data are."], correct: 2, why: "Knowing every coefficient and knowing one target are different things. A target at the row's own covariate mean is pinned down even though the slope is not." },
    { q: "When can two RMST differences be compared?", options: ["Whenever both are called RMST differences.", "When they use the same horizon and the same time units.", "When both models have constant hazard ratios."], correct: 1, why: "RMST is the area under the survival curve up to a chosen time. A different time gives a different quantity. Constant hazards are not needed." },
    { q: "Can more integration points remove a hidden difference between the trials?", options: ["Yes, with enough points.", "Only if R-hat is below 1.01.", "No. Accurate arithmetic and comparable trials are separate questions."], correct: 2, why: "Integration points make the model's arithmetic more accurate. They cannot add covariates that nobody measured." }
  ];
  var checklist = [
    "The two treatments, the outcome, the follow-up, the target population and the effect scale.",
    "Where each dataset came from, that subgroup rows do not overlap, and how well the covariates overlap.",
    "Shared or separate slopes, the priors, the covariate distributions and their correlation.",
    "Sampling checks for every chain, integration checks, and prior sensitivity.",
    "Posterior intervals and the draw counts. For survival, the time of each hazard ratio and each RMST horizon.",
    "The naive and STC benchmarks, labeled with the populations they describe."
  ];
  var cells = {
    integration: {
      intro: "The same calculation as the chart with the spread at 1.5 and 16 points.",
      code: "# 16 patients spread evenly between -1.5 and 1.5, so their mean is 0\nx <- -1.5 + 3 * ((1:16) - 0.5) / 16\nrisk <- plogis(-1.8 + 2.4 * x)\nplogis(-1.8)   # risk of the average patient\nmean(risk)     # average risk"
    },
    dependence: {
      intro: "Build the four kinds of patient and average their risks.",
      code: "rho <- 0.5\npatients <- expand.grid(marker1 = 0:1, marker2 = 0:1)\npatients$share <- ifelse(patients$marker1 == patients$marker2,\n                         (1 + rho) / 4, (1 - rho) / 4)\npatients$risk <- plogis(-2.8 + 1.6 * patients$marker1 + 1.6 * patients$marker2)\npatients\ntapply(patients$share, patients$marker1, sum)  # marker 1 stays at 50%\nsum(patients$share * patients$risk)              # average risk"
    },
    target: {
      intro: "Compare the odds ratio for one patient with the odds ratio for a population.",
      code: "q <- 0.5                                  # share of the target with the marker\nriskA <- plogis(c(-1.8, -1.8 + 2.4))      # A: marker absent, present\nriskB <- plogis(c(-1.1, -1.1 + 2.4))      # B: marker absent, present\npA <- sum(c(1 - q, q) * riskA)\npB <- sum(c(1 - q, q) * riskB)\nexp(-1.8 - (-1.1))                        # odds ratio for any one patient\n(pA / (1 - pA)) / (pB / (1 - pB))         # odds ratio for the population\npA - pB                                   # risk difference"
    },
    survival: {
      intro: "Compute the population hazard ratio and the RMST difference at 12 months.",
      code: "q <- 0.5; beta <- 1.8; hr <- 0.65\nrateB <- 0.06 * exp(c(0, beta)); rateA <- rateB * hr\nS <- function(t, rate) (1 - q) * exp(-rate[1] * t) + q * exp(-rate[2] * t)\nh <- function(t, rate) ((1 - q) * rate[1] * exp(-rate[1] * t) +\n                         q * rate[2] * exp(-rate[2] * t)) / S(t, rate)\nh(12, rateA) / h(12, rateB)    # population hazard ratio at 12 months\nintegrate(S, 0, 12, rate = rateA)$value -\n  integrate(S, 0, 12, rate = rateB)$value   # RMST difference, months"
    },
    priors: {
      intro: "The exact posterior behind the chart: one row at x = 0 and a target at x = 1.",
      code: "x <- 0; y <- 0.4; se <- 0.15      # one subgroup mean at x = 0\nprior_sd <- 3; target <- 1\nX <- cbind(1, x)\npost_cov <- solve(crossprod(X) / se^2 + diag(2) / prior_sd^2)\npost_mean <- post_cov %*% crossprod(X, y) / se^2\ng <- c(1, target)\nc(estimate = sum(g * post_mean), sd = sqrt(drop(t(g) %*% post_cov %*% g)))\nprior_sd <- 0.3                     # now rerun the lines above with a tight prior"
    },
    workflow: {
      mlumr: true,
      intro: "Real mlumr R code and the real mlumr Stan model, running in your browser on made-up data.",
      code: 'set.seed(2026)\n# Trial A: 300 patients with individual data\ntrial_a <- data.frame(trt = "A", study = "index", x = rnorm(300, -0.3, 1))\ntrial_a$event <- rbinom(300, 1, plogis(-0.8 + 0.8 * trial_a$x))\n\n# Trial B: only three published subgroup rows\ntrial_b <- data.frame(trt = "B", study = "comparator",\n  n = c(150, 180, 170), events = c(47, 90, 116),\n  x_mean = c(-0.7, 0.3, 1.3), x_sd = c(0.65, 0.65, 0.65))\n\nipd <- set_ipd(trial_a, treatment = "trt", outcome = "event",\n               covariates = "x", family = "binomial", study = "study")\nagd <- set_agd(trial_b, treatment = "trt", family = "binomial",\n               outcome_n = "n", outcome_r = "events",\n               cov_means = "x_mean", cov_sds = "x_sd",\n               cov_types = "continuous", study = "study")\ndat <- combine_data(ipd, agd)\ndat <- add_integration(dat, n_int = 128,\n  x = distr(qnorm, mean = x_mean, sd = x_sd))\ncheck_identification(dat, link = "logit")\nnaive(dat, link = "logit")\nstc(dat, link = "logit")'
    }
  };

  // ../../../Users/choxos/Documents/GitHub/mlumr-lesson/scenes/style.ts
  var lightTokens = `
  --bg:#eef1f2; --bg-soft:#e6ebec; --surface:#ffffff; --surface-2:#f7f9f9; --surface-3:#f0f4f4;
  --ink:#10242c; --ink-soft:#41555d; --muted:#6b7c82; --faint:#9aa8ac;
  --border:#e3e9ea; --border-strong:#cfd9db; --hairline:#edf1f1;
  --accent:#1a6d73; --accent-strong:#11484d; --button:#1a6d73; --button-hover:#11484d; --on-accent:#ffffff;
  --series-a:#006c98; --series-b:#c8741d; --grid:#e3e9ea; --axis:#9aa8ac;
  --ok:#1f7a5a; --warn:#b5483e; --review:#9a6a14;
  --shadow:0 1px 2px rgba(11,45,58,.05),0 10px 30px -16px rgba(11,45,58,.18);
  --shadow-lg:0 24px 60px -28px rgba(11,45,58,.4);
  color-scheme:light;`;
  var darkTokens = `
  --bg:#0a1316; --bg-soft:#0e191d; --surface:#132127; --surface-2:#0f1c21; --surface-3:#16282f;
  --ink:#e9f0f1; --ink-soft:#b6c4c8; --muted:#8ba0a5; --faint:#65777c;
  --border:#1e2f36; --border-strong:#2a3f47; --hairline:#182830;
  --accent:#5fb8c1; --accent-strong:#c9f5f8; --button:#247982; --button-hover:#1d6670; --on-accent:#ffffff;
  --series-a:#3398c2; --series-b:#cf7b26; --grid:#1c2d33; --axis:#65777c;
  --ok:#46c08d; --warn:#e8756a; --review:#d8a23a;
  --shadow:0 1px 2px rgba(0,0,0,.5),0 14px 36px -18px rgba(0,0,0,.7);
  --shadow-lg:0 24px 60px -28px rgba(0,0,0,.8);
  color-scheme:dark;`;
  var icon = (path2) => `url("data:image/svg+xml,${encodeURIComponent(`<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24'>${path2}</svg>`)}")`;
  var playIcon = icon("<path d='M7 5l12 7-12 7z'/>");
  var pauseIcon = icon("<rect x='6' y='5' width='4' height='14' rx='1'/><rect x='14' y='5' width='4' height='14' rx='1'/>");
  var fullIcon = icon("<path d='M4 9V4h5M20 9V4h-5M4 15v5h5M20 15v5h-5' fill='none' stroke='black' stroke-width='2' stroke-linecap='round'/>");
  var css = `
:root {${lightTokens}
  --tint:color-mix(in srgb,var(--accent) 11%,var(--surface));
  --ring:color-mix(in srgb,var(--accent) 34%,transparent);
  --code-bg:#0b1417; --code-ink:#bfe0d8; --code-muted:#6f9a94; --code-ok:#6fd3a8; --code-err:#f3a79d;
  --font-sans:"IBM Plex Sans",ui-sans-serif,system-ui,-apple-system,"Segoe UI",Roboto,sans-serif;
  --font-mono:"IBM Plex Mono","SFMono-Regular",ui-monospace,Consolas,monospace;
  --radius:14px; --radius-sm:9px;
  --header-h:64px; --chrome-h:58px; --captions-h:46px;
  --board-w:min(26%,380px);
}
@media (prefers-color-scheme: dark) { :root:not([data-theme="light"]) {${darkTokens}} }
:root[data-theme="dark"] {${darkTokens}}

body:has(.ml-player) {background:var(--bg);color:var(--ink)}
.xv-shell:has(.ml-player) {width:100%}
.ml-player {background:var(--bg);color:var(--ink);height:100dvh;aspect-ratio:auto;font-family:var(--font-sans);-webkit-font-smoothing:antialiased}
.ml-player :focus-visible {outline:2px solid var(--accent);outline-offset:2px}

/* Narration notes board */
.ml-player .xv-board {top:var(--header-h);bottom:calc(var(--chrome-h) + var(--captions-h));height:auto;width:var(--board-w);padding:22px 24px;background:var(--surface-2);border-left:1px solid var(--border);color:var(--ink-soft);font-size:clamp(13px,1.1vw,16px);line-height:1.6;pointer-events:auto}
.ml-player .xv-board::before {content:"Notes";display:block;font-size:11px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:var(--muted);margin-bottom:18px}
.ml-player .xv-board-inner {gap:18px}
.ml-player .xv-board .katex {color:var(--ink);font-size:1.25em}
.ml-player .xv-board .katex-display {margin:0;overflow-x:auto;overflow-y:hidden}
.ml-player .xv-hl {background:var(--tint);color:var(--accent-strong)}

/* Captions and playback bar */
.ml-player .xv-captions {left:0;right:var(--board-w);bottom:var(--chrome-h);min-height:var(--captions-h);box-sizing:border-box;display:flex;align-items:center;justify-content:center;padding:8px 24px;background:var(--bg-soft);border-top:1px solid var(--border);color:var(--ink);text-shadow:none;font:15px/1.45 var(--font-sans)}
.ml-player .xv-captions:empty {display:none}
.ml-player .xv-chrome {height:var(--chrome-h);background:var(--surface);border-top:1px solid var(--border);padding-inline:16px;gap:10px}
.ml-player .xv-chrome button {color:var(--ink-soft);border-radius:var(--radius-sm)}
.ml-player .xv-chrome button:hover {background:var(--surface-3);color:var(--ink)}
.ml-player .xv-play,.ml-player .xv-fullscreen {font-size:0!important}
.ml-player .xv-play::before,.ml-player .xv-fullscreen::before {content:"";width:18px;height:18px;background:currentColor;-webkit-mask:var(--icon) center/contain no-repeat;mask:var(--icon) center/contain no-repeat}
.ml-player .xv-play {--icon:${playIcon};width:42px;min-width:42px;height:42px;border-radius:50%!important;background:var(--button)!important;color:var(--on-accent)!important}
.ml-player .xv-play:hover {background:var(--button-hover)!important}
.ml-player .xv-play[aria-label="Pause lesson"] {--icon:${pauseIcon}}
.ml-player .xv-fullscreen {--icon:${fullIcon}}
.ml-player .xv-captions-toggle {font:700 11px/1 var(--font-sans)!important;border:1px solid var(--border-strong)!important;height:32px!important;margin-block:6px}
.ml-player .xv-captions-toggle[aria-pressed="true"] {background:var(--tint)!important;color:var(--accent)!important;border-color:var(--accent)!important}
.ml-player .xv-scrubber {-webkit-appearance:none;appearance:none;background:transparent;accent-color:var(--accent)}
.ml-player .xv-scrubber::-webkit-slider-runnable-track {height:4px;border-radius:999px;background:var(--border-strong)}
.ml-player .xv-scrubber::-webkit-slider-thumb {-webkit-appearance:none;width:14px;height:14px;margin-top:-5px;border-radius:50%;background:var(--accent)}
.ml-player .xv-scrubber::-moz-range-track {height:4px;border-radius:999px;background:var(--border-strong)}
.ml-player .xv-scrubber::-moz-range-progress {height:4px;border-radius:999px;background:var(--accent)}
.ml-player .xv-scrubber::-moz-range-thumb {width:12px;height:12px;border:0;border-radius:50%;background:var(--accent)}
.ml-player .xv-elapsed {color:var(--muted);font:12px var(--font-mono);font-variant-numeric:tabular-nums}
.ml-player .xv-credit {background:transparent;color:var(--muted);box-shadow:inset 0 0 0 1px var(--border);font:500 11px var(--font-sans)}
.ml-player .xv-credit:hover {background:var(--surface-3)}

/* Start screen */
.ml-player .xv-start-screen {background:color-mix(in srgb,var(--bg) 62%,transparent);-webkit-backdrop-filter:blur(6px);backdrop-filter:blur(6px);color:var(--ink);font-family:var(--font-sans)}
.ml-player .xv-start-content {background:var(--surface);border:1px solid var(--border);border-radius:var(--radius);box-shadow:var(--shadow-lg);padding:clamp(22px,3.5vw,38px)}
.ml-player .xv-start-title {font-size:clamp(26px,3.4vw,38px);font-weight:600;line-height:1.12;letter-spacing:-.02em;color:var(--ink);max-width:24ch}
.ml-player .xv-start-interactive {color:var(--ink-soft)}
.ml-player .xv-orientation-notice {color:var(--review)}
.ml-player .xv-start-status {color:var(--muted)}
.ml-player .xv-loading-spinner {border-color:var(--border-strong);border-top-color:var(--accent)}
.ml-player .xv-start-button {background:var(--button);color:var(--on-accent);border-radius:var(--radius-sm);font:600 16px var(--font-sans)}
.ml-player .xv-start-button:hover:not(:disabled) {background:var(--button-hover);transform:none}

/* Lesson frame */
.ml-lesson {position:absolute;inset:0 0 calc(var(--chrome-h) + var(--captions-h)) 0;display:flex;flex-direction:column;color:var(--ink);font:15px/1.5 var(--font-sans);pointer-events:auto;box-sizing:border-box;overflow:hidden}
.ml-player:has(.xv-captions:empty) .ml-lesson {bottom:var(--chrome-h)}
.ml-player:has(.xv-captions:empty) .xv-board {bottom:var(--chrome-h)}
.ml-lesson * {box-sizing:border-box}
.ml-lesson h1,.ml-lesson h2,.ml-lesson h3,.ml-lesson p {margin:0}
.ml-lesson .ml-header {display:flex;align-items:center;justify-content:space-between;gap:12px;height:var(--header-h);padding:0 22px;background:var(--surface);border-bottom:1px solid var(--border);flex-shrink:0;position:relative;z-index:4}
.ml-lesson .brand {display:flex;align-items:center;gap:10px;font-size:17px;font-weight:700;letter-spacing:-.01em;color:var(--ink);text-decoration:none;white-space:nowrap}
.ml-lesson .brand-tag {font-size:12px;font-weight:600;color:var(--ink-soft);padding:3px 10px;border:1px solid var(--border-strong);border-radius:999px;background:var(--surface-2)}
.ml-lesson .header-tools {display:flex;align-items:center;gap:10px}
.ml-lesson .header-tools a {font-size:14px;font-weight:550;color:var(--ink-soft);text-decoration:none;padding:6px 8px;border-radius:999px}
.ml-lesson .header-tools a:hover {color:var(--accent-strong);background:var(--tint)}
.ml-lesson button,.ml-lesson select,.ml-lesson summary,.ml-lesson textarea {font:inherit}
.ml-lesson button,.ml-lesson select {min-height:44px;cursor:pointer;border:1px solid var(--border-strong);border-radius:var(--radius-sm);padding:8px 14px;background:var(--surface);color:var(--ink-soft);font-size:14px;font-weight:600}
.ml-lesson button:hover {background:var(--surface-3);border-color:var(--accent);color:var(--accent-strong)}
.ml-lesson button:disabled {opacity:.4;cursor:default}
.ml-lesson button.primary {background:var(--button);border-color:var(--button);color:var(--on-accent)}
.ml-lesson button.primary:hover {background:var(--button-hover);border-color:var(--button-hover);color:var(--on-accent)}
.ml-lesson .theme-toggle {display:inline-flex;align-items:center;gap:8px;min-height:44px;padding:0 10px;border-radius:999px;background:var(--surface-2);font-size:12px}
.ml-lesson .theme-toggle svg {width:16px;height:16px}
.ml-lesson .theme-track {position:relative;width:34px;height:18px;border-radius:999px;background:var(--border-strong)}
.ml-lesson .theme-thumb {position:absolute;top:3px;left:3px;width:12px;height:12px;border-radius:50%;background:var(--surface);box-shadow:0 1px 2px rgba(0,0,0,.25);transition:transform .16s ease}
.ml-lesson .theme-toggle[aria-pressed="true"] .theme-track {background:var(--button)}
.ml-lesson .theme-toggle[aria-pressed="true"] .theme-thumb {transform:translateX(16px);background:#fff}

/* Chapter navigation */
.ml-chapter-nav {display:flex;align-items:center;gap:4px}
.ml-lesson .ml-chapter-nav>button {width:44px;padding:0;border-color:transparent;background:transparent;font-size:22px;line-height:1;color:var(--ink-soft)}
.ml-lesson .chapter-count {color:var(--muted);font:13px var(--font-mono);min-width:58px;text-align:center}
.ml-lesson .chapter-menu {position:relative}
.ml-lesson .chapter-menu>summary {display:flex;align-items:center;gap:8px;min-height:44px;padding:0 14px;list-style:none;border:1px solid var(--border-strong);border-radius:var(--radius-sm);background:var(--surface);font-size:14px;font-weight:600;color:var(--ink-soft);cursor:pointer}
.ml-lesson .chapter-menu>summary::-webkit-details-marker {display:none}
.ml-lesson .chapter-menu[open]>summary,.ml-lesson .chapter-menu>summary:hover {border-color:var(--accent);color:var(--accent-strong)}
.ml-lesson .menu-icon {width:16px;height:12px;display:inline-block;border-block:2px solid currentColor;position:relative}
.ml-lesson .menu-icon::after {content:"";position:absolute;top:3px;left:0;right:4px;border-top:2px solid currentColor}
.ml-lesson .chapter-list {position:absolute;z-index:6;top:52px;left:50%;transform:translateX(-50%);width:min(470px,calc(100vw - 32px));max-height:calc(100dvh - 180px);overflow:auto;background:var(--surface);border:1px solid var(--border-strong);border-radius:var(--radius);padding:10px;box-shadow:var(--shadow-lg)}
.ml-lesson .chapter-list p {margin:4px 10px 10px;color:var(--muted);font-size:12px}
.ml-lesson .chapter-list button {width:100%;display:flex;align-items:center;gap:12px;text-align:left;border:0;background:transparent;min-height:42px;padding:0 10px;font-weight:500;color:var(--ink)}
.ml-lesson .chapter-list button span {color:var(--muted);font:12px var(--font-mono);width:20px}
.ml-lesson .chapter-list button[aria-current=step] {background:var(--tint);color:var(--accent-strong);font-weight:650}
.ml-lesson .chapter-list button[data-narrated=true]::after {content:"Narration here";margin-left:auto;font-size:11px;font-weight:650;color:var(--accent);border:1px solid var(--accent);border-radius:999px;padding:2px 8px}
.ml-lesson .chapter-list button:hover {background:var(--surface-3)}
.ml-lesson .narration-return {display:flex;align-items:center;justify-content:space-between;gap:16px;margin-right:var(--board-w);padding:6px 22px;background:var(--tint);border-bottom:1px solid var(--border);flex-shrink:0}
.ml-lesson .narration-return[hidden] {display:none}
.ml-lesson .narration-return span {font-size:13px;color:var(--ink-soft);min-width:0}
.ml-lesson .narration-return button {min-height:36px;font-size:13px;color:var(--accent-strong);background:var(--surface);white-space:nowrap}

/* Lab content */
.ml-lesson .lab-scroll {overflow:auto;flex:1;min-height:0;padding:22px 28px 28px;margin-right:var(--board-w);scrollbar-gutter:stable;scrollbar-color:var(--border-strong) transparent;container-type:inline-size}
.ml-lesson .lab-title {display:flex;align-items:flex-end;justify-content:space-between;gap:16px;margin-bottom:18px}
.ml-lesson .eyebrow {display:block;font-size:12px;font-weight:700;letter-spacing:.08em;text-transform:uppercase;color:var(--accent);margin-bottom:6px}
.ml-lesson .lab-title h1 {font-size:clamp(24px,2.6vw,34px);font-weight:600;line-height:1.12;letter-spacing:-.02em;outline:none}
.ml-lesson .question {margin-top:6px;color:var(--ink-soft);font-size:16px}
.ml-lesson .reset {flex-shrink:0}
.ml-lesson .lab-body {display:grid;grid-template-columns:minmax(0,1.6fr) minmax(250px,1fr);grid-template-areas:"charts controls" "charts metrics" "charts note" "extra extra";grid-template-rows:auto auto 1fr auto;gap:14px 16px;align-items:start}
.ml-lesson .visual {display:contents}
.ml-lesson .charts {grid-area:charts;display:grid;gap:14px;min-width:0}
.ml-lesson .lab-controls {grid-area:controls;display:grid;gap:14px;padding:16px 18px;background:var(--surface);border:1px solid var(--border);border-radius:var(--radius);box-shadow:var(--shadow)}
.ml-lesson .lab-controls::before {content:"Explore";font-size:14px;font-weight:650;color:var(--ink)}
.ml-lesson .metrics {grid-area:metrics;display:grid;grid-template-columns:repeat(auto-fit,minmax(118px,1fr));gap:10px}
.ml-lesson .metric {padding:12px 14px;background:var(--surface);border:1px solid var(--border);border-radius:var(--radius-sm);min-width:0}
.ml-lesson .metric span {display:block;font-size:12px;font-weight:600;color:var(--muted)}
.ml-lesson .metric strong {display:block;font-size:clamp(18px,1.8vw,24px);font-weight:500;letter-spacing:-.02em;font-variant-numeric:tabular-nums;color:var(--ink);margin-top:2px;overflow-wrap:anywhere}
.ml-lesson .interpretation {grid-area:note;padding:12px 14px;border-radius:var(--radius-sm);background:var(--tint);font-size:14px;line-height:1.6;color:var(--ink)}
.ml-lesson .extra {grid-area:extra;display:grid;gap:12px;min-width:0}
.ml-lesson .extra p,.ml-lesson .extra li {font-size:14px;line-height:1.65;color:var(--ink-soft);max-width:80ch}
.ml-lesson .formula {white-space:pre-line;padding:12px 16px;border:1px solid var(--border);border-radius:var(--radius-sm);background:var(--surface-2);font:14px/1.9 var(--font-mono);color:var(--ink);overflow-x:auto}
@container (max-width: 720px) {
  .ml-lesson .lab-body {grid-template-columns:minmax(0,1fr);grid-template-areas:"controls" "charts" "metrics" "note" "extra";grid-template-rows:none}
}
.ml-lesson .control {display:grid;gap:4px;min-width:0;font-size:13px;font-weight:600;color:var(--ink-soft)}
.ml-lesson .control>span {display:flex;justify-content:space-between;gap:12px;align-items:baseline}
.ml-lesson output {color:var(--accent);font:500 13px var(--font-mono);white-space:nowrap}
.ml-lesson input[type=range] {-webkit-appearance:none;appearance:none;width:100%;height:44px;cursor:pointer;margin:0;background:transparent}
.ml-lesson input[type=range]::-webkit-slider-runnable-track {height:4px;border-radius:999px;background:linear-gradient(var(--button),var(--button)) 0 0/var(--fill,0%) 100% no-repeat,var(--border-strong)}
.ml-lesson input[type=range]::-webkit-slider-thumb {-webkit-appearance:none;width:18px;height:18px;margin-top:-7px;border-radius:50%;background:var(--surface);border:2px solid var(--button);box-shadow:0 1px 2px rgba(0,0,0,.25)}
.ml-lesson input[type=range]::-moz-range-track {height:4px;border-radius:999px;background:var(--border-strong)}
.ml-lesson input[type=range]::-moz-range-progress {height:4px;border-radius:999px;background:var(--button)}
.ml-lesson input[type=range]::-moz-range-thumb {width:14px;height:14px;border-radius:50%;background:var(--surface);border:2px solid var(--button)}
.ml-lesson input[type=range]:focus-visible {outline:2px solid var(--accent);outline-offset:2px;border-radius:6px}
.ml-lesson input:disabled {opacity:.35;cursor:not-allowed}
.ml-lesson .control select {width:100%;appearance:auto;font-weight:500;color:var(--ink)}

/* Charts */
.ml-lesson .chart-card {margin:0;padding:14px 16px 10px;background:var(--surface);border:1px solid var(--border);border-radius:var(--radius);box-shadow:var(--shadow);min-width:0}
.ml-lesson .chart-card figcaption {display:flex;flex-wrap:wrap;justify-content:space-between;align-items:baseline;gap:6px 14px;margin-bottom:6px}
.ml-lesson .chart-title {font-size:14px;font-weight:650;color:var(--ink)}
.ml-lesson .legend {display:flex;flex-wrap:wrap;gap:4px 14px;font:12px var(--font-mono);color:var(--muted)}
.ml-lesson .legend span {display:inline-flex;align-items:center;gap:6px}
.ml-lesson .legend .k-muted-text {color:var(--muted)}
.ml-lesson .legend i {width:14px;height:3px;border-radius:999px;background:currentColor}
.ml-lesson .legend i.dash {background:repeating-linear-gradient(90deg,currentColor 0 4px,transparent 4px 7px)}
.ml-lesson .chart-card svg {display:block;width:100%;height:auto;overflow:visible;font-family:var(--font-sans)}
.ml-lesson svg .grid {stroke:var(--grid);stroke-width:1}
.ml-lesson svg .axis {stroke:var(--axis);stroke-width:1}
.ml-lesson svg .tick {fill:var(--muted);font:11px var(--font-mono)}
.ml-lesson svg .label {fill:var(--muted);font-size:12px}
.ml-lesson svg .ann {fill:var(--ink);font-size:12.5px;font-weight:500}
.ml-lesson svg .ann-soft {fill:var(--ink-soft);font-size:12px}
.ml-lesson .k-a {color:var(--series-a)} .ml-lesson .k-b {color:var(--series-b)} .ml-lesson .k-ink {color:var(--ink)} .ml-lesson .k-muted {color:var(--faint)} .ml-lesson .k-accent {color:var(--accent)} .ml-lesson .k-warn {color:var(--warn)}
.ml-lesson svg .k-a,.ml-lesson svg .k-b,.ml-lesson svg .k-ink,.ml-lesson svg .k-muted,.ml-lesson svg .k-accent,.ml-lesson svg .k-warn {stroke:currentColor;fill:currentColor}
.ml-lesson svg .line {fill:none!important;stroke-width:2.25;stroke-linejoin:round;stroke-linecap:round}
.ml-lesson svg .dash {stroke-dasharray:6 5}
.ml-lesson svg .band {stroke:none!important;opacity:.13}
.ml-lesson svg .dot {stroke:var(--surface)!important;stroke-width:2}
.ml-lesson svg .ring {fill:var(--surface)!important;stroke-width:2.25}
.ml-lesson svg .bar {stroke:none!important}
.ml-lesson svg .node rect {fill:var(--surface-2);stroke:var(--border-strong)}
.ml-lesson svg .node text {fill:var(--ink-soft)}
.ml-lesson svg .node.on rect {fill:var(--tint);stroke:var(--accent);stroke-width:2}
.ml-lesson svg .node.on text {fill:var(--accent-strong)}
.ml-lesson svg .node[data-step] {cursor:pointer}
.ml-lesson svg .node[data-step]:hover rect {stroke:var(--accent)}
.ml-lesson .stepper {display:flex;align-items:center;gap:8px}
.ml-lesson .stepper button {width:44px;min-height:44px;padding:0;font-size:22px;line-height:1;color:var(--ink-soft)}.ml-lesson .step-count {flex:1;text-align:center;font-weight:600;color:var(--ink)}
.ml-lesson .step-hint {font-weight:500;color:var(--muted)}
.ml-lesson svg .off {fill:var(--border-strong)!important}
.ml-lesson .joint-grid {display:grid;grid-template-columns:1fr 1fr;gap:6px}
.ml-lesson .joint-grid>div {padding:18px;border-radius:var(--radius-sm);border:1px solid var(--border);background:color-mix(in srgb,var(--series-a) var(--w),var(--surface))}
.ml-lesson .joint-grid span {display:block;font-size:13px;color:var(--ink-soft)}
.ml-lesson .joint-grid strong {display:block;font-size:26px;font-weight:500;margin-top:4px;font-variant-numeric:tabular-nums}

/* Text blocks, cases and questions */
.ml-lesson pre {margin:0;overflow:auto;white-space:pre;font:13px/1.7 var(--font-mono);padding:14px 16px;background:var(--code-bg);color:var(--code-ink);border:1px solid var(--border-strong);border-radius:var(--radius-sm);tab-size:2}
.ml-lesson h3 {font-size:18px;line-height:1.4;font-weight:600}
.ml-lesson details:not(.chapter-menu) {padding:4px 16px;border:1px solid var(--border);border-radius:var(--radius-sm);background:var(--surface)}
.ml-lesson details:not(.chapter-menu) summary {color:var(--ink);font-weight:600;font-size:14px;padding:10px 0;cursor:pointer;min-height:44px;display:flex;align-items:center}
.ml-lesson details:not(.chapter-menu)[open] {padding-bottom:14px}
.ml-lesson .read-list {margin:0;padding-left:20px}
.ml-lesson .case {display:grid;gap:10px}
.ml-lesson .case .interpretation {grid-area:auto}
.ml-lesson .answers {display:grid;gap:8px}
.ml-lesson .answers button {text-align:left;font-weight:500;color:var(--ink);line-height:1.5;padding:12px 16px}
.ml-lesson .feedback {min-height:22px;font-size:14px;font-weight:600;color:var(--review)}
.ml-lesson .feedback[data-correct=true] {color:var(--ok)}
.ml-lesson a {color:var(--accent);text-underline-offset:3px}
.ml-lesson footer {margin-top:22px;padding-top:12px;border-top:1px solid var(--border);font-size:12px;color:var(--muted)}

/* Runnable code cells */
.ml-lesson .code-slot:empty {display:none}
.ml-lesson .code-slot {margin-top:16px}
.ml-lesson .code-cell {background:var(--surface);border:1px solid var(--border);border-radius:var(--radius);box-shadow:var(--shadow);overflow:hidden}
.ml-lesson .code-head {display:flex;flex-wrap:wrap;align-items:center;justify-content:space-between;gap:8px 12px;padding:10px 14px;border-bottom:1px solid var(--border)}
.ml-lesson .code-head h2 {font-size:15px;font-weight:650}
.ml-lesson .code-head p {font-size:13px;color:var(--muted)}
.ml-lesson .code-actions {display:flex;flex-wrap:wrap;gap:8px}
.ml-lesson .code-actions button {min-height:40px;padding:6px 12px;font-size:13px}
.ml-lesson .code-body {display:grid;grid-template-columns:minmax(0,1.15fr) minmax(0,1fr)}
@container (max-width: 760px) { .ml-lesson .code-body {grid-template-columns:minmax(0,1fr)} }
.ml-lesson .code-body textarea {display:block;width:100%;min-height:220px;resize:vertical;border:0;border-radius:0;padding:12px 16px;background:var(--code-bg);color:var(--code-ink);font:13px/1.65 var(--font-mono);white-space:pre;tab-size:2;outline-offset:-2px}
.ml-lesson .console {min-height:220px;max-height:420px;overflow:auto;border-left:1px solid #1e2f36;border-radius:0;white-space:pre-wrap;font-size:12.5px;line-height:1.6}
.ml-lesson .console .muted {color:var(--code-muted)} .ml-lesson .console .ok {color:var(--code-ok)} .ml-lesson .console .err {color:var(--code-err)}
.ml-lesson .code-foot {padding:10px 14px;border-top:1px solid var(--border);display:grid;gap:10px}
.ml-lesson .code-foot:empty {display:none}
.ml-lesson .seg {display:inline-flex;border:1px solid var(--border-strong);border-radius:var(--radius-sm);overflow:hidden}
.ml-lesson .seg button {border:0;border-radius:0;min-height:40px;font-size:13px}
.ml-lesson .seg button+button {border-left:1px solid var(--border)}
.ml-lesson .seg button[aria-pressed=true] {background:var(--button);color:var(--on-accent)}
.ml-lesson .fit-table {width:100%;border-collapse:collapse;font:13px var(--font-mono)}
.ml-lesson .fit-table th,.ml-lesson .fit-table td {padding:6px 10px;border-bottom:1px solid var(--hairline);text-align:right;white-space:nowrap}
.ml-lesson .fit-table th:first-child,.ml-lesson .fit-table td:first-child {text-align:left}
.ml-lesson .fit-table th {font:600 12px var(--font-sans);color:var(--muted)}
.ml-lesson .table-wrap {overflow-x:auto}
.ml-lesson .fit-out {display:grid;gap:12px}
.ml-lesson .fit-out .chart-card {max-width:680px;box-shadow:none}
.ml-lesson .fit-table td:nth-child(2),.ml-lesson .fit-table th:nth-child(2) {text-align:left;font-family:var(--font-sans);white-space:normal}

@media (max-width:1100px) { .ml-lesson .brand-tag,.ml-lesson .header-tools a {display:none} .ml-lesson .lab-scroll {padding:16px 18px 22px} .ml-lesson .ml-header {padding:0 14px} }
@media (max-width:900px), (max-height:500px) {
  :root {--header-h:52px;--chrome-h:48px;--captions-h:40px;--board-w:24%}
  .ml-lesson .brand-tag {display:none}
  .ml-lesson .theme-toggle .theme-label {display:none}
  .ml-lesson .chapter-menu>summary {padding:0 10px}
  .ml-lesson .chapter-menu>summary .menu-text {display:none}
  .ml-lesson .ml-chapter-nav>button {width:40px}
  .ml-lesson .lab-title {margin-bottom:10px}
  .ml-lesson .lab-title h1 {font-size:21px}
  .ml-lesson .question {font-size:13px}
  .ml-lesson .lab-scroll {padding:10px 12px 16px}
  .ml-player .xv-board {padding:12px;font-size:12px}
  .ml-player .xv-board::before {margin-bottom:8px}
  .ml-player .xv-captions {font-size:12.5px;padding:4px 10px}
  .ml-player .xv-chrome {gap:4px;padding-inline:6px}
  .ml-player .xv-play {width:36px;min-width:36px;height:36px}
  .ml-player .xv-credit {font-size:9px;padding:0 6px}
  .ml-player .xv-elapsed {min-width:72px;font-size:10px}
}
@media (prefers-reduced-motion:reduce) { .ml-player * {scroll-behavior:auto;transition:none!important} }
`;

  // ../../../Users/choxos/Documents/GitHub/mlumr-lesson/scenes/navigation.ts
  var THEME_KEY = "mlumr-lesson-theme";
  function themeToggle(button2) {
    const media = matchMedia("(prefers-color-scheme: dark)");
    const root = document.documentElement;
    const current = () => root.dataset.theme ?? (media.matches ? "dark" : "light");
    const sync = () => {
      const dark = current() === "dark";
      button2.setAttribute("aria-pressed", String(dark));
      button2.querySelector(".theme-label").textContent = dark ? "Dark" : "Light";
    };
    const click = () => {
      root.dataset.theme = current() === "dark" ? "light" : "dark";
      try {
        localStorage.setItem(THEME_KEY, root.dataset.theme);
      } catch {
      }
      sync();
    };
    button2.addEventListener("click", click);
    media.addEventListener("change", sync);
    sync();
    return () => {
      button2.removeEventListener("click", click);
      media.removeEventListener("change", sync);
    };
  }
  function chapterNavigation(root, overlay, choose) {
    const player = overlay.parentElement;
    const hasNarration = Boolean(player?.querySelector("audio"));
    player?.classList.add("ml-player");
    const entries = Object.entries(labs);
    const nav = document.createElement("nav");
    nav.className = "ml-chapter-nav";
    nav.setAttribute("aria-label", "Lesson chapters");
    nav.innerHTML = `<button type="button" data-direction="-1" aria-label="Previous chapter">\u2039</button><span class="chapter-count" aria-live="polite"></span><button type="button" data-direction="1" aria-label="Next chapter">\u203A</button><details class="chapter-menu"><summary><i class="menu-icon" aria-hidden="true"></i><span class="menu-text">Chapters</span></summary><div class="chapter-list"><p>Open any chapter. The narration keeps playing where it is.</p>${entries.map(([key, [title]], i3) => `<button type="button" data-chapter="${key}"><span>${String(i3 + 1).padStart(2, "0")}</span>${title}</button>`).join("")}</div></details>`;
    root.querySelector(".ml-header .brand").after(nav);
    const menu = nav.querySelector("details");
    const summary = menu.querySelector("summary");
    const count = nav.querySelector(".chapter-count");
    const previous = nav.querySelector('[data-direction="-1"]');
    const next = nav.querySelector('[data-direction="1"]');
    const buttons = [...nav.querySelectorAll("[data-chapter]")];
    const returning = document.createElement("div");
    returning.className = "narration-return";
    returning.hidden = true;
    returning.innerHTML = '<span aria-live="polite"></span><button type="button">Return to narration</button>';
    root.querySelector(".ml-header").after(returning);
    const returnButton = returning.querySelector("button");
    const returnToNarration = () => {
      choose(null);
      root.querySelector("h1").focus({ preventScroll: true });
    };
    returnButton.addEventListener("click", returnToNarration);
    nav.dataset.ready = "true";
    let current = 0;
    let last = "";
    const close2 = () => {
      menu.open = false;
    };
    const click = (event) => {
      const button2 = event.target.closest("button");
      if (!button2) return;
      const index = button2.dataset.chapter ? entries.findIndex(([key]) => key === button2.dataset.chapter) : current + Number(button2.dataset.direction);
      if (index < 0 || index >= entries.length) return;
      choose(entries[index][0]);
      close2();
      if (button2.dataset.chapter) summary.focus();
    };
    const outside = (event) => {
      if (!nav.contains(event.target)) close2();
    };
    const keydown = (event) => {
      if (event.key === "Escape" && menu.open) {
        close2();
        summary.focus();
      }
    };
    nav.addEventListener("click", click);
    document.addEventListener("pointerdown", outside);
    nav.addEventListener("keydown", keydown);
    return {
      update(lab, narrated, exploring) {
        const key = `${lab}:${narrated}:${exploring}`;
        if (last === key) return;
        last = key;
        root.dataset.narratedLab = narrated;
        returning.hidden = !hasNarration || !exploring;
        returning.querySelector("span").textContent = `You are exploring. The narration is on: ${labs[narrated][0]}`;
        current = entries.findIndex(([key2]) => key2 === lab);
        root.dataset.lab = lab;
        count.textContent = `${String(current + 1).padStart(2, "0")} / ${entries.length}`;
        previous.disabled = current === 0;
        next.disabled = current === entries.length - 1;
        buttons.forEach((button2, i3) => {
          if (i3 === current) button2.setAttribute("aria-current", "step");
          else button2.removeAttribute("aria-current");
          button2.dataset.narrated = String(hasNarration && entries[i3][0] === narrated);
        });
      },
      dispose() {
        returnButton.removeEventListener("click", returnToNarration);
        returning.remove();
        nav.removeEventListener("click", click);
        document.removeEventListener("pointerdown", outside);
        nav.removeEventListener("keydown", keydown);
        nav.remove();
        player?.classList.remove("ml-player");
      }
    };
  }

  // ../../../Users/choxos/Documents/GitHub/mlumr-lesson/scenes/charts.ts
  var W = 600;
  var H = 290;
  var L = 52;
  var R = 18;
  var T2 = 16;
  var B = 44;
  var esc = (s2) => s2.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll('"', "&quot;");
  function legend(items) {
    return `<span class="legend">${items.map(([k, text2, dash]) => `<span class="k-${k}"><i class="${dash ? "dash" : ""}"></i><span class="k-muted-text">${esc(text2)}</span></span>`).join("")}</span>`;
  }
  function card(title, body, items = []) {
    return `<figure class="chart-card"><figcaption><span class="chart-title">${esc(title)}</span>${items.length ? legend(items) : ""}</figcaption>${body}</figure>`;
  }
  function lineChart(c2) {
    const X = (v2) => L + (W - L - R) * (v2 - c2.x[0]) / (c2.x[1] - c2.x[0]);
    const Y = (v2) => H - B - (H - B - T2) * (v2 - c2.y[0]) / (c2.y[1] - c2.y[0]);
    const xf = c2.xFmt ?? String, yf = c2.yFmt ?? String;
    const path2 = (pts) => pts.map(([x2, y2]) => `${X(x2).toFixed(1)},${Y(y2).toFixed(1)}`).join(" ");
    const clampY = (v2) => Math.min(c2.y[1], Math.max(c2.y[0], v2));
    let s2 = c2.yTicks.map((v2) => `<line class="grid" x1="${L}" x2="${W - R}" y1="${Y(v2)}" y2="${Y(v2)}"/><text class="tick" x="${L - 8}" y="${Y(v2) + 4}" text-anchor="end">${yf(v2)}</text>`).join("");
    s2 += c2.xTicks.map((v2) => `<text class="tick" x="${X(v2)}" y="${H - B + 18}" text-anchor="middle">${xf(v2)}</text>`).join("");
    s2 += `<line class="axis" x1="${L}" x2="${W - R}" y1="${H - B}" y2="${H - B}"/>`;
    s2 += `<text class="label" x="${(L + W - R) / 2}" y="${H - 6}" text-anchor="middle">${esc(c2.xLabel)}</text>`;
    s2 += `<text class="label" x="${-(T2 + H - B) / 2}" y="14" transform="rotate(-90)" text-anchor="middle">${esc(c2.yLabel)}</text>`;
    for (const b2 of c2.bands ?? []) s2 += `<polygon class="band k-${b2.key}" points="${path2(b2.upper.map(([x2, y2]) => [x2, clampY(y2)]))} ${path2([...b2.lower].reverse().map(([x2, y2]) => [x2, clampY(y2)]))}"/>`;
    for (const h2 of c2.hlines ?? []) s2 += `<line class="line k-${h2.key}${h2.dash ? " dash" : ""}" x1="${L}" x2="${W - R}" y1="${Y(h2.y)}" y2="${Y(h2.y)}"/>`;
    for (const v2 of c2.vlines ?? []) s2 += `<line class="axis dash" x1="${X(v2.x)}" x2="${X(v2.x)}" y1="${T2}" y2="${H - B}"/>${v2.text ? `<text class="ann-soft" x="${X(v2.x) + 6}" y="${T2 + 12}">${esc(v2.text)}</text>` : ""}`;
    for (const l2 of c2.lines ?? []) s2 += `<polyline class="line k-${l2.key}${l2.dash ? " dash" : ""}" points="${path2(l2.points.map(([x2, y2]) => [x2, clampY(y2)]))}"/>`;
    for (const d2 of c2.dots ?? []) s2 += `<circle class="${d2.ring ? "ring" : "dot"} k-${d2.key}" cx="${X(d2.at[0]).toFixed(1)}" cy="${Y(clampY(d2.at[1])).toFixed(1)}" r="${d2.r ?? 5}"/>`;
    for (const n of c2.notes ?? []) s2 += `<text class="${n.soft ? "ann-soft" : "ann"}" x="${X(n.at[0]).toFixed(1)}" y="${(Y(n.at[1]) + (n.dy ?? 0)).toFixed(1)}" text-anchor="${n.anchor ?? "start"}">${esc(n.text)}</text>`;
    return card(c2.title, `<svg viewBox="0 0 ${W} ${H}" role="img" aria-label="${esc(c2.label)}">${s2}</svg>`, c2.legend ?? []);
  }
  function barChart(title, label, rows, max = 1) {
    const rowH = 38, h2 = rows.length * rowH + 10, x0 = 180, x1 = W - 70;
    const s2 = rows.map((r2, i3) => {
      const y2 = 8 + i3 * rowH, w2 = Math.max(0, (x1 - x0) * r2.value / max);
      return `<text class="ann-soft" x="0" y="${y2 + 19}">${esc(r2.name)}</text><rect class="off" x="${x0}" y="${y2 + 8}" width="${x1 - x0}" height="14" rx="4"/><rect class="bar k-${r2.key}" x="${x0}" y="${y2 + 8}" width="${w2.toFixed(1)}" height="14" rx="4"/><text class="ann" x="${x1 + 10}" y="${y2 + 20}">${esc(r2.text)}</text>`;
    }).join("");
    return card(title, `<svg viewBox="0 0 ${W} ${h2}" role="img" aria-label="${esc(label)}">${s2}</svg>`);
  }
  function intervalChart(title, label, rows, range, ticks, refs, fmt2 = (v2) => v2.toFixed(2)) {
    const rowH = 44, top = 26, h2 = top + rows.length * rowH + 34, x0 = 170, x1 = W - 20;
    const X = (v2) => x0 + (x1 - x0) * (Math.min(range[1], Math.max(range[0], v2)) - range[0]) / (range[1] - range[0]);
    let s2 = ticks.map((t2) => `<line class="grid" x1="${X(t2)}" x2="${X(t2)}" y1="${top - 6}" y2="${h2 - 30}"/><text class="tick" x="${X(t2)}" y="${h2 - 12}" text-anchor="middle">${fmt2(t2)}</text>`).join("");
    s2 += refs.map((r2) => `<line class="axis dash" x1="${X(r2.x)}" x2="${X(r2.x)}" y1="${top - 12}" y2="${h2 - 30}"/><text class="ann-soft" x="${X(r2.x)}" y="${top - 14}" text-anchor="middle">${esc(r2.text)}</text>`).join("");
    s2 += rows.map((r2, i3) => {
      const y2 = top + i3 * rowH + rowH / 2;
      return `<text class="ann-soft" x="0" y="${y2 + 4}">${esc(r2.name)}</text><line class="line k-${r2.key}" x1="${X(r2.lo)}" x2="${X(r2.hi)}" y1="${y2}" y2="${y2}"/><circle class="dot k-${r2.key}" cx="${X(r2.mean)}" cy="${y2}" r="6"/>`;
    }).join("");
    return card(title, `<svg viewBox="0 0 ${W} ${h2}" role="img" aria-label="${esc(label)}">${s2}</svg>`);
  }
  function flowChart(title, steps, current) {
    const n = steps.length, gap = 12, w2 = (W - gap * (n - 1)) / n, h2 = 110;
    const s2 = steps.map((st, i3) => {
      const x2 = i3 * (w2 + gap);
      const arrow = i3 < n - 1 ? `<path class="axis" d="M${x2 + w2 + 1} 46h${gap - 3}" stroke-width="2"/><path class="k-muted" d="M${x2 + w2 + gap - 1} 46l-5 -4v8z"/>` : "";
      return `<g class="node${i3 === current ? " on" : ""}" data-step="${i3}"><title>Go to step ${i3 + 1}, ${esc(st.name)}</title><rect x="${x2}" y="10" width="${w2}" height="72" rx="9"/><text x="${x2 + w2 / 2}" y="36" text-anchor="middle" font-size="11" font-family="var(--font-mono)">${String(i3 + 1).padStart(2, "0")}</text><text x="${x2 + w2 / 2}" y="58" text-anchor="middle" font-size="13" font-weight="650">${esc(st.name)}</text><text class="label" x="${x2 + w2 / 2}" y="100" text-anchor="middle" font-size="10.5" font-family="var(--font-mono)">${esc(st.detail)}</text></g>${arrow}`;
    }).join("");
    return card(title, `<svg viewBox="0 0 ${W} ${h2}" role="img" aria-label="${esc(`Analysis steps; step ${current + 1}, ${steps[current].name}, is highlighted`)}">${s2}</svg>`);
  }
  function populations(title, groups) {
    const s2 = groups.map((g, gi) => {
      const x0 = 20 + gi * 310, filled = Math.round(g.share * 100);
      const dots = Array.from({ length: 100 }, (_2, i3) => `<circle class="${i3 < filled ? `k-${g.key}` : "off"}" cx="${x0 + i3 % 20 * 13}" cy="${40 + Math.floor(i3 / 20) * 13}" r="4.2"/>`).join("");
      return `<text class="ann k-${g.key}" x="${x0}" y="18">${esc(g.name)}</text>${dots}<text class="ann-soft" x="${x0}" y="118">${(100 * g.share).toFixed(0)} of 100 have the marker</text>`;
    }).join("");
    return card(title, `<svg viewBox="0 0 ${W} 126" role="img" aria-label="${esc(`${title}: ${groups.map((g) => `${g.name} ${(100 * g.share).toFixed(0)} percent`).join(", ")}`)}">${s2}</svg>`);
  }

  // ../../../Users/choxos/Documents/GitHub/mlumr-lesson/scenes/runner.ts
  var WEBR_URL = "https://webr.r-wasm.org/v0.6.0/webr.mjs";
  var R_PACKAGES = ["randtoolbox", "jsonlite", "detectseparation"];
  var site = (path2) => new URL(path2, document.baseURI).href;
  var RUNNER = String.raw`
.lesson_run <- function(code) {
  out <- character()
  add <- function(kind, text) out <<- c(out, paste0(kind, "\t", paste(text, collapse = "\n")))
  exprs <- tryCatch(parse(text = code, keep.source = TRUE), error = function(e) e)
  if (inherits(exprs, "error")) { add("err", conditionMessage(exprs)); return(out) }
  src <- attr(exprs, "srcref")
  for (i in seq_along(exprs)) {
    add("in", as.character(src[[i]]))
    msgs <- character(); warns <- character(); err <- NULL
    printed <- utils::capture.output(err <- tryCatch(withCallingHandlers({
      res <- withVisible(eval(exprs[[i]], globalenv()))
      if (res$visible) print(res$value)
      NULL
    }, message = function(m) { msgs <<- c(msgs, sub("\n$", "", conditionMessage(m))); invokeRestart("muffleMessage") },
       warning = function(w) { warns <<- c(warns, conditionMessage(w)); invokeRestart("muffleWarning") }),
    error = function(e) e))
    if (length(msgs)) add("note", msgs)
    if (length(printed)) add("out", printed)
    if (length(warns)) add("warn", paste("Warning:", warns))
    if (!is.null(err)) { add("err", paste("Error:", conditionMessage(err))); break }
  }
  out
}`;
  var webRReady;
  var mlumrReady;
  function startR(status) {
    webRReady ??= (async () => {
      status("Downloading R for your browser. The first time takes up to a minute.");
      const url = WEBR_URL;
      const mod = await import(
        /* @vite-ignore */
        url
      );
      const webR = new mod.WebR({ channelType: mod.ChannelType.PostMessage, interactive: false });
      await webR.init();
      await webR.evalRVoid(RUNNER);
      return webR;
    })().catch((error) => {
      webRReady = void 0;
      throw error;
    });
    return webRReady;
  }
  function loadMlumr(status) {
    mlumrReady ??= (async () => {
      const webR = await startR(status);
      status("Installing the R packages mlumr needs here (randtoolbox and jsonlite).");
      await webR.installPackages(R_PACKAGES, { quiet: true });
      status("Loading the mlumr R code.");
      const get = async (path2) => {
        const response2 = await fetch(site(`r/${path2}`));
        if (!response2.ok) throw new Error(`r/${path2} returned ${response2.status}`);
        return response2;
      };
      const files = await (await get("files.json")).json();
      try {
        await webR.FS.mkdir("/home/web_user/mlumr");
      } catch {
      }
      const made = /* @__PURE__ */ new Set();
      for (const file of files) {
        const parts = file.split("/");
        for (let i3 = 1; i3 < parts.length; i3++) {
          const dir = `/home/web_user/mlumr/${parts.slice(0, i3).join("/")}`;
          if (!made.has(dir)) {
            try {
              await webR.FS.mkdir(dir);
            } catch {
            }
            made.add(dir);
          }
        }
        await webR.FS.writeFile(`/home/web_user/mlumr/${file}`, new Uint8Array(await (await get(file)).arrayBuffer()));
      }
      await webR.evalRVoid('source("/home/web_user/mlumr/load.R")');
    })().catch((error) => {
      mlumrReady = void 0;
      throw error;
    });
    return mlumrReady;
  }
  async function runR(code2, status, withMlumr = false) {
    if (withMlumr) await loadMlumr(status);
    const webR = await startR(status);
    status("Running.");
    const shelter = await new webR.Shelter();
    try {
      const result = await shelter.evalR(".lesson_run(code)", { env: { code: code2 } });
      const lines = await result.toArray();
      return lines.map((line) => {
        const tab = line.indexOf("	");
        return { kind: line.slice(0, tab), text: line.slice(tab + 1) };
      });
    } finally {
      shelter.purge();
    }
  }
  async function evalString(code2) {
    const webR = await startR(() => void 0);
    return webR.evalRString(code2);
  }
  async function fitStan(model, json, params, progress) {
    const chains = 2, warmup = 500, samples = 500, seed = 2026;
    const url = site(`stan/mlumr_binary_${model}/main.js`);
    const start = performance.now();
    const runs = await Promise.all(Array.from({ length: chains }, (_2, i3) => chain(url, {
      data: json,
      num_chains: 1,
      id: i3 + 1,
      seed,
      num_warmup: warmup,
      num_samples: samples,
      refresh: 100
    }, (message) => progress(i3 + 1, message))));
    const summaries = params.map((name) => {
      const index = runs[0].paramNames.indexOf(name);
      if (index < 0) throw new Error(`The model has no quantity named ${name}.`);
      const perChain = runs.map((run) => run.draws[index]);
      const all = perChain.flat();
      return { name, mean: mean(all), lo: quantile(all, 0.025), hi: quantile(all, 0.975), rhat: splitRhat(perChain), draws: all };
    });
    return { summaries, seconds: (performance.now() - start) / 1e3, chains, warmup, samples };
  }
  function chain(url, config, progress) {
    return new Promise((resolve, reject) => {
      const worker = new Worker(site("stan/worker.js"), { type: "module" });
      const fail = (message) => {
        worker.terminate();
        reject(new Error(message));
      };
      worker.onerror = (event) => fail(event.message || "The Stan worker failed to start.");
      worker.onmessage = (event) => {
        const message = event.data;
        if (message.type === "loaded") worker.postMessage({ type: "sample", config });
        else if (message.type === "progress") progress(message.message);
        else if (message.type === "error") fail(message.message);
        else {
          worker.terminate();
          resolve(message);
        }
      };
      worker.postMessage({ type: "load", url });
    });
  }

  // ../../../Users/choxos/Documents/GitHub/mlumr-lesson/scenes/codecell.ts
  var esc2 = (s2) => s2.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;");
  var PARAMS = ["lor_comparator", "rd_comparator", "lor_index", "rd_index"];
  var MEANING = {
    lor_comparator: "log odds ratio, trial B population",
    rd_comparator: "risk difference, trial B population",
    lor_index: "log odds ratio, trial A population",
    rd_index: "risk difference, trial A population"
  };
  function render3(lines) {
    return lines.map(({ kind, text: text2 }) => {
      if (kind === "in") return `<span class="muted">${esc2(text2.split("\n").map((l2, i3) => (i3 ? "+ " : "> ") + l2).join("\n"))}</span>`;
      if (kind === "out") return esc2(text2);
      if (kind === "note") return `<span class="ok">${esc2(text2)}</span>`;
      return `<span class="err">${esc2(text2)}</span>`;
    }).join("\n");
  }
  function fitView(model, fit, benchmarks) {
    const rows = fit.summaries.map((s2) => `<tr><td>${s2.name}</td><td>${MEANING[s2.name]}</td><td>${s2.mean.toFixed(3)}</td><td>${s2.lo.toFixed(3)}</td><td>${s2.hi.toFixed(3)}</td><td>${s2.rhat.toFixed(3)}</td></tr>`).join("");
    const lor = fit.summaries[0].draws;
    const lo = Math.min(...lor, benchmarks.naive, benchmarks.stc), hi = Math.max(...lor, benchmarks.naive, benchmarks.stc);
    const bins = 36, width = (hi - lo) / bins || 1, counts = new Array(bins).fill(0);
    for (const d2 of lor) counts[Math.min(bins - 1, Math.floor((d2 - lo) / width))]++;
    const density = counts.map((c2) => c2 / (lor.length * width)), top = Math.max(...density);
    const steps = density.flatMap((d2, i3) => [[lo + i3 * width, d2], [lo + (i3 + 1) * width, d2]]);
    const span = hi - lo, ticks = [0, 0.25, 0.5, 0.75, 1].map((f2) => Number((lo + f2 * span).toFixed(2)));
    const chart = lineChart({
      title: `Posterior of the log odds ratio in trial B's population (${model === "spfa" ? "shared slopes" : "separate slopes"})`,
      label: "Histogram of posterior draws with the naive and STC estimates marked",
      x: [lo - 0.02 * span, hi + 0.02 * span],
      y: [0, top * 1.15],
      xTicks: ticks,
      yTicks: [0, Number((top / 2).toPrecision(2)), Number(top.toPrecision(2))],
      xLabel: "log odds ratio, A versus B",
      yLabel: "density",
      xFmt: (v2) => v2.toFixed(2),
      lines: [{ points: steps, key: "a" }],
      vlines: [{ x: benchmarks.stc, text: "STC" }, { x: benchmarks.naive, text: "naive" }],
      legend: [["a", "mlumr posterior draws"]]
    });
    return `${chart}<div class="table-wrap"><table class="fit-table"><thead><tr><th>Quantity</th><th>Meaning</th><th>Mean</th><th>2.5%</th><th>97.5%</th><th>Split R-hat</th></tr></thead><tbody>${rows}</tbody></table></div>
    <p>${fit.chains} chains, ${fit.warmup} warmup and ${fit.samples} kept draws each, seed 2026, ${fit.seconds.toFixed(1)} seconds in your browser. This is a small teaching budget. mlumr's own summary uses 4 chains and rank-normalized R-hat. STC answers the same question as the trial B population row; naive compares the two trials as they are.</p>`;
  }
  function mountCell(slot, cell) {
    slot.innerHTML = `<section class="code-cell" aria-label="Runnable R code">
    <div class="code-head"><div><h2>${cell.mlumr ? "Run mlumr in your browser" : "Try it in R"}</h2><p>${esc2(cell.intro)}</p></div>
      <div class="code-actions"><button type="button" data-act="reset">Reset code</button><button type="button" class="primary" data-act="run">Run in browser</button></div></div>
    <div class="code-body"><textarea spellcheck="false" autocomplete="off" aria-label="R code">${esc2(cell.code)}</textarea><pre class="console" role="status" aria-live="polite"><span class="muted">Output appears here. Press Run, or Ctrl+Enter in the code. R downloads the first time, which can take up to a minute.</span></pre></div>
    <div class="code-foot">${cell.mlumr ? `<div class="code-actions"><span class="seg" role="group" aria-label="Model"><button type="button" data-model="spfa" aria-pressed="true">Shared slopes</button><button type="button" data-model="relaxed" aria-pressed="false">Separate slopes</button></span><button type="button" class="primary" data-act="fit" disabled>Fit with Stan in browser</button></div><div class="fit-out"><p>Run the R code first. Then fit the real mlumr Stan model to <code>dat</code>, right here.</p></div>` : ""}</div>
  </section>`;
    const textarea = slot.querySelector("textarea");
    const output = slot.querySelector(".console");
    const run = slot.querySelector("[data-act=run]");
    const fitButton = slot.querySelector("[data-act=fit]");
    const fitOut = slot.querySelector(".fit-out");
    let model = "spfa";
    const status = (message) => {
      output.innerHTML = `<span class="muted">${esc2(message)}</span>`;
    };
    async function execute() {
      if (run.disabled) return;
      run.disabled = true;
      try {
        const lines = await runR(textarea.value, status, cell.mlumr);
        output.innerHTML = render3(lines);
        if (fitButton) fitButton.disabled = lines.some((l2) => l2.kind === "err");
      } catch (error) {
        output.innerHTML = `<span class="err">${esc2(error instanceof Error ? error.message : String(error))}</span>`;
      } finally {
        run.disabled = false;
      }
    }
    async function fit() {
      if (!fitButton || !fitOut || fitButton.disabled) return;
      fitButton.disabled = true;
      const progress = ["", ""];
      fitOut.innerHTML = "<p>Preparing the Stan data with mlumr.</p>";
      try {
        const json = await evalString(`lesson_stan_json(dat, model = "${model}")`);
        const benchmarks = JSON.parse(await evalString("lesson_benchmarks(dat)"));
        const result = await fitStan(model, json, PARAMS, (chain2, message) => {
          progress[chain2 - 1] = message.trim().split("\n").pop() ?? "";
          fitOut.innerHTML = `<pre class="console">${esc2(progress.map((m2, i3) => `Chain ${i3 + 1}: ${m2 || "starting"}`).join("\n"))}</pre>`;
        });
        fitOut.innerHTML = fitView(model, result, benchmarks);
      } catch (error) {
        fitOut.innerHTML = `<p class="feedback">${esc2(error instanceof Error ? error.message : String(error))}</p>`;
      } finally {
        fitButton.disabled = false;
      }
    }
    slot.addEventListener("click", (event) => {
      const button2 = event.target.closest("button");
      if (!button2) return;
      if (button2.dataset.act === "run") void execute();
      if (button2.dataset.act === "reset") textarea.value = cell.code;
      if (button2.dataset.act === "fit") void fit();
      if (button2.dataset.model) {
        model = button2.dataset.model;
        slot.querySelectorAll("[data-model]").forEach((b2) => b2.setAttribute("aria-pressed", String(b2 === button2)));
      }
    });
    textarea.addEventListener("keydown", (event) => {
      if (event.key === "Enter" && (event.ctrlKey || event.metaKey)) {
        event.preventDefault();
        void execute();
      }
    });
  }

  // ../../../Users/choxos/Documents/GitHub/mlumr-lesson/scenes/scene.ts
  var scalar = (label, range, value) => ({ type: { kind: "scalar", range }, default: value, interpolate: "lerp", ownership: "shared", label });
  var schema = {
    scene: { type: { kind: "enum", values: Object.keys(labs) }, default: "evidence", interpolate: "snap", ownership: "shared" },
    pIndex: scalar("Share of trial A with the marker", [0, 1], 0.2),
    pComparator: scalar("Share of trial B with the marker", [0, 1], 0.8),
    target: scalar("Share of the target population with the marker", [0, 1], 0.5),
    betaB: scalar("Trial B marker slope on the log odds scale", [-1, 4], 2.4),
    shift: scalar("Hidden difference in trial B on the log odds scale", [-1.5, 1.5], 0),
    width: scalar("Half-width of the covariate spread", [0, 2], 1.5),
    points: scalar("Points used to average", [2, 256], 16),
    rho: scalar("Correlation between two markers", [-1, 1], 0),
    design: { type: { kind: "enum", values: ["one", "duplicate", "separated"] }, default: "one", interpolate: "snap", ownership: "shared" },
    separation: scalar("Distance of each subgroup row from zero", [0, 1], 0.8),
    targetX: scalar("Target covariate value", [-1.5, 1.5], 1),
    priorSD: scalar("Prior standard deviation", [0.2, 4], 2),
    step: scalar("Analysis step", [0, 5], 0),
    family: scalar("Outcome type", [0, 3], 0),
    time: scalar("Months of follow-up and RMST horizon", [0, 36], 12),
    heterogeneity: scalar("Marker effect on the log hazard", [0, 2.5], 1.8),
    diagnostic: scalar("Diagnostic problem", [0, 6], 0),
    question: scalar("Knowledge check", [0, 4], 0)
  };
  var fmt = (x2, digits = 3) => x2.toFixed(digits);
  var pct = (x2) => `${(100 * x2).toFixed(1)}%`;
  var pct0 = (x2) => `${Math.round(100 * x2)}%`;
  var esc3 = (s2) => s2.replaceAll("&", "&amp;").replaceAll("<", "&lt;").replaceAll(">", "&gt;").replaceAll('"', "&quot;");
  var metric = (label, value) => `<div class="metric"><span>${label}</span><strong>${value}</strong></div>`;
  var block = (charts, metrics, note, extra = "") => `<div class="charts">${charts}</div>${metrics ? `<div class="metrics">${metrics}</div>` : ""}${note ? `<p class="interpretation">${note}</p>` : ""}${extra ? `<div class="extra">${extra}</div>` : ""}`;
  var code = (s2) => `<pre><code>${esc3(s2)}</code></pre>`;
  var slider = (key, label, min, max, step) => `<label class="control" for="ml-${key}"><span>${label}<output data-value="${key}"></output></span><input id="ml-${key}" data-param="${key}" type="range" min="${min}" max="${max}" step="${step}"></label>`;
  var select = (key, label, options) => `<label class="control" for="ml-${key}"><span>${label}</span><select id="ml-${key}" data-param="${key}">${options.map(([v2, l2]) => `<option value="${v2}">${l2}</option>`).join("")}</select></label>`;
  var indexed = (key, label, names) => select(key, label, names.map((n, i3) => [String(i3), n]));
  var targetControl = () => slider("target", "Target population: share with the marker", 0, 1, 0.01);
  var designControls = () => select("design", "What trial B reports", [["one", "One subgroup row at x = 0"], ["duplicate", "Two separate rows, both at x = 0"], ["separated", "Two rows at different x"]]) + slider("separation", "Distance of each row from x = 0", 0, 1, 0.01) + slider("targetX", "Target covariate value", -1.5, 1.5, 0.05) + slider("priorSD", "Prior standard deviation", 0.2, 4, 0.1);
  function controls(lab) {
    if (lab === "evidence") return slider("pIndex", "Trial A: share with the marker", 0, 1, 0.01) + slider("pComparator", "Trial B: share with the marker", 0, 1, 0.01) + targetControl();
    if (lab === "assumptions") return slider("shift", "Hidden difference in trial B (log odds)", -1.5, 1.5, 0.05) + targetControl();
    if (lab === "response") return slider("betaB", "Trial B marker slope (A stays at 2.4)", -1, 4, 0.1);
    if (lab === "integration") return slider("width", "How spread out patients are", 0, 2, 0.05) + slider("points", "Points used to average", 2, 256, 1);
    if (lab === "dependence") return slider("rho", "How often the two markers go together", -1, 1, 0.05);
    if (lab === "target") return targetControl() + slider("betaB", "Trial B marker slope (shared value 2.4)", -1, 4, 0.1);
    if (lab === "identification" || lab === "priors") return designControls();
    if (lab === "workflow") return '<div class="control"><span>Analysis step</span><div class="stepper" role="group" aria-label="Analysis step"><button type="button" data-step-move="-1" aria-label="Previous step">\u2039</button><span class="step-count" aria-live="polite"></span><button type="button" data-step-move="1" aria-label="Next step">\u203A</button></div><span class="step-hint">Or click a step in the chart.</span></div>';
    if (lab === "families") return indexed("family", "Outcome type", families.map((s2) => s2.name));
    if (lab === "survival") return slider("time", "Months of follow-up (and RMST horizon)", 0, 36, 0.5) + targetControl() + slider("heterogeneity", "How much the marker raises the hazard", 0, 2.5, 0.1);
    if (lab === "diagnostics") return indexed("diagnostic", "Problem to inspect", diagnosticCases.map((s2) => s2.name));
    return indexed("question", "Knowledge check", questions.map((_2, i3) => `Question ${i3 + 1} of ${questions.length}`));
  }
  function evidence(s2) {
    const pA = Number(s2.pIndex), pB = Number(s2.pComparator), t2 = binary(Number(s2.target));
    const a2 = binary(pA).a, c2 = binary(pB).b;
    return block(
      populations("Who was studied (each dot is a patient)", [{ name: "Trial A: individual patient data", share: pA, key: "a" }, { name: "Trial B: published summaries only", share: pB, key: "b" }]) + barChart("Chance of the adverse event", "Risk for each treatment in its own trial and in the target population", [
        { name: "A, in trial A", value: a2, key: "a", text: pct(a2) },
        { name: "B, in trial B", value: c2, key: "b", text: pct(c2) },
        { name: "A, in the target", value: t2.a, key: "a", text: pct(t2.a) },
        { name: "B, in the target", value: t2.b, key: "b", text: pct(t2.b) }
      ]),
      metric("Crude difference", fmt(a2 - c2)) + metric("Target difference", fmt(t2.rd)),
      "Both differences are A minus B. The event is harmful, so a negative difference favors A. The crude difference mixes up the treatments with who was studied. The target difference compares both treatments in the same population."
    );
  }
  function assumptions(s2) {
    const shift = Number(s2.shift), q = Number(s2.target), base = binary(q), shifted = binary(q, 2.4, shift);
    return block(
      lineChart({
        title: "Treatment B in the target, as trial B would report it",
        label: "Risk for treatment B as the hidden difference in trial B grows, compared with the risk without it and with treatment A",
        x: [-1.5, 1.5],
        y: [0, 1],
        xTicks: [-1.5, -0.75, 0, 0.75, 1.5],
        yTicks: [0, 0.25, 0.5, 0.75, 1],
        yFmt: pct0,
        xLabel: "Hidden difference in trial B (log odds)",
        yLabel: "Chance of the event",
        lines: [{ points: [[-1.5, base.a], [1.5, base.a]], key: "a" }, { points: [[-1.5, base.b], [1.5, base.b]], key: "b", dash: true }, { points: grid(-1.5, 1.5, 61).map((x2) => [x2, binary(q, 2.4, x2).b]), key: "b" }],
        dots: [{ at: [shift, shifted.b], key: "b", r: 6 }],
        vlines: [{ x: 0, text: "no hidden difference" }],
        legend: [["a", "A"], ["b", "B as reported"], ["b", "B without the hidden difference", true]]
      }),
      metric("True difference", fmt(base.rd)) + metric("Difference we would report", fmt(shifted.rd)) + metric("Error from the hidden difference", fmt(shifted.rd - base.rd)),
      "The hidden difference belongs to trial B, not to treatment B. Trial B has one intercept, and it soaks up both. No covariate adjustment can pull them apart.",
      `<h3>What an unanchored comparison has to assume</h3><ul class="read-list"><li>The treatments, outcome definitions and follow-up are comparable.</li><li>Every factor that affects the outcome, or changes the treatment effect, was measured and modeled.</li><li>After adjusting for those factors, patients in the two trials are comparable, and their covariates overlap.</li><li>The outcome model and the covariate distributions are close enough to the truth.</li></ul>`
    );
  }
  function response(s2) {
    const beta = Number(s2.betaB), shared = Math.abs(beta - 2.4) < 1e-3, gap1 = 1.7 - beta;
    return block(
      lineChart({
        title: "Log odds of the event, by marker",
        label: "Two straight lines of log odds for treatments A and B, for patients without and with the marker",
        x: [0, 1],
        y: [-3, 3],
        xTicks: [0, 1],
        yTicks: [-3, -2, -1, 0, 1, 2, 3],
        xFmt: (v2) => v2 ? "marker present" : "marker absent",
        xLabel: "Patient marker",
        yLabel: "Log odds of the event",
        lines: [{ points: [[0, -1.8], [1, 0.6]], key: "a" }, { points: [[0, -1.1], [1, -1.1 + beta]], key: "b" }],
        dots: [{ at: [0, -1.8], key: "a" }, { at: [1, 0.6], key: "a" }, { at: [0, -1.1], key: "b" }, { at: [1, -1.1 + beta], key: "b" }],
        notes: [{ at: [0.03, -1.45], text: `gap ${fmt(-0.7, 2)}` }, { at: [0.97, (0.6 - 1.1 + beta) / 2], text: `gap ${fmt(gap1, 2)}`, anchor: "end" }],
        legend: [["a", "A: slope 2.4"], ["b", `B: slope ${fmt(beta, 1)}`]]
      }),
      metric("Odds ratio, marker absent", fmt(Math.exp(-0.7))) + metric("Odds ratio, marker present", fmt(Math.exp(gap1))),
      shared ? "Shared slopes: the two lines are parallel, so the gap between treatments is the same for every patient on the log odds scale." : "Separate slopes: the lines are not parallel, so the gap depends on the marker. The marker now changes the treatment effect.",
      `<p class="formula">log odds for A = \u22121.8 + 2.4 \xD7 marker<br>log odds for B = \u22121.1 + slope \xD7 marker</p>`
    );
  }
  function integration(s2) {
    const w2 = Number(s2.width), q = quadrature(w2, Number(s2.points)), mid = logistic(-1.8);
    return block(
      lineChart({
        title: "Risk for each patient, and two kinds of average",
        label: "An S-shaped risk curve with the averaging points, the average risk, and the risk of the average patient",
        x: [-2, 2],
        y: [0, 1],
        xTicks: [-2, -1, 0, 1, 2],
        yTicks: [0, 0.25, 0.5, 0.75, 1],
        yFmt: pct0,
        xLabel: "Patient covariate x",
        yLabel: "Chance of the event",
        bands: w2 > 0 ? [{ upper: [[-w2, 1], [w2, 1]], lower: [[-w2, 0], [w2, 0]], key: "a" }] : [],
        lines: [{ points: grid(-2, 2, 81).map((x2) => [x2, logistic(-1.8 + 2.4 * x2)]), key: "a" }, { points: [[-2, q.value], [2, q.value]], key: "b", dash: true }],
        dots: [...q.points.map((x2) => ({ at: [x2, logistic(-1.8 + 2.4 * x2)], key: "a", r: q.points.length > 64 ? 2.5 : 4 })), { at: [0, mid], key: "ink", r: 6, ring: true }],
        notes: [{ at: [2, q.value], text: `average risk ${pct(q.value)}`, anchor: "end", dy: -8 }, { at: [0.08, mid], text: `average patient ${pct(mid)}`, dy: 22 }],
        legend: [["a", "risk for one patient"], ["b", "average risk", true]]
      }),
      metric("Risk of the average patient", pct(mid)) + metric("Average risk", pct(q.value)) + metric("Points used", String(Math.round(Number(s2.points)))),
      "The curve bends, so the average of the risks is not the risk at the average. Aggregate results are averages of patients' outcomes, so the model has to average predictions too.",
      `<p class="formula">average risk = mean over patients of g\u207B\xB9(\u03B1 + \u03B2x)<br>risk of the average patient = g\u207B\xB9(\u03B1 + \u03B2 \xD7 mean of x)</p><p>This chart uses an even grid you can see. mlumr uses Sobol quasi-random points drawn from the distributions you choose. In mlumr, predict(type = "link") returns the link of the average risk, not the average of the linear predictor.</p>`
    );
  }
  function dependenceView(s2) {
    const rho = Number(s2.rho), d2 = dependence(rho), names = ["Neither marker", "Marker 2 only", "Marker 1 only", "Both markers"];
    return block(
      card("The four kinds of patient: share of the population", `<div class="joint-grid">${names.map((n, i3) => `<div style="--w:${Math.round(4 + 62 * d2.weights[i3])}%"><span>${n}, risk ${pct(d2.risks[i3])}</span><strong>${pct(d2.weights[i3])}</strong></div>`).join("")}</div>`) + lineChart({
        title: "Average risk as the markers go together more often",
        label: "Average risk rises in a straight line as the correlation between the two markers increases",
        x: [-1, 1],
        y: [0.2, 0.35],
        xTicks: [-1, -0.5, 0, 0.5, 1],
        yTicks: [0.2, 0.25, 0.3, 0.35],
        yFmt: pct0,
        xLabel: "Correlation between the two markers",
        yLabel: "Average risk",
        lines: [{ points: grid(-1, 1, 41).map((r2) => [r2, dependence(r2).risk]), key: "a" }],
        dots: [{ at: [rho, d2.risk], key: "a", r: 6 }]
      }),
      metric("Marker 1", "50%") + metric("Marker 2", "50%") + metric("Average risk", pct(d2.risk)),
      "Each marker stays at 50% whatever the correlation. What changes is how often they occur together, and because the risk curve bends, two markers together add more risk than each adds alone.",
      "<p>mlumr joins covariate distributions with a Gaussian copula. By default the correlation is estimated from trial A's patient data. Carrying that correlation over to trial B is an assumption, so try other plausible values with the cor argument of add_integration().</p>"
    );
  }
  function target(s2) {
    const q = Number(s2.target), beta = Number(s2.betaB), b2 = binary(q, beta), xs = grid(0, 1, 51), shared = Math.abs(beta - 2.4) < 1e-3;
    const lors = [...xs.map((p2) => binary(p2, beta).lor), -0.7, 1.7 - beta], mid = (Math.min(...lors) + Math.max(...lors)) / 2;
    const half = Math.max(0.3, (Math.max(...lors) - Math.min(...lors)) / 2 + 0.1), lo = mid - half, hi = mid + half;
    return block(
      lineChart({
        title: "Chance of the event in different target populations",
        label: "Risk for A and B as the share of the target population with the marker changes",
        x: [0, 1],
        y: [0, 1],
        xTicks: [0, 0.25, 0.5, 0.75, 1],
        xFmt: pct0,
        yTicks: [0, 0.25, 0.5, 0.75, 1],
        yFmt: pct0,
        xLabel: "Share of the target population with the marker",
        yLabel: "Chance of the event",
        lines: [{ points: xs.map((p2) => [p2, binary(p2, beta).a]), key: "a" }, { points: xs.map((p2) => [p2, binary(p2, beta).b]), key: "b" }],
        vlines: [{ x: q }],
        dots: [{ at: [q, b2.a], key: "a" }, { at: [q, b2.b], key: "b" }],
        legend: [["a", "A"], ["b", "B"]]
      }) + lineChart({
        title: "Log odds ratio, A versus B",
        label: "Population log odds ratio across target populations compared with the log odds ratio for one patient",
        x: [0, 1],
        y: [lo, hi],
        xTicks: [0, 0.25, 0.5, 0.75, 1],
        xFmt: pct0,
        yTicks: grid(lo, hi, 5),
        yFmt: (v2) => v2.toFixed(2),
        xLabel: "Share of the target population with the marker",
        yLabel: "Log odds ratio",
        hlines: [{ y: -0.7, key: "muted", dash: true }, { y: 1.7 - beta, key: "muted", dash: true }],
        lines: [{ points: xs.map((p2) => [p2, binary(p2, beta).lor]), key: "ink" }],
        dots: [{ at: [q, b2.lor], key: "ink", r: 6 }],
        legend: [["ink", "population"], ["muted", "one patient", true]]
      }),
      metric("Risk difference", fmt(b2.rd)) + metric("Population odds ratio", fmt(Math.exp(b2.lor))) + metric(shared ? "Odds ratio for one patient" : "Patient odds ratio, absent / present", shared ? fmt(Math.exp(-0.7)) : `${fmt(Math.exp(-0.7))} / ${fmt(Math.exp(1.7 - beta))}`),
      shared ? "Shared slopes: every patient has the same odds ratio, yet the population odds ratio still moves with the mix. Odds ratios do not average simply, which is called non-collapsibility." : "Separate slopes: the marker changes the treatment effect, and the mix of the population changes the population effect as well.",
      "<p>mlumr predicts, averages and compares inside every posterior draw, then summarizes. marginal_effects() does this for trial A's population, trial B's population, or a target population you pass as newdata.</p>"
    );
  }
  function evidenceBand(title, s2) {
    const design = String(s2.design), sep = Number(s2.separation), tx = Number(s2.targetX), sd = Number(s2.priorSD);
    const xs = grid(-1.5, 1.5, 61), fits = xs.map((x2) => identification(design, sep, x2, sd)), d2 = identification(design, sep, tx, sd);
    return {
      d: d2,
      chart: lineChart({
        title,
        label: "Estimated mean outcome across covariate values with a 95% interval band, the subgroup rows, and the target",
        x: [-1.5, 1.5],
        y: [-2, 3],
        xTicks: [-1.5, -1, -0.5, 0, 0.5, 1, 1.5],
        yTicks: [-2, -1, 0, 1, 2, 3],
        xLabel: "Covariate value x",
        yLabel: "Mean outcome",
        bands: [{ upper: xs.map((x2, i3) => [x2, fits[i3].estimate + 1.96 * fits[i3].sd]), lower: xs.map((x2, i3) => [x2, fits[i3].estimate - 1.96 * fits[i3].sd]), key: "b" }],
        lines: [{ points: xs.map((x2, i3) => [x2, fits[i3].estimate]), key: "b" }, { points: [[-1.5, -0.8], [1.5, 1.6]], key: "muted", dash: true }],
        dots: [...d2.xs.map((x2, i3) => ({ at: [x2, d2.ys[i3]], key: "b", r: 7 })), { at: [tx, d2.estimate], key: "ink", r: 6, ring: true }],
        vlines: [{ x: tx, text: "target" }],
        legend: [["b", "estimate and 95% interval"], ["muted", "truth behind the made-up data", true]]
      }),
      metrics: metric("Independent directions", `${d2.rank} of 2`) + metric("Target pinned down by data?", d2.targetIdentified ? "Yes" : "No") + metric("Target estimate", `${fmt(d2.estimate, 2)} \xB1 ${fmt(1.96 * d2.sd, 2)}`)
    };
  }
  function identificationView(s2) {
    const v2 = evidenceBand("What trial B's subgroup rows pin down", s2);
    return block(
      v2.chart,
      v2.metrics,
      v2.d.rank === 1 ? "One direction of information: the data fix the mean at x = 0 but not the slope, so the band is narrow only there. Two rows at the same x add precision at that point, not a new direction." : "Two different x values give two directions, so the intercept and the slope can both be estimated. Move the rows closer together: the band widens away from them, and the prior matters more.",
      '<p class="formula">row mean ~ Normal(\u03B1 + \u03B2 \xD7 x, 0.15\xB2)<br>\u03B1, \u03B2 ~ Normal(0, prior SD\xB2)</p><p>This is an exact calculation for a normal outcome, not an mlumr fit. With K covariates, trial B needs at least K + 1 summaries. For binary and count outcomes, or a continuous outcome with a log link, each row passes through a curved link, so check_identification() only describes the rows and reports flagged = NA when there are enough of them. It refuses reconstructed survival data.</p>'
    );
  }
  function priorsView(s2) {
    const v2 = evidenceBand("How the prior changes the answer", s2);
    const design = String(s2.design), sep = Number(s2.separation), tx = Number(s2.targetX), sd = Number(s2.priorSD);
    const row = (name, prior, key) => {
      const d2 = identification(design, sep, tx, prior);
      return { name, mean: d2.estimate, lo: d2.estimate - 1.96 * d2.sd, hi: d2.estimate + 1.96 * d2.sd, key };
    };
    return block(
      v2.chart + intervalChart(
        "Target estimate under different priors",
        "Point estimates and 95% intervals for the target under several prior standard deviations",
        [row("prior SD 0.3", 0.3, "b"), row("prior SD 1", 1, "b"), row("prior SD 3", 3, "b"), row(`your prior SD ${fmt(sd, 1)}`, sd, "a")],
        [-6, 7],
        [-6, -3, 0, 3, 6],
        [{ x: 0.4 + 0.8 * tx, text: "truth" }],
        (x2) => x2.toFixed(0)
      ),
      v2.metrics,
      "A tighter prior narrows the interval even though no new data arrived. Near the observed rows the data do the work; far from them, the prior does.",
      "<p>In mlumr, prior_summary() lists the priors, plot_prior_posterior() draws each posterior over its prior, and prior_sensitivity() refits the model over several prior scales. prior_normal(autoscale = TRUE) divides a slope prior's scale by each covariate's standard deviation.</p>"
    );
  }
  function familyChart(i3) {
    if (i3 === 0) return lineChart({ title: "Binary: the logit link turns a straight line into a chance", label: "S-shaped logistic curve", x: [-5, 5], y: [0, 1], xTicks: [-4, -2, 0, 2, 4], yTicks: [0, 0.5, 1], yFmt: pct0, xLabel: "Linear predictor \u03B1 + \u03B2x", yLabel: "Chance of the event", hlines: [{ y: 0.5, key: "muted", dash: true }], lines: [{ points: grid(-5, 5, 81).map((x2) => [x2, logistic(x2)]), key: "a" }] });
    if (i3 === 1) return lineChart({ title: "Continuous: the mean moves in a straight line", label: "Straight line for the mean outcome with a band where most outcomes fall", x: [-2, 2], y: [-1, 3], xTicks: [-2, -1, 0, 1, 2], yTicks: [-1, 0, 1, 2, 3], xLabel: "Covariate x", yLabel: "Outcome", bands: [{ upper: [[-2, -0.1], [2, 3.1]], lower: [[-2, -1.1], [2, 2.1]], key: "a" }], lines: [{ points: [[-2, -0.6], [2, 2.6]], key: "a" }], legend: [["a", "mean outcome"]] });
    if (i3 === 2) return lineChart({ title: "Counts: the log link keeps the event rate positive", label: "Exponential curve of events per person-year", x: [-2, 2], y: [0, 4], xTicks: [-2, -1, 0, 1, 2], yTicks: [0, 1, 2, 3, 4], xLabel: "Covariate x", yLabel: "Events per person-year", lines: [{ points: grid(-2, 2, 41).map((x2) => [x2, Math.exp(-0.2 + 0.7 * x2)]), key: "a" }] });
    return lineChart({ title: "Survival: the share of patients still event-free", label: "Two falling survival curves, A above B", x: [0, 36], y: [0, 1], xTicks: [0, 12, 24, 36], yTicks: [0, 0.5, 1], yFmt: pct0, xLabel: "Months", yLabel: "Still event-free", lines: [{ points: grid(0, 36, 37).map((t2) => [t2, Math.exp(-0.06 * 0.65 * t2)]), key: "a" }, { points: grid(0, 36, 37).map((t2) => [t2, Math.exp(-0.06 * t2)]), key: "b" }], legend: [["a", "A"], ["b", "B"]] });
  }
  function survivalView(s2) {
    const t2 = Number(s2.time), q = Number(s2.target), het = Number(s2.heterogeneity), r2 = survival(t2, q, het);
    const ts = grid(0, 36, 73), vals = ts.map((x2) => survival(x2, q, het)), area = t2 > 0 ? grid(0, t2, 40).map((x2) => survival(x2, q, het)) : [];
    return block(
      lineChart({
        title: "Survival in the target population",
        label: "Survival curves for A and B with the area between them shaded up to the chosen time",
        x: [0, 36],
        y: [0, 1],
        xTicks: [0, 6, 12, 18, 24, 30, 36],
        yTicks: [0, 0.25, 0.5, 0.75, 1],
        yFmt: pct0,
        xLabel: "Months",
        yLabel: "Still event-free",
        bands: area.length ? [{ upper: area.map((v2, i3) => [t2 * i3 / 39, v2.a.s]), lower: area.map((v2, i3) => [t2 * i3 / 39, v2.b.s]), key: "accent" }] : [],
        lines: [{ points: ts.map((x2, i3) => [x2, vals[i3].a.s]), key: "a" }, { points: ts.map((x2, i3) => [x2, vals[i3].b.s]), key: "b" }],
        vlines: [{ x: t2, text: `${fmt(t2, 1)} months` }],
        legend: [["a", "A"], ["b", "B"], ["accent", "RMST difference (shaded area)"]]
      }) + lineChart({
        title: "Hazard ratio, A versus B, over time",
        label: "Population hazard ratio changing over time, compared with the constant hazard ratio for each patient",
        x: [0, 36],
        y: [0.5, 1],
        xTicks: [0, 6, 12, 18, 24, 30, 36],
        yTicks: [0.5, 0.6, 0.7, 0.8, 0.9, 1],
        yFmt: (v2) => v2.toFixed(1),
        xLabel: "Months",
        yLabel: "Hazard ratio",
        hlines: [{ y: 0.65, key: "muted", dash: true }],
        lines: [{ points: ts.map((x2, i3) => [x2, vals[i3].hr]), key: "ink" }],
        dots: [{ at: [t2, r2.hr], key: "ink", r: 6 }],
        legend: [["ink", "population"], ["muted", "each patient", true]]
      }),
      metric("Each patient's hazard ratio", "0.650") + metric(`Population hazard ratio, month ${fmt(t2, 1)}`, fmt(r2.hr)) + metric(`RMST difference to month ${fmt(t2, 1)}`, fmt(r2.rmstd, 2)),
      "High-risk patients have their events sooner, so the people still at risk drift toward low risk, and faster under B. The population hazard ratio therefore changes over time even though every patient's hazard ratio stays at 0.65.",
      "<p>In mlumr, a population hazard ratio needs a time (at_time in marginal_effects()), and an RMST needs a horizon (rmst_horizon in mlumr()). predict() gives survival, hazard, cumhaz, rmst, median and loghr.</p>"
    );
  }
  function diagnosticChart(i3) {
    const rng = random(2026 + i3), its = grid(1, 200, 200);
    const trace = (shift) => its.map((x2) => [x2, shift + 0.55 * rng.normal()]);
    if (i3 === 0) return lineChart({ title: "Two chains for one parameter, divergences marked (illustration)", label: "Trace plot of two chains with divergent iterations marked below", x: [1, 200], y: [-3, 2], xTicks: [1, 50, 100, 150, 200], yTicks: [-3, -2, -1, 0, 1, 2], xLabel: "Iteration after warmup", yLabel: "Parameter value", lines: [{ points: trace(0), key: "a" }, { points: trace(0), key: "b" }], dots: [23, 71, 72, 140, 166].map((x2) => ({ at: [x2, -2.7], key: "warn", r: 4 })), legend: [["a", "chain 1"], ["b", "chain 2"], ["warn", "divergence"]] });
    if (i3 === 1) return lineChart({ title: "Two chains that never meet (illustration)", label: "Trace plot where the two chains stay at different levels", x: [1, 200], y: [-2, 4], xTicks: [1, 50, 100, 150, 200], yTicks: [-2, 0, 2, 4], xLabel: "Iteration after warmup", yLabel: "Parameter value", lines: [{ points: trace(0), key: "a" }, { points: trace(2.2), key: "b" }], legend: [["a", "chain 1"], ["b", "chain 2"]] });
    if (i3 === 2) {
      const dens = (m2) => grid(-3, 3, 91).map((x2) => [x2, Math.exp(-(((x2 - m2) / 0.65) ** 2) / 2) / (0.65 * Math.sqrt(2 * Math.PI))]);
      return lineChart({ title: "Three subgroup rows: means and spreads (illustration)", label: "Three bell curves for the covariate distribution in each subgroup row", x: [-3, 3], y: [0, 0.7], xTicks: [-3, -2, -1, 0, 1, 2, 3], yTicks: [0, 0.35, 0.7], xLabel: "Covariate x", yLabel: "Density", lines: [-0.7, 0.3, 1.3].map((m2) => ({ points: dens(m2), key: "b" })), dots: [-0.7, 0.3, 1.3].map((m2) => ({ at: [m2, 0.614], key: "b", r: 5 })), notes: [{ at: [-2.9, 0.66], text: "dots mark each row's mean", soft: true }] });
    }
    if (i3 === 3) return lineChart({ title: "Estimated effect as integration points grow (illustration)", label: "Estimated effect settles as the number of integration points doubles", x: [4, 10], y: [-0.5, -0.1], xTicks: [4, 5, 6, 7, 8, 9, 10], xFmt: (v2) => String(2 ** v2), yTicks: [-0.5, -0.4, -0.3, -0.2, -0.1], yFmt: (v2) => v2.toFixed(1), xLabel: "Integration points (n_int)", yLabel: "Estimated log odds ratio", lines: [{ points: grid(4, 10, 25).map((v2) => [v2, -0.42 + 1.2 / Math.sqrt(2 ** v2)]), key: "a" }], dots: [4, 5, 6, 7, 8, 9, 10].map((v2) => ({ at: [v2, -0.42 + 1.2 / Math.sqrt(2 ** v2)], key: "a" })) });
    if (i3 === 4) return intervalChart("Effect under two comparator slope priors (illustration)", "Intervals in trial B population stay stable while intervals in trial A population move and widen", [
      { name: "B population, SD 0.5", mean: -0.12, lo: -0.2, hi: -0.04, key: "b" },
      { name: "B population, SD 2.5", mean: -0.12, lo: -0.21, hi: -0.03, key: "b" },
      { name: "A population, SD 0.5", mean: -0.1, lo: -0.19, hi: -0.01, key: "a" },
      { name: "A population, SD 2.5", mean: -0.02, lo: -0.24, hi: 0.2, key: "a" }
    ], [-0.3, 0.3], [-0.3, -0.15, 0, 0.15, 0.3], [{ x: 0, text: "no difference" }]);
    if (i3 === 5) return barChart("Posterior draws requested and used (illustration)", "Bars showing 1000 draws requested, 700 used and 300 dropped", [
      { name: "draws requested", value: 1e3, key: "muted", text: "1,000" },
      { name: "draws used", value: 700, key: "a", text: "700" },
      { name: "draws dropped", value: 300, key: "warn", text: "300" }
    ], 1e3);
    return intervalChart("Predictive scores with \xB12 standard errors (illustration)", "Two overlapping intervals for expected log predictive density", [
      { name: "shared slopes", mean: -512, lo: -530, hi: -494, key: "a" },
      { name: "separate slopes", mean: -508, lo: -528, hi: -488, key: "b" }
    ], [-540, -480], [-540, -520, -500, -480], [], (v2) => String(v2));
  }
  function view(lab, s2) {
    if (lab === "evidence") return evidence(s2);
    if (lab === "assumptions") return assumptions(s2);
    if (lab === "response") return response(s2);
    if (lab === "integration") return integration(s2);
    if (lab === "dependence") return dependenceView(s2);
    if (lab === "target") return target(s2);
    if (lab === "identification") return identificationView(s2);
    if (lab === "priors") return priorsView(s2);
    if (lab === "survival") return survivalView(s2);
    if (lab === "workflow") {
      const i3 = Math.round(Number(s2.step)), st = workflowSteps[i3];
      return block(
        flowChart("The six steps of an mlumr analysis", workflowSteps, i3),
        "",
        st.text,
        `<h3>${st.title}</h3>${code(st.code)}<p>The complete companion script: <a href="workflow.R" download>download workflow.R</a>. Run it in R with mlumr installed, and add --fit for the real Stan fits.</p>`
      );
    }
    if (lab === "families") {
      const i3 = Math.round(Number(s2.family)), f2 = families[i3];
      return block(
        familyChart(i3),
        "",
        f2.scale,
        `<h3>${f2.name} outcomes</h3><p class="formula">${esc3(f2.equation)}</p>${code(f2.input)}<p>${esc3(f2.effects)}</p><p>${esc3(f2.boundary)}</p>${i3 === 3 ? `<details><summary>Survival distributions and shapes</summary><p>${esc3(survivalChoices)}</p></details>` : ""}`
      );
    }
    if (lab === "diagnostics") {
      const c2 = diagnosticCases[Math.round(Number(s2.diagnostic))];
      return block(
        diagnosticChart(Math.round(Number(s2.diagnostic))),
        "",
        "",
        `<div class="case"><span class="eyebrow">A fit arrives on your desk</span><h3>${esc3(c2.symptom)}</h3><details><summary>Reveal interpretation and next action</summary><p class="interpretation">${esc3(c2.answer)}</p>${code(c2.tool)}</details></div><p>Sampling, numerical integration, identification, model fit and comparable trials are separate questions. Passing one check says nothing about the others.</p>`
      );
    }
    const qi = Math.round(Number(s2.question)), q = questions[qi];
    return block(
      intervalChart("What a report shows: estimate, interval and target", "Risk difference in a made-up target population from the companion script fits, with the true value marked", [
        { name: "Shared slopes (SPFA)", mean: -0.09132, lo: -0.1666, hi: -0.01359, key: "a" },
        { name: "Separate slopes (relaxed)", mean: -0.09105, lo: -0.17276, hi: -0.01359, key: "b" }
      ], [-0.2, 0.05], [-0.2, -0.15, -0.1, -0.05, 0, 0.05], [{ x: -0.12408, text: "true value" }, { x: 0, text: "no difference" }]),
      "",
      "These are the companion script's real mlumr fits to made-up data, for a target population the script defines. Both intervals contain the true value. The separate slopes model is a little less certain, because trial B's slope has to be learned from three summaries.",
      `<div class="case"><span class="eyebrow">Question ${qi + 1} of ${questions.length}</span><h3>${esc3(q.q)}</h3><div class="answers">${q.options.map((a2, i3) => `<button type="button" data-answer="${i3}">${esc3(a2)}</button>`).join("")}</div><p class="feedback" role="status" aria-live="polite"></p></div><details><summary>Checklist for your report</summary><ul class="read-list">${checklist.map((c2) => `<li>${esc3(c2)}</li>`).join("")}</ul></details><p><a href="sources.html" target="_blank" rel="noopener">Sources and scope</a> \xB7 <a href="workflow.R" download>Companion R script</a> \xB7 <a href="https://choxos.github.io/mlumr/" target="_blank" rel="noopener">mlumr documentation</a></p>`
    );
  }
  var moon = '<svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" aria-hidden="true"><path d="M20 14.5A8 8 0 1 1 9.5 4a6.5 6.5 0 0 0 10.5 10.5z"/></svg>';
  var scene = {
    schema,
    create(ctx) {
      const root = document.createElement("section");
      root.className = "ml-lesson";
      const style = document.createElement("style");
      style.textContent = css;
      root.innerHTML = `<header class="ml-header"><a class="brand" href="https://choxos.github.io/mlumr/" target="_blank" rel="noopener">mlumr<span class="brand-tag">Lesson</span></a><div class="header-tools"><a href="sources.html" target="_blank" rel="noopener">Sources</a><button type="button" class="theme-toggle" aria-label="Dark theme" aria-pressed="false">${moon}<span class="theme-label">Light</span><span class="theme-track" aria-hidden="true"><span class="theme-thumb"></span></span></button></div></header><div class="lab-scroll"><div class="lab-title"><div><span class="eyebrow"></span><h1 tabindex="-1"></h1><p class="question"></p></div><button type="button" class="reset" data-reset aria-label="Reset lab">Reset</button></div><div class="lab-body"><div class="visual"></div><aside class="lab-controls" aria-label="Experiment controls"></aside></div><div class="code-slot"></div><footer>Teaching models with made-up numbers. The code cells run real R, and the cell in the Run mlumr chapter runs real mlumr code and its Stan model, all in your browser. mlumr development version 0.1.0.9000, working toward 0.2.0.</footer></div>`;
      ctx.overlay.append(style, root);
      const disposeTheme = themeToggle(root.querySelector(".theme-toggle"));
      const defaults = Object.fromEntries(Object.entries(schema).map(([key, spec]) => [key, spec.default]));
      let narratedState = defaults;
      let exploration = null;
      const navigation = chapterNavigation(root, ctx.overlay, (lab) => {
        exploration = lab === null || lab === narratedState.scene ? null : { ...defaults, scene: lab };
        draw();
      });
      const visual = root.querySelector(".visual");
      const control = root.querySelector(".lab-controls");
      const codeSlot = root.querySelector(".code-slot");
      let current;
      let last = "";
      let latest = defaults;
      const writeParameter = (param, value) => {
        if (exploration) {
          exploration[param] = value;
          draw();
        } else ctx.write(param, value);
      };
      const onInput = (event) => {
        const input = event.target;
        const param = input.dataset.param;
        if (!param) return;
        writeParameter(param, param === "design" ? input.value : Number(input.value));
      };
      const onClick = (event) => {
        const node = event.target.closest("[data-step]");
        if (node) {
          writeParameter("step", Number(node.dataset.step));
          return;
        }
        const button2 = event.target.closest("button");
        if (!button2) return;
        if (button2.dataset.stepMove) {
          const i3 = Math.round(Number(latest.step)) + Number(button2.dataset.stepMove);
          writeParameter("step", Math.min(workflowSteps.length - 1, Math.max(0, i3)));
        }
        if (button2.hasAttribute("data-reset")) {
          control.querySelectorAll("[data-param]").forEach((input) => {
            const key = input.dataset.param;
            writeParameter(key, schema[key].default);
          });
          if (current === "workflow") writeParameter("step", schema.step.default);
        }
        if (button2.dataset.answer !== void 0) {
          const q = questions[Math.round(Number(latest.question))];
          const right = Number(button2.dataset.answer) === q.correct;
          const feedback = visual.querySelector(".feedback");
          feedback.textContent = `${right ? "Correct." : "Try again."} ${q.why}`;
          feedback.dataset.correct = String(right);
        }
      };
      root.addEventListener("input", onInput);
      root.addEventListener("click", onClick);
      function draw() {
        const state = exploration ?? narratedState;
        latest = state;
        const lab = state.scene;
        navigation.update(lab, narratedState.scene, exploration !== null);
        if (current !== lab) {
          current = lab;
          const [title, question, topic] = labs[lab];
          root.querySelector(".eyebrow").textContent = `Chapter ${Object.keys(labs).indexOf(lab) + 1} \xB7 ${topic}`;
          root.querySelector("h1").textContent = title;
          root.querySelector(".question").textContent = question;
          control.innerHTML = controls(lab);
          const cell = cells[lab];
          if (cell) mountCell(codeSlot, cell);
          else codeSlot.replaceChildren();
          root.querySelector(".lab-scroll").scrollTop = 0;
        }
        const key = JSON.stringify(state);
        if (key === last) return;
        last = key;
        visual.innerHTML = view(lab, state);
        const stepper = control.querySelector(".stepper");
        if (stepper) {
          const i3 = Math.round(Number(state.step));
          stepper.querySelector(".step-count").textContent = `Step ${i3 + 1} of ${workflowSteps.length}: ${workflowSteps[i3].name}`;
          stepper.querySelectorAll("[data-step-move]").forEach((b2) => {
            const to = i3 + Number(b2.dataset.stepMove);
            b2.disabled = to < 0 || to >= workflowSteps.length;
          });
        }
        control.querySelectorAll("[data-param]").forEach((input) => {
          const param = input.dataset.param, value = state[param];
          input.value = String(input.tagName === "SELECT" && typeof value === "number" ? Math.round(value) : value);
          if (param === "separation") {
            input.disabled = state.design !== "separated";
            input.title = input.disabled ? "Choose two rows at different x to change the distance." : "";
          }
          if (input instanceof HTMLInputElement && input.type === "range") input.style.setProperty("--fill", `${100 * (Number(input.value) - Number(input.min)) / (Number(input.max) - Number(input.min))}%`);
          const output = control.querySelector(`[data-value="${param}"]`);
          if (output) output.value = param === "points" ? String(Math.round(Number(value))) : fmt(Number(value), 2);
        });
      }
      return {
        render(state) {
          narratedState = state;
          draw();
        },
        handles: () => [],
        dispose() {
          disposeTheme();
          navigation.dispose();
          root.removeEventListener("input", onInput);
          root.removeEventListener("click", onClick);
          root.remove();
          style.remove();
        }
      };
    }
  };

  // ../../../Users/choxos/Documents/GitHub/mlumr-lesson/entry.ts
  var HAS_ASSISTANT = false;
  var ASSISTANT_START_OPEN = false;
  var INTRODUCTION = { "title": "ML-UMR: compare treatments, understand populations" };
  async function required(url) {
    const response2 = await fetch(url);
    if (!response2.ok) throw new Error(url + " returned " + response2.status);
    return response2;
  }
  async function main2() {
    const [tracks, vtt, assistantContext] = await Promise.all([
      required("./tracks.json").then((response2) => response2.json()),
      required("./captions.vtt").then((response2) => response2.text()),
      HAS_ASSISTANT ? required("./assistant.json").then((response2) => response2.json()) : void 0
    ]);
    const assistant = HAS_ASSISTANT ? { context: assistantContext, startOpen: ASSISTANT_START_OPEN } : void 0;
    const style = document.createElement("style");
    style.textContent = PLAYER_CSS;
    document.head.append(style);
    const mount = document.getElementById("app");
    mount.replaceChildren();
    const player = new Player({
      mount,
      scene,
      tracks,
      captionsVtt: vtt,
      introduction: INTRODUCTION,
      audioLoader: async () => {
        const src = preferredAudioSource(tracks.audio.src);
        if (HAS_ASSISTANT) return [src];
        const buffer = await (await required("./" + src)).arrayBuffer();
        return [URL.createObjectURL(new Blob([buffer], { type: mimeForAudio(src) }))];
      },
      baseUrl: "",
      assistant
    });
    window.__player = player;
    player.start();
  }
  main2().catch((error) => {
    console.error("lesson loading failed:", error);
    const status = document.querySelector(".xv-bootstrap-status");
    if (status) status.textContent = "This lesson could not load. Check your connection and reload the page.";
  });
})();
