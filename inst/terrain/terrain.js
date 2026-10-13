(() => {
  'use strict';

  const D = JSON.parse(document.getElementById('terrain-data').textContent);
  const DARK = D.theme === 'dark';
  document.documentElement.dataset.theme = D.theme;

  // ---------------------------------------------------------------- colour --
  const hex2rgb = (h) => {
    const v = parseInt(h.replace('#', ''), 16);
    return [((v >> 16) & 255) / 255, ((v >> 8) & 255) / 255, (v & 255) / 255];
  };
  const toLin = (c) => (c <= 0.04045 ? c / 12.92 : Math.pow((c + 0.055) / 1.055, 2.4));
  const toSrgb = (c) => (c <= 0.0031308 ? c * 12.92 : 1.055 * Math.pow(Math.max(c, 0), 1 / 2.4) - 0.055);
  const clamp01 = (x) => Math.min(1, Math.max(0, x));
  const lerp = (a, b, t) => a + (b - a) * t;
  const mix3 = (a, b, t) => [lerp(a[0], b[0], t), lerp(a[1], b[1], t), lerp(a[2], b[2], t)];

  function rgb2oklab(c) {
    const r = toLin(c[0]), g = toLin(c[1]), b = toLin(c[2]);
    const l = Math.cbrt(0.4122214708 * r + 0.5363325363 * g + 0.0514459929 * b);
    const m = Math.cbrt(0.2119034982 * r + 0.6806995451 * g + 0.1073969566 * b);
    const s = Math.cbrt(0.0883024619 * r + 0.2817188376 * g + 0.6299787005 * b);
    return [
      0.2104542553 * l + 0.793617785 * m - 0.0040720468 * s,
      1.9779984951 * l - 2.428592205 * m + 0.4505937099 * s,
      0.0259040371 * l + 0.7827717662 * m - 0.808675766 * s,
    ];
  }
  function oklab2rgb(c) {
    const l = Math.pow(c[0] + 0.3963377774 * c[1] + 0.2158037573 * c[2], 3);
    const m = Math.pow(c[0] - 0.1055613458 * c[1] - 0.0638541728 * c[2], 3);
    const s = Math.pow(c[0] - 0.0894841775 * c[1] - 1.291485548 * c[2], 3);
    return [
      clamp01(toSrgb(4.0767416621 * l - 3.3077115913 * m + 0.2309699292 * s)),
      clamp01(toSrgb(-1.2684380046 * l + 2.6097574011 * m - 0.3413193965 * s)),
      clamp01(toSrgb(-0.0041960863 * l - 0.7034186147 * m + 1.707614701 * s)),
    ];
  }
  const mixOk = (a, b, t) => oklab2rgb(mix3(rgb2oklab(a), rgb2oklab(b), t));

  const PAPER = hex2rgb(DARK ? '#3a3450' : '#efe7f1');
  const INERT = hex2rgb(DARK ? '#2c2936' : '#e2dbdc');
  const BG = hex2rgb(DARK ? '#121019' : '#f6f0ea');
  const RAMP = (DARK
    ? ['#2b2545', '#463b86', '#7a4fc4', '#c44fa8', '#ff6e9e', '#ffd0e0']
    : ['#e3dcf0', '#c3b3e6', '#a28ad8', '#c46fc0', '#ec5591', '#ff9ec0']
  ).map(hex2rgb);
  const RAMP_LAB = RAMP.map(rgb2oklab);
  function ramp(t) {
    t = clamp01(t) * (RAMP.length - 1);
    const i = Math.min(RAMP.length - 2, Math.floor(t));
    return oklab2rgb(mix3(RAMP_LAB[i], RAMP_LAB[i + 1], t - i));
  }

  // ------------------------------------------------------------------ grid --
  const NX = D.nx, NY = D.ny, N = D.cells.length;
  const G = D.groups.name.length;
  const GROUPED = D.grouped;
  const CS = 2 / Math.max(NX, NY);
  const gCol = D.groups.colour.map(hex2rgb);
  const gLab = gCol.map(rgb2oklab);

  const cellOf = new Int32Array(NX * NY).fill(-1);
  const ci = new Int32Array(N), cj = new Int32Array(N);
  const offsets = new Float32Array(N * 2);
  const gStart = new Int32Array(N + 1);
  const dTotal = new Float32Array(N);
  let minX = Infinity, maxX = -Infinity, minZ = Infinity, maxZ = -Infinity;
  for (let k = 0; k < N; k++) {
    const id = D.cells[k], i = id % NX, j = Math.floor(id / NX);
    cellOf[id] = k; ci[k] = i; cj[k] = j;
    const x = (i + 0.5 - NX / 2) * CS, z = -(j + 0.5 - NY / 2) * CS;
    offsets[2 * k] = x; offsets[2 * k + 1] = z;
    minX = Math.min(minX, x); maxX = Math.max(maxX, x);
    minZ = Math.min(minZ, z); maxZ = Math.max(maxZ, z);
    gStart[k + 1] = gStart[k] + D.gCount[k];
    let s = 0;
    for (let q = gStart[k]; q < gStart[k + 1]; q++) s += D.gVal[q];
    dTotal[k] = s;
  }
  let dMax = 0;
  for (let k = 0; k < N; k++) dMax = Math.max(dMax, dTotal[k]);
  const LOGK = dMax / 40;
  const quantile = (arr, q) => {
    const a = Float32Array.from(arr).sort();
    return a[Math.min(a.length - 1, Math.floor(q * (a.length - 1)))];
  };
  // Relief: below "sea level" the ground only undulates gently; above it the
  // height climbs steeply, and the very top is softened (log) so that a single
  // dominant cluster does not flatten every other peak.
  const SEA = quantile(dTotal, 0.4);
  const RIDGE = Math.max(quantile(dTotal, 0.995), SEA * 1.5);
  const reliefRaw = (d) => {
    let x = Math.max(0, d - SEA) / (RIDGE - SEA);
    if (x > 1) x = 1 + 0.6 * Math.log(x);
    return 0.035 * Math.min(1, d / SEA) + Math.pow(x, 1.3);
  };
  const RELIEF_MAX = reliefRaw(dMax);
  function heightOf(d) {
    const r = d / dMax;
    if (D.heightScale === 'relief') return clamp01(reliefRaw(d) / RELIEF_MAX);
    if (D.heightScale === 'linear') return clamp01(r);
    if (D.heightScale === 'log') return clamp01(Math.log1p(d / LOGK) / Math.log1p(dMax / LOGK));
    return Math.sqrt(clamp01(r));
  }

  const MAXH = 0.42 * D.exaggeration;
  const BASE = 0.011;
  const CENTRE = [(minX + maxX) / 2, 0, (minZ + maxZ) / 2];
  const RADIUS = 0.5 * Math.hypot(maxX - minX + CS, maxZ - minZ + CS);

  // Small fixed grain so the plains read as a textured surface rather than a slab.
  const grain = new Float32Array(N);
  for (let k = 0; k < N; k++) {
    const h = Math.sin(ci[k] * 127.1 + cj[k] * 311.7) * 43758.5453;
    grain[k] = (h - Math.floor(h)) * 0.0045;
  }

  const samples = new Map();
  if (D.samples) D.samples.cell.forEach((c, i) => samples.set(c, D.samples.text[i]));

  // ----------------------------------------------------------------- state --
  let active = new Set(Array.from({ length: G }, (_, i) => i));
  let flat = false;
  const allActive = () => active.size === G;

  const hFrom = new Float32Array(N), hTo = new Float32Array(N);
  const topFrom = new Float32Array(N * 3), topTo = new Float32Array(N * 3);
  const botFrom = new Float32Array(N * 3), botTo = new Float32Array(N * 3);
  const occFrom = new Float32Array(N * 2), occTo = new Float32Array(N * 2);
  const shareActive = new Float32Array(N);
  let morphT = 1, morphStart = 0;
  const MORPH_MS = 750;

  // Enrichment: how over-represented a group is in a bin compared with its
  // overall share. ALPHA shrinks sparse bins towards the overall mix so a
  // handful of documents cannot paint a strong colour.
  const ENRICH = GROUPED && D.colourBy === 'enrichment';
  const BASE_SHARE = D.groups.share;
  const ALPHA = quantile(dTotal, 0.5) * 1.5;
  const NEUTRAL = hex2rgb(DARK ? '#4a4560' : '#d9d1df');
  const NEUTRAL_TOP = hex2rgb(DARK ? '#8d86a3' : '#b9aec4');
  // Odds ratio rather than a plain ratio of shares, so a group that is already
  // 80% of the data can still show where it reaches 95%.
  const odds = (p) => Math.min(p, 0.9999) / (1 - Math.min(p, 0.9999));
  function enrichment(k) {
    let best = -1, lift = 1;
    for (let q = gStart[k]; q < gStart[k + 1]; q++) {
      const g = D.gIdx[q];
      const sh = (D.gVal[q] + ALPHA * BASE_SHARE[g]) / (dTotal[k] + ALPHA);
      const l = odds(sh) / odds(BASE_SHARE[g]);
      if (l > lift) { lift = l; best = g; }
    }
    return { g: best, lift };
  }

  // Count mode: each group's own density is scaled so its smooth peak sits at
  // (its size / the largest group's size); heights then compare topic sizes.
  const COUNT = D.heightBy === 'count';
  const GSCALE = D.groups.scale;

  function computeTarget() {
    const all = allActive();
    for (let k = 0; k < N; k++) {
      let d = 0, hc = 0;
      const lab = [0, 0, 0];
      let w = 0;
      for (let q = gStart[k]; q < gStart[k + 1]; q++) {
        const g = D.gIdx[q];
        if (!all && !active.has(g)) continue;
        const v = D.gVal[q];
        d += v;
        hc += v * GSCALE[g];
        lab[0] += gLab[g][0] * v; lab[1] += gLab[g][1] * v; lab[2] += gLab[g][2] * v;
        w += v;
      }
      const hn = COUNT
        ? 0.035 * Math.min(1, d / SEA) + 0.965 * Math.min(1.08, hc)
        : heightOf(d);
      const sa = dTotal[k] > 0 ? d / dTotal[k] : 0;
      shareActive[k] = sa;
      hTo[k] = flat ? 0 : hn * MAXH + grain[k] * (all ? 1 : sa);

      let top, bot;
      if (!GROUPED) {
        top = ramp(Math.pow(hn, 0.85));
        bot = mix3(PAPER, ramp(0.08), 0.6);
      } else if (ENRICH && all) {
        const e = enrichment(k);
        const t = Math.pow(clamp01(hn / 0.75), 0.7);
        const strength = e.g < 0 ? 0 : clamp01(Math.log(e.lift) / Math.log(3.5));
        const base = mixOk(NEUTRAL, NEUTRAL_TOP, t);
        top = e.g < 0 ? base : mixOk(base, gCol[e.g], strength * (0.45 + 0.55 * t));
        bot = e.g < 0 ? NEUTRAL : mixOk(NEUTRAL, gCol[e.g], strength * 0.35);
      } else {
        const col = w > 0 ? oklab2rgb([lab[0] / w, lab[1] / w, lab[2] / w]) : INERT;
        const t = Math.pow(clamp01(hn / 0.75), 0.7);
        top = mixOk(PAPER, col, 0.36 + 0.64 * t);
        // On dark backgrounds the peaks glow slightly towards white.
        if (DARK) top = mixOk(top, [1, 1, 1], 0.28 * t * t);
        bot = mixOk(PAPER, col, 0.3);
        if (!all) {
          const fade = 1 - clamp01(sa * 1.6);
          top = mix3(top, INERT, fade);
          bot = mix3(bot, INERT, fade);
        }
      }
      topTo.set(top, k * 3);
      botTo.set(bot, k * 3);
    }
    computeOcclusion();
  }

  // Ambient occlusion from neighbouring columns: how much taller the
  // surroundings are, and to what height they shade the column's sides.
  const NB = [];
  for (let dj = -2; dj <= 2; dj++)
    for (let di = -2; di <= 2; di++)
      if (di || dj) NB.push([di, dj, 1 / Math.hypot(di, dj)]);
  function computeOcclusion() {
    for (let k = 0; k < N; k++) {
      const h = hTo[k];
      let ex = 0, wsum = 0, hmax = h;
      for (const [di, dj, w] of NB) {
        const i = ci[k] + di, j = cj[k] + dj;
        let nh = 0;
        if (i >= 0 && j >= 0 && i < NX && j < NY) {
          const m = cellOf[i + j * NX];
          if (m >= 0) nh = hTo[m];
        }
        ex += Math.max(0, nh - h) * w;
        wsum += w;
        if (nh > hmax) hmax = nh;
      }
      ex /= wsum;
      occTo[2 * k] = 0.55 * (1 - Math.exp(-ex / 0.01));
      occTo[2 * k + 1] = BASE + h + (hmax - h) * 0.6;
    }
  }

  function startMorph() {
    // Freeze the current interpolated state as the new starting point.
    const t = ease(morphT);
    for (let k = 0; k < N; k++) hFrom[k] = lerp(hFrom[k], hTo[k], t);
    for (let k = 0; k < N * 3; k++) {
      topFrom[k] = lerp(topFrom[k], topTo[k], t);
      botFrom[k] = lerp(botFrom[k], botTo[k], t);
    }
    for (let k = 0; k < N * 2; k++) occFrom[k] = lerp(occFrom[k], occTo[k], t);
    computeTarget();
    uploadInstances();
    morphT = 0;
    morphStart = performance.now();
    requestRender();
  }
  const ease = (t) => (t < 0.5 ? 4 * t * t * t : 1 - Math.pow(-2 * t + 2, 3) / 2);
  const curH = (k) => BASE + lerp(hFrom[k], hTo[k], ease(morphT));

  // ---------------------------------------------------------------- mat4 --
  const M4 = {
    mul(a, b) {
      const o = new Float32Array(16);
      for (let c = 0; c < 4; c++)
        for (let r = 0; r < 4; r++)
          o[c * 4 + r] = a[r] * b[c * 4] + a[4 + r] * b[c * 4 + 1] + a[8 + r] * b[c * 4 + 2] + a[12 + r] * b[c * 4 + 3];
      return o;
    },
    perspective(fovy, aspect, n, f) {
      const t = 1 / Math.tan(fovy / 2), o = new Float32Array(16);
      o[0] = t / aspect; o[5] = t; o[10] = (f + n) / (n - f); o[11] = -1; o[14] = (2 * f * n) / (n - f);
      return o;
    },
    ortho(l, r, b, t, n, f) {
      const o = new Float32Array(16);
      o[0] = 2 / (r - l); o[5] = 2 / (t - b); o[10] = -2 / (f - n);
      o[12] = -(r + l) / (r - l); o[13] = -(t + b) / (t - b); o[14] = -(f + n) / (f - n); o[15] = 1;
      return o;
    },
    lookAt(e, c, up) {
      let z = [e[0] - c[0], e[1] - c[1], e[2] - c[2]];
      let l = Math.hypot(...z); z = z.map((v) => v / l);
      let x = [up[1] * z[2] - up[2] * z[1], up[2] * z[0] - up[0] * z[2], up[0] * z[1] - up[1] * z[0]];
      l = Math.hypot(...x); x = x.map((v) => v / l);
      const y = [z[1] * x[2] - z[2] * x[1], z[2] * x[0] - z[0] * x[2], z[0] * x[1] - z[1] * x[0]];
      return new Float32Array([
        x[0], y[0], z[0], 0, x[1], y[1], z[1], 0, x[2], y[2], z[2], 0,
        -(x[0] * e[0] + x[1] * e[1] + x[2] * e[2]),
        -(y[0] * e[0] + y[1] * e[1] + y[2] * e[2]),
        -(z[0] * e[0] + z[1] * e[1] + z[2] * e[2]), 1,
      ]);
    },
    apply(m, v) {
      return [0, 1, 2, 3].map((r) => m[r] * v[0] + m[4 + r] * v[1] + m[8 + r] * v[2] + m[12 + r] * v[3]);
    },
    invert(m) {
      const inv = new Float32Array(16);
      inv[0] = m[5] * m[10] * m[15] - m[5] * m[11] * m[14] - m[9] * m[6] * m[15] + m[9] * m[7] * m[14] + m[13] * m[6] * m[11] - m[13] * m[7] * m[10];
      inv[4] = -m[4] * m[10] * m[15] + m[4] * m[11] * m[14] + m[8] * m[6] * m[15] - m[8] * m[7] * m[14] - m[12] * m[6] * m[11] + m[12] * m[7] * m[10];
      inv[8] = m[4] * m[9] * m[15] - m[4] * m[11] * m[13] - m[8] * m[5] * m[15] + m[8] * m[7] * m[13] + m[12] * m[5] * m[11] - m[12] * m[7] * m[9];
      inv[12] = -m[4] * m[9] * m[14] + m[4] * m[10] * m[13] + m[8] * m[5] * m[14] - m[8] * m[6] * m[13] - m[12] * m[5] * m[10] + m[12] * m[6] * m[9];
      inv[1] = -m[1] * m[10] * m[15] + m[1] * m[11] * m[14] + m[9] * m[2] * m[15] - m[9] * m[3] * m[14] - m[13] * m[2] * m[11] + m[13] * m[3] * m[10];
      inv[5] = m[0] * m[10] * m[15] - m[0] * m[11] * m[14] - m[8] * m[2] * m[15] + m[8] * m[3] * m[14] + m[12] * m[2] * m[11] - m[12] * m[3] * m[10];
      inv[9] = -m[0] * m[9] * m[15] + m[0] * m[11] * m[13] + m[8] * m[1] * m[15] - m[8] * m[3] * m[13] - m[12] * m[1] * m[11] + m[12] * m[3] * m[9];
      inv[13] = m[0] * m[9] * m[14] - m[0] * m[10] * m[13] - m[8] * m[1] * m[14] + m[8] * m[2] * m[13] + m[12] * m[1] * m[10] - m[12] * m[2] * m[9];
      inv[2] = m[1] * m[6] * m[15] - m[1] * m[7] * m[14] - m[5] * m[2] * m[15] + m[5] * m[3] * m[14] + m[13] * m[2] * m[7] - m[13] * m[3] * m[6];
      inv[6] = -m[0] * m[6] * m[15] + m[0] * m[7] * m[14] + m[4] * m[2] * m[15] - m[4] * m[3] * m[14] - m[12] * m[2] * m[7] + m[12] * m[3] * m[6];
      inv[10] = m[0] * m[5] * m[15] - m[0] * m[7] * m[13] - m[4] * m[1] * m[15] + m[4] * m[3] * m[13] + m[12] * m[1] * m[7] - m[12] * m[3] * m[5];
      inv[14] = -m[0] * m[5] * m[14] + m[0] * m[6] * m[13] + m[4] * m[1] * m[14] - m[4] * m[2] * m[13] - m[12] * m[1] * m[6] + m[12] * m[2] * m[5];
      inv[3] = -m[1] * m[6] * m[11] + m[1] * m[7] * m[10] + m[5] * m[2] * m[11] - m[5] * m[3] * m[10] - m[9] * m[2] * m[7] + m[9] * m[3] * m[6];
      inv[7] = m[0] * m[6] * m[11] - m[0] * m[7] * m[10] - m[4] * m[2] * m[11] + m[4] * m[3] * m[10] + m[8] * m[2] * m[7] - m[8] * m[3] * m[6];
      inv[11] = -m[0] * m[5] * m[11] + m[0] * m[7] * m[9] + m[4] * m[1] * m[11] - m[4] * m[3] * m[9] - m[8] * m[1] * m[7] + m[8] * m[3] * m[5];
      inv[15] = m[0] * m[5] * m[10] - m[0] * m[6] * m[9] - m[4] * m[1] * m[10] + m[4] * m[2] * m[9] + m[8] * m[1] * m[6] - m[8] * m[2] * m[5];
      const det = m[0] * inv[0] + m[1] * inv[4] + m[2] * inv[8] + m[3] * inv[12];
      for (let i = 0; i < 16; i++) inv[i] /= det;
      return inv;
    },
  };

  // ------------------------------------------------------------------- GL --
  const stage = document.getElementById('stage');
  const canvas = document.getElementById('gl');
  const overlay = document.getElementById('overlay');
  const octx = overlay.getContext('2d');
  const gl = canvas.getContext('webgl2', { antialias: true, alpha: true, premultipliedAlpha: true });
  if (!gl) {
    document.getElementById('nogl').hidden = false;
    return;
  }
  // If the browser reclaims the GPU (low memory, many tabs), say so rather than
  // leaving a blank page.
  canvas.addEventListener('webglcontextlost', (e) => {
    e.preventDefault();
    const msg = document.getElementById('nogl');
    msg.textContent = 'The browser paused this map to free graphics memory. Reload the page to show it again.';
    msg.hidden = false;
  });

  const SHADOW_GLSL = `
    float shadowVis(vec4 lp) {
      vec3 p = lp.xyz / lp.w * 0.5 + 0.5;
      if (p.x < 0.0 || p.x > 1.0 || p.y < 0.0 || p.y > 1.0 || p.z > 1.0) return 1.0;
      vec2 ts = 1.0 / vec2(textureSize(u_shadow, 0));
      float s = 0.0;
      for (int x = -2; x <= 2; x++)
        for (int y = -2; y <= 2; y++)
          s += texture(u_shadow, vec3(p.xy + vec2(x, y) * ts * 0.9, p.z - 0.0012));
      return s / 25.0;
    }`;

  const VS_TERRAIN = `#version 300 es
    layout(location=0) in vec3 a_pos;
    layout(location=1) in vec3 a_nrm;
    layout(location=2) in vec2 a_off;
    layout(location=3) in vec2 a_h;
    layout(location=4) in vec3 a_topA;
    layout(location=5) in vec3 a_topB;
    layout(location=6) in vec3 a_botA;
    layout(location=7) in vec3 a_botB;
    layout(location=8) in vec4 a_occ;
    uniform mat4 u_vp, u_lvp;
    uniform float u_t, u_cell, u_base;
    uniform int u_hover;
    out vec3 v_col, v_nrm, v_world;
    out vec4 v_lpos;
    flat out vec2 v_occ;
    flat out float v_side;
    vec3 lin(vec3 c) { return pow(c, vec3(2.2)); }
    void main() {
      float h = u_base + mix(a_h.x, a_h.y, u_t);
      vec3 p = vec3(a_off.x + a_pos.x * u_cell, a_pos.y * h, a_off.y + a_pos.z * u_cell);
      vec3 top = lin(mix(a_topA, a_topB, u_t));
      vec3 bot = lin(mix(a_botA, a_botB, u_t));
      v_col = mix(bot, top, a_pos.y);
      if (gl_InstanceID == u_hover) v_col = mix(v_col, vec3(1.0), 0.45);
      v_occ = mix(a_occ.xy, a_occ.zw, u_t);
      v_side = 1.0 - step(0.5, a_nrm.y);
      v_nrm = a_nrm;
      v_world = p;
      v_lpos = u_lvp * vec4(p + a_nrm * 0.0025, 1.0);
      gl_Position = u_vp * vec4(p, 1.0);
    }`;

  const FS_TERRAIN = `#version 300 es
    precision highp float;
    precision highp sampler2DShadow;
    in vec3 v_col, v_nrm, v_world;
    in vec4 v_lpos;
    flat in vec2 v_occ;
    flat in float v_side;
    uniform sampler2DShadow u_shadow;
    uniform vec3 u_L, u_sun, u_sky, u_gnd, u_fog, u_eye;
    uniform float u_fogNear, u_fogFar, u_fogAmt;
    out vec4 o;
    ${SHADOW_GLSL}
    void main() {
      vec3 n = normalize(v_nrm);
      float ndl = max(dot(n, u_L), 0.0);
      float sh = ndl > 0.0 ? shadowVis(v_lpos) : 0.0;
      float below = 1.0 - smoothstep(0.0, max(v_occ.y, 1e-4), v_world.y);
      float foot = 1.0 - smoothstep(0.0, 0.03, v_world.y);
      float ao = 1.0 - v_side * (v_occ.x * below * below + 0.2 * foot);
      vec3 amb = mix(u_gnd, u_sky, n.y * 0.5 + 0.5);
      vec3 c = v_col * (amb * ao + u_sun * ndl * sh * mix(0.8, 1.0, ao));
      float fog = smoothstep(u_fogNear, u_fogFar, distance(v_world, u_eye));
      c = mix(c, u_fog, fog * u_fogAmt);
      o = vec4(pow(c, vec3(1.0 / 2.2)), 1.0);
    }`;

  const VS_DEPTH = `#version 300 es
    layout(location=0) in vec3 a_pos;
    layout(location=2) in vec2 a_off;
    layout(location=3) in vec2 a_h;
    uniform mat4 u_lvp;
    uniform float u_t, u_cell, u_base;
    void main() {
      float h = u_base + mix(a_h.x, a_h.y, u_t);
      vec3 p = vec3(a_off.x + a_pos.x * u_cell, a_pos.y * h, a_off.y + a_pos.z * u_cell);
      gl_Position = u_lvp * vec4(p, 1.0);
    }`;
  const FS_DEPTH = `#version 300 es
    precision mediump float;
    void main() {}`;

  const VS_GROUND = `#version 300 es
    layout(location=0) in vec2 a_xz;
    uniform mat4 u_vp, u_lvp;
    out vec3 v_world;
    out vec4 v_lpos;
    void main() {
      vec3 p = vec3(a_xz.x, -0.0005, a_xz.y);
      v_world = p;
      v_lpos = u_lvp * vec4(p, 1.0);
      gl_Position = u_vp * vec4(p, 1.0);
    }`;
  const FS_GROUND = `#version 300 es
    precision highp float;
    precision highp sampler2DShadow;
    in vec3 v_world;
    in vec4 v_lpos;
    uniform sampler2DShadow u_shadow;
    uniform sampler2D u_mask;
    uniform vec2 u_grid;      // NX, NY
    uniform float u_cs;
    uniform vec2 u_shift;     // contact shadow offset, world units
    uniform vec3 u_shade;
    uniform vec3 u_centre;
    uniform float u_radius;
    out vec4 o;
    ${SHADOW_GLSL}
    void main() {
      vec2 q = v_world.xz + u_shift;
      vec2 uv = vec2((q.x / u_cs + u_grid.x * 0.5) / u_grid.x, (-q.y / u_cs + u_grid.y * 0.5) / u_grid.y);
      float inside = step(0.0, uv.x) * step(uv.x, 1.0) * step(0.0, uv.y) * step(uv.y, 1.0);
      float contact = texture(u_mask, uv).r * inside;
      float castSh = 1.0 - shadowVis(v_lpos);
      float fade = 1.0 - smoothstep(u_radius * 0.9, u_radius * 1.6, distance(v_world.xz, u_centre.xz));
      float a = clamp(contact * 0.3 + castSh * 0.22, 0.0, 0.42) * fade;
      o = vec4(u_shade * a, a);
    }`;

  function program(vs, fs) {
    const p = gl.createProgram();
    for (const [type, src] of [[gl.VERTEX_SHADER, vs], [gl.FRAGMENT_SHADER, fs]]) {
      const s = gl.createShader(type);
      gl.shaderSource(s, src);
      gl.compileShader(s);
      if (!gl.getShaderParameter(s, gl.COMPILE_STATUS)) throw new Error(gl.getShaderInfoLog(s));
      gl.attachShader(p, s);
    }
    gl.linkProgram(p);
    if (!gl.getProgramParameter(p, gl.LINK_STATUS)) throw new Error(gl.getProgramInfoLog(p));
    const u = {};
    const n = gl.getProgramParameter(p, gl.ACTIVE_UNIFORMS);
    for (let i = 0; i < n; i++) {
      const name = gl.getActiveUniform(p, i).name;
      u[name] = gl.getUniformLocation(p, name);
    }
    return { p, u };
  }
  const progTerrain = program(VS_TERRAIN, FS_TERRAIN);
  const progDepth = program(VS_DEPTH, FS_DEPTH);
  const progGround = program(VS_GROUND, FS_GROUND);

  // Unit column: top and four sides (the base is never seen).
  const box = (() => {
    const faces = [
      [[0, 1, 0], [[-0.5, 1, -0.5], [-0.5, 1, 0.5], [0.5, 1, 0.5], [0.5, 1, -0.5]]],
      [[1, 0, 0], [[0.5, 0, 0.5], [0.5, 0, -0.5], [0.5, 1, -0.5], [0.5, 1, 0.5]]],
      [[-1, 0, 0], [[-0.5, 0, -0.5], [-0.5, 0, 0.5], [-0.5, 1, 0.5], [-0.5, 1, -0.5]]],
      [[0, 0, 1], [[-0.5, 0, 0.5], [0.5, 0, 0.5], [0.5, 1, 0.5], [-0.5, 1, 0.5]]],
      [[0, 0, -1], [[0.5, 0, -0.5], [-0.5, 0, -0.5], [-0.5, 1, -0.5], [0.5, 1, -0.5]]],
    ];
    const v = [], idx = [];
    faces.forEach(([n, quad], f) => {
      quad.forEach((p) => v.push(...p, ...n));
      idx.push(f * 4, f * 4 + 1, f * 4 + 2, f * 4, f * 4 + 2, f * 4 + 3);
    });
    return { v: new Float32Array(v), i: new Uint16Array(idx) };
  })();

  const vao = gl.createVertexArray();
  gl.bindVertexArray(vao);
  const vbo = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, vbo);
  gl.bufferData(gl.ARRAY_BUFFER, box.v, gl.STATIC_DRAW);
  gl.enableVertexAttribArray(0);
  gl.vertexAttribPointer(0, 3, gl.FLOAT, false, 24, 0);
  gl.enableVertexAttribArray(1);
  gl.vertexAttribPointer(1, 3, gl.FLOAT, false, 24, 12);
  const ibo = gl.createBuffer();
  gl.bindBuffer(gl.ELEMENT_ARRAY_BUFFER, ibo);
  gl.bufferData(gl.ELEMENT_ARRAY_BUFFER, box.i, gl.STATIC_DRAW);

  function instAttr(loc, size, buf, stride, off) {
    gl.bindBuffer(gl.ARRAY_BUFFER, buf);
    gl.enableVertexAttribArray(loc);
    gl.vertexAttribPointer(loc, size, gl.FLOAT, false, stride, off);
    gl.vertexAttribDivisor(loc, 1);
  }
  const bufOff = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, bufOff);
  gl.bufferData(gl.ARRAY_BUFFER, offsets, gl.STATIC_DRAW);
  instAttr(2, 2, bufOff, 0, 0);

  const bufH = gl.createBuffer(), bufTop = gl.createBuffer(), bufBot = gl.createBuffer(), bufOcc = gl.createBuffer();
  const pairH = new Float32Array(N * 2), pairTop = new Float32Array(N * 6), pairBot = new Float32Array(N * 6), pairOcc = new Float32Array(N * 4);
  for (const b of [[bufH, pairH], [bufTop, pairTop], [bufBot, pairBot], [bufOcc, pairOcc]]) {
    gl.bindBuffer(gl.ARRAY_BUFFER, b[0]);
    gl.bufferData(gl.ARRAY_BUFFER, b[1].byteLength, gl.DYNAMIC_DRAW);
  }
  instAttr(3, 2, bufH, 0, 0);
  instAttr(4, 3, bufTop, 24, 0);
  instAttr(5, 3, bufTop, 24, 12);
  instAttr(6, 3, bufBot, 24, 0);
  instAttr(7, 3, bufBot, 24, 12);
  instAttr(8, 4, bufOcc, 0, 0);
  gl.bindVertexArray(null);

  function uploadInstances() {
    for (let k = 0; k < N; k++) {
      pairH[2 * k] = hFrom[k]; pairH[2 * k + 1] = hTo[k];
      for (let c = 0; c < 3; c++) {
        pairTop[6 * k + c] = topFrom[3 * k + c]; pairTop[6 * k + 3 + c] = topTo[3 * k + c];
        pairBot[6 * k + c] = botFrom[3 * k + c]; pairBot[6 * k + 3 + c] = botTo[3 * k + c];
      }
      pairOcc[4 * k] = occFrom[2 * k]; pairOcc[4 * k + 1] = occFrom[2 * k + 1];
      pairOcc[4 * k + 2] = occTo[2 * k]; pairOcc[4 * k + 3] = occTo[2 * k + 1];
    }
    for (const [b, a] of [[bufH, pairH], [bufTop, pairTop], [bufBot, pairBot], [bufOcc, pairOcc]]) {
      gl.bindBuffer(gl.ARRAY_BUFFER, b);
      gl.bufferSubData(gl.ARRAY_BUFFER, 0, a);
    }
  }

  // Ground quad
  const groundVao = gl.createVertexArray();
  gl.bindVertexArray(groundVao);
  const gb = gl.createBuffer();
  gl.bindBuffer(gl.ARRAY_BUFFER, gb);
  const R3 = RADIUS * 2.2;
  gl.bufferData(gl.ARRAY_BUFFER, new Float32Array([
    CENTRE[0] - R3, CENTRE[2] - R3, CENTRE[0] + R3, CENTRE[2] - R3, CENTRE[0] + R3, CENTRE[2] + R3,
    CENTRE[0] - R3, CENTRE[2] - R3, CENTRE[0] + R3, CENTRE[2] + R3, CENTRE[0] - R3, CENTRE[2] + R3,
  ]), gl.STATIC_DRAW);
  gl.enableVertexAttribArray(0);
  gl.vertexAttribPointer(0, 2, gl.FLOAT, false, 0, 0);
  gl.bindVertexArray(null);

  // Soft footprint of the landmass, used as a contact shadow on the ground.
  const maskTex = (() => {
    let m = new Float32Array(NX * NY);
    for (let k = 0; k < N; k++) m[D.cells[k]] = 1;
    const tmp = new Float32Array(NX * NY), r = 3;
    for (let pass = 0; pass < 3; pass++) {
      for (let j = 0; j < NY; j++)
        for (let i = 0; i < NX; i++) {
          let s = 0;
          for (let d = -r; d <= r; d++) { const ii = i + d; if (ii >= 0 && ii < NX) s += m[ii + j * NX]; }
          tmp[i + j * NX] = s / (2 * r + 1);
        }
      for (let j = 0; j < NY; j++)
        for (let i = 0; i < NX; i++) {
          let s = 0;
          for (let d = -r; d <= r; d++) { const jj = j + d; if (jj >= 0 && jj < NY) s += tmp[i + jj * NX]; }
          m[i + j * NX] = s / (2 * r + 1);
        }
    }
    const bytes = new Uint8Array(NX * NY);
    for (let i = 0; i < bytes.length; i++) bytes[i] = Math.round(m[i] * 255);
    const t = gl.createTexture();
    gl.bindTexture(gl.TEXTURE_2D, t);
    gl.pixelStorei(gl.UNPACK_ALIGNMENT, 1);
    gl.texImage2D(gl.TEXTURE_2D, 0, gl.R8, NX, NY, 0, gl.RED, gl.UNSIGNED_BYTE, bytes);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
    gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
    return t;
  })();

  // Shadow map
  // 2048 looks the same as 4096 here and uses a quarter of the GPU memory.
  const SHADOW_SIZE = Math.min(2048, gl.getParameter(gl.MAX_TEXTURE_SIZE));
  const shadowTex = gl.createTexture();
  gl.bindTexture(gl.TEXTURE_2D, shadowTex);
  gl.texStorage2D(gl.TEXTURE_2D, 1, gl.DEPTH_COMPONENT24, SHADOW_SIZE, SHADOW_SIZE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MIN_FILTER, gl.LINEAR);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_MAG_FILTER, gl.LINEAR);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_S, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_WRAP_T, gl.CLAMP_TO_EDGE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_COMPARE_MODE, gl.COMPARE_REF_TO_TEXTURE);
  gl.texParameteri(gl.TEXTURE_2D, gl.TEXTURE_COMPARE_FUNC, gl.LEQUAL);
  const shadowFbo = gl.createFramebuffer();
  gl.bindFramebuffer(gl.FRAMEBUFFER, shadowFbo);
  gl.framebufferTexture2D(gl.FRAMEBUFFER, gl.DEPTH_ATTACHMENT, gl.TEXTURE_2D, shadowTex, 0);
  gl.drawBuffers([gl.NONE]);
  gl.readBuffer(gl.NONE);
  gl.bindFramebuffer(gl.FRAMEBUFFER, null);

  // Light from the front left, high enough to keep the plains bright.
  const L = (() => { const v = [-0.62, 0.7, 0.36], l = Math.hypot(...v); return v.map((x) => x / l); })();
  const lightVP = (() => {
    const eye = [CENTRE[0] + L[0] * 4, L[1] * 4, CENTRE[2] + L[2] * 4];
    const view = M4.lookAt(eye, CENTRE, [0, 1, 0]);
    let l = Infinity, r = -Infinity, b = Infinity, t = -Infinity, n = Infinity, f = -Infinity;
    for (const x of [minX - CS, maxX + CS])
      for (const z of [minZ - CS, maxZ + CS])
        for (const y of [0, BASE + MAXH + 0.01]) {
          const p = M4.apply(view, [x, y, z, 1]);
          l = Math.min(l, p[0]); r = Math.max(r, p[0]); b = Math.min(b, p[1]); t = Math.max(t, p[1]);
          n = Math.min(n, -p[2]); f = Math.max(f, -p[2]);
        }
    return M4.mul(M4.ortho(l, r, b, t, n - 0.05, f + 0.05), view);
  })();

  const LIGHT = DARK
    ? { sun: [1.0, 0.94, 0.96].map((v) => v * 1.5), sky: [0.95, 0.9, 1.15], gnd: [0.38, 0.32, 0.46], shade: [0, 0, 0], fog: 0.12 }
    : { sun: [1.0, 0.96, 0.93].map((v) => v * 0.78), sky: [0.72, 0.71, 0.8], gnd: [0.46, 0.41, 0.45], shade: [0.16, 0.1, 0.2], fog: 0.4 };

  // ---------------------------------------------------------------- camera --
  const FOV = (24 * Math.PI) / 180;
  const DEFAULT_EL = 0.66, DEFAULT_AZ = 0;
  const cam = { target: [...CENTRE], dist: 3, az: DEFAULT_AZ, el: DEFAULT_EL };
  const goal = { target: [...CENTRE], dist: 3, az: DEFAULT_AZ, el: DEFAULT_EL };
  let cssW = 1, cssH = 1, dpr = 1;
  let VP = new Float32Array(16), eye = [0, 0, 0];

  // The free area between the masthead and the legend, in CSS pixels.
  function availRect() {
    const top = document.getElementById('masthead').hidden ? 16
      : document.getElementById('title').getBoundingClientRect().bottom + 12;
    const bottom = document.getElementById('legend').getBoundingClientRect().top - 12;
    const side = cssW > 640 ? 64 : 24;
    return { l: 24, r: cssW - side, t: Math.min(top, cssH * 0.4), b: Math.max(bottom, cssH * 0.6) };
  }
  let lens = [0, 0];
  let FIT_DIST = 1;
  // A sample of cells (plus the outline extremes) to frame the view on.
  let fitCache = null;
  const fitCells = () => fitCache || (fitCache = (() => {
    // Frame the core of the data (1st to 99th percentile of documents along
    // each axis) so a far island cannot shrink the main landmass.
    const wq = (axis, q) => {
      const idx = Array.from({ length: N }, (_, k) => k).sort((p, r) => offsets[2 * p + axis] - offsets[2 * r + axis]);
      const total = D.raw.reduce((a, b) => a + b, 0);
      let acc = 0;
      for (const k of idx) { acc += D.raw[k]; if (acc >= q * total) return offsets[2 * k + axis]; }
      return offsets[2 * idx[idx.length - 1] + axis];
    };
    const pad = 6 * CS;
    const x0 = wq(0, 0.01) - pad, x1 = wq(0, 0.99) + pad, z0 = wq(1, 0.01) - pad, z1 = wq(1, 0.99) + pad;
    const inCore = (k) => offsets[2 * k] >= x0 && offsets[2 * k] <= x1 && offsets[2 * k + 1] >= z0 && offsets[2 * k + 1] <= z1;
    // Width is framed on the core; height on all land, so small islands may
    // bleed off the sides but never into the title or legend.
    const step = Math.max(1, Math.floor(N / 4000));
    const out = [];
    for (let k = 0; k < N; k += step) out.push(k);
    // The tallest columns, so narrow spikes never poke into the title.
    const tallest = Array.from(hTo.keys()).sort((p, q) => hTo[q] - hTo[p]).slice(0, 200);
    return out.concat(tallest).map((k) => [k, inCore(k)]);
  })());
  function resetView(instant, keepAz = false) {
    const a = availRect();
    lens = [((a.l + a.r) / 2 - cssW / 2) / (cssW / 2), -((a.t + a.b) / 2 - cssH / 2) / (cssH / 2)];
    goal.target = [...CENTRE];
    if (!keepAz) goal.az = DEFAULT_AZ;
    goal.el = flat ? 1.565 : DEFAULT_EL;
    // Binary search the distance at which the landmass just fits the free
    // area, then shift the lens so the projected land (whose tall peaks pull it
    // upwards) is centred in that area. A few passes settle both.
    const saved = { ...cam, target: [...cam.target] };
    Object.assign(cam, { target: goal.target, az: goal.az, el: goal.el });
    const bounds = () => {
      updateCamera();
      let x0 = Infinity, x1 = -Infinity, y0 = Infinity, y1 = -Infinity;
      for (const [k, core] of fitCells()) for (const y of [0, BASE + hTo[k]]) {
        const p = project([offsets[2 * k], y, offsets[2 * k + 1]]);
        if (!p) continue;
        if (core) { x0 = Math.min(x0, p[0] * cssW); x1 = Math.max(x1, p[0] * cssW); }
        y0 = Math.min(y0, p[1] * cssH); y1 = Math.max(y1, p[1] * cssH);
      }
      return [x0, x1, y0, y1];
    };
    const fw = (a.r - a.l) * 0.96, fh = (a.b - a.t) * 0.96;
    let hi = RADIUS * 20;
    for (let pass = 0; pass < 4; pass++) {
      let lo = RADIUS * 0.3;
      hi = RADIUS * 20;
      for (let it = 0; it < 30; it++) {
        cam.dist = (lo + hi) / 2;
        const b = bounds();
        if (b[1] - b[0] > fw || b[3] - b[2] > fh) lo = cam.dist; else hi = cam.dist;
      }
      cam.dist = hi;
      const b = bounds();
      lens[0] += ((a.l + a.r) / 2 - (b[0] + b[1]) / 2) / (cssW / 2);
      lens[1] -= ((a.t + a.b) / 2 - (b[2] + b[3]) / 2) / (cssH / 2);
    }
    goal.dist = hi;
    FIT_DIST = hi;
    Object.assign(cam, saved);
    if (instant) { cam.target = [...goal.target]; cam.dist = goal.dist; cam.az = goal.az; cam.el = goal.el; }
    requestRender();
  }

  function updateCamera() {
    const aspect = cssW / cssH;
    const ce = Math.cos(cam.el);
    eye = [
      cam.target[0] + cam.dist * ce * Math.sin(cam.az),
      cam.target[1] + cam.dist * Math.sin(cam.el),
      cam.target[2] + cam.dist * ce * Math.cos(cam.az),
    ];
    const up = cam.el > 1.5 ? [-Math.sin(cam.az), 0, -Math.cos(cam.az)] : [0, 1, 0];
    const view = M4.lookAt(eye, cam.target, up);
    const proj = M4.perspective(FOV, aspect, cam.dist * 0.05, cam.dist * 4);
    proj[8] -= lens[0]; proj[9] -= lens[1]; // shift the image centre into the free area
    VP = M4.mul(proj, view);
  }

  function stepCamera() {
    let moving = false;
    const k = 0.16;
    const step = (a, b) => { const d = b - a; if (Math.abs(d) > 1e-5) moving = true; return a + d * k; };
    cam.dist = step(cam.dist, goal.dist);
    cam.az = step(cam.az, goal.az);
    cam.el = step(cam.el, goal.el);
    for (let i = 0; i < 3; i++) cam.target[i] = step(cam.target[i], goal.target[i]);
    return moving;
  }

  // ------------------------------------------------------------- rendering --
  let needsRender = true, spinning = false, userMoved = false, booted = false;
  const requestRender = () => { needsRender = true; };

  function resize() {
    dpr = Math.min(window.devicePixelRatio || 1, 2);
    cssW = canvas.clientWidth; cssH = canvas.clientHeight;
    for (const c of [canvas, overlay]) {
      c.width = Math.round(cssW * dpr);
      c.height = Math.round(cssH * dpr);
    }
    if (!userMoved && booted) resetView(true);
    requestRender();
  }

  function drawScene(width, height) {
    const t = ease(morphT);
    updateCamera();

    // Shadow pass
    gl.bindFramebuffer(gl.FRAMEBUFFER, shadowFbo);
    gl.viewport(0, 0, SHADOW_SIZE, SHADOW_SIZE);
    gl.clear(gl.DEPTH_BUFFER_BIT);
    gl.enable(gl.DEPTH_TEST);
    gl.enable(gl.POLYGON_OFFSET_FILL);
    gl.polygonOffset(1.5, 3);
    gl.useProgram(progDepth.p);
    gl.uniformMatrix4fv(progDepth.u.u_lvp, false, lightVP);
    gl.uniform1f(progDepth.u.u_t, t);
    gl.uniform1f(progDepth.u.u_cell, CS);
    gl.uniform1f(progDepth.u.u_base, BASE);
    gl.bindVertexArray(vao);
    gl.drawElementsInstanced(gl.TRIANGLES, box.i.length, gl.UNSIGNED_SHORT, 0, N);
    gl.disable(gl.POLYGON_OFFSET_FILL);

    // Main pass
    gl.bindFramebuffer(gl.FRAMEBUFFER, null);
    gl.viewport(0, 0, width, height);
    gl.clearColor(0, 0, 0, 0);
    gl.clear(gl.COLOR_BUFFER_BIT | gl.DEPTH_BUFFER_BIT);
    gl.activeTexture(gl.TEXTURE0);
    gl.bindTexture(gl.TEXTURE_2D, shadowTex);

    gl.useProgram(progTerrain.p);
    const u = progTerrain.u;
    gl.uniformMatrix4fv(u.u_vp, false, VP);
    gl.uniformMatrix4fv(u.u_lvp, false, lightVP);
    gl.uniform1f(u.u_t, t);
    gl.uniform1f(u.u_cell, CS * 1.001);
    gl.uniform1f(u.u_base, BASE);
    gl.uniform1i(u.u_hover, hover);
    gl.uniform1i(u.u_shadow, 0);
    gl.uniform3fv(u.u_L, L);
    gl.uniform3fv(u.u_sun, LIGHT.sun);
    gl.uniform3fv(u.u_sky, LIGHT.sky);
    gl.uniform3fv(u.u_gnd, LIGHT.gnd);
    gl.uniform3fv(u.u_fog, BG.map(toLin));
    gl.uniform3fv(u.u_eye, eye);
    gl.uniform1f(u.u_fogNear, cam.dist * 0.9);
    gl.uniform1f(u.u_fogFar, cam.dist * 1.9);
    gl.uniform1f(u.u_fogAmt, LIGHT.fog);
    gl.drawElementsInstanced(gl.TRIANGLES, box.i.length, gl.UNSIGNED_SHORT, 0, N);

    // Ground shadows, blended over the transparent background.
    gl.enable(gl.BLEND);
    gl.blendFunc(gl.ONE, gl.ONE_MINUS_SRC_ALPHA);
    gl.depthMask(false);
    gl.useProgram(progGround.p);
    const g = progGround.u;
    gl.uniformMatrix4fv(g.u_vp, false, VP);
    gl.uniformMatrix4fv(g.u_lvp, false, lightVP);
    gl.uniform1i(g.u_shadow, 0);
    gl.activeTexture(gl.TEXTURE1);
    gl.bindTexture(gl.TEXTURE_2D, maskTex);
    gl.uniform1i(g.u_mask, 1);
    gl.uniform2f(g.u_grid, NX, NY);
    gl.uniform1f(g.u_cs, CS);
    gl.uniform2f(g.u_shift, L[0] * 0.035, L[2] * 0.035);
    gl.uniform3fv(g.u_shade, LIGHT.shade);
    gl.uniform3fv(g.u_centre, CENTRE);
    gl.uniform1f(g.u_radius, RADIUS);
    gl.bindVertexArray(groundVao);
    gl.drawArrays(gl.TRIANGLES, 0, 6);
    gl.depthMask(true);
    gl.disable(gl.BLEND);
    gl.bindVertexArray(null);
  }

  function frame(now) {
    let animating = false;
    if (morphT < 1) {
      morphT = Math.min(1, (now - morphStart) / MORPH_MS);
      animating = true;
    }
    if (spinning) { goal.az += 0.0016; animating = true; }
    if (stepCamera()) animating = true;
    // Poster mode at the overview; map mode once you zoom in or open a place.
    const navigating = cam.dist < FIT_DIST * 0.85 || !panel.hidden;
    if (navigating !== stage.classList.contains('navigating')) stage.classList.toggle('navigating', navigating);
    if (animating || needsRender) {
      drawScene(canvas.width, canvas.height);
      octx.setTransform(dpr, 0, 0, dpr, 0, 0);
      octx.clearRect(0, 0, cssW, cssH);
      drawLabels(octx, cssW, cssH, reservedRects());
      needsRender = false;
    }
    requestAnimationFrame(frame);
  }

  // ---------------------------------------------------------------- labels --
  const css = getComputedStyle(document.documentElement);
  const INK = css.getPropertyValue('--ink').trim();
  const INK2 = css.getPropertyValue('--ink-2').trim();
  const BGHEX = css.getPropertyValue('--bg').trim();
  const FONT = '"Archivo", "Helvetica Neue", Helvetica, Arial, sans-serif';
  const nMax = Math.max(...D.groups.n);
  const fmt = (n) => Math.round(n).toLocaleString('en-GB');

  // Places: optional layers of keyword-named regions computed in R.
  const PLACES = D.places || null;
  const LAYERS = PLACES ? PLACES.layers : [];
  const titleCase = (t) => t.replace(/(^|[\s·-])(\p{L})/gu, (m, a, b) => a + b.toUpperCase());
  const regionName = (r, n = 2) => (r.keywords || []).slice(0, n).join(' · ');

  const labels = [];
  if (GROUPED) {
    D.groups.label.forEach((text, g) => labels.push({
      kind: 'group', g, owner: g, text: String(text).toUpperCase(), n: D.groups.n[g],
      k: cellOf[D.groups.anchor[g]], minZoom: 0, choice: 0, hit: null,
    }));
  }
  LAYERS.forEach((layer, li) => {
    const nTop = Math.max(...layer.regions.map((r) => r.n));
    layer.regions.forEach((r, i) => {
      if (!r.keywords.length || r.n < 5) return;
      const text = regionName(r).toLowerCase();
      // The same place can be named at two zoom levels; keep the coarser one.
      if (labels.some((l) => l.kind === 'region' && l.text === text)) return;
      labels.push({
        kind: 'region', level: layer.level, li, idx: i, owner: r.owner,
        text, n: r.n, rel: r.n / nTop,
        k: cellOf[r.anchor], minZoom: layer.level === 1 ? 0 : 1.35, choice: 0, hit: null,
      });
    });
  });
  const rank = (l) => (l.kind === 'group' ? 0 : l.level);
  labels.sort((a, b) => rank(a) - rank(b) || b.n - a.n);

  function spacedWidth(ctx, s, sp) {
    let w = 0;
    for (const ch of s) w += ctx.measureText(ch).width + sp;
    return w - sp;
  }
  function drawSpaced(ctx, s, x, y, sp, mode) {
    // x is the left edge
    for (const ch of s) {
      if (mode === 'stroke') ctx.strokeText(ch, x, y); else ctx.fillText(ch, x, y);
      x += ctx.measureText(ch).width + sp;
    }
  }
  const project = (p) => {
    const c = M4.apply(VP, [p[0], p[1], p[2], 1]);
    if (c[3] <= 0) return null;
    return [(c[0] / c[3] + 1) * 0.5, (1 - c[1] / c[3]) * 0.5];
  };
  const overlaps = (a, b) => a[0] < b[2] && a[2] > b[0] && a[1] < b[3] && a[3] > b[1];

  function reservedRects(forExport = false) {
    const out = [];
    const ids = stage.classList.contains('navigating') && !forExport
      ? ['toolbar', 'legend']
      : ['kicker', 'title', 'toolbar', 'legend', 'caption'];
    if (!forExport) ids.push('panel', 'search-wrap');
    for (const id of ids) {
      const node = document.getElementById(id);
      if (!node || node.hidden || (!node.textContent.trim() && id !== 'toolbar' && id !== 'search-wrap')) continue;
      const r = node.getBoundingClientRect();
      if (r.width) out.push([r.left - 6, r.top - 6, r.right + 6, r.bottom + 6]);
    }
    return out;
  }

  // Draws group and place labels. `scale` lets the PNG export reuse it.
  function drawLabels(ctx, W, H, reserved, scale = 1) {
    if (!labels.length) return;
    const placed = reserved.map((r) => r.map((v) => v * scale));
    const all = allActive();
    const zoom = FIT_DIST / cam.dist;
    const shrink = Math.min(1, Math.max(0.78, W / scale / 1100));
    for (const lab of labels) {
      if (scale === 1) lab.hit = null;
      if (lab.k < 0) continue;
      const isGroup = lab.kind === 'group';
      const on = all || lab.owner < 0 || active.has(lab.owner);
      if (!isGroup && !on) continue;
      let alpha = isGroup ? (on ? 1 : 0.22) : clamp01((zoom - lab.minZoom) / 0.2 + (lab.minZoom === 0 ? 1 : 0));
      if (alpha <= 0.01) continue;
      const p = project([offsets[2 * lab.k], curH(lab.k), offsets[2 * lab.k + 1]]);
      if (!p) continue;
      const ax = p[0] * W, ay = p[1] * H;

      let fs, sp, weight, sub = '', sfs = 0, sw = 0;
      if (isGroup) {
        fs = (10 + 6 * Math.sqrt(lab.n / nMax)) * scale * shrink;
        sp = fs * 0.14; weight = 600;
        sub = fmt(lab.n); sfs = fs * 0.72;
        ctx.font = `400 ${sfs}px ${FONT}`;
        sw = ctx.measureText(sub).width;
      } else {
        fs = (lab.level === 1 ? 10 + 3 * Math.sqrt(lab.rel) : 9 + 2.5 * Math.sqrt(lab.rel)) * scale * shrink;
        sp = fs * 0.05; weight = lab.level === 1 ? 600 : 500;
      }
      ctx.font = `${weight} ${fs}px ${FONT}`;
      const tw = spacedWidth(ctx, lab.text, sp);
      const bw = Math.max(tw, sw) + 8 * scale;
      const bh = fs + (sub ? sfs + 6 * scale : 3 * scale);

      const leaders = (isGroup ? [18, 30, 44, 60, 80, 104, 132, 165, 200] : [8, 16, 26, 38]).map((v) => v * scale);
      const cands = [];
      for (const L of leaders) for (const dx of (isGroup ? [0, -0.45, 0.45, -0.9, 0.9] : [0, -0.45, 0.45])) cands.push([L, dx * bw]);
      // Fallback for peaks that reach the title: sit beside the anchor instead.
      for (const L of [-bh / 2, 0, 20 * scale]) for (const side of [1, -1]) cands.push([L, side * (bw / 2 + 14 * scale)]);
      // Last resort for anchors tucked under the title: hang the label below.
      if (isGroup) for (const L of [16, 40, 70]) for (const dx of [0, -0.45, 0.45]) cands.push([-(bh + L * scale), dx * bw]);
      const order = [lab.choice, ...cands.keys()];
      let best = -1, box = null;
      for (const c of order) {
        const [L, dx] = cands[c];
        const cx = ax + dx, bottom = ay - L;
        const b = [cx - bw / 2, bottom - bh, cx + bw / 2, bottom];
        if (b[0] < 4 || b[2] > W - 4 || b[1] < 4 || b[3] > H - 4) continue;
        if (placed.some((q) => overlaps(b, q))) continue;
        best = c; box = b; break;
      }
      if (best < 0) continue;
      lab.choice = best;
      placed.push(box);
      if (scale === 1) lab.hit = box;
      const [L, dx] = cands[best];
      const lx = ax + dx, top = box[1];

      ctx.save();
      ctx.strokeStyle = INK;
      ctx.lineWidth = (isGroup ? 1 : 0.8) * scale;
      ctx.globalAlpha = alpha * (isGroup ? 0.6 : 0.4);
      ctx.beginPath();
      ctx.moveTo(ax, ay);
      if (Math.abs(dx) > bw / 2) ctx.lineTo(dx > 0 ? box[0] : box[2], Math.min(box[3] - 2 * scale, Math.max(box[1] + 2 * scale, ay)));
      else if (L < -bh / 2) ctx.lineTo(lx, box[1] - 2 * scale);
      else ctx.lineTo(lx, ay - L + 2 * scale);
      ctx.stroke();
      ctx.globalAlpha = alpha;
      ctx.fillStyle = INK;
      ctx.beginPath();
      ctx.arc(ax, ay, (isGroup ? 2 : 1.4) * scale, 0, Math.PI * 2);
      ctx.fill();
      // text with halo
      ctx.textBaseline = 'alphabetic';
      ctx.lineJoin = 'round';
      ctx.strokeStyle = BGHEX;
      ctx.lineWidth = (isGroup ? 3.5 : 3) * scale;
      ctx.font = `${weight} ${fs}px ${FONT}`;
      ctx.fillStyle = isGroup ? INK : INK2;
      const ty = top + fs;
      drawSpaced(ctx, lab.text, lx - tw / 2, ty, sp, 'stroke');
      drawSpaced(ctx, lab.text, lx - tw / 2, ty, sp, 'fill');
      if (isGroup) {
        ctx.font = `400 ${sfs}px ${FONT}`;
        ctx.fillStyle = INK2;
        ctx.strokeText(sub, lx - sw / 2, ty + sfs + 3 * scale);
        ctx.fillText(sub, lx - sw / 2, ty + sfs + 3 * scale);
        // colour tick under the label
        ctx.fillStyle = D.groups.colour[lab.g];
        ctx.fillRect(lx - 6 * scale, ty + sfs + 6 * scale, 12 * scale, 2 * scale);
      }
      ctx.restore();
    }
  }
  const labelAt = (x, y) => labels.find((l) => l.hit && x >= l.hit[0] && x <= l.hit[2] && y >= l.hit[1] && y <= l.hit[3]);

  // --------------------------------------------------------------- picking --
  let hover = -1;
  function pick(mx, my) {
    const inv = M4.invert(VP);
    const nx = (mx / cssW) * 2 - 1, ny = 1 - (my / cssH) * 2;
    const a = M4.apply(inv, [nx, ny, -1, 1]), b = M4.apply(inv, [nx, ny, 1, 1]);
    const o = [a[0] / a[3], a[1] / a[3], a[2] / a[3]];
    const e = [b[0] / b[3], b[1] / b[3], b[2] / b[3]];
    const d = [e[0] - o[0], e[1] - o[1], e[2] - o[2]];
    const len = Math.hypot(...d);
    d[0] /= len; d[1] /= len; d[2] /= len;
    // Clip the ray to the slab between the ground and the highest peak.
    const top = BASE + MAXH + 0.02;
    let t0 = 0, t1 = len;
    if (Math.abs(d[1]) > 1e-6) {
      const ta = (top - o[1]) / d[1], tb = (0 - o[1]) / d[1];
      t0 = Math.max(t0, Math.min(ta, tb));
      t1 = Math.min(t1, Math.max(ta, tb));
    }
    const step = CS * 0.25;
    for (let t = t0; t <= t1; t += step) {
      const x = o[0] + d[0] * t, y = o[1] + d[1] * t, z = o[2] + d[2] * t;
      const i = Math.floor(x / CS + NX / 2), j = Math.floor(-z / CS + NY / 2);
      if (i < 0 || j < 0 || i >= NX || j >= NY) continue;
      const k = cellOf[i + j * NX];
      if (k >= 0 && y <= curH(k)) return k;
    }
    return -1;
  }

  const tooltip = document.getElementById('tooltip');
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;' })[c]);
  function nearbyCount(k, r) {
    let s = 0;
    for (let dj = -r; dj <= r; dj++)
      for (let di = -r; di <= r; di++) {
        const i = ci[k] + di, j = cj[k] + dj;
        if (i < 0 || j < 0 || i >= NX || j >= NY) continue;
        const m = cellOf[i + j * NX];
        if (m >= 0) s += D.raw[m];
      }
    return s;
  }
  function sampleNear(k) {
    for (let r = 0; r <= 2; r++)
      for (let dj = -r; dj <= r; dj++)
        for (let di = -r; di <= r; di++) {
          const id = ci[k] + di + (cj[k] + dj) * NX;
          if (samples.has(id)) return samples.get(id);
        }
    return null;
  }
  function showTooltip(k, mx, my) {
    const rows = [];
    for (let q = gStart[k]; q < gStart[k + 1]; q++) rows.push([D.gIdx[q], D.gVal[q] / dTotal[k]]);
    rows.sort((a, b) => b[1] - a[1]);
    const near = nearbyCount(k, 2);
    let html = `<div class="tt-head"><b>${fmt(D.raw[k])}</b> ${D.raw[k] === 1 ? 'document' : 'documents'} in this column, <b>${fmt(near)}</b> in the 5 × 5 area around it</div>`;
    if (ENRICH) {
      // Say why the column has its colour: the group most above its usual share.
      const e = enrichment(k);
      const strength = e.g < 0 ? 0 : Math.log(e.lift) / Math.log(3.5);
      if (strength > 0.1) {
        const sh = rows.find((r) => r[0] === e.g);
        const ratio = sh ? sh[1] / BASE_SHARE[e.g] : 1;
        html += `<div class="tt-why"><span class="dot" style="background:${D.groups.colour[e.g]}"></span>` +
          `<span>Coloured <b>${esc(D.groups.label[e.g])}</b> because it is ${ratio.toFixed(1)}× its usual share here.</span></div>`;
      } else {
        html += `<div class="tt-why"><span class="dot ghost"></span><span>Close to the overall mix, so no group colour.</span></div>`;
      }
    }
    if (GROUPED) {
      html += rows.slice(0, 5).map(([g, s]) =>
        `<div class="row"><span class="dot" style="background:${D.groups.colour[g]}"></span>` +
        `<span class="name">${esc(D.groups.label[g])}</span>` +
        `<span class="track"><span class="fill" style="width:${(s * 100).toFixed(1)}%;background:${D.groups.colour[g]}"></span>` +
        (ENRICH ? `<span class="tick" style="left:${(BASE_SHARE[g] * 100).toFixed(1)}%"></span>` : '') + `</span>` +
        `<span class="pct">${Math.round(s * 100)}%${ENRICH ? ` <i>${(s / BASE_SHARE[g]).toFixed(1)}×</i>` : ''}</span></div>`).join('');
      if (ENRICH) html += `<div class="tt-note">Shares of the documents in and around this column. The tick and the × figure compare each share with the whole map.</div>`;
    } else {
      const pct = Math.round(heightOf(dTotal[k]) * 100);
      html += `<div class="row"><span class="dot" style="background:${'#' + ramp(heightOf(dTotal[k])).map((v) => Math.round(v * 255).toString(16).padStart(2, '0')).join('')}"></span><span class="name">Relative height</span><span class="track"><span class="fill" style="width:${pct}%;background:var(--ink-2)"></span></span><span class="pct">${pct}%</span></div>`;
    }
    const s = sampleNear(k);
    if (s) html += `<div class="sample">“${esc(s)}”</div>`;
    tooltip.innerHTML = html;
    tooltip.hidden = false;
    const r = tooltip.getBoundingClientRect();
    let x = mx + 16, y = my + 16;
    if (x + r.width > cssW - 8) x = mx - r.width - 16;
    if (y + r.height > cssH - 8) y = my - r.height - 16;
    tooltip.style.left = `${Math.max(8, x)}px`;
    tooltip.style.top = `${Math.max(8, y)}px`;
  }
  function setHover(k, mx, my) {
    if (k !== hover) { hover = k; requestRender(); }
    if (k >= 0) showTooltip(k, mx, my); else tooltip.hidden = true;
  }

  // ------------------------------------------------------------- controls --
  const pointers = new Map();
  let drag = null, pinchDist = 0, movedSinceDown = 0;
  const hint = document.getElementById('hint');

  canvas.addEventListener('contextmenu', (e) => e.preventDefault());
  canvas.addEventListener('pointerdown', (e) => {
    canvas.setPointerCapture(e.pointerId);
    pointers.set(e.pointerId, [e.clientX, e.clientY]);
    drag = { x: e.clientX, y: e.clientY, pan: e.button === 2 || e.shiftKey || e.metaKey || e.ctrlKey };
    movedSinceDown = 0;
    canvas.classList.add('dragging');
    if (pointers.size === 2) {
      const [a, b] = [...pointers.values()];
      pinchDist = Math.hypot(a[0] - b[0], a[1] - b[1]);
    }
  });
  canvas.addEventListener('pointermove', (e) => {
    if (!drag) {
      const r = canvas.getBoundingClientRect();
      const mx = e.clientX - r.left, my = e.clientY - r.top;
      const onLabel = labelAt(mx, my);
      canvas.style.cursor = onLabel ? 'pointer' : '';
      setHover(onLabel ? -1 : pick(mx, my), mx, my);
      return;
    }
    pointers.set(e.pointerId, [e.clientX, e.clientY]);
    if (pointers.size === 2) {
      const [a, b] = [...pointers.values()];
      const d = Math.hypot(a[0] - b[0], a[1] - b[1]);
      if (pinchDist) goal.dist = clampDist(goal.dist * (pinchDist / d));
      pinchDist = d;
      return;
    }
    const dx = e.clientX - drag.x, dy = e.clientY - drag.y;
    drag.x = e.clientX; drag.y = e.clientY;
    movedSinceDown += Math.abs(dx) + Math.abs(dy);
    if (drag.pan) {
      const s = (goal.dist * Math.tan(FOV / 2) * 2) / cssH;
      const ca = Math.cos(goal.az), sa = Math.sin(goal.az);
      const fwd = Math.max(0.35, Math.sin(goal.el));
      goal.target[0] += (-dx * ca - (dy * sa) / fwd) * s;
      goal.target[2] += (dx * sa - (dy * ca) / fwd) * s;
    } else {
      goal.az -= dx * 0.006;
      goal.el = Math.min(1.565, Math.max(0.12, goal.el + dy * 0.005));
    }
    if (hover >= 0) setHover(-1);
    userMoved = true;
    hint.classList.add('gone');
  });
  const endDrag = (e) => {
    if (e.type === 'pointerup' && pointers.size === 1 && movedSinceDown < 5 && e.button === 0) {
      const r = canvas.getBoundingClientRect();
      handleClick(e.clientX - r.left, e.clientY - r.top);
    }
    pointers.delete(e.pointerId);
    if (pointers.size < 2) pinchDist = 0;
    if (pointers.size === 0) { drag = null; canvas.classList.remove('dragging'); }
  };
  canvas.addEventListener('pointerup', endDrag);
  canvas.addEventListener('pointercancel', endDrag);
  canvas.addEventListener('pointerleave', () => { if (!drag) setHover(-1); });
  const clampDist = (d) => Math.min(RADIUS * 12, Math.max(RADIUS * 0.35, d));
  canvas.addEventListener('wheel', (e) => {
    e.preventDefault();
    goal.dist = clampDist(goal.dist * Math.exp(e.deltaY * 0.0012));
    userMoved = true;
    hint.classList.add('gone');
  }, { passive: false });

  const btnFlat = document.getElementById('btn-flat');
  const btnSpin = document.getElementById('btn-spin');
  function toggleFlat() {
    flat = !flat;
    btnFlat.setAttribute('aria-pressed', String(flat));
    resetView(false, true);
    startMorph();
  }
  function toggleSpin() {
    spinning = !spinning;
    btnSpin.setAttribute('aria-pressed', String(spinning));
  }
  document.getElementById('btn-reset').addEventListener('click', () => { userMoved = false; resetView(false); });
  btnFlat.addEventListener('click', toggleFlat);
  btnSpin.addEventListener('click', toggleSpin);
  document.getElementById('btn-png').addEventListener('click', () => exportPng());
  window.addEventListener('keydown', (e) => {
    if (e.target.closest && e.target.closest('input, textarea')) return;
    const key = e.key.toLowerCase();
    const panStep = goal.dist * 0.08;
    const ca = Math.cos(goal.az), sa = Math.sin(goal.az);
    const move = (fx, fz) => { goal.target[0] += (fx * ca + fz * sa) * panStep; goal.target[2] += (-fx * sa + fz * ca) * panStep; userMoved = true; e.preventDefault(); };
    if (key === 'arrowleft') move(-1, 0);
    else if (key === 'arrowright') move(1, 0);
    else if (key === 'arrowup') move(0, -1);
    else if (key === 'arrowdown') move(0, 1);
    else if (key === '+' || key === '=') { goal.dist = clampDist(goal.dist * 0.8); userMoved = true; }
    else if (key === '-' || key === '_') { goal.dist = clampDist(goal.dist * 1.25); userMoved = true; }
    else if (key === '/') { e.preventDefault(); searchBox.focus(); }
    else if (key === 'r') { userMoved = false; resetView(false); }
    else if (key === 'f') toggleFlat();
    else if (key === 's') toggleSpin();
    else if (key === 'p') exportPng();
    else if (key === 'escape') { if (!panel.hidden) closePanel(); else setActive(null); }
  });

  // ---------------------------------------------------------------- places --
  // Clicking a label or the terrain flies the camera there and opens a panel
  // describing the place: its size, make-up, distinctive words and examples.
  const panel = document.getElementById('panel');
  const el = (tag, cls, text) => {
    const n = document.createElement(tag);
    if (cls) n.className = cls;
    if (text != null) n.textContent = text;
    return n;
  };
  const pct = (v) => `${Math.round(v * 100)}%`;
  const swatch = (g) => { const d = el('span', 'dot'); d.style.background = D.groups.colour[g]; return d; };

  function flyTo(k, factor) {
    goal.target = [offsets[2 * k], 0, offsets[2 * k + 1]];
    goal.dist = clampDist(FIT_DIST * factor);
    userMoved = true;
    hint.classList.add('gone');
  }

  function section(title) {
    const h = el('div', 'p-sec', title);
    panel.appendChild(h);
  }
  function words(list) {
    if (!list || !list.length) return;
    section('Distinctive words');
    const w = el('div', 'p-words');
    for (const t of list) w.appendChild(el('span', 'word', t));
    panel.appendChild(w);
  }
  function samplesBlock(list) {
    if (!list || !list.length) return;
    section('From the documents');
    for (const t of list) panel.appendChild(el('blockquote', 'p-quote', t));
  }
  function placeList(title, items) {
    if (!items.length) return;
    section(title);
    const ul = el('div', 'p-list');
    for (const it of items) {
      const b = el('button', 'p-item');
      if (it.g != null) b.appendChild(swatch(it.g));
      b.appendChild(el('span', 'p-name', it.name));
      b.appendChild(el('span', 'p-n', it.note));
      b.addEventListener('click', it.go);
      ul.appendChild(b);
    }
    panel.appendChild(ul);
  }
  function compBar(comp, n) {
    if (!GROUPED || !n || Math.max(...comp) / n > 0.97) return;
    const bar = el('div', 'p-bar');
    const order = comp.map((c, g) => [c, g]).filter((x) => x[0] > 0).sort((a, b) => b[0] - a[0]);
    for (const [c, g] of order) {
      const seg = el('span');
      seg.style.width = `${(c / n) * 100}%`;
      seg.style.background = D.groups.colour[g];
      bar.appendChild(seg);
    }
    panel.appendChild(bar);
    const rows = el('div', 'p-comp');
    for (const [c, g] of order.slice(0, 4)) {
      const r = el('div', 'p-row');
      r.appendChild(swatch(g));
      r.appendChild(el('span', 'p-name', D.groups.label[g]));
      const share = c / n;
      const note = ENRICH ? `${pct(share)} · ${(share / BASE_SHARE[g]).toFixed(1)}× usual` : pct(share);
      r.appendChild(el('span', 'p-n', note));
      rows.appendChild(r);
    }
    panel.appendChild(rows);
  }
  function openPanel(head, title) {
    panel.replaceChildren();
    const close = el('button', 'p-close', '×');
    close.setAttribute('aria-label', 'Close');
    close.addEventListener('click', closePanel);
    panel.appendChild(close);
    panel.appendChild(el('div', 'p-kicker', head));
    panel.appendChild(el('h2', 'p-title', title));
    panel.hidden = false;
    panel.scrollTop = 0;
  }
  function closePanel() { panel.hidden = true; requestRender(); }

  function focusGroup(g) {
    flyTo(cellOf[D.groups.anchor[g]], 0.6);
    const info = PLACES ? PLACES.groups[g] : null;
    openPanel(`${fmt(D.groups.n[g])} documents · ${pct(D.groups.n[g] / D.nDocs)} of the map`, D.groups.label[g]);
    const iso = el('button', 'p-action', !allActive() && active.size === 1 && active.has(g) ? 'Show every group' : 'Isolate on the map');
    iso.addEventListener('click', () => {
      setActive(!allActive() && active.size === 1 && active.has(g) ? null : new Set([g]));
      iso.textContent = !allActive() ? 'Show every group' : 'Isolate on the map';
    });
    panel.appendChild(iso);
    if (info) words(info.keywords);
    const fine = LAYERS.length - 1;
    if (fine >= 0) {
      const regs = LAYERS[fine].regions.map((r, i) => ({ r, i }));
      let items;
      if (ENRICH) {
        // Where this group is most over-represented.
        items = regs.filter(({ r }) => r.n >= 20)
          .map(({ r, i }) => ({ r, i, lift: (r.comp[g] / r.n) / BASE_SHARE[g] }))
          .filter((x) => x.lift > 1.15).sort((a, b) => b.lift - a.lift).slice(0, 6)
          .map(({ r, i, lift }) => ({ name: regionName(r, 3), note: `${lift.toFixed(1)}×`, go: () => focusRegion(fine, i) }));
        placeList('Where it is over-represented', items);
      } else {
        items = regs.filter(({ r }) => r.owner === g).sort((a, b) => b.r.n - a.r.n)
          .map(({ r, i }) => ({ name: regionName(r, 3), note: fmt(r.n), go: () => focusRegion(fine, i) }));
        placeList('Places within', items);
      }
    }
    if (info) samplesBlock(info.samples);
  }

  function focusRegion(li, i) {
    const layer = LAYERS[li], r = layer.regions[i];
    flyTo(cellOf[r.anchor], layer.level === 1 ? 0.55 : 0.36);
    const where = r.owner >= 0 ? ` · in ${D.groups.label[r.owner]}` : '';
    openPanel(`Place · ${fmt(r.n)} documents${where}`, titleCase(regionName(r, 3)));
    compBar(r.comp, r.n);
    words(r.keywords);
    if (layer.level === 1 && LAYERS[li + 1]) {
      const fine = LAYERS[li + 1];
      const items = fine.regions.map((f, j) => ({ f, j }))
        .filter(({ f }) => layer.cellRegion[cellOf[f.anchor]] === i)
        .sort((a, b) => b.f.n - a.f.n)
        .map(({ f, j }) => ({ name: regionName(f, 3), note: fmt(f.n), go: () => focusRegion(li + 1, j) }));
      placeList('Places within', items);
    }
    if (r.owner >= 0) placeList('Part of', [{ g: r.owner, name: D.groups.label[r.owner], note: fmt(D.groups.n[r.owner]), go: () => focusGroup(r.owner) }]);
    samplesBlock(r.samples);
  }

  function focusLabel(lab) {
    if (lab.kind === 'group') focusGroup(lab.g); else focusRegion(lab.li, lab.idx);
  }

  function handleClick(mx, my) {
    const lab = labelAt(mx, my);
    if (lab) return focusLabel(lab);
    const k = pick(mx, my);
    if (k < 0) return closePanel();
    for (let li = LAYERS.length - 1; li >= 0; li--) {
      const r = LAYERS[li].cellRegion[k];
      if (r >= 0) return focusRegion(li, r);
    }
    if (GROUPED) {
      let best = -1, bv = 0;
      for (let q = gStart[k]; q < gStart[k + 1]; q++) if (D.gVal[q] > bv) { bv = D.gVal[q]; best = D.gIdx[q]; }
      if (best >= 0) return focusGroup(best);
    }
    flyTo(k, 0.5);
  }

  // Search over group names, place names and their distinctive words.
  const searchWrap = document.getElementById('search-wrap');
  const searchBox = document.getElementById('search');
  const results = document.getElementById('results');
  const index = [];
  if (GROUPED) D.groups.label.forEach((t, g) => index.push({
    name: String(t), kind: 'Group', g, words: PLACES ? PLACES.groups[g].keywords : [], n: D.groups.n[g], go: () => focusGroup(g),
  }));
  LAYERS.forEach((layer, li) => layer.regions.forEach((r, i) => {
    if (!r.keywords.length) return;
    index.push({
      name: titleCase(regionName(r, 3)), kind: r.owner >= 0 ? `Place in ${D.groups.label[r.owner]}` : 'Place',
      g: r.owner >= 0 ? r.owner : null, words: r.keywords, n: r.n, go: () => focusRegion(li, i),
    });
  }));
  if (!index.length) searchWrap.hidden = true;
  let hits = [];
  function runSearch() {
    const q = searchBox.value.trim().toLowerCase();
    results.replaceChildren();
    if (!q) { results.hidden = true; hits = []; return; }
    hits = index.map((e) => {
      const name = e.name.toLowerCase();
      let score = name.startsWith(q) ? 3 : name.includes(q) ? 2 : 0;
      const wi = e.words.findIndex((w) => w.startsWith(q));
      if (wi >= 0) score = Math.max(score, 1.5 - wi * 0.1);
      return { e, score };
    }).filter((x) => x.score > 0).sort((a, b) => b.score - a.score || b.e.n - a.e.n).slice(0, 8).map((x) => x.e);
    for (const e of hits) {
      const b = el('button', 'r-item');
      if (e.g != null) b.appendChild(swatch(e.g)); else b.appendChild(el('span', 'dot ghost'));
      const txt = el('span', 'r-text');
      txt.appendChild(el('span', 'r-name', e.name));
      txt.appendChild(el('span', 'r-kind', `${e.kind} · ${fmt(e.n)}`));
      b.appendChild(txt);
      b.addEventListener('click', () => { e.go(); results.hidden = true; });
      results.appendChild(b);
    }
    if (!hits.length) results.appendChild(el('div', 'r-empty', 'No places match'));
    results.hidden = false;
  }
  searchBox.addEventListener('input', runSearch);
  searchBox.addEventListener('keydown', (e) => {
    if (e.key === 'Enter' && hits[0]) { hits[0].go(); results.hidden = true; searchBox.blur(); }
    if (e.key === 'Escape') { searchBox.value = ''; runSearch(); searchBox.blur(); }
  });
  searchBox.addEventListener('focus', runSearch);
  document.addEventListener('pointerdown', (e) => { if (!searchWrap.contains(e.target)) results.hidden = true; });

  // ---------------------------------------------------------------- legend --
  const legend = document.getElementById('legend');
  function setActive(set) {
    active = set && set.size ? set : new Set(Array.from({ length: G }, (_, i) => i));
    for (const chip of legend.querySelectorAll('.chip')) {
      const g = +chip.dataset.g;
      const all = allActive();
      chip.classList.toggle('on', !all && active.has(g));
      chip.classList.toggle('off', !all && !active.has(g));
      chip.setAttribute('aria-pressed', String(!all && active.has(g)));
    }
    startMorph();
  }
  if (GROUPED) {
    const order = D.groups.n.map((n, g) => [n, g]).sort((a, b) => b[0] - a[0]).map((x) => x[1]);
    for (const g of order) {
      const b = document.createElement('button');
      b.className = 'chip';
      b.dataset.g = g;
      b.setAttribute('aria-pressed', 'false');
      b.innerHTML = `<span class="dot" style="background:${D.groups.colour[g]}"></span>` +
        `<span class="lab">${esc(D.groups.label[g])}</span><span class="n">${fmt(D.groups.n[g])}</span>`;
      b.addEventListener('click', (e) => {
        if (e.shiftKey || e.metaKey || e.ctrlKey) {
          const s = allActive() ? new Set() : new Set(active);
          if (s.has(g)) s.delete(g); else s.add(g);
          setActive(s);
        } else if (!allActive() && active.size === 1 && active.has(g)) {
          setActive(null);
        } else {
          setActive(new Set([g]));
        }
      });
      legend.appendChild(b);
    }
    if (ENRICH) {
      const note = document.createElement('div');
      note.id = 'legend-note';
      note.textContent = 'Colour marks where each group is over-represented';
      legend.prepend(note);
    }
  } else {
    const stops = Array.from({ length: 9 }, (_, i) => {
      const c = ramp(i / 8).map((v) => Math.round(v * 255));
      return `rgb(${c.join(',')}) ${(i / 8) * 100}%`;
    });
    legend.innerHTML = `<div class="ramp"><span>Sparse</span><span class="bar" style="background:linear-gradient(90deg,${stops.join(',')})"></span><span>Dense</span></div>`;
  }

  // ----------------------------------------------------------------- text --
  document.getElementById('kicker').textContent = D.subtitle;
  document.getElementById('title').textContent = D.title;
  document.getElementById('caption').textContent = D.caption ||
    `${fmt(D.nDocs)} documents · ${NX} × ${NY} bins` +
    (D.nDrawn < D.nDocs ? ` · ${fmt(D.nDocs - D.nDrawn)} in sparse outskirts not drawn` : '');
  if (!D.title && !D.subtitle) document.getElementById('masthead').hidden = true;

  // ---------------------------------------------------------------- export --
  async function exportPng() {
    // Up to 3x, but capped near 10 megapixels: the canvas is resized for the
    // export, and its antialiased buffers briefly need several times that in GPU memory.
    const S = Math.min(3, 8192 / Math.max(cssW, cssH), Math.sqrt(10e6 / (cssW * cssH)));
    const W = Math.round(cssW * S), H = Math.round(cssH * S);
    // Capture the settled view rather than a frame mid-animation.
    cam.target = [...goal.target]; cam.dist = goal.dist; cam.az = goal.az; cam.el = goal.el;
    morphT = 1;
    const prevHover = hover;
    hover = -1;
    canvas.width = W; canvas.height = H;
    drawScene(W, H);

    const out = document.createElement('canvas');
    out.width = W; out.height = H;
    const ctx = out.getContext('2d');
    const bg2 = css.getPropertyValue('--bg-2').trim();
    const grad = ctx.createRadialGradient(W / 2, H * 0.45, 0, W / 2, H * 0.45, Math.max(W, H) * 0.75);
    grad.addColorStop(0, BGHEX); grad.addColorStop(0.45, BGHEX); grad.addColorStop(1, bg2);
    ctx.fillStyle = grad;
    ctx.fillRect(0, 0, W, H);
    ctx.drawImage(canvas, 0, 0);

    drawLabels(ctx, W, H, reservedRects(true), S);
    drawDomText(ctx, S);

    hover = prevHover;
    resize();
    const a = document.createElement('a');
    a.download = `${(D.title || 'landscape').toLowerCase().replace(/[^a-z0-9]+/g, '-')}.png`;
    a.href = out.toDataURL('image/png');
    a.click();
  }

  // Re-draws the HTML masthead, legend and caption onto the export canvas at
  // the positions the browser laid them out.
  function drawDomText(ctx, S) {
    const text = (el, weight, ls) => {
      if (!el || !el.textContent.trim() || el.closest('[hidden]')) return;
      const cs = getComputedStyle(el);
      const r = el.getBoundingClientRect();
      const fs = parseFloat(cs.fontSize) * S;
      ctx.font = `${weight} ${fs}px ${FONT}`;
      if ('fontStretch' in ctx && cs.fontStretch !== '100%') ctx.fontStretch = 'semi-expanded';
      ctx.fillStyle = cs.color;
      ctx.textBaseline = 'middle';
      const s = cs.textTransform === 'uppercase' ? el.textContent.toUpperCase() : el.textContent;
      const sp = fs * ls;
      const w = spacedWidth(ctx, s, sp);
      drawSpaced(ctx, s, ((r.left + r.right) / 2) * S - w / 2, ((r.top + r.bottom) / 2) * S, sp, 'fill');
      if ('fontStretch' in ctx) ctx.fontStretch = 'normal';
    };
    text(document.getElementById('kicker'), 500, 0.22);
    text(document.getElementById('title'), 800, 0.01);
    text(document.getElementById('caption'), 500, 0.18);
    text(document.getElementById('legend-note'), 500, 0.14);
    for (const chip of legend.querySelectorAll('.chip')) {
      const r = chip.getBoundingClientRect();
      const dot = chip.querySelector('.dot').getBoundingClientRect();
      ctx.globalAlpha = parseFloat(getComputedStyle(chip).opacity);
      ctx.fillStyle = D.groups.colour[+chip.dataset.g];
      ctx.beginPath();
      ctx.arc((dot.left + dot.width / 2) * S, (dot.top + dot.height / 2) * S, (dot.width / 2) * S, 0, Math.PI * 2);
      ctx.fill();
      const lab = chip.querySelector('.lab'), n = chip.querySelector('.n');
      const lr = lab.getBoundingClientRect(), nr = n.getBoundingClientRect();
      const fs = parseFloat(getComputedStyle(lab).fontSize) * S;
      ctx.textBaseline = 'middle';
      ctx.font = `500 ${fs}px ${FONT}`;
      ctx.fillStyle = INK;
      drawSpaced(ctx, lab.textContent.toUpperCase(), lr.left * S, ((r.top + r.bottom) / 2) * S, fs * 0.08, 'fill');
      ctx.font = `400 ${fs}px ${FONT}`;
      ctx.fillStyle = css.getPropertyValue('--ink-3').trim();
      ctx.fillText(n.textContent, nr.left * S, ((r.top + r.bottom) / 2) * S);
      ctx.globalAlpha = 1;
    }
    const ramp = legend.querySelector('.ramp');
    if (ramp) {
      const bar = ramp.querySelector('.bar').getBoundingClientRect();
      const g = ctx.createLinearGradient(bar.left * S, 0, bar.right * S, 0);
      for (let i = 0; i <= 8; i++) {
        const c = rampColour(i / 8);
        g.addColorStop(i / 8, c);
      }
      ctx.fillStyle = g;
      ctx.fillRect(bar.left * S, bar.top * S, bar.width * S, bar.height * S);
      for (const sp of ramp.querySelectorAll('span:not(.bar)')) text(sp, 500, 0.16);
    }
  }
  const rampColour = (t) => `rgb(${ramp(t).map((v) => Math.round(v * 255)).join(',')})`;

  // ------------------------------------------------------------------ boot --
  computeTarget();
  hFrom.set(hTo); topFrom.set(topTo); botFrom.set(botTo); occFrom.set(occTo);
  // Rise from flat on load.
  hFrom.fill(0);
  uploadInstances();
  morphStart = performance.now();
  morphT = 0;
  resize();
  booted = true;
  resetView(true);
  new ResizeObserver(resize).observe(canvas);
  if (document.fonts) document.fonts.ready.then(() => { if (!userMoved) resetView(true); requestRender(); });
  requestAnimationFrame(frame);
  window.__terrain = { exportPng, setActive, cam, goal, resetView, toggleFlat, labels, focusGroup, focusRegion };
})();
