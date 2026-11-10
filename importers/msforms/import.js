// panform -> Microsoft Forms importer.
//
// Paste this whole file into the DevTools console of a logged-in Microsoft
// Forms editor tab that has an existing form open (the URL has `&id=...`),
// then run:
//
//   await panformImport(json)   // json: panform `msforms` output (object or string)
//
// It replays the calls the Forms editor itself makes to its internal
// formapi (captured live on 2026-09-27; see README.md). Microsoft does not
// document this API and may change it. Credentials never leave the page's
// own origin and are never logged: the script reuses the editor's session
// cookies and the request headers the editor sets on its own formapi calls,
// which it starts watching for as soon as it is pasted. Both the API root
// and the request the headers are copied from must be on the page's origin,
// and the headers are only ever sent back to the root they were copied from.
(() => {
  "use strict";

  const FORMAPI_PATH = /^(\/formapi\/api\/[^/]+\/users\/[^/]+)\//i;
  const SKIP_HEADERS = new Set(["content-type", "content-length", "accept"]);

  // The API root (".../formapi/api/{tenant}/users/{user}") of a request
  // URL, or null unless the request goes to this page's own origin over
  // https. A formapi-shaped URL on any other host is never trusted: it must
  // not choose where the editor's credentials are sent, or supply them.
  function apiRootOf(url) {
    let u, here;
    try {
      here = new URL(location.href);
      u = new URL(String(url), here);
    } catch {
      return null;
    }
    if (u.protocol !== "https:" || u.origin !== here.origin) return null;
    const m = u.pathname.match(FORMAPI_PATH);
    return m ? u.origin + m[1] : null;
  }

  function randomId() {
    const b = new Uint8Array(16);
    crypto.getRandomValues(b);
    return "r" + Array.from(b, (x) => x.toString(16).padStart(2, "0")).join("");
  }

  function randomGuid() {
    return crypto.randomUUID ? crypto.randomUUID() : randomId().slice(1).replace(/^(.{8})(.{4})(.{4})(.{4})/, "$1-$2-$3-$4-");
  }

  // The API root (".../formapi/api/{tenant}/users/{user}") from requests the
  // page has already made, and the form id from the editor URL.
  function locateForm() {
    const formId = new URL(location.href).searchParams.get("id");
    if (!formId) throw new Error("open an existing form in the editor first: the page URL needs &id=");
    for (const e of performance.getEntriesByType("resource")) {
      const root = apiRootOf(e.name);
      if (root) return { root, formId, formUrl: `${root}/forms('${formId}')` };
    }
    throw new Error("no formapi request to this page's own origin seen yet; reload the editor and try again");
  }

  // Copies the headers the editor sets on its own formapi requests
  // (authorization, __requestverificationtoken, x-ms-form-request-*). The
  // hook goes in as soon as this file is pasted and stays until one formapi
  // request has been seen; the values stay in this closure. Cookies alone
  // are refused (403), so the import waits for these headers.
  let capturedHeaders = null;
  let capturedRoot = null;
  let headersSeen;
  const headersReady = new Promise((resolve) => (headersSeen = resolve));

  function installHeaderHook() {
    const XHR = XMLHttpRequest.prototype;
    const origOpen = XHR.open;
    const origSet = XHR.setRequestHeader;
    const origSend = XHR.send;
    const origFetch = window.fetch;
    const keep = (url, pairs) => {
      if (capturedHeaders) return;
      const root = apiRootOf(url);
      if (!root) return;
      const h = {};
      for (const [k, v] of pairs) if (!SKIP_HEADERS.has(k.toLowerCase())) h[k.toLowerCase()] = v;
      if (!h.authorization && !h.__requestverificationtoken) return;
      capturedHeaders = h;
      capturedRoot = root;
      XHR.open = origOpen;
      XHR.setRequestHeader = origSet;
      XHR.send = origSend;
      window.fetch = origFetch;
      headersSeen(h);
    };
    XHR.open = function (method, url, ...rest) {
      this.__panformUrl = String(url);
      this.__panformHeaders = [];
      return origOpen.call(this, method, url, ...rest);
    };
    XHR.setRequestHeader = function (k, v) {
      if (this.__panformHeaders) this.__panformHeaders.push([k, v]);
      return origSet.call(this, k, v);
    };
    // Read at send time, once every header of the request is set.
    XHR.send = function (...args) {
      if (this.__panformUrl) keep(this.__panformUrl, this.__panformHeaders || []);
      return origSend.apply(this, args);
    };
    window.fetch = function (input, init) {
      const url = typeof input === "string" ? input : input && input.url;
      if (url && apiRootOf(url)) {
        const hs = new Headers((init && init.headers) || (input instanceof Request ? input.headers : undefined));
        keep(url, Array.from(hs.entries()));
      }
      return origFetch.apply(this, arguments);
    };
  }

  function makeClient(formUrl, headers) {
    return async function call(method, path, body) {
      const res = await fetch(formUrl + path, {
        method,
        credentials: "include",
        headers: { ...headers, "content-type": "application/json", accept: "application/json", "odata-version": "4.0" },
        body: body === undefined ? undefined : JSON.stringify(body),
      });
      if (!res.ok) {
        const text = await res.text().catch(() => "");
        const err = new Error(`${method} ${path || "(form)"} -> ${res.status}`);
        err.status = res.status;
        err.body = text.slice(0, 300);
        throw err;
      }
      if (res.status === 204) return null;
      return res.json().catch(() => null);
    };
  }

  // Waits, without a time limit, for the editor to make one request.
  async function authenticate(log) {
    if (capturedHeaders) return capturedHeaders;
    log("waiting for the editor: make any small edit (e.g. retype a letter of the title)");
    return headersReady;
  }

  // Microsoft's consumer tenant: personal accounts.
  const MSA_TENANT = "9188040d-6c67-4c5b-b112-36a304b66dad";
  const isPersonal = (root, headers) =>
    (headers["x-ms-form-request-ring"] || "").toLowerCase() === "msa" || root.toLowerCase().includes(`/api/${MSA_TENANT}/`);

  const parseInfo = (s) => {
    if (!s) return null;
    if (typeof s === "object") return s;
    try {
      return JSON.parse(s);
    } catch {
      return null;
    }
  };

  function hasBranch(info) {
    return !!info && (!!info.BranchInfo || (Array.isArray(info.Choices) && info.Choices.some((c) => c && c.BranchInfo)));
  }

  // questionInfo with ids remapped, and optionally without any jumps. A
  // jump whose target is not an item this import created (it failed, or
  // the id names nothing in the import) is left out, and its source target
  // id is added to `missing`.
  function rewriteInfo(info, idMap, withBranches, created, missing) {
    if (!info) return null;
    const fix = (b) => {
      if (!b || !withBranches) return undefined;
      if (b.TargetQuestionId) {
        const t = idMap.get(b.TargetQuestionId);
        if (t === null) return { ToTheEnd: true };
        if (!t || !created.has(t)) {
          missing.push(b.TargetQuestionId);
          return undefined;
        }
        return { ...b, TargetQuestionId: t };
      }
      return b;
    };
    const out = { ...info };
    if ("BranchInfo" in out) {
      const b = fix(out.BranchInfo);
      if (b) out.BranchInfo = b;
      else delete out.BranchInfo;
    }
    if (Array.isArray(out.Choices)) {
      out.Choices = out.Choices.map((c) => {
        const cc = { ...c };
        const b = fix(cc.BranchInfo);
        if (b) cc.BranchInfo = b;
        else delete cc.BranchInfo;
        return cc;
      });
    }
    return JSON.stringify(out);
  }

  let running = false;

  // One import at a time: a second call while one runs would duplicate
  // content.
  async function panformImport(json, opts = {}) {
    if (running) throw new Error("an import is already running in this page; wait for it to finish");
    running = true;
    try {
      return await runImport(json, opts);
    } finally {
      running = false;
    }
  }

  async function runImport(json, opts) {
    const log = opts.log || ((m) => console.log("[panform]", m));
    const src = typeof json === "string" ? JSON.parse(json) : json;
    const summary = { formId: null, created: [], patched: [], errors: [], warnings: [], idMap: {}, stopped: null };

    const { root, formUrl, formId } = locateForm();
    summary.formId = formId;
    const headers = await authenticate(log);
    // The headers go back only to the API root the editor sent them to.
    if (capturedRoot !== root) throw new Error("the editor's request went to a different API root than this page's; nothing was sent");
    const call = makeClient(formUrl, headers);
    const personal = isPersonal(root, headers);

    // Items go after anything already in the form. Without the form's
    // current content there is no safe place to put them, so a failed read
    // stops the import before anything is changed.
    let existing;
    try {
      existing = await call("GET", "?$expand=questions($expand=choices),descriptiveQuestions");
    } catch (e) {
      const err = new Error(`could not read the form's current content (${e.message}); nothing was changed`);
      err.status = e.status || null;
      throw err;
    }
    if (!existing || typeof existing !== "object") throw new Error("could not read the form's current content (empty reply); nothing was changed");
    const existingOrders = [...(existing.questions || []), ...(existing.descriptiveQuestions || [])].map((q) => Number(q.order) || 0);
    // Strictly above the last existing order, so even an item without an
    // order of its own (0) lands after the existing content.
    const offset = existingOrders.length ? Math.floor(Math.max(...existingOrders) / 1000000) * 1000000 + 1000000 : 0;
    if (offset) {
      log(`form is not empty: appending after order ${offset}`);
      summary.warnings.push({ id: formId, message: "the form already had content: the imported items were added after it (running an import twice adds everything twice)" });
    }

    const sections = (src.descriptiveQuestions || []).map((q) => ({ ...q, __section: true }));
    const all = [...sections, ...(src.questions || [])].sort((a, b) => (Number(a.order) || 0) - (Number(b.order) || 0));
    // File upload fails on personal accounts (POST -> 500, AADSTS70011: the
    // OneDrive scope for personal content does not exist).
    const skip = (q) => personal && q.type === "Question.FileUpload";
    for (const q of all.filter(skip))
      summary.warnings.push({ id: q.id, message: "file upload question skipped: Microsoft Forms refuses it on personal accounts" });
    const items = all.filter((q) => !skip(q));
    const idMap = new Map(items.map((q) => [q.id, randomId()]));
    // A jump to a skipped question goes to whatever follows it.
    all.forEach((q, i) => {
      if (!skip(q)) return;
      const next = all.slice(i + 1).find((x) => !skip(x));
      idMap.set(q.id, next ? idMap.get(next.id) : null);
    });
    summary.idMap = Object.fromEntries(idMap);

    const step = async (what, id, fn) => {
      try {
        await fn();
        return true;
      } catch (e) {
        summary.errors.push({ step: what, id, status: e.status || null, message: e.message, body: e.body || null });
        log(`failed: ${what} ${id || ""}: ${e.message}`);
        return false;
      }
    };

    // 1. Title and description.
    const meta = {};
    if (src.title) Object.assign(meta, { title: src.title, formsProRTTitle: src.formsProRTTitle || src.title });
    if (src.description) Object.assign(meta, { description: src.description, formsProRTDescription: src.formsProRTDescription || src.description });
    if (Object.keys(meta).length && (await step("patch form title/description", formId, () => call("PATCH", "", meta)))) summary.patched.push("form");

    // 2. Sections need question shuffling off; the editor sets this when
    // the first section is added. Only that flag is changed. If it cannot
    // be set, nothing is created: sections would not behave as sections.
    if (sections.length) {
      const ok = await step("patch settings for sections", formId, async () => {
        const cur = parseInfo(existing.settings) || {};
        if (cur.ShuffleQuestionOrder === false) return;
        await call("PATCH", "", { settings: JSON.stringify({ ...cur, ShuffleQuestionOrder: false }) });
        summary.patched.push("settings");
      });
      if (!ok) {
        summary.stopped = "could not turn off 'Shuffle questions', which sections need; no section or question was created";
        log(`stopped: ${summary.stopped}`);
        return summary;
      }
      // The editor adds an untitled first section when questions come
      // before the first section; do the same.
      if (items.length && !items[0].__section && !offset) {
        const first = Number(items[0].order) || 1000500;
        const id = randomId();
        if (await step("create implicit first section", id, () => call("POST", "/descriptiveQuestions", { type: "Question.ColumnGroup", title: "", id, order: Math.max(1, first - 500000), isQuiz: false })))
          summary.created.push({ id, kind: "implicit first section" });
      }
    }

    // 3. Sections and questions in form order, without jumps (their
    // targets may not exist yet). `createdIds` holds the new ids of the
    // items that now exist.
    const createdIds = new Set();
    for (const q of items) {
      const id = idMap.get(q.id);
      const info = parseInfo(q.questionInfo);
      const body = {
        id,
        type: q.type,
        title: q.title || "",
        formsProRTQuestionTitle: q.formsProRTQuestionTitle || q.title || "",
        order: (Number(q.order) || 0) + offset,
        isQuiz: false,
      };
      if (q.subtitle) Object.assign(body, { subtitle: q.subtitle, formsProRTSubtitle: q.formsProRTSubtitle || q.subtitle });
      if (!q.__section) {
        body.required = !!q.required;
        if (q.allowMultipleValues != null) body.allowMultipleValues = q.allowMultipleValues;
        // Likert rows point at their header. A row whose header was not
        // created would point at nothing, so it is not created either.
        if (q.groupId) {
          const header = idMap.get(q.groupId);
          if (header && !createdIds.has(header)) {
            summary.errors.push({ step: `create ${q.type}`, id: q.id, status: null, message: `not created: its header ${q.groupId} was not created`, body: null });
            continue;
          }
          body.groupId = header || q.groupId;
        }
      }
      const qi = rewriteInfo(info, idMap, false);
      if (qi) body.questionInfo = qi;
      const path = q.__section ? "/descriptiveQuestions" : "/questions";
      if (!(await step(`create ${q.type}`, q.id, () => call("POST", path, body)))) continue;
      createdIds.add(id);
      summary.created.push({ id, from: q.id, type: q.type });

      // Ranking and Likert options are separate entities.
      for (const [i, c] of (q.choices || []).entries()) {
        const choice = {
          customProperties: JSON.stringify({ IsGenerated: false }),
          displayText: c.displayText,
          key: randomGuid(),
          order: c.order != null ? c.order : i + 1,
          description: null,
        };
        if (await step("create option", `${q.id}:${c.displayText}`, () => call("POST", `/questions('${id}')/choices`, choice)))
          summary.created.push({ id: `${id}/choices:${choice.order}`, kind: "option" });
      }
    }

    // 4. Jumps, once every target exists: PATCH the whole questionInfo. A
    // jump is only set from an item that was created, to one that was.
    for (const q of items) {
      const info = parseInfo(q.questionInfo);
      if (!hasBranch(info)) continue;
      const id = idMap.get(q.id);
      if (!createdIds.has(id)) continue;
      const missing = [];
      const questionInfo = rewriteInfo(info, idMap, true, createdIds, missing);
      for (const t of missing)
        summary.errors.push({ step: "patch branching", id: q.id, status: null, message: `jump to ${t} not set: no such item was created`, body: null });
      if (!hasBranch(parseInfo(questionInfo))) continue;
      const path = q.__section ? `/descriptiveQuestions('${id}')` : `/questions('${id}')`;
      if (await step("patch branching", q.id, () => call("PATCH", path, { questionInfo }))) summary.patched.push(id);
    }

    log(`done: ${summary.created.length} created, ${summary.patched.length} patched, ${summary.errors.length} errors, ${summary.warnings.length} warnings`);
    return summary;
  }

  // Pasting twice would stack a second hook and a second importer.
  if (window.panformImport) {
    console.log("[panform] importer already loaded; call await panformImport(json)");
    return;
  }
  installHeaderHook();
  window.panformImport = panformImport;
  console.log("[panform] importer loaded; make any small edit in the editor, then call await panformImport(json)");
})();
