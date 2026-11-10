// Runs import.js against a mocked Forms editor page: node --test importers/msforms/
import { test } from "node:test";
import assert from "node:assert/strict";
import fs from "node:fs";
import vm from "node:vm";

const SCRIPT = fs.readFileSync(new URL("./import.js", import.meta.url), "utf8");
const API = "https://forms.cloud.microsoft/formapi/api/TENANT/users/USER";
const FORM = `${API}/forms('FORMID')`;

// A form with a section, a question that jumps past a file upload, and a
// jump to the end.
const SAMPLE = {
  title: "Commute",
  descriptiveQuestions: [{ id: "s1", type: "Question.ColumnGroup", title: "Drivers", order: 3000500 }],
  questions: [
    {
      id: "q1",
      type: "Question.Choice",
      title: "Do you drive?",
      required: true,
      order: 1000500,
      questionInfo: JSON.stringify({
        Choices: [
          { Description: "Yes", BranchInfo: { TargetQuestionId: "up" } },
          { Description: "No", BranchInfo: { ToTheEnd: true } },
        ],
        ChoiceType: 1,
      }),
    },
    { id: "up", type: "Question.FileUpload", title: "Licence photo", order: 2000500, questionInfo: "{}" },
    { id: "q2", type: "Question.TextField", title: "Which car?", order: 4000500, questionInfo: '{"Multiline":false}' },
  ],
};

// A fresh page: the script is pasted into it, and `editorRequest` plays the
// editor sending one formapi request with its own headers.
// `resources` are the URLs the page has already loaded; `respond` may answer
// a request itself (return undefined to fall through to the default).
function page({ resources = [`${FORM}?$select=id`], respond = () => undefined } = {}) {
  const calls = [];
  class FakeXHR {
    open() {}
    setRequestHeader() {}
    send() {}
  }
  const ctx = {
    console: { log: () => {} },
    crypto: globalThis.crypto,
    URL,
    Headers,
    Request,
    JSON,
    Promise,
    setTimeout,
    location: { href: "https://forms.cloud.microsoft/Pages/DesignPageV2.aspx?subpage=design&id=FORMID" },
    performance: { getEntriesByType: () => resources.map((name) => ({ name })) },
    XMLHttpRequest: FakeXHR,
    fetch: async (url, init = {}) => {
      const method = init.method || "GET";
      const headers = init.headers || {};
      const call = { method, url, path: url.replace(FORM, ""), auth: !!headers.authorization, body: init.body ? JSON.parse(init.body) : null };
      calls.push(call);
      // (parsed with the test realm's JSON, so bodies are plain data)
      if (!headers.authorization) return { ok: false, status: 403, text: async () => "forbidden" };
      const custom = respond(call);
      if (custom) return custom;
      if (method === "GET") return { ok: true, status: 200, json: async () => ({ title: "t", settings: '{"IsAnonymous":true}', questions: [], descriptiveQuestions: [] }) };
      return { ok: true, status: method === "POST" ? 201 : 204, json: async () => ({}) };
    },
  };
  ctx.window = ctx;
  vm.createContext(ctx);
  vm.runInContext(SCRIPT, ctx);
  const editorRequest = (ring = "msa", url = FORM) => {
    const xhr = new ctx.XMLHttpRequest();
    xhr.open("PATCH", url);
    xhr.setRequestHeader("authorization", "Bearer SECRET");
    xhr.setRequestHeader("x-ms-form-request-ring", ring);
    xhr.send("{}");
  };
  return { ctx, calls, editorRequest };
}

// Results come from another realm; compare them as plain data.
const plain = (x) => JSON.parse(JSON.stringify(x));

const tick = () => new Promise((r) => setTimeout(r, 20));

test("waits for the editor's headers and never writes without them", async () => {
  const { ctx, calls, editorRequest } = page();
  const run = ctx.panformImport(SAMPLE, { log: () => {} });
  await tick();
  assert.equal(calls.length, 0, "nothing is sent before the editor's headers are seen");
  editorRequest("business");
  const res = plain(await run);
  assert.ok(calls.every((c) => c.auth), "every call carries the editor's authorization");
  assert.deepEqual(res.errors, []);
  assert.deepEqual(res.warnings, []);
  assert.equal(res.created.length, 5, "implicit first section, section, three questions");
});

test("refuses a second import while one runs", async () => {
  const { ctx, editorRequest } = page();
  const first = ctx.panformImport(SAMPLE, { log: () => {} });
  await assert.rejects(ctx.panformImport(SAMPLE, { log: () => {} }), /already running/);
  editorRequest();
  await first;
  // Once finished, another run is allowed.
  await ctx.panformImport(SAMPLE, { log: () => {} });
});

test("personal accounts skip file upload and retarget jumps to it", async () => {
  const { ctx, calls, editorRequest } = page();
  editorRequest("msa");
  const res = plain(await ctx.panformImport(SAMPLE, { log: () => {} }));
  assert.deepEqual(res.errors, []);
  assert.equal(res.warnings.length, 1);
  assert.match(res.warnings[0].message, /file upload/);
  assert.ok(!calls.some((c) => c.body && c.body.type === "Question.FileUpload"));

  const posts = calls.filter((c) => c.method === "POST");
  const ids = posts.map((c) => c.body.id);
  assert.ok(ids.every((id) => /^r[0-9a-f]{32}$/.test(id)), "every id is r + 32 hex");
  const sectionId = posts.find((c) => c.body.title === "Drivers").body.id;
  const q1 = posts.find((c) => c.body.title === "Do you drive?").body;
  assert.ok(!q1.questionInfo.includes("BranchInfo"), "questions are created without jumps");

  const patch = calls.find((c) => c.method === "PATCH" && c.path.startsWith("/questions("));
  assert.equal(patch.path, `/questions('${q1.id}')`);
  const choices = JSON.parse(patch.body.questionInfo).Choices;
  assert.deepEqual(choices[0].BranchInfo, { TargetQuestionId: sectionId }, "the jump to the skipped upload goes to what follows it");
  assert.deepEqual(choices[1].BranchInfo, { ToTheEnd: true });
  assert.ok(calls.indexOf(patch) > calls.lastIndexOf(posts[posts.length - 1]), "jumps are patched after every item exists");

  const settings = calls.find((c) => c.body && c.body.settings);
  assert.deepEqual(JSON.parse(settings.body.settings), { IsAnonymous: true, ShuffleQuestionOrder: false }, "only the shuffle flag changes");
});

const EVIL = "https://attacker.example/formapi/api/TENANT/users/USER";
const failed = (status) => ({ ok: false, status, text: async () => "no" });

test("the API root is only ever taken from the page's own origin", async () => {
  // A formapi-shaped resource on another host comes first; it is ignored.
  const { ctx, calls, editorRequest } = page({ resources: [`${EVIL}/forms('X')?$select=id`, `${FORM}?$select=id`] });
  editorRequest();
  await ctx.panformImport(SAMPLE, { log: () => {} });
  assert.ok(calls.length > 0);
  assert.ok(calls.every((c) => c.url.startsWith(API + "/")), "every request goes to the page's own API root");

  // With only the foreign resource there is nothing to import into.
  const lone = page({ resources: [`${EVIL}/forms('X')?$select=id`] });
  lone.editorRequest();
  await assert.rejects(lone.ctx.panformImport(SAMPLE, { log: () => {} }), /own origin/);
  assert.equal(lone.calls.length, 0, "nothing is sent");
});

test("headers are not taken from a request to another origin", async () => {
  const { ctx, calls, editorRequest } = page();
  const run = ctx.panformImport(SAMPLE, { log: () => {} });
  editorRequest("msa", `${EVIL}/forms('FORMID')`);
  editorRequest("msa", "http://forms.cloud.microsoft/formapi/api/TENANT/users/USER/forms('FORMID')");
  await tick();
  assert.equal(calls.length, 0, "still waiting: neither request was the editor talking to this origin over https");
  editorRequest("msa", "/formapi/api/TENANT/users/USER/forms('FORMID')");
  const res = plain(await run);
  assert.deepEqual(res.errors, []);
  assert.ok(calls.every((c) => c.url.startsWith(API + "/")));
});

test("headers captured for one API root are not sent to another", async () => {
  const { ctx, calls, editorRequest } = page();
  editorRequest("msa", "https://forms.cloud.microsoft/formapi/api/OTHER/users/SOMEONE/forms('FORMID')");
  await assert.rejects(ctx.panformImport(SAMPLE, { log: () => {} }), /different API root/);
  assert.equal(calls.length, 0);
});

test("stops before changing anything when the form cannot be read", async () => {
  const { ctx, calls, editorRequest } = page({ respond: (c) => (c.method === "GET" ? failed(500) : undefined) });
  editorRequest();
  await assert.rejects(ctx.panformImport(SAMPLE, { log: () => {} }), /could not read the form.*nothing was changed/);
  assert.deepEqual(calls.map((c) => c.method), ["GET"], "no PATCH or POST follows the failed read");
});

test("creates nothing when the setting sections need cannot be set", async () => {
  const { ctx, calls, editorRequest } = page({ respond: (c) => (c.method === "PATCH" && c.body && c.body.settings ? failed(400) : undefined) });
  editorRequest();
  const res = plain(await ctx.panformImport(SAMPLE, { log: () => {} }));
  assert.match(res.stopped, /Shuffle questions/);
  assert.equal(res.errors.length, 1);
  assert.deepEqual(res.created, []);
  assert.ok(!calls.some((c) => c.method === "POST"), "no section or question is created");
});

test("a jump to an item that was not created is not set", async () => {
  // The section "Drivers" fails; q1's "Yes" jumps to the upload, which on
  // a personal account means the item after it: that section.
  const { ctx, calls, editorRequest } = page({ respond: (c) => (c.method === "POST" && c.body.title === "Drivers" ? failed(500) : undefined) });
  editorRequest("msa");
  const res = plain(await ctx.panformImport(SAMPLE, { log: () => {} }));
  assert.ok(res.errors.some((e) => /jump to up not set/.test(e.message)), "the dropped jump is reported");
  const patch = calls.find((c) => c.method === "PATCH" && c.path.startsWith("/questions("));
  const choices = JSON.parse(patch.body.questionInfo).Choices;
  assert.equal(choices[0].BranchInfo, undefined, "no jump to the missing section");
  assert.deepEqual(choices[1].BranchInfo, { ToTheEnd: true }, "the other jump is still set");
  const made = new Set(res.created.map((c) => c.id));
  for (const c of calls.filter((x) => x.method === "PATCH" && x.body.questionInfo))
    for (const ch of JSON.parse(c.body.questionInfo).Choices || [])
      if (ch.BranchInfo && ch.BranchInfo.TargetQuestionId) assert.ok(made.has(ch.BranchInfo.TargetQuestionId), "every jump target exists");
});

test("a Likert row is not created when its header was not, and jumps from missing items are skipped", async () => {
  const likert = {
    questions: [
      { id: "h", type: "Question.MatrixChoiceGroup", title: "Rate", order: 1000500, questionInfo: "{}" },
      { id: "r1", type: "Question.MatrixChoice", title: "Row", order: 1000600, groupId: "h", questionInfo: "{}" },
      { id: "j", type: "Question.Choice", title: "Jumper", order: 2000500, questionInfo: JSON.stringify({ Choices: [{ Description: "a", BranchInfo: { TargetQuestionId: "nowhere" } }] }) },
    ],
  };
  const { ctx, calls, editorRequest } = page({ respond: (c) => (c.method === "POST" && c.body.title === "Rate" ? failed(500) : undefined) });
  editorRequest();
  const res = plain(await ctx.panformImport(likert, { log: () => {} }));
  assert.ok(!calls.some((c) => c.method === "POST" && c.body.title === "Row"), "the orphan row is not posted");
  assert.ok(res.errors.some((e) => e.id === "r1" && /header h was not created/.test(e.message)));
  assert.ok(res.errors.some((e) => e.id === "j" && /jump to nowhere not set/.test(e.message)), "a jump to an id outside the import is reported, not sent");
  assert.ok(!calls.some((c) => c.method === "PATCH" && c.body.questionInfo), "no branching patch is left to send");
});

test("importing into a form that has content says so", async () => {
  const { ctx, calls, editorRequest } = page({
    respond: (c) => (c.method === "GET" ? { ok: true, status: 200, json: async () => ({ settings: "{}", questions: [{ order: 1000500 }], descriptiveQuestions: [] }) } : undefined),
  });
  editorRequest("business");
  const res = plain(await ctx.panformImport(SAMPLE, { log: () => {} }));
  assert.ok(res.warnings.some((w) => /already had content/.test(w.message)));
  assert.ok(calls.filter((c) => c.method === "POST").every((c) => c.body.order > 2000000), "new items go after the existing ones");
});

test("imported items land after existing content even when it ends on a round order", async () => {
  const { ctx, calls, editorRequest } = page({
    respond: (c) => (c.method === "GET" ? { ok: true, status: 200, json: async () => ({ settings: "{}", questions: [{ order: 2000000 }], descriptiveQuestions: [] }) } : undefined),
  });
  editorRequest("business");
  const src = { questions: [{ id: "n", type: "Question.TextField", title: "No order", questionInfo: "{}" }] };
  await ctx.panformImport(src, { log: () => {} });
  const posts = calls.filter((c) => c.method === "POST");
  assert.equal(posts.length, 1);
  assert.ok(posts[0].body.order > 2000000, `order ${posts[0].body.order} is after the existing 2000000`);
});
