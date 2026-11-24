import { describe, expect, it } from "vitest";

import {
  type SchemaAnswer,
  type SchemaDatabase,
  type SchemaField,
  type SchemaTable,
  failureText,
  filterSchema,
  indexText,
  DF_GLOB,
  outsideDirectories,
  outsideWatches,
  qualifiedName,
  tableHtml,
  typeText,
  viewMessage,
} from "./schemaModel";

const at = (line: number) => ({ start: { line, character: 10 }, end: { line, character: 20 } });

function field(name: string, extra: Partial<SchemaField> = {}): SchemaField {
  return {
    name,
    dataType: "character",
    extent: null,
    format: null,
    label: null,
    initial: null,
    mandatory: false,
    description: null,
    decimals: null,
    caseSensitive: false,
    help: null,
    order: null,
    range: at(1),
    ...extra,
  };
}

function table(name: string, fields: SchemaField[], extra: Partial<SchemaTable> = {}): SchemaTable {
  return {
    name,
    declared: true,
    label: null,
    description: null,
    dumpName: null,
    area: null,
    hidden: false,
    frozen: false,
    fields,
    indexes: [],
    range: at(0),
    ...extra,
  };
}

const SALES: SchemaDatabase = {
  name: "sales",
  path: "/w/db/sales.df",
  uri: "file:///w/db/sales.df",
  complete: true,
  problems: [],
  tables: [
    table("Customer", [field("CustNum", { label: "Customer number" }), field("Phone", { description: "Home, work and mobile" })], {
      dumpName: "kunde",
      indexes: [
        {
          name: "CustNum",
          unique: true,
          primary: true,
          word: false,
          inactive: false,
          description: null,
          fields: [{ name: "CustNum", ascending: true, abbreviated: false }],
          range: at(9),
        },
      ],
    }),
    table("Order-Line", [field("Qty", { label: "Größe der Lieferung" }), field("Note")], { label: "Order line" }),
  ],
  sequences: [{ name: "NextCust", initial: 1, increment: 1, minValue: null, maxValue: null, cycleOnLimit: false, range: at(20) }],
};

const STOCK: SchemaDatabase = {
  ...SALES,
  name: "stock",
  path: "/w/db/stock.df",
  tables: [table("Bin", [field("Shelf")])],
  sequences: [],
};

const shape = (databases: SchemaDatabase[]) =>
  databases.map((d) => ({
    db: d.name,
    tables: d.tables.map((t) => `${t.name}: ${t.fields.map((f) => f.name).join(",")} | ${t.indexes.map((i) => i.name).join(",")}`),
    sequences: d.sequences.map((s) => s.name),
  }));

describe("filterSchema", () => {
  it("keeps everything for an empty or blank query", () => {
    expect(filterSchema([SALES, STOCK], "")).toEqual([SALES, STOCK]);
    expect(filterSchema([SALES, STOCK], "   ")).toEqual([SALES, STOCK]);
  });

  it("finds a field by its name, case-insensitive, and keeps only that field", () => {
    expect(shape(filterSchema([SALES, STOCK], "PHONE"))).toEqual([{ db: "sales", tables: ["Customer: Phone | "], sequences: [] }]);
  });

  it("finds a field by its label", () => {
    expect(shape(filterSchema([SALES, STOCK], "customer number"))).toEqual([
      { db: "sales", tables: ["Customer: CustNum | "], sequences: [] },
    ]);
    expect(shape(filterSchema([SALES, STOCK], "grösse"))).toEqual([]);
    expect(shape(filterSchema([SALES, STOCK], "größe"))).toEqual([{ db: "sales", tables: ["Order-Line: Qty | "], sequences: [] }]);
  });

  it("finds a field by its description", () => {
    expect(shape(filterSchema([SALES], "mobile"))).toEqual([{ db: "sales", tables: ["Customer: Phone | "], sequences: [] }]);
  });

  it("finds a table by an ABL abbreviation of its name, with all its fields and indexes", () => {
    expect(shape(filterSchema([SALES, STOCK], "cust"))).toEqual([
      { db: "sales", tables: ["Customer: CustNum,Phone | CustNum"], sequences: ["NextCust"] },
    ]);
    expect(shape(filterSchema([SALES, STOCK], "order-l"))).toEqual([{ db: "sales", tables: ["Order-Line: Qty,Note | "], sequences: [] }]);
  });

  it("finds a table by its dump name and by its label", () => {
    expect(shape(filterSchema([SALES, STOCK], "KUNDE"))).toEqual([{ db: "sales", tables: ["Customer: CustNum,Phone | CustNum"], sequences: [] }]);
    expect(shape(filterSchema([SALES, STOCK], "order line"))).toEqual([{ db: "sales", tables: ["Order-Line: Qty,Note | "], sequences: [] }]);
  });

  it("drops a database with nothing that matches", () => {
    expect(shape(filterSchema([SALES, STOCK], "shelf"))).toEqual([{ db: "stock", tables: ["Bin: Shelf | "], sequences: [] }]);
    expect(filterSchema([SALES, STOCK], "nothing-is-called-this")).toEqual([]);
  });

  it("does not change the answer it filters", () => {
    const before = JSON.stringify([SALES, STOCK]);
    filterSchema([SALES, STOCK], "phone");
    expect(JSON.stringify([SALES, STOCK])).toBe(before);
  });
});

describe("labels", () => {
  it("shows the type with its extent and the index flags with its fields", () => {
    expect(typeText(field("Phone", { extent: 3 }))).toBe("character[3]");
    expect(typeText(field("Odd", { dataType: null }))).toBe("?");
    expect(indexText(SALES.tables[0].indexes[0])).toBe("primary unique (CustNum)");
    expect(qualifiedName(SALES.tables[0], SALES.tables[0].fields[1])).toBe("Customer.Phone");
  });
});

describe("tableHtml", () => {
  it("escapes the text of the .df and links each name to its line", () => {
    const evil = table("T<script>", [field("F", { label: '<img src=x onerror="alert(1)">', description: "a & b" })]);
    const html = tableHtml(
      { database: { name: "sales", path: "/w/db/sales.df", uri: "file:///w/db/sales.df", complete: false }, table: evil },
      (range) => `command:open?${range?.start.line}`,
      "default-src 'none'",
    );
    expect(html).not.toContain("<script>");
    expect(html).not.toContain("<img");
    expect(html).toContain("T&lt;script&gt;");
    expect(html).toContain("&lt;img src=x onerror=&quot;alert(1)&quot;&gt;");
    expect(html).toContain("a &amp; b");
    expect(html).toContain('<a href="command:open?1" title="Open in .df">F</a>');
    expect(html).toContain("can miss fields");
  });
});

describe("viewMessage", () => {
  const answer = (extra: Partial<SchemaAnswer> = {}): SchemaAnswer => ({
    config: "/w/oxabl.toml",
    reason: null,
    warnings: [],
    databases: [SALES],
    directories: ["/w/db"],
    ...extra,
  });

  it("says nothing for a complete answer without a filter", () => {
    expect(viewMessage(answer(), [SALES], "")).toBeUndefined();
  });

  it("shows the warnings of the configuration beside the databases", () => {
    const warned = answer({ warnings: ["`/w/empty` matched no .df files"] });
    expect(viewMessage(warned, [SALES], "")).toContain("matched no .df files");
    expect(viewMessage(warned, [SALES], "phone")).toContain("matched no .df files");
    expect(viewMessage(warned, [SALES], "phone")).toContain('Filter "phone"');
  });

  it("gives the reason for an empty schema and the failure first", () => {
    expect(viewMessage(answer({ databases: [], reason: "no oxabl.toml" }), [], "")).toBe("No schema: no oxabl.toml");
    expect(viewMessage(answer(), [SALES], "", "down")).toBe("down");
  });
});

describe("failureText", () => {
  it("says that a server without the request is too old", () => {
    expect(failureText("The schema", -32601, "Unhandled method oxabl/schema")).toContain("too old");
    expect(failureText("The schema", -32603, "boom")).toBe("The schema could not be read: boom");
    expect(failureText("The table", undefined, "gone")).toBe("The table could not be read: gone");
  });
});

describe("outsideDirectories", () => {
  it("names each schema directory that no workspace folder holds, once", () => {
    const dirs = outsideDirectories(["/w/db", "/w2", "/schema", "/w/../schema", "/wx", "/w/../up", "/w/../empty"], ["/w", "/w2/"]);
    expect(dirs).toEqual(["/empty", "/schema", "/up", "/wx"]);
  });
});

describe("outsideWatches", () => {
  const answer = (config: string | null, directories: string[]): SchemaAnswer => ({
    config,
    reason: null,
    warnings: [],
    databases: [],
    directories,
  });

  it("watches the .df files of an outside directory in any case of the extension", () => {
    expect(outsideWatches(answer("/w/oxabl.toml", ["/w/db", "/schema"]), ["/w"])).toEqual([{ dir: "/schema", glob: DF_GLOB }]);
    for (const name of ["a.df", "A.DF", "a.Df", "a.dF"]) {
      expect(new RegExp(`^${DF_GLOB.replace("*.", ".*\\.").replace("{", "(").replace("}", ")").replaceAll(",", "|")}$`).test(name), name).toBe(true);
    }
  });

  it("watches an oxabl.toml above the workspace folder", () => {
    expect(outsideWatches(answer("/proj/oxabl.toml", ["/proj/db"]), ["/proj/src"])).toEqual([
      { dir: "/proj/db", glob: DF_GLOB },
      { dir: "/proj", glob: "oxabl.toml" },
    ]);
    expect(outsideWatches(answer(null, []), ["/proj/src"])).toEqual([]);
  });
});
