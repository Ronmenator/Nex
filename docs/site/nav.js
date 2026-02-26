const NAV_HTML = `
<div class="search-box"><input type="text" placeholder="Search docs..." /></div>

<div class="nav-section">
  <div class="nav-section-header"><span class="chevron">&#9662;</span> Getting Started</div>
  <ul class="nav-links">
    <li><a href="index.html">Introduction</a></li>
    <li><a href="getting-started.html">Installation &amp; Setup</a></li>
    <li><a href="hello-world.html">Hello World</a></li>
  </ul>
</div>

<div class="nav-section">
  <div class="nav-section-header"><span class="chevron">&#9662;</span> Language Guide</div>
  <ul class="nav-links">
    <li><a href="variables-types.html">Variables &amp; Types</a></li>
    <li><a href="operators.html">Operators &amp; Expressions</a></li>
    <li><a href="control-flow.html">Control Flow</a></li>
    <li><a href="functions.html">Functions</a></li>
    <li><a href="classes.html">Classes &amp; Inheritance</a></li>
    <li><a href="structs.html">Structs</a></li>
    <li><a href="interfaces.html">Interfaces</a></li>
    <li><a href="enums.html">Enums</a></li>
    <li><a href="string-interpolation.html">String Interpolation</a></li>
    <li><a href="modules-imports.html">Modules &amp; Imports</a></li>
    <li><a href="error-handling.html">Error Handling</a></li>
    <li><a href="resource-management.html">Resource Management</a></li>
  </ul>
</div>

<div class="nav-section">
  <div class="nav-section-header"><span class="chevron">&#9662;</span> Advanced Topics</div>
  <ul class="nav-links">
    <li><a href="generics.html">Generics</a></li>
    <li><a href="multiple-inheritance.html">Multiple Inheritance</a></li>
    <li><a href="operator-overloading.html">Operator Overloading</a></li>
    <li><a href="dynamic-var.html">Dynamic Typing (Var)</a></li>
    <li><a href="nullable-types.html">Nullable Types</a></li>
    <li><a href="function-types.html">Function Types</a></li>
    <li><a href="closures.html">Closures &amp; Lambdas</a></li>
    <li><a href="async-await.html">Async / Await</a></li>
    <li><a href="annotations.html">Attributes</a></li>
    <li><a href="reflection.html">Reflection</a></li>
  </ul>
</div>

<div class="nav-section">
  <div class="nav-section-header"><span class="chevron">&#9662;</span> Standard Library</div>
  <ul class="nav-links">
    <li><a href="stdlib-overview.html">Overview</a></li>
    <li><a href="stdlib-core.html">std.core</a></li>
    <li><a href="stdlib-math.html">std.math</a></li>
    <li><a href="stdlib-string.html">std.string</a></li>
    <li><a href="stdlib-collections.html">std.collections</a></li>
    <li><a href="stdlib-io.html">std.io</a></li>
    <li><a href="stdlib-convert.html">std.convert</a></li>
    <li><a href="stdlib-time.html">std.time</a></li>
    <li><a href="stdlib-env.html">std.env</a></li>
    <li><a href="stdlib-path.html">std.path</a></li>
    <li><a href="stdlib-json.html">std.json</a></li>
    <li><a href="stdlib-process.html">std.process</a></li>
    <li><a href="stdlib-net.html">std.net</a></li>
    <li><a href="stdlib-threading.html">std.threading</a></li>
    <li><a href="stdlib-logging.html">std.logging</a></li>
    <li><a href="stdlib-testing.html">std.testing</a></li>
  </ul>
</div>

<div class="nav-section">
  <div class="nav-section-header"><span class="chevron">&#9662;</span> Standard Libraries</div>
  <ul class="nav-links">
    <li><a href="lib-overview.html">Overview</a></li>
    <li><a href="stdlib-crypto.html">crypto</a></li>
    <li><a href="stdlib-http.html">http</a></li>
    <li><a href="stdlib-regex.html">regex</a></li>
    <li><a href="stdlib-torch.html">torch</a></li>
    <li><a href="stdlib-ui.html">nex_ui</a></li>
    <li><a href="nexui-markup.html">.nexui Markup</a></li>
    <li><a href="lib-nex3d.html">nex3d</a></li>
  </ul>
</div>

<div class="nav-section">
  <div class="nav-section-header"><span class="chevron">&#9662;</span> Tooling</div>
  <ul class="nav-links">
    <li><a href="tooling-nex.html">Build Tool (nex)</a></li>
    <li><a href="tooling-project.html">Project Configuration</a></li>
    <li><a href="tooling-repl.html">REPL</a></li>
    <li><a href="tooling-formatter.html">Formatter &amp; Linter</a></li>
    <li><a href="tooling-vscode.html">VS Code Extension</a></li>
  </ul>
</div>

<div class="nav-section">
  <div class="nav-section-header"><span class="chevron">&#9662;</span> Reference</div>
  <ul class="nav-links">
    <li><a href="grammar.html">Grammar (EBNF)</a></li>
    <li><a href="keywords.html">Keywords &amp; Operators</a></li>
    <li><a href="runtime.html">Runtime &amp; GC</a></li>
  </ul>
</div>
`;

document.addEventListener('DOMContentLoaded', () => {
  const sidebar = document.querySelector('.sidebar');
  if (sidebar) sidebar.innerHTML = NAV_HTML;
});
