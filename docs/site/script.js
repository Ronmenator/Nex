document.addEventListener('DOMContentLoaded', () => {
  initSidebar();
  initCopyButtons();
  highlightActiveLink();
  initMobileMenu();
  initSearch();
});

function initSidebar() {
  document.querySelectorAll('.nav-section-header').forEach(header => {
    header.addEventListener('click', () => {
      header.parentElement.classList.toggle('collapsed');
    });
  });
}

function initCopyButtons() {
  document.querySelectorAll('pre').forEach(block => {
    const btn = document.createElement('button');
    btn.className = 'copy-btn';
    btn.textContent = 'Copy';
    btn.addEventListener('click', () => {
      const code = block.querySelector('code');
      const text = code ? code.textContent : block.textContent;
      navigator.clipboard.writeText(text).then(() => {
        btn.textContent = 'Copied!';
        setTimeout(() => { btn.textContent = 'Copy'; }, 2000);
      });
    });
    block.appendChild(btn);
  });
}

function highlightActiveLink() {
  const current = window.location.pathname.split('/').pop() || 'index.html';
  document.querySelectorAll('.sidebar .nav-links a').forEach(link => {
    const href = link.getAttribute('href');
    if (href === current) {
      link.classList.add('active');
      const section = link.closest('.nav-section');
      if (section) section.classList.remove('collapsed');
    }
  });
}

function initMobileMenu() {
  const toggle = document.querySelector('.menu-toggle');
  const sidebar = document.querySelector('.sidebar');
  if (toggle && sidebar) {
    toggle.addEventListener('click', () => {
      sidebar.classList.toggle('open');
    });
    document.querySelector('.main-content')?.addEventListener('click', () => {
      sidebar.classList.remove('open');
    });
  }
}

function initSearch() {
  const input = document.querySelector('.search-box input');
  if (!input) return;
  input.addEventListener('input', () => {
    const q = input.value.toLowerCase();
    document.querySelectorAll('.sidebar .nav-links a').forEach(link => {
      const match = link.textContent.toLowerCase().includes(q);
      link.style.display = match || !q ? '' : 'none';
    });
    document.querySelectorAll('.nav-section').forEach(sec => {
      if (!q) { sec.classList.remove('collapsed'); return; }
      const visible = sec.querySelectorAll('.nav-links a[style=""],.nav-links a:not([style])');
      const hidden = Array.from(sec.querySelectorAll('.nav-links a')).every(a => a.style.display === 'none');
      if (hidden) sec.classList.add('collapsed');
      else sec.classList.remove('collapsed');
    });
  });
}

function hl(code) {
  return code
    .replace(/\/\/.*$/gm, m => `<span class="cmt">${m}</span>`)
    .replace(/\/\*[\s\S]*?\*\//g, m => `<span class="cmt">${m}</span>`)
    .replace(/"(?:[^"\\]|\\.)*"/g, m => `<span class="str">${m}</span>`)
    .replace(/'(?:[^'\\]|\\.)*'/g, m => `<span class="str">${m}</span>`)
    .replace(/\b(import|from|as|class|struct|interface|def|public|virtual|override|static|shared|alias|return|if|else|while|for|break|continue|try|catch|finally|throw|using|var|new|null|true|false|self)\b/g, '<span class="kw">$1</span>')
    .replace(/\b(Int|Int64|Float|Double|String|Bool|Byte|Char|Unit|Var|Object|Error|Disposable)\b/g, '<span class="typ">$1</span>')
    .replace(/\b(\d+\.?\d*[fFdDiIbBlL]?)\b/g, '<span class="num">$1</span>')
    .replace(/(-&gt;|->|::|[+\-*\/%=!<>&|]+)/g, '<span class="op">$1</span>');
}
