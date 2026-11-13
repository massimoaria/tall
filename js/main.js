// TALL — editorial site JS
// Handles: scroll-triggered navbar state, fade-in on intersection,
// copy-to-clipboard, active nav link highlighting, mobile menu toggle.

(() => {
  'use strict';

  // ---- Mobile menu toggle ----
  const burger = document.querySelector('.nav-burger');
  const mobileMenu = document.querySelector('.mobile-menu');
  if (burger && mobileMenu) {
    burger.addEventListener('click', () => {
      const open = mobileMenu.style.display === 'block';
      mobileMenu.style.display = open ? 'none' : 'block';
      burger.setAttribute('aria-expanded', String(!open));
    });
  }

  // ---- Active nav link ----
  const currentPage = (window.location.pathname.split('/').pop() || 'index.html').toLowerCase();
  document.querySelectorAll('.nav-links a, .mobile-menu a').forEach(link => {
    const href = (link.getAttribute('href') || '').toLowerCase();
    if (href === currentPage || (currentPage === '' && href === 'index.html')) {
      link.classList.add('active');
    }
  });

  // ---- Navbar scroll state ----
  const nav = document.querySelector('.site-nav');
  if (nav) {
    const onScroll = () => {
      if (window.scrollY > 12) nav.classList.add('scrolled');
      else nav.classList.remove('scrolled');
    };
    onScroll();
    window.addEventListener('scroll', onScroll, { passive: true });
  }

  // ---- Intersection-triggered fade-in ----
  const io = new IntersectionObserver((entries) => {
    entries.forEach(entry => {
      if (entry.isIntersecting) {
        entry.target.classList.add('visible');
        io.unobserve(entry.target);
      }
    });
  }, { threshold: 0.1, rootMargin: '0px 0px -60px 0px' });

  document.querySelectorAll('.fade-in').forEach(el => io.observe(el));
})();

// ---- Lightbox (click any figure image to enlarge) ----
(() => {
  const targets = document.querySelectorAll('.figure img, .fig-frame img');
  if (!targets.length) return;

  const overlay = document.createElement('div');
  overlay.className = 'lightbox';
  overlay.setAttribute('role', 'dialog');
  overlay.setAttribute('aria-modal', 'true');
  overlay.setAttribute('aria-label', 'Enlarged figure view');
  overlay.innerHTML = `
    <button class="lightbox-close" aria-label="Close enlarged view">
      <svg viewBox="0 0 24 24" fill="none" stroke="currentColor" stroke-width="2" stroke-linecap="round" stroke-linejoin="round"><path d="M6 6l12 12M18 6L6 18"/></svg>
    </button>
    <div class="lightbox-content">
      <img alt="">
      <div class="lightbox-caption"></div>
    </div>
    <div class="lightbox-hint">press <kbd>Esc</kbd> or click outside to close</div>
  `;
  document.body.appendChild(overlay);

  const imgEl = overlay.querySelector('img');
  const capEl = overlay.querySelector('.lightbox-caption');
  const closeBtn = overlay.querySelector('.lightbox-close');

  const open = (src, alt, captionHTML) => {
    imgEl.src = src;
    imgEl.alt = alt || '';
    capEl.innerHTML = captionHTML || '';
    document.body.classList.add('lightbox-open');
    requestAnimationFrame(() => overlay.classList.add('open'));
  };

  const close = () => {
    overlay.classList.remove('open');
    setTimeout(() => {
      document.body.classList.remove('lightbox-open');
      imgEl.removeAttribute('src');
      capEl.innerHTML = '';
    }, 320);
  };

  targets.forEach(el => {
    el.addEventListener('click', () => {
      const fig = el.closest('figure');
      const cap = fig ? fig.querySelector('.figcaption, figcaption') : null;
      open(el.currentSrc || el.src, el.alt, cap ? cap.innerHTML : '');
    });
  });

  overlay.addEventListener('click', (e) => {
    if (e.target === overlay || e.target === imgEl || e.target.closest('.lightbox-close')) {
      close();
    }
  });

  document.addEventListener('keydown', (e) => {
    if (e.key === 'Escape' && overlay.classList.contains('open')) close();
  });
})();

// ---- Copy-to-clipboard ----
function copyCode(btn) {
  const block = btn.closest('.code-block');
  if (!block) return;
  const code = block.querySelector('code');
  if (!code) return;
  navigator.clipboard.writeText(code.textContent.trim()).then(() => {
    const prev = btn.textContent;
    btn.textContent = 'Copied';
    btn.style.color = '#4DB280';
    setTimeout(() => {
      btn.textContent = prev;
      btn.style.color = '';
    }, 1800);
  });
}
