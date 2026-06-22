(function() {
  function getDecimals(text) {
    if (!text) return 0;
    var match = text.match(/\.(\d+)/);
    return match ? match[1].length : 0;
  }

  function formatNumber(value, decimals) {
    var factor = Math.pow(10, decimals);
    var rounded = Math.round(value * factor) / factor;
    var parts = rounded.toFixed(decimals).split('.');
    parts[0] = parts[0].replace(/\B(?=(\d{3})+(?!\d))/g, ',');
    return parts.join('.');
  }

  function animateKpiValue(el) {
    var targetAttr = el.getAttribute('data-target');
    if (!targetAttr) return;
    var target = parseFloat(targetAttr);
    if (!isFinite(target)) return;

    var finalText = el.textContent || '';
    el.setAttribute('data-final', finalText);
    var decimals = getDecimals(finalText);

    var duration = 3000;
    var start = null;
    function step(timestamp) {
      if (!start) start = timestamp;
      var progress = Math.min((timestamp - start) / duration, 1);
      var current = target * progress;
      el.textContent = formatNumber(current, decimals);
      if (progress < 1) {
        requestAnimationFrame(step);
      } else {
        el.textContent = finalText;
      }
    }
    requestAnimationFrame(step);
  }

  function init() {
    document.querySelectorAll('.kpi-value[data-target]').forEach(animateKpiValue);
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
