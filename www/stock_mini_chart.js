(function() {
  var RENDERED_ATTR = 'data-rendered';
  var SELECTOR = '.stock-mini-chart';
  var HIGHCHARTS_WAIT_MS = 15000;

  function isVisible(el) {
    return !!(el.offsetParent || el.getClientRects().length);
  }

  function setStatus(el, msg, color) {
    el.innerHTML = '<div style="padding: 8px 0; font-size: 11px; color: ' + (color || '#64748B') + ';">' + msg + '</div>';
  }

  function renderChart(el) {
    if (el.getAttribute(RENDERED_ATTR) === '1') return true;
    if (!isVisible(el)) return false;
    if (typeof window.Highcharts === 'undefined') {
      var waited = parseInt(el.getAttribute('data-wait-ms') || '0', 10);
      if (waited >= HIGHCHARTS_WAIT_MS) {
        setStatus(el, 'Highcharts 库未加载，请检查网络或刷新页面', '#E3232E');
        return true;
      }
      setStatus(el, '等待 Highcharts 加载… (' + Math.round(waited / 1000) + 's)', '#64748B');
      el.setAttribute('data-wait-ms', String(waited + 500));
      return false;
    }

    el.setAttribute(RENDERED_ATTR, '1');
    try {
      var datesAttr = el.getAttribute('data-dates');
      var pricesAttr = el.getAttribute('data-prices');
      if (!datesAttr || !pricesAttr) {
        setStatus(el, '股价数据缺失', '#E3232E');
        return true;
      }
      var dates = JSON.parse(datesAttr);
      var prices = JSON.parse(pricesAttr);
      Highcharts.chart(el, {
        chart: { type: 'line', backgroundColor: 'transparent', height: 150, margin: [20, 25, 25, 25] },
        title: { text: null },
        xAxis: { categories: dates, labels: { style: { fontSize: '8px' }, rotation: -45 }, tickLength: 2 },
        yAxis: { title: { text: null }, labels: { style: { fontSize: '9px' } } },
        series: [{
          name: '收盘价',
          data: prices,
          color: '#002B5B',
          lineWidth: 1.5,
          marker: { enabled: false },
          states: { hover: { lineWidthPlus: 2 } }
        }],
        legend: { enabled: false },
        credits: { enabled: false },
        tooltip: { valueDecimals: 2, headerFormat: '<b>{point.key}</b><br/>', pointFormat: '收盘价: {point.y:.2f}' },
        plotOptions: { series: { animation: false } }
      });
      return true;
    } catch(e) {
      var debug = '图表渲染失败: ' + e.message +
        '<br>data-dates raw: ' + String(datesAttr).replace(/</g, '&lt;').slice(0, 200) +
        '<br>data-prices raw: ' + String(pricesAttr).replace(/</g, '&lt;').slice(0, 200);
      setStatus(el, debug, '#E3232E');
      console.warn('stock-mini-chart debug:', { datesAttr: datesAttr, pricesAttr: pricesAttr, error: e });
      return true;
    }
  }

  function scanCharts() {
    var charts = document.querySelectorAll(SELECTOR);
    var allDone = true;
    Array.prototype.forEach.call(charts, function(el) {
      if (el.getAttribute(RENDERED_ATTR) !== '1') {
        var done = renderChart(el);
        if (!done) allDone = false;
      }
    });
    return allDone;
  }

  var intervalId = null;
  function startScan() {
    if (intervalId) return;
    // initial scan
    scanCharts();
    // poll until all charts are done, then keep a slow watchdog for dynamically added rows
    intervalId = setInterval(function() {
      var allDone = scanCharts();
      if (allDone && document.querySelectorAll(SELECTOR).length > 0) {
        clearInterval(intervalId);
        intervalId = null;
        // restart on any click in case rows are expanded later
      }
    }, 400);
  }

  function init() {
    startScan();
    // re-scan after clicks (reactable row expand/collapse)
    document.addEventListener('click', function() {
      setTimeout(startScan, 50);
      setTimeout(startScan, 250);
    });
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', init);
  } else {
    init();
  }
})();
