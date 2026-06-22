document.addEventListener('DOMContentLoaded', function() {
  function fixChartWidths() {
    var widgets = document.querySelectorAll('.chart-widget');
    widgets.forEach(function(widget) {
      var innerDiv = widget.querySelector('div[id^="htmlwidget-"]');
      if (innerDiv) innerDiv.style.width = '100%';
      var hcDiv = widget.querySelector('.highcharts-container');
      if (hcDiv) hcDiv.style.width = '100%';
    });
  }
  fixChartWidths();
  window.addEventListener('resize', fixChartWidths);
  setTimeout(fixChartWidths, 500);
  setTimeout(fixChartWidths, 1500);
});
