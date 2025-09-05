HTMLWidgets.widget({

  name: 'roughViz',

  type: 'output',

  factory: function(el, width, height) {

    var chart = null;

    return {

      renderValue: function(x) {
        
        // Clear any existing content
        el.innerHTML = '';
        
        // Set the element
        x.config.element = '#' + el.id;
        
        // Set dimensions if not provided
        if (!x.config.width) {
          x.config.width = width;
        }
        if (!x.config.height) {
          x.config.height = height;
        }
        
        // Create the appropriate chart type
        // Note: window.roughViz is the global object exported by the UMD build
        switch(x.chartType) {
          case 'Bar':
            chart = new window.roughViz.Bar(x.config);
            break;
          case 'BarH':
            chart = new window.roughViz.BarH(x.config);
            break;
          case 'Donut':
            chart = new window.roughViz.Donut(x.config);
            break;
          case 'Line':
            chart = new window.roughViz.Line(x.config);
            break;
          case 'Pie':
            chart = new window.roughViz.Pie(x.config);
            break;
          case 'Scatter':
            chart = new window.roughViz.Scatter(x.config);
            break;
          case 'StackedBar':
            chart = new window.roughViz.StackedBar(x.config);
            break;
          default:
            console.error('Unknown chart type: ' + x.chartType);
        }
      },

      resize: function(width, height) {
        // Handle resize if needed
        // Note: roughViz may not support dynamic resizing
        // so we might need to recreate the chart
        if (chart) {
          // For now, we'll leave this empty as roughViz doesn't have built-in resize
        }
      }

    };
  }
});