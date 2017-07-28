HTMLWidgets.widget({

  name: 'displayWidget',

  type: 'output',

  factory: function(el, width, height) {

    var viewer = new Viewer(el.id);

    return {

      renderValue: function(x) {
        viewer.reset(x.data, x.width, x.height);
      },

      resize: function(width, height) {
        viewer.resetCanvas();
      }

    };
  }
});
