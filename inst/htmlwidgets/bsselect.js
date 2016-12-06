HTMLWidgets.widget({

  name: 'bsselectR',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(opts) {

        $('.selectpicker').selectpicker({
        actionsBox: opts.actionsBox,
        dropdownAlignRight: opts.dropdownAlignRight,
        dropupAuto: opts.dropupAuto,
        header: opts.header,
        liveSearch: opts.liveSearch,
        liveSearchStyle: opts.liveSearchStyle,
        showTick: opts.showTick

        }).addTo(el);


      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size

      }

    };
  }
});