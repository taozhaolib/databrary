'use strict';

module.factory('panelService', [
  '$rootScope',
  function ($rootScope) {
    var panels = [];

    //

    panels.add = function (panel) {
      this.push(panel);

      if (panel.refreshPanel)
        panel.refreshPanel();

      return panel;
    };

    //

    $rootScope.$on('$routeChangeSuccess', function () {
      panels.splice(0, panels.length);
    });

    //

    return panels;
  }
]);
