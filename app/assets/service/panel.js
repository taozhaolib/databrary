'use strict';

module.factory('panelService', [
  '$rootScope', '$timeout', 'displayService',
  function ($rootScope, $timeout, display) {
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
