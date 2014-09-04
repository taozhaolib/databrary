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

    panels.toggleFold = function (panel, state) {
      if (!panel.foldable) {
        return undefined;
      }

      return panel.toggleFold(state);
    };

    //

    panels.focus = function (panel) {
      if (angular.isFunction(panel.toggleFold)) {
        panel.toggleFold(false);
      }

      var $document = $(document);
      var oldHeight = 0;
      var newHeight = 0;

      var checkHeight = function () {
        newHeight = $document.innerHeight();

        if (oldHeight == newHeight) {
          display.scrollTo(panel.id);
        } else {
          $timeout(function () {
            checkHeight();
          }, 150);
          oldHeight = newHeight;
        }
      };

      checkHeight();

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
