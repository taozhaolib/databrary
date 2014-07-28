'use strict';

module.factory('panelService', [
  '$rootScope',
  '$location',
  'eventService',
  '$timeout',
  'ArrayHelper',
  'displayService',
  function ($rootScope, $location, events, $timeout, ArrayHelper, display) {
    var panels = new ArrayHelper([]);

    //

    panels.add = function (panel) {
      var newPanel = ArrayHelper.prototype.add.call(this, panel);

      if (angular.isFunction(newPanel.bootPanel)) {
	newPanel.bootPanel();
      }

      if (angular.isFunction(newPanel.refreshPanel)) {
	newPanel.refreshPanel();
      }

      return newPanel;
    };

    //

    panels.enable = function (panel) {
      return panels.toggle(panel, 'enabled', true);
    };

    panels.disable = function (panel) {
      return panels.toggle(panel, 'enabled', false);
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

    panels.refresh = function () {
      angular.forEach(panels, function (panel) {
	if (angular.isFunction(panel.refresh)) {
	  panel.refreshPanel();
	}
      });
    };

    //

    events.listen($rootScope, 'panelService-refresh', function () {
      panels.refresh();
    });

    $rootScope.$on('$routeChangeSuccess', function () {
      panels.reset();
    });

    //

    return panels;
  }
]);
