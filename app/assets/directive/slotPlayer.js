'use strict';

module.directive('slotPlayer', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
        var player = this;
        var ctrl = page.$parse($attrs.ctrl)($scope);

        var $viewport;
        var viewportW;

        var resizeFn = function () {
          var newW = $viewport.width();

          if (newW !== viewportW) {
            viewportW = newW;
            $viewport.height(viewportW * 9 / 16);
          }
        };

        page.$w.resize(resizeFn);
        page.$timeout(function () {
          $viewport = $element.find('.player-main-viewport');
          resizeFn();
        });
      }
    ];

    //

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'slotPlayer.html',
      controller: controller,
      controllerAs: 'player',
    };
  }
]);
