'use strict';

module.directive('slotPlayer', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
        var player = this;
        var ctrl = page.$parse($attrs.ctrl)($scope);
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
