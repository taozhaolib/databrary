'use strict';

module.directive('slotToolbar', [
  'pageService', function (page) {
    var controller = [
      '$scope', '$element', '$attrs', function ($scope, $element, $attrs) {

      }
    ];

    //

    return {
      restrict: 'E',
      scope: true,
      templateUrl: 'slotToolbar.html',
      controller: controller,
      controllerAs: 'toolbar',
    };
  }
]);
