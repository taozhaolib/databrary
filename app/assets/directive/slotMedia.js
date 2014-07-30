'use strict';

module.directive('slotMedia', [
  'pageService', function (page) {
    var pre = function ($scope, $element, $attrs) {
      var media = {
        asset: page.$parse($attrs.slotMedia)($scope),
        element: $element[0],
        $scope: $scope,
        $element: $element,
      };

      $scope.ctrl.media.registerMedia(media);

      // controller

      return media;
    };

    //

    return {
      restrict: 'A',
      link: {
        pre: pre
      },
    };
  }
]);
