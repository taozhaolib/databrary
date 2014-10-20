'use strict';

app.directive('frame', [
  'constantService',
  function (constants) {
    var compile = function ($element, $attrs, transclude) {
      return function ($scope, $element, $attrs) {
        var start = function () {
          $scope.id = $attrs.id;
          $scope.title = constants.message($attrs.frameTitle);

          transclude($scope, function ($clone) {
            $element.find('[frame-body]').append($clone);
          });
        };

        start();
      };
    };

    return {
      restrict: 'A',
      templateUrl: 'site/frame.html',
      scope: true,
      transclude: true,
      replace: true,
      priority: 100,
      compile: compile
    };
  }
]);
