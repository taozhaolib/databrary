'use strict';

app.directive('clickElsewhere', [
  '$parse', '$document',
  function ($parse, $document) { return {
    restrict: 'A',
    compile: function ($element, $attrs) {
      var action = $parse($attrs.clickElsewhere);
      return function ($scope, $element) {
        $document.on('click', function (event) {
          if ($element[0] != event.target && !$.contains($element[0], event.target))
            $scope.$apply(function () {
              action($scope, {$event:event});
            });
        });
      };
    }
  }; }
]);
