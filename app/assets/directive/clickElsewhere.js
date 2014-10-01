'use strict';

module.directive('clickElsewhere', [
  '$parse', '$document',
  function ($parse, $document) { return {
    compile: function ($element, $attrs) {
      var action = $parse($attrs.clickElsewhere);
      return function ($scope, $element) {
        $document.on('click', function (event) {
          if (!$.contains($element[0], event.target))
            $scope.$apply(function () {
              action($scope, {$event:event});
            });
        });
      };
    }
  }; }
]);
