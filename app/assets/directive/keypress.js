'use strict';

(function () {

var keys = {
  Enter: 13,
  Tab: 9,
  Escape: 27
};
angular.forEach(keys, function (key, name) {
  var directive = 'key' + name;
  keys[directive] = [
    '$parse',
    function ($parse) { return {
      compile: function ($element, $attrs) {
        var action = $parse($attrs[directive]);
        return function ($scope, $element) {
          $element.on('keydown', function (event) {
            if (
              (event.key == name) ||
              (event.keyIdentifier == name) ||
              (event.keyCode == key) ||
              (event.which == key))
              return $scope.$apply(function () {
                return action($scope, {$event:event});
              });
          });
        };
      }
    }; }
  ];
});

app.directive(keys);

})();
