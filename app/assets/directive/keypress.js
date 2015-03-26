'use strict';

(function () {

var keys = {
  Tab: 9,
  Enter: 13,
  Escape: 27,
  Left: 37,
  Up: 38,
  Right: 39,
  Down: 40,
};
_.each(keys, function(key, name){
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
