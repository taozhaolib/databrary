'use strict';

(function () {

var keys = {
  Enter: 13,
  Tab: 9
};
angular.forEach(keys, function (key, name) {
  var directive = 'key' + name;
  keys[directive] = [
    '$parse',
    function ($parse) { return {
      compile: function ($element, $attr) {
	var action = $parse($attr[directive]);
	return function ($scope, $element) {
	  $element.on('keypress', function (event) {
	    if (
	      (event.key == name) ||
	      (event.keyIdentifier == name) ||
	      (event.keyCode == key) ||
	      (event.which == key))
	      $scope.$apply(function () {
		action($scope, {$event:event});
	      });
	  });
	};
      }
    }; }
  ];
});

module.directive(keys);

})();
