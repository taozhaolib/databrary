'use strict';
//to be paired with directive trashable. 'outside' is a shared parent scope

module.directive('expandingSelect', [
  function () {
    var link = function ($scope, $el, $attr) {
      $scope.choices = $attr.choices || [
          {short: 'short', long: 'laaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaang'},
          {short: 'b-short', long: 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'}
      ];
      $scope.focus = false;
      $scope.value = $scope.choices[0];
    };

    return {
      restrict: 'E',
      scope: {
	//choices: "="
      },
      link: link,
      templateUrl: 'expandingSelect.html',
      replace: false
    };
  }]);
