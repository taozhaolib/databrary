'use strict';

module.directive('assetDisplay', [
  'pageService', function (page) { return {
    restrict: 'E',
    templateUrl: 'assetDisplay.html',
    scope: {
      assetFn: '&asset',
    },
    link: function ($scope) {
      var asset = $scope.assetFn();
      $scope.asset = asset.inContext();

      if ($scope.asset !== asset)
	$scope.clip = asset.segment.relativeTo($scope.asset.segment);
    }
  }; }
]);
