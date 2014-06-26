module.controller('DataPanel', [
	'$scope', 'pageService', function ($scope, page) {
		$scope.refreshPanel = function () {
			$scope.enabled = $scope.volumes[0];
		}
	}
]);
