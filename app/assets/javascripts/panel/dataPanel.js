module.controller('DataPanel', [
	'$scope', 'pageService', function ($scope, page) {
		$scope.refreshPanel = function () {
			$scope.enabled = $scope.volumes[0] || page.auth.hasAccess('EDIT', $scope.party) && page.auth.hasAuth('PUBLIC');
		}
	}
]);
