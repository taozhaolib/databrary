define(['app/config/module'], function (module) {
	'use strict';

	module.directive('authSearchForm', ['PartyAuthorize', 'AuthService', 'EventService', function (PartyAuthorize, authService, eventService) {
		var link = function ($scope, $element, $attrs) {
			$scope.authSearchForm.name = '';
			$scope.authSearchForm.found = [];
			$scope.authSearchForm.id = $attrs.party || undefined;
			$scope.authSearchForm.apply = $attrs.apply || false;
			$scope.authSearchForm.apply = angular.isDefined($attrs.child);

			$scope.authSearchForm.search = function () {
				if (!$scope.authSearchForm.name)
					$scope.authSearchForm.found = [];
				else
					PartyAuthorize.search({
						id: $scope.authSearchForm.id || authService.user.id,
						apply: $scope.authSearchForm.apply,
						name: $scope.authSearchForm.name
					}, function (data) {
						$scope.authSearchForm.found = data;
					});
			};

			$scope.authSearchForm.selectFn = undefined;

			$scope.authSearchForm.select = function (found) {
				$scope.authSearchForm.name = '';
				$scope.authSearchForm.search();

				if (angular.isFunction($scope.authSearchForm.selectFn))
					$scope.authSearchForm.selectFn(found, $scope.authSearchForm);
			};

			eventService.talk('authSearchForm-init', $scope.authSearchForm);
		};

		return {
			restrict: 'E',
			templateUrl: 'authSearchForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}]);
});
