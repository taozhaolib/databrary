define(['config/module'], function (module) {
	'use strict';

	module.directive('authSearchForm', ['PartyAuthorize', 'authService', 'pageService', function (PartyAuthorize, authService, page) {
		var link = function ($scope, $element, $attrs) {
			$scope.authSearchForm.name = '';
			$scope.authSearchForm.found = [];
			$scope.authSearchForm.id = $attrs.party || undefined;
			$scope.authSearchForm.apply = angular.isUndefined($attrs.child);

			//

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
					}, function (res) {
						page.messages.addError({
							body: page.constants.message('auth.search.error'),
							errors: res[0],
							status: res[1]
						});
					});
			};

			//

			$scope.authSearchForm.selectFn = undefined;

			$scope.authSearchForm.select = function (found) {
				$scope.authSearchForm.name = '';
				$scope.authSearchForm.search();

				if (angular.isFunction($scope.authSearchForm.selectFn))
					$scope.authSearchForm.selectFn(found, $scope.authSearchForm);
			};

			//

			$scope.authSearchForm.notFoundFn = undefined;

			$scope.authSearchForm.notFound = function () {
				var query = $scope.authSearchForm.name;

				$scope.authSearchForm.name = '';
				$scope.authSearchForm.search();

				if (angular.isFunction($scope.authSearchForm.notFoundFn))
					$scope.authSearchForm.notFoundFn(query, $scope.authSearchForm);
			};

			//

			page.events.talk('authSearchForm-init', $scope.authSearchForm);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'authSearchForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}]);
});
