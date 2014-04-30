module.directive('authSearchForm', [
	'PartyAuthorize', 'authService', 'pageService', '$timeout', function (PartyAuthorize, authService, page, $timeout) {
		var link = function ($scope, $element, $attrs) {
			$scope.authSearchForm.name = '';
			$scope.authSearchForm.found = [];
			$scope.authSearchForm.id = $attrs.party || undefined;
			$scope.authSearchForm.apply = angular.isDefined($attrs.child);

			//

			var recentSearch = undefined;
			var sentSearch = undefined;

			var fin = function () {
				sentSearch = undefined;

				if (recentSearch) {
					recentSearch = undefined;
					$scope.authSearchForm.search();
				}
			};

			$scope.authSearchForm.search = function () {
				if (!$scope.authSearchForm.name || $scope.authSearchForm.name.length < 3)
					$scope.authSearchForm.found = [];
				else if (sentSearch)
					recentSearch = $scope.authSearchForm.name;
				else
					sentSearch = PartyAuthorize.search({
						id: $scope.authSearchForm.id || authService.user.id,
						apply: $scope.authSearchForm.apply,
						name: $scope.authSearchForm.name
					}, function (data) {
						$scope.authSearchForm.found = data;

						fin();
					}, function (res) {
						page.messages.addError({
							body: page.constants.message('auth.search.error'),
							errors: res[0],
							status: res[1]
						});

						fin();
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
	}
]);
