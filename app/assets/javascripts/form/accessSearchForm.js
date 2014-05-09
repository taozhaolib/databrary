module.directive('accessSearchForm', [
	'PartyAuthorize', 'authService', 'pageService', '$timeout', function (PartyAuthorize, authService, page, $timeout) {
		var link = function ($scope, $element, $attrs) {
			$scope.accessSearchForm.name = '';
			$scope.accessSearchForm.found = [];
			$scope.accessSearchForm.id = $attrs.party || undefined;
			$scope.accessSearchForm.apply = angular.isDefined($attrs.child);

			$attrs.$observe('institution', function () {
				$scope.accessSearchForm.name = '';
				$scope.accessSearchForm.found = [];
			});

			//

			var recentSearch = undefined;
			var sentSearch = undefined;

			var fin = function () {
				sentSearch = undefined;

				if (recentSearch) {
					recentSearch = undefined;
					$scope.accessSearchForm.search();
				}
			};

			$scope.accessSearchForm.search = function () {
				if (!$scope.accessSearchForm.name || $scope.accessSearchForm.name.length < 3)
					$scope.accessSearchForm.found = [];
				else if (sentSearch)
					recentSearch = $scope.accessSearchForm.name;
				else
					sentSearch = PartyAuthorize.search({
						id: $scope.accessSearchForm.id || authService.user.id,
						apply: $scope.accessSearchForm.apply,
						name: $scope.accessSearchForm.name,
						institution: $element.attr('institution') === 'true'
					}, function (data) {
						$scope.accessSearchForm.found = data;

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

			$scope.accessSearchForm.selectFn = undefined;

			$scope.accessSearchForm.select = function (found) {
				$scope.accessSearchForm.name = '';
				$scope.accessSearchForm.search();

				if (angular.isFunction($scope.accessSearchForm.selectFn))
					$scope.accessSearchForm.selectFn(found, $scope.accessSearchForm);
			};

			//

			$scope.accessSearchForm.notFoundFn = undefined;

			$scope.accessSearchForm.notFound = function () {
				var query = $scope.accessSearchForm.name;

				$scope.accessSearchForm.name = '';
				$scope.accessSearchForm.search();

				if (angular.isFunction($scope.accessSearchForm.notFoundFn))
					$scope.accessSearchForm.notFoundFn(query, $scope.accessSearchForm);
			};

			//

			page.events.talk('accessSearchForm-init', $scope.accessSearchForm);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'accessSearchForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
