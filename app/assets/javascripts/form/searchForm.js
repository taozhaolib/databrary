module.directive('searchForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.searchForm;

			form.data = {
				query: page.$location.search().query
			};
			form.filterMode = true;

			$scope.$watch('searchForm.data.query', function (val, old) {
				if (page.$location.path() !== '/search' && val !== old) {
					page.$location.path('/search');
				}

				if (form.filterMode === true) {
					page.$location.search('query', form.data.query || undefined);
				}
			});

			//

			form.searchFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;
			form.resetFn = undefined;

			//

			form.search = function () {
				if (angular.isFunction(form.saveFn)) {
					form.saveFn(form);
				}
			};

			form.reset = function () {
				if (angular.isFunction(form.resetFn)) {
					form.resetFn(form);
				}

				form.data = {};
				form.filterMode = true;
				form.$setPristine();
			};

			//

			page.events.talk('searchForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'searchForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
