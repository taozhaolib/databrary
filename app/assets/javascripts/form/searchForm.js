module.directive('searchForm', [
	'pageService', function (page) {
		var link = function ($scope) {
			var form = $scope.searchForm;

			form.data = {
				query: page.$location.search().query
			};

			$scope.$watch('searchForm.data.query', function (val, old) {
				if (page.$location.path() !== '/search' && val !== old) {
					page.$location.path('/search');
				}

				page.$location.search('query', form.data.query || undefined);
			});

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
