module.directive('searchForm', [
	'pageService',
	function (page) {
		var link = function ($scope) {
			var form = $scope.searchForm;

			form.data = {
				query: page.$location.search().query
			};
			form.filterMode = true;

			$scope.$watch('searchForm.data.query', function (val, old) {
				if (page.$location.path() !== '/search' && val !== old)
					page.$location.path('/search');

				if (form.filterMode === true)
					page.$location.search('query', form.data.query || undefined);
			});

			//

			form.seearchFn = undefined;
			form.successFn = undefined;
			form.errorFn = undefined;
			form.resetFn = undefined;

			//

			var query = function () {
				page.models.Volume.query(form.data,
					function (res) {
						if (form.filterMode)
							page.messages.add({
								type: 'yellow',
								countdown: 3000,
								body: page.constants.message('search.deep.info')
							});

						if (angular.isFunction(form.successFn))
							form.successFn(form, res);

						form.filterMode = false;
						page.$location.search('query', form.data.query);
					}, function (res) {
						page.messages.addError({
							body: page.constants.message('search.error'),
							report: res
						});

						if (angular.isFunction(form.errorFn))
							form.errorFn(form, res);
					});
			};

			form.search = function () {
				if (angular.isFunction(form.saveFn))
					form.saveFn(form);

				query();
			};

			form.reset = function () {
				if (angular.isFunction(form.resetFn))
					form.resetFn(form);

				form.data = {};
				form.filterMode = true;
				form.$setPristine();

				query();
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
