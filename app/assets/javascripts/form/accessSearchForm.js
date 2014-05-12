module.directive('accessSearchForm', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			var form = $scope.accessSearchForm;

			form.name = '';
			form.found = [];
			form.id = $attrs.volume || undefined;
			form.institution = $element.attr('institution') === 'true';

			$attrs.$observe('institution', function () {
				form.name = '';
				form.found = [];
			});

			//

			var recentSearch = undefined;
			var sentSearch = undefined;

			var fin = function () {
				sentSearch = undefined;

				if (recentSearch) {
					recentSearch = undefined;
					form.search();
				}
			};

			form.search = function () {
				if (!form.name || form.name.length < 3) {
					form.found = [];
				}
				else if (sentSearch) {
					recentSearch = form.name;
				}
				else {
					sentSearch = page.models.VolumeAccess.search({
						id: form.id,
						name: form.name,
						institution: form.institution,
					}, function (data) {
						form.found = data;

						fin();
					}, function (res) {
						page.messages.addError({
							body: page.constants.message('access.search.error'),
							errors: res[0],
							status: res[1]
						});

						fin();
					});
				}
			};

			//

			form.selectFn = undefined;

			form.select = function (found) {
				form.name = '';
				form.search();

				if (angular.isFunction(form.selectFn)) {
					form.selectFn(found, form);
				}

				form.$setPristine();
			};

			//

			form.notFoundFn = undefined;

			form.notFound = function () {
				page.messages.add({
					type: 'yellow',
					countdown: 3000,
					body: page.constants.message('access.grant.notfound.message'),
				});

				var query = form.name;

				form.name = '';
				form.search();

				if (angular.isFunction(form.notFoundFn)) {
					form.notFoundFn(query, form);
				}

				form.$setPristine();
			};

			//

			$scope.$emit('accessSearchForm-init', form);
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
