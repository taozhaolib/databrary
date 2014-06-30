module.directive('authSearchForm', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			var form = $scope.authSearchForm;

			form.nameVal = '';
			form.found = [];
			form.id = $attrs.party || undefined;

			form.principal = $attrs.principal;

			$scope.$watch(function () {
				return form.principal
			}, function (principal) {
				form.nameVal = '';
				form.found = [];
				form.validator.client({
					name: {
						tips: page.constants.message('auth.search.' + (principal || 'placeholder') + '.help')
					}
				}, true);
			});

			//

			var recentSearch = undefined;
			var sentSearch = undefined;

			var fin = function (res) {
				sentSearch = undefined;

				if (recentSearch) {
					recentSearch = undefined;
					form.search();
				}
			};

			form.search = function () {
				if (!form.nameVal || form.nameVal.length < 3) {
					form.found = [];
				} else if (sentSearch) {
					recentSearch = form.nameVal;
				} else {
					sentSearch = page.models.PartyAuthorize.search({
						id: form.id || page.auth.user.id,
						name: form.nameVal,
						institution: form.principal === 'principal' ? true :
								form.principal === 'affiliate' ? false : undefined,
					}, function (data) {
						form.found = data;

						fin();
					}, function (res) {
						fin(res);
					});
				}
			};

			//

			form.selectFn = undefined;

			form.select = function (found) {
				form.nameVal = '';
				form.search();

				if (angular.isFunction(form.selectFn)) {
					form.selectFn(found, form);
				}
			};

			//

			form.notFoundFn = undefined;

			form.notFound = function () {
				var query = form.nameVal;

				form.nameVal = '';
				form.search();

				if (angular.isFunction(form.notFoundFn)) {
					form.notFoundFn(query, form);
				}
			};

			//

			form.validator.client({
				name: {
					tips: page.constants.message('auth.search.name.help'),
				},
			}, true);

			//

			page.events.talk('authSearchForm-init', form);
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
