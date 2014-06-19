module.directive('authSearchForm', [
	'pageService', function (page) {
		var link = function ($scope, $element, $attrs) {
			var form = $scope.authSearchForm;

			form.name = '';
			form.found = [];
			form.id = $attrs.party || undefined;
			form.apply = angular.isDefined($attrs.child);

			$attrs.$observe('principal', function (principal) {
				form.name = '';
				form.found = [];
			});

			//

			var recentSearch = undefined;
			var sentSearch = undefined;

			var fin = function (res) {
				form.validator.server(res || {});

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
					sentSearch = page.models.PartyAuthorize.search({
						id: form.id || page.auth.user.id,
						apply: form.apply,
						name: form.name,
						institution: $element.attr('principal') ? $element.attr('principal') === 'true' : undefined
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
				form.name = '';
				form.search();

				if (angular.isFunction(form.selectFn)) {
					form.selectFn(found, form);
				}
			};

			//

			form.notFoundFn = undefined;

			form.notFound = function () {
				var query = form.name;

				form.name = '';
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
