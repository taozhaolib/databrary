module.directive('toolbar', [
	'pageService', function (page) {
		var controller = [function () {
			this.hoverUser = false;

			this.hideHover = function () {
				this.hoverUser = false;
			};

			//

			this.links = function () {
				return page.$filter('filter')(page.display.toolbarLinks, function (link) {
					return link.access && link.object ? page.auth.hasAccess(link.access, link.object) :
						link.auth ? page.auth.hasAuth(link.auth) : true;
				});
			};
		}];

		return {
			restrict: 'A',
			templateUrl: 'toolbar.html',
			replace: true,
			scope: true,
			controller: controller,
			controllerAs: 'toolbar',
		};
	}
]);
