module.directive('toolbar', [
	'pageService', function (page) {
		var controller = function ($scope) {
			this.home = function () {
				return page.auth.hasAuth('VIEW') ? page.router.home() : page.router.index();
			};

			//

			this.hoverUser = false;

			this.hideHover = function () {
				this.hoverUser = false;
			};

			//

			this.links = function () {
				return page.$filter('filter')(page.display.toolbarLinks, function (link) {
					return !link.access || !link.object ? true : page.auth.hasAccess(link.access, link.object);
				});
			};

			//

			this.stopProp = function ($event) { console.log(arguments);
				$event.stopImmediatePropagation();
				$event.stopPropagation();
			}
		};

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
