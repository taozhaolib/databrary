module.factory('playService', [
	'$rootScope',
	'$window',
	'$location',
	'pageService',
	'typeService',
	'$sessionStorage',
	function ($rootScope, $window, $location, page, typeService, $sessionStorage) {
		var playService = {};

		//

		playService.run = function () {
			if (!$window.$play || !$window.$play.object)
				return;

			switch (typeService.getType($window.$play.object)) {
				case 'token':
					if ($window.$play.object.reset) {
						$location.url('/password');
					} else {
						$location.url('/register');
					}

					break;
			}
		};

		//

		return playService;
	}
]);
