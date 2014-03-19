define(['config/module'], function (module) {
	'use strict';

	module.factory('PlayService', ['$rootScope', '$window', '$location', 'ConstantService', 'TypeService', function ($rootScope, $window, $location, constantService, typeService) {
		var playService = {};

		//

		playService.run = function () {
			if(!$window.$play || !$window.$play.object)
				return;

			switch(typeService.getType($window.$play.object)) {
//				case 'record':
//					constantService.$promise.then(function (data) {
//						$location.url('/volume/' + $window.$play.object.volume + '?' + constant.data.category[$window.$play.object.category].name + '_limit=' + $window.$play.object.id);
//					});
//					break;
//
//				case 'session':
//					$location.url('/volume/' + $window.$play.object.volume + '?session_limit=' + $window.$play.object.id);
//					break;
//
//				case 'asset':
//					$location.url('/volume/' + $window.$play.object.container.volume + '?session_limit=' + $window.$play.object.container.id + '&asset_limit=' + $window.$play.object.asset.id);
//					break;

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
	}]);
});
