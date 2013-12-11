// general

define(['app/config/module'], function (angular) {
	'use strict';

	return angular;
});

// controller

define(['app/config/module'], function (module) {
	'use strict';

	module.controller('Ctrl', ['$scope', function ($scope) {

	}]);
});

// directive

define(['app/config/module'], function (module) {
	'use strict';

	module.directive('', [function () {
		var link = function ($scope, $element, $attrs) {

		};

		return {
			restrict: 'AECM',
			link: link
		};
	}]);
});

// service, provider

define(['app/config/module'], function (module) {
	'use strict';

	module.provider('Service', ['$rootScope', function ($rootScope) {
		return {
			$get: {

			}
		};
	}]);
});

// service, factory

define(['app/config/module'], function (module) {
	'use strict';

	module.factory('Service', ['$rootScope', function ($rootScope) {
		var Service;

		return Service;
	}]);
});

// service, factory

define(['app/config/module'], function (module) {
	'use strict';

	var Service = function () {

	};

//	Service.prototype.method = function () {};

	module.service('Service', ['$rootScope', Service]);
});
