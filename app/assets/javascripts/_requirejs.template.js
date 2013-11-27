// general

define(['angular'], function (angular) {
	'use strict';

	return angular;
});

// controller

define(['app/modules/dbControllers'], function (db) {
	'use strict';

	db.controller('Ctrl', ['$scope', function ($scope) {

	}]);
});

// directive

define(['app/modules/dbDirectives'], function (db) {
	'use strict';

	db.directive('', [function () {
		var link = function ($scope, $element, $attrs) {

		};

		return {
			restrict: 'AECM',
			link: link
		};
	}]);
});

// service, provider

define(['app/modules/dbServices'], function (db) {
	'use strict';

	db.provider('Service', ['$rootScope', function ($rootScope) {
		return {
			$get: {

			}
		};
	}]);
});

// service, factory

define(['app/modules/dbServices'], function (db) {
	'use strict';

	db.factory('Service', ['$rootScope', function ($rootScope) {
		var Service;

		return Service;
	}]);
});

// service, factory

define(['app/modules/dbServices'], function (db) {
	'use strict';

	var Service = function () {

	};

//	Service.prototype.method = function () {};

	db.service('Service', ['$rootScope', Service]);
});
