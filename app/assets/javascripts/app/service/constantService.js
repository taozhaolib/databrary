define(['app/config/module'], function (module) {
	'use strict';

	module.factory('ConstantService', ['$rootScope', '$http', function ($rootScope, $http) {
		var constantService = {
			data: {}
		};

		//

		constantService.update = function () {
			constantService.$promise = $http.get('/api/constants');

			constantService.$promise.then(function (result) {
				constantService.data = result.data;
				console.log(constantService);
			});
		};

		constantService.get = function (key, id) {
			constantService.$promise.then(function () {
				if (angular.isUndefined(constantService.data[key]))
					return undefined;

				if (angular.isDefined(id))
					return constantService.data[key][id] || undefined;

				return constantService.data[key];
			});
		};

		//

		constantService.update();

		return constantService;
	}]);
});
