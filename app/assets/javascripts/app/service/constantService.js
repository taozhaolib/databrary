define(['app/config/module'], function (module) {
	'use strict';

	module.factory('ConstantService', ['$rootScope', '$http', function ($rootScope, $http) {
		var constantService = {
			data: {}
		};

		//

		var STATIC_DATA = {
			preset: [
				{
					inherit: 0,
					direct: 0,
					name: 'No access.'
				},
				{
					inherit: 2,
					direct: 0,
					name: 'Explore Databrary.'
				},
				{
					inherit: 2,
					direct: 3,
					name: 'Manage my lab.'
				},
				{
					inherit: 3,
					direct: 0,
					name: 'Investigator.'
				},
				{
					inherit: undefined,
					direct: undefined,
					name: 'Custom...'
				}
			]
		};

		//

		constantService.update = function () {
			constantService.$promise = $http.get('/api/constants');

			constantService.$promise.then(function (result) {
				angular.extend(constantService.data, STATIC_DATA, result.data);
			});
		};

		constantService.get = function (key, id) {
			if (angular.isUndefined(constantService.data[key]))
				return undefined;

			if (angular.isDefined(id))
				return constantService.data[key][id] || undefined;

			return constantService.data[key];
		};

		constantService.find = function (key, name) {
			var data = constantService.data[key];

			if (angular.isDefined(data))
				for (var id in data)
					if (data.hasOwnProperty(id) && data[id].name == name)
						return data[id];

			return undefined;
		};

		//

		constantService.update();

		return constantService;
	}]);
});
