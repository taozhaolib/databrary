define(['config/module'], function (module) {
	'use strict';

	module.factory('ConstantService', ['$http', function ($http) {
		var constantService = {
			data: {}
		};

		//

		var STATIC_DATA = {
			preset: {
				institution: [
					{
						inherit: 2,
						direct: 0
					}, {
						inherit: 3,
						direct: 0
					}, {
						inherit: 4,
						direct: 0
					}, {
						inherit: undefined,
						direct: undefined,
						custom: true
					}
				],

				individual: [
					{
						inherit: 2,
						direct: 2
					}, {
						inherit: 3,
						direct: 3
					}, {
						inherit: 4,
						direct: 4
					}, {
						inherit: undefined,
						direct: undefined,
						custom: true
					}
				]
			}
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

		constantService.message = function (key /*, args...*/) {
			if(!constantService.data || !constantService.data.messages || !constantService.data.messages[key])
				// warning? error? placeholder.
				return '[' + key + ']';

			var msg = constantService.data.messages[key];

			for (var i = 1, length = arguments.length; i < length; i++)
				msg = msg.replace('{' + (i-1) + '}', arguments[i], 'g');

			return msg;
		};

		//

		constantService.update();

		return constantService;
	}]);
});
