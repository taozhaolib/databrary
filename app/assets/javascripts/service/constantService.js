define(['config/module'], function (module) {
	'use strict';

	module.factory('ConstantService', ['$rootScope', '$http', function ($rootScope, $http) {
		var constantService = {
			data: {}
		};

		//

		var STATIC_DATA = {
			preset: {
				institution: [
					{
						inherit: 2,
						direct: 0,
						name: '<strong>Affiliate</strong>: View shared Databrary data.'
					}, {
						inherit: 3,
						direct: 0,
						name: '<strong>Investigator</strong>: Create and modify datasets and studies.'
					}, {
						inherit: 4,
						direct: 0,
						name: '<strong>Admin</strong>: Delegate authorization and data access.'
					}, {
						inherit: undefined,
						direct: undefined,
						custom: true,
						name: '<strong>Custom...</strong>'
					}
				],

				individual: [
					{
						inherit: 2,
						direct: 2,
						name: '<strong>Read-only Affiliate</strong>: View shared Databrary data and all my data.'
					}, {
						inherit: 3,
						direct: 3,
						name: '<strong>Data Editor</strong>: Create and modify datasets and studies.'
					}, {
						inherit: 4,
						direct: 4,
						name: '<strong>Lab Manager</strong>: Delegate authorization and data access.'
					}, {
						inherit: undefined,
						direct: undefined,
						custom: true,
						name: '<strong>Custom...</strong>'
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
