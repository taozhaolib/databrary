module.factory('constantService', [
	'$http', function ($http) {
		var constants = {
			data: {}
		};

		//

		var STATIC_DATA = {
			preset: {
				institution: [
					{
						inherit: 2,
						direct: 0
					},
					{
						inherit: 3,
						direct: 0
					},
					{
						inherit: 4,
						direct: 0
					},
					{
						inherit: undefined,
						direct: undefined,
						custom: true
					}
				],

				individual: [
					{
						inherit: 2,
						direct: 2
					},
					{
						inherit: 3,
						direct: 3
					},
					{
						inherit: 4,
						direct: 4
					},
					{
						inherit: undefined,
						direct: undefined,
						custom: true
					}
				]
			}
		};

		//

		constants.update = function () {
			constants.$promise = $http.get('/api/constants');

			constants.$promise.then(function (result) {
				angular.extend(constants.data, STATIC_DATA, result.data);
			});
		};

		constants.get = function (key, id) {
			if (angular.isUndefined(constants.data[key]))
				return undefined;

			if (angular.isDefined(id))
				return constants.data[key][id] || undefined;

			return constants.data[key];
		};

		constants.find = function (key, name) {
			var data = constants.data[key];

			if (angular.isDefined(data))
				for (var id in data)
					if (data.hasOwnProperty(id) && data[id].name == name)
						return data[id];

			return undefined;
		};

		constants.message = function (key /*, args...*/) {
			if (!constants.data || !constants.data.messages || !constants.data.messages[key])
			// warning? error? placeholder.
				return '[' + key + ']';

			var msg = constants.data.messages[key];

			for (var i = 1, length = arguments.length; i < length; i++)
				msg = msg.replace('{' + (i - 1) + '}', arguments[i], 'g');

			return msg;
		};

		constants.permission = function (key) {
			for (var prop in constants.data.permission) {
				if (!constants.data.permission.hasOwnProperty(prop))
					continue;

				if (constants.data.permission[prop].id == key || constants.data.permission[prop].name === key)
					return constants.data.permission[prop];
			}
		};

		//

		constants.update();

		return constants;
	}
]);
