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
			},
			regex: {
				doi: /^(?:doi:|(?:http:\/\/)?dx\.doi\.org\/)?(10\.[0-9\.]+\/.*)$/,
			},
		};

		var invertArray = function (data) {
			var r = {};
			for (var id in data) {
				if (data.hasOwnProperty(id))
					r[data[id]] = id;
			}
			return r;
		};

		var invertBy = function (data, field) {
			var r = {};
			for (var id in data) {
				if (data.hasOwnProperty(id) && field in data[id])
					r[data[id][field]] = data[id];
			}
			return r;
		};

		constants.update = function () {
			constants.$promise = $http.get('/api/constants');

			constants.$promise.then(function (result) {
				angular.extend(constants.data, STATIC_DATA, result.data);
				constants.data.permissionName = invertArray(constants.data.permission);
				constants.data.classificationName = invertArray(constants.data.classification);
				constants.data.consentName = invertArray(constants.data.consent);
				constants.data.categoryName = invertBy(constants.data.category, "name");
			});
		};

		constants.message = function (key /*, args...*/) {
			if (!constants.data || !constants.data.messages || !constants.data.messages[key])
			// warning? error? placeholder.
			{
				return '[' + key + ']';
			}

			var msg = constants.data.messages[key];

			for (var i = 1, length = arguments.length; i < length; i++) {
				msg = msg.replace('{' + (i - 1) + '}', arguments[i], 'g');
			}

			return msg;
		};

		//

		constants.update();

		return constants;
	}
]);
