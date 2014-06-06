module.factory('constantService', [
	'$http', '$log', 'constantData', function ($http, $log, constantData) {
		var constants = {
			data: constantData,
			regex: {
				doi: /^(?:doi:|(?:http:\/\/)?dx\.doi\.org\/)?(10\.[0-9\.]+\/.*)$/,
			},
		};

		//

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

		var addPresets = function(data) {
			data.preset = {
				institution: [
					{
						inherit: data.permissionName.DOWNLOAD,
						direct: 0
					},
					{
						inherit: data.permissionName.CONTRIBUTE,
						direct: 0
					},
					{
						inherit: data.permissionName.ADMIN,
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
						inherit: data.permissionName.DOWNLOAD,
						direct: data.permissionName.DOWNLOAD
					},
					{
						inherit: data.permissionName.CONTRIBUTE,
						direct: data.permissionName.CONTRIBUTE
					},
					{
						inherit: data.permissionName.ADMIN,
						direct: data.permissionName.ADMIN
					},
					{
						inherit: undefined,
						direct: undefined,
						custom: true
					}
				]
			};
		};

		constants.update = function () {
			constants.data.permissionName = invertArray(constants.data.permission);
			constants.data.classificationName = invertArray(constants.data.classification);
			constants.data.consentName = invertArray(constants.data.consent);
			constants.data.categoryName = invertBy(constants.data.category, "name");

			/* convenient aliases: */
			constants.data.permissionName.EDIT = constants.data.permissionName.CONTRIBUTE;
			constants.data.permissionName.SUPER = constants.data.permission.length;

			addPresets(constants.data);
		};

		constants.message = function (key /*, args...*/) {
			var msg = constants.data.messages[key];

			if (!msg) {
				$log.info('Message key ['+key+'] is undefined.');
				return '[' + key + ']';
			}

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
