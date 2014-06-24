module.factory('constantService', [
	'$http', '$log', 'constantData', function ($http, $log, constantData) {
		var constants = {
			data: constantData,
			regex: {
				doi: /^(?:[dD][oO][iI]:|(?:http:\/\/)?dx\.doi\.org\/)?(10\.[0-9\.]+\/\S+)\s*$/,
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

		constants.update = function () {
			constants.data.permissionName = invertArray(constants.data.permission);
			constants.data.classificationName = invertArray(constants.data.classification);
			constants.data.consentName = invertArray(constants.data.consent);
			constants.data.categoryName = invertBy(constants.data.category, "name");

			constants.data.authPresets = {
				site: {},
				member: {},
			};

			angular.forEach(constants.data.permission, function (name, val) {
				switch(name) {
					case 'NONE':
					case 'READ':
						constants.data.authPresets.site[val] = name;
					case 'EDIT':
					case 'ADMIN':
						constants.data.authPresets.member[val] = name;
				}
			});

			/* convenient aliases: */
			constants.data.permissionName.CONTRIBUTE = constants.data.permissionName.EDIT;
			constants.data.permissionName.SUPER = constants.data.permission.length;
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
