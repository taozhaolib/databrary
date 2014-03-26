define(['config/module'], function (module) {
	'use strict';

	module.directive('browserList', ['BrowserService', '$filter', 'ConstantService', 'RouterService', 'TypeService', 'AuthService', function (browserService, $filter, constantService, routerService, typeService, auth) {
		var link = function ($scope, $el, $attrs) {
			if (!$scope.browser)
				$scope.browser = browserService;

			if (!$scope.router)
				$scope.router = routerService;

			if (!$scope.constant)
				$scope.constant = constantService;

			if (!$scope.type)
				$scope.type = typeService;

			$scope.data = $scope.data || browserService.data;

			$scope.getInclude = function () {
				if (!$scope.data.items[0])
					return '';

				switch ($scope.data.items[0].group) {
					case 'volume':
						return 'browserVolume.html';

					case 'session':
						return 'browserSession.html';

					case 'asset':
						return 'browserAsset.html';

					default:
						return 'browserRecord.html';
				}
			};

			$scope.itemClasses = function (data) {
				var classes = [];

				if (!data.expand)
					classes.push('deepest');

				return classes;
			};

			$scope.setItemSelect = function (data) {
				$scope.browser.setItemSelect(data);
			};

			//

			$scope.setItemPlayer = function (data) {
				if (auth.hasAccess('DOWNLOAD', data))
					$scope.browser.setItemPlayer(data);
			};

			//

			$scope.getName = function (data) {
				switch ($scope.type.getType(data.object)) {
					case 'volume':
						return (auth.hasAccess('CONTRIBUTE', data) && data.object.alias) ? data.object.alias : data.object.name;

					case 'record':
						var category = $scope.constant.data.category[data.object.category].name;
						return category.charAt(0).toUpperCase() + category.slice(1) + ': ' + (data.object.measures.ident || data.object.id);

					case 'session':
						return 'Session: ' + (data.object.name || data.object.id);
				}
			};

			$scope.formatAge = function (age) {
				return $filter('age')(age);
			};

			$scope.formatSessionCategory = function (data, categoryID, records) {
				var category = $scope.constant.get('category', categoryID);

				if (!category)
					return 'Uncategorized';

				if (!records[1])
					return category.name.charAt(0).toUpperCase() + category.name.slice(1);
				else
					switch (category.name) {
						default:
							return category.name.charAt(0).toUpperCase() + category.name.slice(1) + 's';
					}
			};

			$scope.capitalize = function (input) {
				return input.charAt(0).toUpperCase() + input.slice(1);
			};

			$scope.recordIdentifier = function (record) {
				if (record.id != 0)
					switch (record.category) {
						case -700:
							return record.measures.reason;

						case -100:
							var out = record.measures.setting;

							if(record.measures.state)
								out += ', '+record.measures.state;

							if(record.measures.country)
								out += ', '+record.measures.country;

							return out;

						default:
							return record.measures.ident;
					}
			};

			var measures = undefined;

			$scope.getMeasures = function (data) {
				if(measures)
					return measures;

				// TODO: something with better performance!
				var measures = {}, skip = ['description', 'pilot', 'exclusion'];

				switch (data.object.category) {
					case -700:
						skip.push('reason');
						break;

					case -100:
						skip.push('setting');
						skip.push('state');
						skip.push('country');
						break;

					default:
						skip.push('ident');
						break;
				}

				angular.forEach(data.object.measures, function (value, key) {
					if (skip.indexOf(key) == -1)
						measures[key] = value;
				});

				return measures;
			};

			var sessionRecords = undefined;

			$scope.getSessionRecords = function (data) {
				if(sessionRecords)
					return sessionRecords;

				// TODO: something with better performance!
				var sessionRecords = {}, skip = ['-700', '-800'];

				angular.forEach(data.object.categories, function (value, key) {
					if (skip.indexOf(key) == -1)
						sessionRecords[key] = value;
				});

				return sessionRecords;
			};

			$scope.nameRecord = function (data) {
				var category = $scope.constant.get('category', data.object.category),
					name;

				if (data.object.id == 0) {
					switch (category.id) {
						case -800:
							name = 'Not pilot';
							break;

						case -700:
							name = 'Included';
							break;

						case -500:
							name = 'No participants';
							break;

						case -400:
							name = 'No conditions';
							break;

						case -200:
							name = 'Not grouped';
							break;

						case -100:
							name = 'No location';
							break;

						default:
							name = 'No ' + category.name;
							break;
					}
				} else {
					name = $scope.capitalize($scope.constant.get('category', data.object.category).name);
				}

				var identifier = $scope.recordIdentifier(data.object);

				if (identifier)
					name += ': ' + identifier;

				return name;
			};

			$scope.queryFilter = function (data) {
				if (!browserService.query)
					return true;

				var regex = new RegExp(browserService.query.toLowerCase().split(' ').join("|"));

				if (!typeService.isVolume(data.object))
					return true;

				if (data.object.name && regex.test(data.object.name.toLowerCase()))
					return true;

				if (data.object.body && regex.test(data.object.body.toLowerCase()))
					return true;

				return false;
			};

			//

			$scope.editLink = function (data) {
				switch ($scope.type.getType(data.object)) {
					case 'volume':
						return $scope.router.volumeEdit(data.object);

					case 'record':
						return $scope.router.recordEdit(data.object);

					case 'session':
						return $scope.router.slotEdit(data.object);

					case 'asset':
						return $scope.router.assetEdit(data.object);
				}
			};
		};

		return {
			restrict: 'E',
			scope: false,
			templateUrl: 'browserList.html',
			replace: true,
			priority: 100,
			link: link
		};
	}]);
});
