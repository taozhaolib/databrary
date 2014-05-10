module.directive('browserList', [
	'pageService', function (page) {
		var link = function ($scope) {
			if (!$scope.browser) {
				$scope.browser = page.browser;
			}

			if (!$scope.router) {
				$scope.router = page.router;
			}

			if (!$scope.constant) {
				$scope.constant = page.constants;
			}

			if (!$scope.type) {
				$scope.type = page.types;
			}

			$scope.data = $scope.data || page.browser.data;

			$scope.getInclude = function () {
				if (!$scope.data.items[0]) {
					return '';
				}

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

				if (!data.expand) {
					classes.push('deepest');
				}

				return classes;
			};

			$scope.setItemSelect = function (data) {
				$scope.browser.setItemSelect(data);
			};

			//

			$scope.setItemPlayer = function (data) {
				if (page.auth.hasAccess('DOWNLOAD', data)) {
					$scope.browser.setItemPlayer(data);
				}
				else if (data) {
					page.messages.add({
						type: 'yellow',
						countdown: 2000,
						body: page.constants.message('browser.noaccess')
					});
				}
			};

			//

			$scope.volumeClasses = function (data) {
				var cls = [], study = false;

				for (var prop in data.object.providers) {
					if (data.object.providers.hasOwnProperty(prop)) {
						study = true;
						break;
					}
				}

				if (study) {
					cls.push('browser_study');
				}
				else {
					cls.push('browser_dataset');
				}

				return cls;
			};

			//

			$scope.getName = function (data) {
				switch (page.types.getType(data.object)) {
					case 'volume':
						return (page.auth.hasAccess('CONTRIBUTE', data) && data.object.alias) ? data.object.alias : data.object.name;

					case 'record':
						var category = page.constants.data.category[data.object.category].name;
						return category.charAt(0).toUpperCase() + category.slice(1) + ': ' + (data.object.measures.ident || data.object.id);

					case 'session':
						return 'Session: ' + (data.object.name || data.object.id);
				}
			};

			$scope.formatAge = function (age) {
				return page.$filter('age')(age);
			};

			$scope.formatSessionCategory = function (data, categoryID, records) {
				var category = page.constants.get('category', categoryID);

				if (!category) {
					return 'Uncategorized';
				}

				if (!records[1]) {
					return category.name.charAt(0).toUpperCase() + category.name.slice(1);
				}
				else {
					switch (category.name) {
						default:
							return category.name.charAt(0).toUpperCase() + category.name.slice(1) + 's';
					}
				}
			};

			$scope.capitalize = function (input) {
				return input.charAt(0).toUpperCase() + input.slice(1);
			};

			$scope.recordIdentifier = function (record) {
				if (record.id != 0) {
					switch (record.category) {
						case -700:
							return record.measures.reason;

						case -100:
							var out = record.measures.setting;

							if (record.measures.state) {
								out += ' (' + record.measures.state;
								if (record.measures.country) {
									out += ', ' + record.measures.country;
								}
								out += ')';
							}
							else if (record.measures.country) {
								out += ' (' + record.measures.country + ')';
							}

							return out;

						default:
							return record.measures.ident;
					}
				}
			};

			var measures = undefined;

			$scope.getMeasures = function (data) {
				if (measures) {
					return measures;
				}

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
					if (skip.indexOf(key) == -1) {
						measures[key] = value;
					}
				});

				return measures;
			};

			var sessionRecords = {};

			$scope.getSessionRecords = function (data) {
				if (sessionRecords[data.object.id]) {
					return sessionRecords[data.object.id];
				}

				sessionRecords[data.object.id] = [];
				var skip = ['-700', '-800'];

				angular.forEach(data.object.categories, function (records, key) {
					if (data.object.categories.hasOwnProperty(key) && skip.indexOf(key) == -1) {
						sessionRecords[data.object.id].push({
							id: parseInt(key),
							records: records
						});
					}
				});

				sessionRecords[data.object.id].sort(function (a, b) {
					return a.id > b.id;
				});

				return sessionRecords[data.object.id];
			};

			$scope.nameRecord = function (data) {
				var category = page.constants.get('category', data.object.category),
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
					name = $scope.capitalize(page.constants.get('category', data.object.category).name);
				}

				var identifier = $scope.recordIdentifier(data.object);

				if (identifier) {
					name += ': ' + identifier;
				}

				return name;
			};

			$scope.queryFilter = function (data) {
				if (!page.browser.query) {
					return true;
				}

				var regex = new RegExp(page.browser.query.toLowerCase().split(' ').join("|"));

				if (!page.types.isVolume(data.object)) {
					return true;
				}

				if (data.object.name && regex.test(data.object.name.toLowerCase())) {
					return true;
				}

				if (data.object.body && regex.test(data.object.body.toLowerCase())) {
					return true;
				}

				if (data.object.more && regex.test(data.object.more.toLowerCase())) {
					return true;
				}

				return false;
			};

			//

			$scope.editLink = function (data) {
				switch (page.types.getType(data.object)) {
					case 'volume':
						return page.router.volumeEdit(data.object);

					case 'record':
						return page.router.recordEdit(data.object);

					case 'session':
						return page.router.slotEdit(data.object);

					case 'asset':
						return page.router.assetEdit(data.object);
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
	}
]);
