'use strict';

module.directive('browserList', [
	'pageService', function (page) {
		var link = function ($scope) {
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

			//

			$scope.setItemPlayer = function (data) {
				if (!data || page.auth.hasAccess(page.permission.READ, data)) {
					page.browser.setItemPlayer(data);
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

			$scope.formatSessionCategory = function (data, categoryID, records) {
				var category = page.constants.data.category[categoryID];

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
				if (record.id !== 0) {
                                        // basically mirrors models.RecordCategory.ident definitions
					switch (page.constants.data.category[record.category].name) {
						case 'exclusion':
							return record.measures.reason;

						case 'context':
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

			$scope.getMeasures = function (data) {
				var measures = {};
                                angular.extend(measures, data.object.measures);
                                
                                delete measures.description;

				switch (page.constants.data.category[data.object.category].name) {
                                        case 'exclusion':
                                                delete measures.reason;
						break;

                                        case 'context':
						delete measures.setting;
						delete measures.state;
						delete measures.country;
						break;

					default:
						delete measures.ident;
						break;
				}

				return measures;
			};

			$scope.getSessionRecords = function (data) {
                                if ('sessionRecords' in data)
                                        return data.sessionRecords;

				data.sessionRecords = [];
				angular.forEach(data.object.categories, function (records, key) {
					if (data.object.categories.hasOwnProperty(key)) {
						data.sessionRecords.push({
							id: parseInt(key),
							records: relevantRecords(data, key, records)
						});
					}
				});

				data.sessionRecords.sort(function (a, b) {
					return a.id > b.id;
				});
				return data.sessionRecords;
			};

			var relevantRecords = function (data, cat, records) {
				if (angular.isUndefined(data.segment)) return records; //no logical difference, just efficiency
				return page.$filter('filter')(records, function (x) {
					return page.types.overlaps(x.segment, data.segment);
				});
			};

			$scope.nameRecord = function (data) {
				var category = page.constants.data.category[data.object.category],
					name = category.name;

				if (data.object.id === 0) {
                                        name = page.constants.data.messages['not.' + name] || 'No ' + name;
				} else {
					name = $scope.capitalize(name);
				}

				var identifier = $scope.recordIdentifier(data.object);

				if (identifier) {
					name += ': ' + identifier;
				}

				return name;
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
