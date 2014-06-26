module.controller('ExcerptsPanel', [
	'$scope',
	'pageService',
	function ($scope, page) {
		$scope.bootPanel = function () {
			if (angular.isArray($scope.volume.excerpts) && $scope.volume.excerpts.length > 0) {
				$scope.current = $scope.volume.excerpts[0] || undefined;
			}
		};

		$scope.refreshPanel = function () {
			$scope.enabled = angular.isArray($scope.volume.excerpts) && $scope.volume.excerpts.length > 0;
		};

		//

		$scope.setCurrent = function (asset) {
			$scope.current = asset;
		};

		$scope.getMimeGroup = function (asset) {
			var mimetype = page.types.assetFormat(asset).mimetype,
				type = mimetype.split('/')[0];

			return type == 'text' ? mimetype[1] : type;
		};

		$scope.hasThumbnail = function(asset){
			return $scope.getMimeGroup(asset) == 'image' || $scope.getMimeGroup(asset) == 'video';
		}

		$scope.listClass = function (excerpt) {
			var cls = [];

			if (angular.isArray(excerpt.segment)) {
				cls.push('video');
			}

			return cls;
		};

		$scope.supported = function () {
			return page.$window.navigator.userAgent.toLowerCase().indexOf('firefox') == -1 || page.$window.navigator.platform.toLowerCase().indexOf('mac') == -1;
		};

		$scope.jump = function (asset) {
			var found;

			for (var i = 0, l = page.browser.groups.session.length; i < l; i++) {
				if (page.browser.groups.session[i].object.id == asset.container.id) {
					found = page.browser.groups.session[i];
					break;
				}
			}

			if (!found) {
				return expandTo(page.browser.data.items[0].volume.sessions[asset.container.id], asset);
			}

			var $item = $('#' + found.id);

			if ($item.length == 0) {
				return addTo(found, asset);
			}

			page.display.scrollTo($item);
			page.browser.setItemExpand(found, true);
		};

		var expandTo = function (session, asset) {
			var dirty;

			angular.forEach(page.browser.groups, function (objects, group) {
				if (!$.isNumeric(group)) {
					return;
				}

				var recordIDs = session.categories[group];

				if (recordIDs) {
					recordIDs = recordIDs.map(function (obj) {
						return obj.id;
					});
				}
				else {
					recordIDs = [0];
				}

				angular.forEach(objects, function (data) {
					if (recordIDs.indexOf(data.object.id) > -1) {
						page.browser.setItemExpand(data, true);
						dirty = true;
					}
				});
			});

			if (dirty) {
				page.$timeout(function () {
					$scope.jump(asset);
				}, 1);
			}
		};

		var addTo = function (session, asset) {
			var data = session.parent, index;

			for (var i = 0, l = data.items.length; i < l; i++) {
				if (data.items[i] == session) {
					index = i;
					break;
				}
			}

			if (index) {
				data.items.splice(9, 0, data.items.splice(index, 1)[0]);

				page.$timeout(function () {
					$scope.jump(asset);
				}, 1);
			}
		};
	}
]);
