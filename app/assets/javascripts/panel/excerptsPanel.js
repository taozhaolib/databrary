define(['config/module'], function (module) {
	'use strict';

	module.controller('ExcerptsPanel', ['$scope', 'BrowserService', '$location', '$timeout', '$rootScope', function ($scope, browser, $location, $timeout, $rootScope) {
		$scope.bootPanel = function () {
			if (angular.isArray($scope.volume.excerpts) && $scope.volume.excerpts.length > 0)
				$scope.current = $scope.volume.excerpts[0] || undefined;
		};

		$scope.refreshPanel = function () {
			$scope.enabled = angular.isArray($scope.volume.excerpts) && $scope.volume.excerpts.length > 0;
		};

		//

		$scope.setCurrent = function (asset) {
			$scope.current = asset;
		};

		$scope.getMimeGroup = function (asset) {
			var mimetype = asset.format ? asset.format.mimetype : asset.asset.format.mimetype,
				type = mimetype.split('/')[0];

			return type == 'text' ? mimetype[1] : type;
		};

		$scope.jump = function (asset) {
			var found;

			for (var i = 0, l = browser.groups.session.length; i < l; i++) {
				if (browser.groups.session[i].object.id == asset.container.id) {
					found = browser.groups.session[i];
					break;
				}
			}

			if (!found)
				return expandTo(browser.data.items[0].volume.sessions[asset.container.id], asset, false);

			$(window).scrollTop($('#' + found.id).offset().top - 76);
			browser.setItemExpand(found, true);
		};

		var expandTo = function (session, asset, jump) {
			var dirty;

			angular.forEach(browser.groups, function (objects, group) {
				if (!$.isNumeric(group))
					return;

				var recordIDs = session.categories[group];

				if (recordIDs)
					recordIDs = recordIDs.map(function (obj) {
						return obj.id;
					});
				else
					recordIDs = [0];

				angular.forEach(objects, function (data) {
					console.log(recordIDs, data.object.id);
					if (recordIDs.indexOf(data.object.id) > -1) {
						browser.setItemExpand(data, true);
						dirty = true;
					}
				});
			});

			if (dirty)
				$timeout(function () {
					$scope.jump(asset);
				}, 1);
		};
	}]);
});
