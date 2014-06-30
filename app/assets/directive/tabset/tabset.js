module.directive('tabset', [
	function () {
		var controller = ['$scope', '$element', '$attrs', function ($scope, $element) {
			var ctrl = {};

			$scope.tabList = [];
			$scope.tabHash = {};

			//

			ctrl.addTab = function (tab) {
				$scope.tabList.push(tab);
				$scope.tabHash[tab.id] = tab;

				if (tab.active) {
					return ctrl.activateTab(tab);
				}

				//

				var anyActive;

				angular.forEach($scope.tabList, function (thisTab) {
					if (thisTab.active) {
						anyActive = true;
					}
				});

				if (anyActive) {
					return true;
				}

				for (var i = 0; i < $scope.tabList.length; i = i + 1) {
					if ($scope.tabList[i].enabled) {
						return ctrl.activateTab($scope.tabList[0]);
					}
				}
			};

			ctrl.removeTab = function (tab) {
				var i = $scope.tabList.indexOf(tab);

				if (i == -1) {
					return false;
				}

				delete $scope.tabHash[tab.id];
				$scope.tabList.splice($scope.tabList.indexOf(tab), 1);

				return true;
			};

			ctrl.activateTab = function (tab) {
				if (!tab.enabled) {
					return false;
				}

				angular.forEach($scope.tabList, function (thisTab) {
					thisTab.active = thisTab == tab;
				});

				return true;
			};

			//

			ctrl.checkTabList = function () {
				var c = 0;

				for (var i = 0; i < $scope.tabList.length; i = i + 1) {
					if ($scope.tabList[i].enabled) {
						c++;
					}

					if (c == 2) {
						return true;
					}
				}

				return false;
			};

			ctrl.tabListFilter = function (tab) {
				return tab.enabled;
			};

			//

			ctrl.tabListClass = function (tab) {
				var cls = [];

				if (tab.active) {
					cls.push('active');
				}

				return cls;
			};

			//

			return ctrl;
		}];

		return {
			restrict: 'E',
			templateUrl: 'tabset.html',
			replace: true,
			transclude: true,
			scope: {},
			controller: controller,
			controllerAs: 'tabset'
		};
	}
]);
