define(['config/module'], function (module) {
	'use strict';

	module.directive('fold', ['$sessionStorage', function ($sessionStorage) {
		var foldableClass = 'foldable',
			folderClass = 'folder',
			foldClass = 'fold',
			foldedClass = 'folded',
			folderAttr = '[folder]',
			foldAttr = '[folded]';

		var link = function ($scope, $element, $attrs) {
			$scope.$storage = $sessionStorage;

			//

			var enabled;

			$scope.isFoldable = function () {
				return enabled;
			};

			$scope.enableFold = function () {
				enabled = true;

				$element.addClass(foldableClass);
				$element.find(folderAttr).addClass(folderClass);
				$element.find(foldAttr).addClass(foldClass);

				$scope.restoreFolding();
			};

			$scope.disableFold = function () {
				enabled = false;

				$element.removeClass(foldableClass + ' ' + foldedClass);
				$element.find(folderAttr).removeClass(folderClass);
				$element.find(folderAttr).removeClass(foldClass);
			};

			$scope.isEnabled = function () {
				return enabled;
			};

			//

			$scope.fold = function () {
				$scope.folded = true;
				$element.addClass(foldedClass);
			};

			$scope.unfold = function () {
				$scope.folded = false;
				$element.removeClass(foldedClass);
			};

			$scope.toggleFold = function (state) {
				if ((angular.isDefined(state) && !state) || $scope.folded)
					$scope.unfold();
				else
					$scope.fold();
			};

			//

			var isForgetful = function () {
				return angular.isDefined($attrs.forget) && (!$attrs.forget || $scope.$eval($attrs.forget));
			};

			$scope.setFolding = function () {
				if (!isForgetful())
					$scope.$storage['folding_' + $scope.id] = $scope.folded;
			};

			$scope.getFolding = function () {
				if (isForgetful() || angular.isUndefined($scope.$storage['folding_' + $scope.id]))
					return undefined;

				return $scope.$storage['folding_' + $scope.id];
			};

			$scope.restoreFolding = function () {
				var folded = $scope.getFolding();

				if(angular.isUndefined(folded))
					folded = (angular.isString($attrs.foldDefault)) ? $attrs.foldDefault != 'false' : false;

				if (folded)
					$scope.fold();
				else
					$scope.unfold();
			};

			//

			$scope.$watch('folded', function () {
				$scope.setFolding();
			});

			//

			var start = function () {
				$scope.id = $attrs.id;

				if(angular.isDefined($attrs.fold) && (!$attrs.fold || $scope.$eval($attrs.fold)))
					$scope.enableFold();
				else
					$scope.disableFold();
			};

			start();
		};

		return {
			restrict: 'A',
			priority: 0,
			scope: true,
			link: link
		}
	}]);
});
