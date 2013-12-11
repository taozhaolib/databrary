define(['app/config/module'], function (module) {
	'use strict';

	module.directive('fold', ['$sessionStorage', function ($sessionStorage) {
		var foldableClass = 'foldable',
			folderClass = 'folder',
			foldClass = 'fold',
			foldedClass = 'folded',
			folderAttr = '[folder]',
			foldAttr = '[folded]',
			slideTime = 500;

		var link = function ($scope, $element, $attrs) {
			$scope.$storage = $sessionStorage;

			$scope.id = $element.attr('id') || 'unknown';

			$element.on('$destroy', function () {
				$scope.disableFold();
			});

			//

			$scope.isFoldable = function () {
				return true;
			};

			//

			var enabled;

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

			$scope.foldUp = function () {
				$scope.isFolded = true;
			};

			$scope.foldDown = function () {
				$scope.isFolded = false;
			};

			$scope.foldToggle = function () {
				if ($scope.isFolded)
					$scope.foldDown();
				else
					$scope.foldUp();
			};

			//

			$scope.setFolding = function () {
				if ($attrs.dbFoldForget)
					return undefined;

				$scope.$storage['folding_' + $scope.id] = $scope.isFolded;
			};

			$scope.getFolding = function () {
				if ($attrs.dbFoldForget || typeof($scope.$storage['folding_' + $scope.id]) == 'undefined')
					return undefined;

				return $scope.$storage['folding_' + $scope.id];
			};

			$scope.restoreFolding = function () {
				var isFolded = $scope.getFolding();

				if (typeof(isFolded) == 'undefined')
					$scope.isFolded = $attrs.dbFoldCurrently == "true";
				else
					$scope.isFolded = isFolded;

				$element.removeAttr('db-fold-currently');
			};

			//

			$scope.$watch('isFolded', function () {
				$scope.setFolding();
			});

			//

			var start = function () {
				if($attrs.fold !== false)
					$scope.enableFold();
			};

			start();
		};

		return {
			restrict: 'A',
			priority: 50,
			scope: true,
			link: link
		}
	}]);
});
