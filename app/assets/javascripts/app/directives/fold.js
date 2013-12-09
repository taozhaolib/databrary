define(['app/modules/dbDirectives'], function (db) {
	'use strict';

	db.directive('fold', ['$sessionStorage', function ($sessionStorage) {
		var foldableClass = 'foldable',
			folderClass = 'folder',
			foldClass = 'fold',
			foldedClass = 'folded',
			folderAttr = '[db-fold-folder]',
			foldAttr = '[db-fold-folded]',
			slideTime = 500;

		var link = function ($scope, $element, $attrs) {
			$scope.$storage = $sessionStorage;

			$scope.id = $element.attr('id') || 'unknown';

			$element.addClass(foldableClass);
			$element.find(folderAttr).addClass(folderClass);
			$element.find(foldAttr).addClass(foldClass);

			$element.on('$destroy', function () {
				$element.removeClass(foldableClass + ' ' + foldedClass);
				$element.find(folderAttr).removeClass(folderClass);
				$element.find(folderAttr).removeClass(foldClass);
			});

			//

			$scope.isFoldable = function () {
				return true;
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

			$scope.restoreFolding();
		};

		return {
			restrict: 'A',
			scope: true,
			link: link
		}
	}]);
});
