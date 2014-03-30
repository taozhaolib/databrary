define(['config/module'], function (module) {
	'use strict';

	module.directive('excerpt', ['TypeService', 'RouterService', function (type, router) {
		var link = function ($scope, $el, $attr) {
			$scope.srcRoute = router.assetLink({
				id: $scope[$attr.excerpt].asset.id,
				sid: $scope[$attr.excerpt].container.id
			});

			var excerpt = type.assetProperty($scope[$attr.excerpt], 'segment', false) || [null, null];
			excerpt = excerpt.map(function (old) {
				return isNaN(old / 1000) ? null : old / 1000;
			});

			if (angular.isNumber(excerpt[0]))
				$el.on('loadedmetadata', function () {
					this.currentTime = excerpt[0];

					$el.off('loadedmetadata');
				});

			if (angular.isNumber(excerpt[1]))
				$el.on('timeupdate', function () {
					if (this.currentTime < excerpt[1])
						return;

					this.pause();
					$el.off('timeupdate');
				});
		};

		return {
			restrict: 'A',
			link: link
		};
	}]);
});
