define(['config/module'], function (module) {
	'use strict';

	module.directive('excerpt', ['TypeService', 'RouterService', function (type, router) {
		var link = function ($scope, $el, $attr) {
			$scope.srcRoute = router.assetLink({
				id: $scope[$attr.excerpt].asset.id,
				sid: $scope[$attr.excerpt].container.id
			});

			var excerpt = type.assetProperty($scope[$attr.excerpt], 'segment', false).map(function (old) {
				return old / 1000
			});

			$el.on('loadedmetadata', function () {
				this.currentTime = excerpt[0];
				$el.off('loadedmetadata');
			});

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
