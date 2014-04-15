module.directive('excerpt', [
	'typeService', 'pageService', function (type, page) {
		var link = function ($scope, $el, $attr) {
			var obj = $scope[$attr.excerpt].object ? $scope[$attr.excerpt].object : $scope[$attr.excerpt];

			$scope.srcRoute = page.router.assetLink({
				id: obj.asset.id,
				sid: obj.container.id
			});

			var excerpt = type.assetProperty(obj, 'segment', false) || [null, null];
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
	}
]);
