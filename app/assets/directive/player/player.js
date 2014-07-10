'use strict';

module.directive('player', [
	'pageService', function (page) {
		var isNothing = function (val) {
			return angular.isUndefined(val) || val === null;
		};

		var controller = ['$scope', '$element', '$attrs', function ($scope, $element, $attrs) {
			var player = this;

			player.slot = page.$parse($attrs.slot)($scope);
			player.main = [];
			player.playable = [];
			player.unplayable = [];

			player.slot.assets.map(function (asset) {
				return asset;
			}).sort(function (a, b) {
				if (isNothing(a.segment) && isNothing(b.segment))
					return 0;
				else if (isNothing(a.segment))
					return 1;
				else if (isNothing(b.segment))
					return -1;

				var aSeg = angular.isArray(a.segment) ? a.segment[0] : a.segment;
				var bSeg = angular.isArray(b.segment) ? b.segment[0] : b.segment;
				return aSeg < bSeg;
			}).forEach(function (asset) {
				// TODO: Type and TypeList classes that do this sort of thing
				asset.container = player.slot.container;

				if (asset.segment === null)
					return;
				if (['video', 'image'].indexOf(page.types.assetMimeArray(asset, true)[0]) > -1)
					player.playable.push(asset);
				else
					player.unplayable.push(asset);
			});

			player.main.push(player.playable[0] || player.unplayable[0]);

			//

			player.select = function (media) {
				player.main[0] = media;
			};
		}];

		//

		return {
			restrict: 'E',
			scope: true,
			templateUrl: 'player.html',
			controller: controller,
			controllerAs: 'player',
		};
	}
]);
