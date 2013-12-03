define(['app/modules/dbDirectives'], function (db) {
	'use strict';

	db.directive('metricsRepeater', ['$http', function ($http) {
		var link = function ($scope, $element, $attrs) {
			$scope.repeats = $scope.repeats || [];

			$scope.categories = $scope.categories || [];
			$scope.metrics = $scope.metrics || [];

			$scope.category = $scope.category || undefined;
			$scope.newMetric = undefined;

			//

			var initialize = function () {
				for(var i = 0; i < $scope.metrics.length; i++) {
					var id = $scope.metrics[i].id;

					$scope.metrics[i].used = !!($.grep($scope.repeats, function (i) { return i.metric == id; }).length > 0);

					if($scope.metrics[i].values.length > 0)
						$scope.metrics[i].dataType = 'select';
				}
			};

			//

			$scope.getIndex = function (repeat) {
				return $scope.repeats.indexOf(repeat);
			};

			$scope.getRepeat = function (id) {
				return $.grep($scope.repeats, function (i) { return i.metric == id; }).shift();
			};

			$scope.createRepeat = function (id) {
				if($scope.getRepeat(id))
					return false;

				$scope.repeats.push({
					metric: id
				});

				$scope.useMetric(id, true);

				$scope.newMetric = undefined;

				return $scope.repeats.slice(-1)[0];
			};

			$scope.updateRepeat = function (old, repeat) {
				var index = $scope.getIndex(old);

				if (!~index)
					return false;

				$scope.repeats[index] = $.extend(true, {}, $scope.repeats[index], repeat);

				return $scope.repeats[index];
			};

			$scope.deleteRepeat = function (repeat) {
				var index = $scope.getIndex(repeat);

				if (!~index)
					return false;

				var deleted = $scope.repeats.splice(index, 1);

				$scope.useMetric(repeat.metric, false);

				return deleted;
			};

			//

			$scope.isMoveable = function () {
				return false;
			};

			$scope.isAddable = function () {
				return false;
			};

			$scope.isDeletable = function () {
				return true;
			};

			//

			$scope.getMetric = function (id) {
				return $.grep($scope.metrics, function (i) { return i.id == id; }).shift();
			};

			$scope.useMetric = function (id, used) {
				$scope.getMetric(id).used = used;
			};

			//

			$scope.getCategory = function (id) {
				return $.grep($scope.categories, function (i) { return i.id == id; }).shift();
			};

			$scope.updateCategory = function (id) {
				var cat = $scope.getCategory(id);

				for(var i = 0; i < cat.template.length; i++) {
					$scope.createRepeat(cat.template[i]);
				}
			};

			//

			$scope.submit = function () {
				var data = {
					category: $scope.category,
					measure: $scope.repeats
				};

				$http.post($scope.formAction, data).success(function (data, status, headers, config) {
					console.log(config);

					$scope.category = config.data.category;
					$scope.repeats = config.data.measure;
				});
			};

			//

			initialize();
		};

		return {
			restrict: 'A',
			scope: true,
			link: link
		}
	}]);
});
