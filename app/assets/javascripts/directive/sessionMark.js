module.directive('sessionMark', ['pageService', function (page) {
	var link = function ($scope, $el, $attrs) {
		$scope.mark = $attrs.mark;
		var message = '', type = 'orange';

		switch ($scope.mark) {
			case 'excluded':
				if (!$scope.data.object.categories[-700])
					return $el.remove();

				message = page.constants.message('notice.help.exclusion', $scope.data.volume.records[$scope.data.object.categories[-700][0].id].measures.reason);
				type = 'purple';
				break;

			case 'pilot':
				if (!$scope.data.object.categories[-800])
					return $el.remove();

				message = page.constants.message('notice.help.pilot');
				type = 'red';
				break;

			default:
				return $el.remove();
				break;
		}

		var tooltip = page.tooltips.add({
			message: message,
			type: type,
			$target: $el
		});

		//

		$scope.$on('$destroy', function () {
			page.tooltips.remove(tooltip);
		});

		//

		$scope.getMarkClasses = function () {
			var classes = [];

			classes.push('session_mark_' + $scope.mark);

			return classes;
		};
	};

	return {
		restrict: 'E',
		templateUrl: 'sessionMark.html',
		scope: false,
		replace: true,
		link: link
	};
}]);
