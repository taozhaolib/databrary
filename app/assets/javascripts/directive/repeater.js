module.directive('repeater', ['pageService', function (page) {
	var link = function ($scope, $el, $attrs, transclude) {
		transclude($scope, function ($clone) {
			$el.find('[repeat-body]').append($clone);
		});

		$scope.retrieve()($scope);

		$scope.repeats = [{}];
	};

	return {
		restrict: 'E',
		templateUrl: 'repeater.html',
		priority: 50,
		scope: {
			retrieve: '='
		},
		link: link
	}
}
]);
