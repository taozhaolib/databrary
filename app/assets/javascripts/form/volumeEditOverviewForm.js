module.directive('volumeEditOverviewForm', [
	'pageService',
	function (page) {
		var link = function ($scope) {
			var form = $scope.authApplyForm;

			//

			page.events.talk('volumeEditOverviewForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'volumeEditOverviewForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
