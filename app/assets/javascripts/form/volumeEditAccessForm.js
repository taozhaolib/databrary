module.directive('volumeEditAccessForm', [
	'pageService',
	function (page) {
		var link = function ($scope) {
			var form = $scope.authApplyForm;

			//

			page.events.talk('volumeEditAccessForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'volumeEditAccessForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
