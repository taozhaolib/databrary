module.directive('volumeEditPublicationsForm', [
	'pageService',
	function (page) {
		var link = function ($scope) {
			var form = $scope.authApplyForm;

			//

			page.events.talk('volumeEditPublicationsForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'volumeEditPublicationsForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
