module.directive('volumeEditMaterialsForm', [
	'pageService',
	function (page) {
		var link = function ($scope) {
			var form = $scope.authApplyForm;

			//

			page.events.talk('volumeEditMaterialsForm-init', form, $scope);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'volumeEditMaterialsForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}
]);
