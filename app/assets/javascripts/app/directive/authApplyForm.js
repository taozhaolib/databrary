define(['app/config/module'], function (module) {
	'use strict';

	module.directive('authApplyForm', ['PartyAuthorize', 'AuthService', 'EventService', 'AuthPresetService', function (PartyAuthorize, authService, eventService, authPresetService) {
		var link = function ($scope, $element, $attrs) {
			$scope.authApplyForm.presets = authPresetService;

//			$scope.authSearchForm.name = '';
//			$scope.authSearchForm.found = [];
//
//			$scope.authSearchForm.search = function () {
//				if (!$scope.authSearchForm.name)
//					$scope.authSearchForm.found = [];
//				else
//					PartyAuthorize.search({
//						id: $scope.authSearchForm.id || authService.user.id,
//						apply: $scope.authSearchForm.apply,
//						name: $scope.authSearchForm.name
//					}, function (data) {
//						$scope.authSearchForm.found = data;
//					});
//			};

			//

			$scope.authApplyForm.saveFn = undefined;

			$scope.authApplyForm.save = function () {
				if (angular.isFunction($scope.authApplyForm.saveFn))
					$scope.authApplyForm.saveFn($scope.authApplyForm);
			};

			//

			$scope.authApplyForm.cancelFn = undefined;

			$scope.authApplyForm.cancel = function () {
				if (angular.isFunction($scope.authApplyForm.cancelFn))
					$scope.authApplyForm.cancelFn($scope.authApplyForm);
			};

			//

			eventService.talk('authApplyForm-init', $scope.authApplyForm);
		};

		//

		return {
			restrict: 'E',
			templateUrl: 'authApplyForm.html',
			scope: false,
			replace: true,
			link: link
		};
	}]);
});
