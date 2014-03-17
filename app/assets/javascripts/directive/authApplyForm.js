define(['config/module'], function (module) {
	'use strict';

	module.directive('authApplyForm', ['PartyAuthorize', 'AuthService', 'EventService', 'AuthPresetService', function (PartyAuthorize, authService, eventService, authPresetService) {
		var link = function ($scope, $element, $attrs) {
			$scope.authApplyForm.presets = authPresetService;
			$scope.authApplyForm.party = authService.user;
			$scope.authApplyForm.other = undefined;

			//

			$scope.authApplyForm.saveFn = undefined;
			$scope.authApplyForm.successFn = undefined;
			$scope.authApplyForm.errorFn = undefined;

			$scope.authApplyForm.save = function () {
				if (angular.isFunction($scope.authApplyForm.saveFn))
					$scope.authApplyForm.saveFn($scope.authApplyForm);

				$scope.authApplyForm.partyAuthorize = new PartyAuthorize();

				$scope.authApplyForm.partyAuthorize.direct = $scope.authApplyForm.other.direct;
				$scope.authApplyForm.partyAuthorize.inherit = $scope.authApplyForm.other.inherit;

				$scope.authApplyForm.partyAuthorize.$apply({
					id: $scope.authApplyForm.party.id,
					partyId: $scope.authApplyForm.other.party.id
				}, function () {
					if (angular.isFunction($scope.authApplyForm.successFn))
						$scope.authApplyForm.successFn($scope.authApplyForm, arguments);
				}, function () {
					if (angular.isFunction($scope.authApplyForm.errorFn))
						$scope.authApplyForm.errorFn($scope.authApplyForm, arguments);
				});
			};

			//

			$scope.authApplyForm.cancelFn = undefined;

			$scope.authApplyForm.cancel = function () {
				if (angular.isFunction($scope.authApplyForm.cancelFn))
					$scope.authApplyForm.cancelFn($scope.authApplyForm);

				$scope.authApplyForm.other.inherit = 0;
				$scope.authApplyForm.other.direct = 0;
				$scope.authApplyForm.other.preset = undefined;
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
