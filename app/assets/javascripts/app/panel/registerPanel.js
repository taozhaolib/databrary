define(['app/config/module'], function (module) {
	'use strict';

	module.controller('RegisterPanel', ['$scope', 'AuthService', function ($scope, authService) {
		$scope.auth = $scope.auth || authService;

		$scope.wizard = {};

		$scope.registerReady = false;
		$scope.registerSubmit = false;
		$scope.registerData = {
			name: undefined,
			email: undefined,
			affiliation: undefined,
			agreement: false
		};
		$scope.password = true; // TODO: pull this from $http
		$scope.passwordSubmit = true;
		$scope.authParty = true;
		$scope.requestSubmit = false;

		$scope.retrieveWizard = function (wizard) {
			$scope.wizard = wizard;
			$scope.wizard.addFn = $scope.updateSteps();
		};

		$scope.updateStep = {
			'register_create': function (step) {
				if ($scope.password)
					return step.complete = true;

				if ($scope.auth.isLoggedIn())
					return;

				if ($scope.registerReady)
					step.complete = true;
				else
					step.active = true;

				if (!$scope.registerSubmit)
					step.allow = true;
			},

			'register_agreement': function (step) {
				if ($scope.password)
					return step.complete = true;

				if ($scope.auth.isLoggedIn() || !$scope.registerReady)
					return;

				if ($scope.registerSubmit)
					step.complete = true;
				else {
					step.active = true;
					step.allow = true;
				}
			},

			'register_email': function (step) {
				if ($scope.password)
					return step.complete = true;

				if ($scope.auth.isLoggedIn() || !$scope.registerReady || !$scope.registerSubmit)
					return;

				step.active = true;
				step.allow = true;
			},

			'register_password': function (step) {
				if ($scope.auth.isLoggedIn())
					return step.complete = true;

				if (!$scope.password)
					return;

				if ($scope.passwordSubmit)
					step.complete = true;
				else {
					step.active = true;
					step.allow = true;
				}
			},

			'register_agent': function (step) {
				if ($scope.auth.hasAuth('VIEW'))
					return step.complete = true;

				if (!$scope.auth.isLoggedIn() || ($scope.password && !$scope.passwordSubmit))
					return;

				if ($scope.authParty)
					step.complete = true;
				else
					step.active = true;

				step.allow = true;
			},

			'register_request': function (step) {
				if ($scope.auth.hasAuth('VIEW'))
					return step.complete = true;

				if (!$scope.auth.isLoggedIn() || !$scope.authParty)
					return;

				if ($scope.requestSubmit)
					step.complete = true;
				else {
					step.active = true;
					step.allow = true;
				}
			},

			'register_pending': function (step) {
				if (!$scope.auth.isLoggedIn() || ($scope.auth.isAuth('NONE') && !$scope.requestSubmit))
					return;

				step.active = true;
				step.allow = true;
			}
		};

		$scope.updateSteps = function () {
			angular.forEach($scope.wizard.steps, function (step) {
				$scope.updateStep[step.id](step);
			});
		};

		//

		$scope.$watch('auth.user', function () {
			$scope.updateSteps();
		});
	}]);
});
