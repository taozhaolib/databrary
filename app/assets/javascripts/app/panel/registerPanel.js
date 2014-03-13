define(['app/config/module'], function (module) {
	'use strict';

	module.controller('RegisterPanel', ['$scope', 'AuthService', '$http', function ($scope, authService, $http) {
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

		$scope.agreement = {
			page: 1,
			pages: 4
		};

		$scope.password = false; // TODO: pull this from $http
		$scope.passwordSubmit = false;
		$scope.authParty = undefined;
		$scope.requestSubmit = false;

		$scope.retrieveWizard = function (wizard) {
			$scope.wizard = wizard;
			$scope.wizard.addFn = $scope.updateSteps();
		};

		$scope.updateStep = {
			'register_create': function (step, activate) {
				if ($scope.auth.isLoggedIn() || $scope.password)
					return step.complete = true;

				step.complete = $scope.registerReady ? true : undefined;
				step.allow = !$scope.registerSubmit;

				if (activate !== false)
					step.active = !$scope.registerReady;
			},

			'register_agreement': function (step, activate) {
				if ($scope.auth.isLoggedIn() || $scope.password)
					return step.complete = true;

				step.complete = $scope.registerSubmit ? true : undefined;
				step.allow = !$scope.registerSubmit;

				if (activate !== false)
					step.active = $scope.registerReady && !$scope.registerSubmit;
			},

			'register_email': function (step, activate) {
				if ($scope.auth.isLoggedIn() || $scope.password)
					return step.complete = true;

				if (!$scope.registerReady || !$scope.registerSubmit)
					return;

				if (activate !== false)
					step.active = true;

				step.allow = true;
			},

			'register_password': function (step, activate) {
				if ($scope.auth.isLoggedIn())
					return step.complete = true;

				if (!$scope.password)
					return;

				if ($scope.passwordSubmit)
					step.complete = true;
				else {
					if (activate !== false)
						step.active = true;

					step.allow = true;
				}
			},

			'register_agent': function (step, activate) {
				if ($scope.auth.hasAuth('VIEW'))
					return step.complete = true;

				if (!$scope.auth.isLoggedIn() || ($scope.password && !$scope.passwordSubmit))
					return;

				if ($scope.authParty)
					step.complete = true;
				else if (activate !== false)
					step.active = true;

				step.allow = true;
			},

			'register_request': function (step, activate) {
				if ($scope.auth.hasAuth('VIEW'))
					return step.complete = true;

				if (!$scope.auth.isLoggedIn() || !$scope.authParty)
					return;

				if ($scope.requestSubmit)
					step.complete = true;
				else {
					if (activate !== false)
						step.active = true;

					step.allow = true;
				}
			},

			'register_pending': function (step, activate) {
				if (!$scope.auth.isLoggedIn() || ($scope.auth.isAuth('NONE') && !$scope.requestSubmit))
					return;

				if (activate !== false)
					step.active = true;

				step.allow = true;
			}
		};

		$scope.updateSteps = function (activate) {
			angular.forEach($scope.wizard.steps, function (step) {
				$scope.updateStep[step.id](step, activate);
			});
		};

		//

		var regexEmail = /^(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/

		$scope.$watch('auth.user', function () {
			$scope.updateSteps();

			if ($scope.wizard.stepsList['register_create'] && !$scope.wizard.stepsList['register_create'].regexEmail) {
				$scope.wizard.stepsList['register_create'].regexEmail = regexEmail;

				$scope.wizard.stepsList['register_create'].data = $scope.registerData;

				$scope.wizard.stepsList['register_create'].testProceed = function (form) {
					var ready = form.$dirty && form.$valid && $scope.registerData.name && $scope.registerData.email && $scope.registerData.affiliation;

					$scope.registerReady = ready;
					$scope.updateSteps(false);

					return ready;
				};

				$scope.wizard.stepsList['register_create'].proceed = function (form) {
					$scope.registerReady = true;
					$scope.updateSteps();
				};
			}
			if ($scope.wizard.stepsList['register_agreement'] && !$scope.wizard.stepsList['register_agreement'].agreement) {
				$scope.wizard.stepsList['register_agreement'].agreement = $scope.agreement;

				$scope.wizard.stepsList['register_agreement'].back = function () {
					$scope.agreement.page--;
					$scope.registerData.agreement = false;
				};

				$scope.wizard.stepsList['register_agreement'].next = function () {
					$scope.agreement.page++;
				};

				$scope.wizard.stepsList['register_agreement'].proceed = function () {
					$scope.registerData.agreement = true;

					$http
						.post('/register', $scope.registerData)
						.success(function (data) {
							$scope.registerSubmit = true;
							$scope.updateSteps();
						});
				}
			}
		});
	}]);
});
