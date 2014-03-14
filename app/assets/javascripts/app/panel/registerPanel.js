define(['app/config/module'], function (module) {
	'use strict';

	module.controller('RegisterPanel', ['$scope', 'AuthService', '$http', '$window', 'EventService', function ($scope, authService, $http, $window, eventService) {
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

		$scope.passwordData = {
			token: undefined,
			auth: undefined,
			password: {
				once: undefined,
				again: undefined
			}
		};

		if ($scope.type.isToken($window.$play.object)) {
			$scope.passwordData.token = $window.$play.object.id;
			$scope.passwordData.auth = $window.$play.object.auth;
		}

		$scope.passwordSubmit = false;
		$scope.authParty = undefined;
		$scope.requestSubmit = false;

		$scope.retrieveWizard = function (wizard) {
			$scope.wizard = wizard;
			$scope.wizard.addFn = $scope.updateSteps();
//			$scope.wizard.onFn['register_request'] = onRegisterRequest;
		};

		var prePasswordComplete = function (step, activate) {
			if (!$scope.auth.isLoggedIn() && !$scope.passwordData.token)
				return false;

			step.complete = true;
			step.allow = false;

			if (activate !== false)
				step.active = false;

			return true;
		};

		var postLoginBlock = function (step, activate) {
			if ($scope.auth.isLoggedIn() && !$scope.auth.hasAuth('VIEW'))
				return false;

			step.complete = $scope.auth.hasAuth('VIEW') ? true : undefined;
			step.allow = false;

			if (activate !== false)
				step.active = false;

			return true;
		};

		$scope.updateStep = {
			'register_create': function (step, activate) {
				if (prePasswordComplete(step, activate))
					return;

				step.complete = $scope.registerReady ? true : undefined;
				step.allow = !$scope.registerSubmit;

				if (activate !== false)
					step.active = !$scope.registerReady;
			},

			'register_agreement': function (step, activate) {
				if (prePasswordComplete(step, activate))
					return;

				step.complete = $scope.registerSubmit ? true : undefined;
				step.allow = $scope.registerReady && !$scope.registerSubmit;

				if (activate !== false)
					step.active = $scope.registerReady && !$scope.registerSubmit;
			},

			'register_email': function (step, activate) {
				if (prePasswordComplete(step, activate))
					return;

				if (!$scope.registerReady || !$scope.registerSubmit)
					return;

				if (activate !== false)
					step.active = true;

				step.allow = true;
			},

			'register_password': function (step, activate) {
				if ($scope.auth.isLoggedIn()) {
					step.complete = true;
					step.allow = false;

					if (activate !== false)
						step.active = false;

					return true
				}

				if (!$scope.passwordData.token)
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
				if (postLoginBlock(step, activate))
					return;

				step.allow = true;
				step.complete = $scope.authParty ? true : undefined;

				if (activate !== false)
					step.active = !$scope.authParty;
			},

			'register_request': function (step, activate) {
				if (postLoginBlock(step, activate))
					return;

				step.allow = !!$scope.authParty;
				step.complete = $scope.requestSubmit ? true : undefined;

				if (activate !== false)
					step.active = !!$scope.authParty;
			},

			'register_pending': function (step, activate) {
				if (postLoginBlock(step, activate))
					return;

				if (!$scope.requestSubmit)
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

			if ($scope.wizard.stepsList['register_password'] && !$scope.wizard.stepsList['register_password'].data) {
				$scope.wizard.stepsList['register_password'].data = $scope.passwordData;

				$scope.wizard.stepsList['register_password'].testProceed = function (form) {
					var ready = form.$dirty && form.$valid && $scope.passwordData.password.once && $scope.passwordData.password.once == $scope.passwordData.password.again;

					$scope.updateSteps(false);

					return ready;
				};

				$scope.wizard.stepsList['register_password'].proceed = function (form) {
					$http
						.post('/api/party/' + $window.$play.object.party + '/password', $scope.passwordData)
						.success(function (data) {
							$window.$play.object = null;
							$scope.passwordSubmit = true;
							$scope.auth.updateUser(data);
							$scope.updateSteps();
						})
						.error(function () {
							$window.$play.object = null;
							console.log(arguments);
						});
				};
			}

			if ($scope.wizard.stepsList['register_agent'] && !$scope.wizard.stepsList['register_agent'].authSearchForm.selectFn) {
				$scope.wizard.stepsList['register_agent'].authSearchForm.selectFn = function (found, form) {
					$scope.authParty = found;
					$scope.updateSteps();
				};
			}
		});

		//

		// TODO: hook with authApplyForm properly
		// TODO: replace authApplyForm in network panel
		// TODO: do messages for lisa
		// TODO: update network apply for proper requests

//		var onRegisterRequest = function (oldStep, newStep) {
//			newStep.authApplyForm.party = $scope.auth.user;
//			newStep.authApplyForm.other = $scope.authParty;
//			console.log(true)
//		};
//
//		eventService.listen($scope, 'authApplyForm-init', function (event, form) {
//			form.party = $scope.auth.user;
//			form.other = $scope.authParty;
//
//			form.saveFn = function (form) {
//
//			};
//
//			form.cancelFn = function (form) {
//
//			};
//
//			event.stopPropagation();
//		});
	}]);
});
