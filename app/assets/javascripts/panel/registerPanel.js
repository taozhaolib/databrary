define(['config/module'], function (module) {
	'use strict';

	module.controller('RegisterPanel', ['$scope', 'AuthService', '$http', '$window', 'EventService', 'PartyAuthorize', 'MessageService', 'ConstantService', function ($scope, authService, $http, $window, eventService, PartyAuthorize, messages, constants) {
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
			pages: 5
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

		$scope.partyAuth = {};

		$scope.retrieveWizard = function (wizard) {
			$scope.wizard = wizard;
			$scope.wizard.addFn = $scope.updateSteps();
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

				step.allow = !$scope.requestSubmit;
				step.complete = $scope.authParty || $scope.requestSubmit ? true : undefined;

				if (activate !== false)
					step.active = !$scope.authParty && !$scope.requestSubmit;
			},

			'register_request': function (step, activate) {
				if (postLoginBlock(step, activate))
					return;

				step.allow = !!$scope.authParty && !$scope.requestSubmit;
				step.complete = $scope.requestSubmit ? true : undefined;

				if (activate !== false)
					step.active = !!$scope.authParty && !$scope.requestSubmit;
			},

			'register_pending': function (step, activate) {
				if (!$scope.auth.isLoggedIn()) {
					step.allow = false;
					step.complete = undefined;

					if (activate !== false)
						step.active = false;

					return;
				} else if ($scope.auth.hasAuth('VIEW')) {
					step.allow = false;
					step.complete = true;

					if (activate !== false)
						step.active = false;

					return;
				} else if (!$scope.requestSubmit) {
					step.allow = false;
					step.complete = undefined;

					if (activate !== false)
						step.active = false;

					return;
				}

				step.allow = true;
				step.complete = undefined;

				if (activate !== false)
					step.active = true;
			}
		};

		$scope.prepareStep = {
			'register_create': function (step, activate) {
				if (step.testProceed)
					return;

//				step.regexEmail = regexEmail;

				step.data = $scope.registerData;

				step.testProceed = function (form) {
					return form.$dirty && form.$valid && $scope.registerData.name && $scope.registerData.email && $scope.registerData.affiliation;
				};

				step.proceed = function (form) {
					$scope.registerReady = true;
					$scope.updateSteps();
				};

				//

				messages.add({
					type: 'alert',
					target: '#field_name',
					body: constants.message('wizard.register_form.name.help')
				});

				messages.add({
					type: 'alert',
					target: '#field_email',
					body: constants.message('wizard.register_form.email.help')
				});

				messages.add({
					type: 'alert',
					target: '#field_affiliation',
					body: constants.message('wizard.register_form.affiliation.help')
				});

				//

				var emailError = messages.add({
					type: 'error',
					enabled: false,
					body: constants.message('wizard.register_form.email.error')
				});

				step.$watch('registerForm.fieldEmail.$valid', function () {
					if (step.registerForm.fieldEmail.$valid || !step.registerForm.fieldEmail.$dirty)
						messages.disable(emailError);
					else
						messages.enable(emailError);
				});
			},

			'register_agreement': function (step, activate) {
				if (step.agreement)
					return;

				step.agreement = $scope.agreement;

				step.back = function () {
					$scope.agreement.page--;
					$scope.registerData.agreement = false;
				};

				step.scrolled = false;

				step.scroll = function ($scroll) {
					var $el = $scroll.$element;

					if ($el.height() + $el.scrollTop() >= $el.prop('scrollHeight')) {
						step.scrolled = true;
					}
				};

				step.canProceed = function () {
					return step.scrolled;
				};

				step.next = function () {
					$scope.agreement.page++;
					step.scrolled = false;
				};

				step.proceed = function () {
					$scope.registerData.agreement = true;
					step.scrolled = false;

					$http
						.post('/register', $scope.registerData)
						.success(function (data) {
							$scope.registerSubmit = true;
							$scope.updateSteps();
						});
				};
			},

			'register_email': function (step, activate) {
			},

			'register_password': function (step, activate) {
				if (step.data)
					return;

				step.data = $scope.passwordData;

				step.testProceed = function (form) {
					var ready = form.$dirty && form.$valid && $scope.passwordData.password.once && $scope.passwordData.password.once == $scope.passwordData.password.again;

					$scope.updateSteps(false);

					return ready;
				};

				step.proceed = function (form) {
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
			},

			'register_agent': function (step, activate) {
				if (step.authSearchForm.selectFn)
					return;

				step.authSearchForm.selectFn = function (found, form) {
					$scope.authParty = found;
					$scope.updateSteps();
				};
			},

			'register_request': function (step, activate) {
				step.authApplyForm.party = $scope.auth.user;

				if ($scope.authParty && !step.authApplyForm.other)
					step.authApplyForm.other = {
						id: $scope.authParty.id,
						party: $scope.authParty,
						inherit: 0,
						direct: 0
					};

				if (step.authApplyForm.successFn)
					return;

				step.authApplyForm.successFn = function (form) {
					$scope.requestSubmit = true;
				};

				step.authApplyForm.cancelFn = function (form) {
					$scope.authParty = undefined;
					$scope.updateSteps();
				};
			},

			'register_pending': function (step, activate) {
			}
		};

		$scope.updateSteps = function (activate) {
			activate = angular.isUndefined(activate) ? true : activate;

			angular.forEach($scope.wizard.steps, function (step) {
				$scope.updateStep[step.id](step, activate);
				$scope.prepareStep[step.id](step, activate);
			});
		};

		//

		var regexEmail = /^(([^<>()[\]\\.,;:\s@\"]+(\.[^<>()[\]\\.,;:\s@\"]+)*)|(\".+\"))@((\[[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\.[0-9]{1,3}\])|(([a-zA-Z\-0-9]+\.)+[a-zA-Z]{2,}))$/;

		$scope.$watch('auth.user', function () {
			$scope.updateSteps();

			if ($scope.auth.user)
				PartyAuthorize.query({
					id: $scope.auth.user.id
				}, function (data) {
					$scope.partyAuth = data;

					angular.forEach($scope.partyAuth.parents, function (parent) {
						$scope.requestSubmit = true;
					});

					$scope.updateSteps();
				});
		});

		//

		// TODO: replace authApplyForm in network panel
		// TODO: do messages for lisa
		// TODO: update network apply for proper requests
	}]);
});
