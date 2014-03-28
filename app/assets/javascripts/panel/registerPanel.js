define(['config/module'], function (module) {
	'use strict';

	module.controller('RegisterPanel', ['$scope', 'AuthService', '$http', '$window', 'EventService', 'PartyAuthorize', 'MessageService', 'ConstantService', 'Scraper', function ($scope, authService, $http, $window, eventService, PartyAuthorize, messages, constants, Scraper) {
		$scope.auth = $scope.auth || authService;

		var mess = [];
		$scope.$on('$destroy', function () {
			angular.forEach(mess, function (message) {
				messages.remove(message);
			})
		});

		//

		$scope.wizard = {};

		var user = {
			anon: true,
			password: false,
			pending: false,
			auth: false
		};

		$scope.retrieveWizard = function (wizard) {
			$scope.wizard = wizard;
		};

		$scope.updateWizard = function (activate) {
			activate = angular.isUndefined(activate) ? true : activate;

			if ($scope.wizard.newStep) {
				$scope.prepareStep[$scope.wizard.newStep.id]($scope.wizard.newStep);
			}

			user.anon = !authService.isLoggedIn();
			user.password = authService.isPasswordPending();
			user.auth = authService.isAuthorized();

			angular.forEach($scope.wizard.steps, function (step) {
				$scope.updateStep[step.id](step, activate);
			});
		};

		//

		$scope.registerForm = {};
		$scope.passwordForm = {};
		$scope.authSearchForm = {};
		$scope.authApplyForm = {};
		$scope.infoForm = {};

		//

		$scope.agreement = {
			current: 1,
			total: 1,
			pages: []
		};

		Scraper('http://databrary.org/policies/investigator-agreement.html')
			.then(function (pages) {
				pages = $('<response>' + pages + '</response>');
				pages.find('a').each(function () {
					$(this).attr('target', '_blank');
				});
				pages = pages.html();

				pages = pages.split(/<!--page_.*-->/).slice(1, -1);

				$scope.agreement.total = pages.length;
				$scope.agreement.pages = pages;
			});

		//

		var updateUserAuth = function () {
			PartyAuthorize.query({
				id: $scope.auth.user.id
			}, function (data) {
				angular.forEach(data.parents, function (parent) {
					user.pending = true;
				});

				$scope.updateWizard();
			});
		};

		$scope.$watch('auth.user', function () {
			$scope.updateWizard();

			if (authService.isLoggedIn())
				updateUserAuth();
		});

		//

		$scope.prepareStep = {
			'register_create': function (step) {
				$scope.registerForm = step.registerForm;
				$scope.registerForm.data = {};
				$scope.registerForm.sent = false;

				$scope.registerForm.ready = function (form) {
					return $scope.registerForm.$dirty &&
						$scope.registerForm.$valid &&
						$scope.registerForm.data.name &&
						$scope.registerForm.data.email &&
						$scope.registerForm.data.affiliation;
				};

				$scope.registerForm.proceed = function () {
					$scope.updateWizard();
				};

				//

				mess.push(messages.add({
					type: 'alert',
					target: '#field_name',
					body: constants.message('wizard.register_form.name.help')
				}));

				mess.push(messages.add({
					type: 'alert',
					target: '#field_email',
					body: constants.message('wizard.register_form.email.help')
				}));

				mess.push(messages.add({
					type: 'alert',
					target: '#field_affiliation',
					body: constants.message('wizard.register_form.affiliation.help')
				}));

				//

				var emailError = messages.add({
					type: 'error',
					enabled: false,
					body: constants.message('wizard.register_form.email.error')
				});

				mess.push(emailError);

				step.$watch('registerForm.fieldEmail.$valid', function () {
					if ($scope.registerForm.fieldEmail.$valid || !$scope.registerForm.fieldEmail.$dirty)
						messages.disable(emailError);
					else
						messages.enable(emailError);
				});
			},

			'register_agreement': function (step) {
				step.agreement = $scope.agreement;
				step.scrolled = false;
				step.$el = undefined;

				step.back = function () {
					$scope.agreement.current--;
					$scope.registerForm.data.agreement = false;
				};

				step.scroll = function ($scroll) {
					step.$el = step.$el || $scroll.$element;

					if (step.$el.height() + step.$el.scrollTop() >= step.$el.prop('scrollHeight')) {
						step.scrolled = true;
					}
				};

				step.ready = function () {
					return step.scrolled;
				};

				step.next = function () {
					$scope.agreement.current++;

					if (step.$el)
						step.$el.scrollTop(0);

					step.scrolled = false;
				};

				step.proceed = function () {
					$scope.registerForm.data.agreement = true;
					step.scrolled = false;

					$http
						.post('/register', $scope.registerForm.data)
						.success(function (data) {
							$scope.registerForm.sent = true;
							$scope.updateWizard();
						});
				};
			},

			'register_email': function (step) {
				step.registerForm = $scope.registerForm;
			},

			'register_password': function (step) {
				$scope.passwordForm = step.passwordForm;
				$scope.passwordForm.data = {
					token: undefined,
					auth: undefined,
					password: {
						once: undefined,
						again: undefined
					}
				};
				$scope.passwordForm.sent = false;

				if (user.password) {
					$scope.passwordForm.data.token = $window.$play.object.id;
					$scope.passwordForm.data.auth = $window.$play.object.auth;
				}

				$scope.passwordForm.ready = function () {
					return $scope.passwordForm.$dirty &&
						$scope.passwordForm.$valid &&
						$scope.passwordForm.data.password.once &&
						$scope.passwordForm.data.password.once ==
						$scope.passwordForm.data.password.again;
				};

				$scope.passwordForm.proceed = function () {
					$http
						.post('/api/party/' + $window.$play.object.party + '/password', $scope.passwordForm.data)
						.success(function (data) {
							$window.$play.object = null;

							$scope.passwordForm.sent = true;
							authService.updateUser(data);

							$scope.updateWizard();
						})
						.error(function () {
							$window.$play.object = null;
						});
				};
			},

			'register_agent': function (step) {
				$scope.authSearchForm = step.authSearchForm;
				$scope.authSearchForm.data = {};

				$scope.authSearchForm.selectFn = function (found) {
					$scope.authSearchForm.data.party = found;
					$scope.updateWizard();
				};

				$scope.authSearchForm.notFoundFn = function (query) {
					$scope.authSearchForm.data.party = true;
					$scope.infoForm.data.query = query;
					$scope.updateWizard();
				};
			},

			'register_request': function (step) {
				$scope.authApplyForm = step.authApplyForm;
				$scope.authApplyForm.sent = false;

				$scope.infoForm = step.infoForm;
				$scope.infoForm.data = {};

				step.ifInfo = function () {
					return angular.isString($scope.infoForm.data.query);
				};

				//

				$scope.authApplyForm.successFn = function (form) {
					$scope.authApplyForm.sent = true;
					updateUserAuth();
					$scope.updateWizard();
				};

				$scope.authApplyForm.cancelFn = function (form) {
					$scope.authSearchForm.data.party = undefined;
					$scope.updateWizard();
				};

				//

				$scope.infoForm.ready = function () {
					return $scope.infoForm.$dirty &&
						$scope.infoForm.$valid &&
						$scope.infoForm.data.info;
				};

				$scope.infoForm.proceed = function () {
					$http
						.get('/api/party/' + authService.user.id + '/authorize/search', {
							params: {
								apply: true,
								notfound: true,
								name: $scope.infoForm.data.query,
								target: $scope.infoForm.data.info
							}

						})
						.success(function (data) {
							$scope.authApplyForm.successFn();
						});
				};

				$scope.infoForm.cancel = function () {
					$scope.infoForm.data.info = '';
					$scope.authSearchForm.data.party = undefined;
					$scope.infoForm.data.query = undefined;
					$scope.updateWizard();
				};
			},

			'register_pending': function (step) {
			}
		};

		//

		$scope.updateStep = {
			'register_create': function (step, activate) {
				step.allow = user.anon && !user.password && !$scope.registerForm.sent;

				if (activate)
					step.active = user.anon && !user.password && !$scope.registerForm.ready();

				step.complete = !user.anon || user.password || !!$scope.registerForm.ready();
			},

			'register_agreement': function (step, activate) {
				step.allow = user.anon && !user.password && $scope.registerForm.ready() && !$scope.registerForm.sent;

				if (activate)
					step.active = user.anon && !user.password && $scope.registerForm.ready() && !$scope.registerForm.sent;

				step.complete = !user.anon || user.password || !!$scope.registerForm.sent;
			},

			'register_email': function (step, activate) {
				step.allow = user.anon && !user.password && $scope.registerForm.sent;

				if (activate)
					step.active = user.anon && !user.password && $scope.registerForm.sent;

				step.complete = !user.anon || user.password;
			},

			'register_password': function (step, activate) {
				step.allow = user.anon && user.password && !$scope.passwordForm.sent;

				if (activate)
					step.active = user.anon && user.password && !$scope.passwordForm.sent;

				step.complete = !user.anon;
			},

			'register_agent': function (step, activate) {
				step.allow = !user.anon && !user.pending && !$scope.authApplyForm.sent;

				if (activate)
					step.active = !user.anon && !user.pending && !$scope.authSearchForm.data.party;

				step.complete = !user.anon && (user.pending || user.auth || !!$scope.authSearchForm.data.party);
			},

			'register_request': function (step, activate) {
				step.allow = !user.anon && !user.pending && $scope.authSearchForm.data.party && !$scope.authApplyForm.sent;

				if (activate)
					step.active = !user.anon && !user.pending && $scope.authSearchForm.data.party && !$scope.authApplyForm.sent;

				step.complete = !user.anon && (user.pending || user.auth || !!$scope.authApplyForm.sent);

				//

				$scope.authApplyForm.party = $scope.auth.user;

				if ($scope.authSearchForm.data.party && !$scope.authApplyForm.other)
					$scope.authApplyForm.other = {
						id: $scope.authSearchForm.data.party.id,
						party: $scope.authSearchForm.data.party,
						inherit: 0,
						direct: 0
					};
			},

			'register_pending': function (step, activate) {
				step.allow = !user.anon && !user.auth && (user.pending || !!$scope.authApplyForm.sent);

				if (activate)
					step.active = !user.anon && !user.auth && (user.pending || !!$scope.authApplyForm.sent);

				step.complete = user.auth;
			}
		};
	}]);
});
