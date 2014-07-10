'use strict';

module.controller('RegisterView', [
	'$scope', 'pageService', function ($scope, page) {
		page.display.title = page.constants.message('page.title.register');

		//

		$scope.auth = $scope.auth || page.auth;

		// TODO: Remove analytics
		$scope.$watch(function () {
			if (!$scope.wizard) {
				return;
			}

			for (var i = 0, l = $scope.wizard.steps.length; i < l; i++) {
				if ($scope.wizard.steps[i].active) {
					return $scope.wizard.steps[i]
				}
			}
		}, function (step) {
			if (!step) {
				return;
			}

			page.analytics.add('change', {
				type: 'wizard',
				id: step.id,
				name: step.name
			});

			page.models.Analytic.send();
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

			user.anon = !page.auth.isLoggedIn();
			user.password = page.auth.isPasswordPending();
			user.auth = page.auth.isAuthorized();

			angular.forEach($scope.wizard.steps, function (step) {
				$scope.updateStep[step.id](step, activate);
			});
		};

		//

		$scope.registerForm = {};
		$scope.userPasswordForm = {};
		$scope.authSearchForm = {};
		$scope.authApplyForm = {};
		$scope.infoForm = {};

		//

		var updateUserAuth = function () {
			page.models.PartyAuthorize.query({
				id: page.auth.user.id
			}, function (data) {
				angular.forEach(data.parents, function () {
					user.pending = true;
				});

				$scope.updateWizard();
			}, function (res) {
				page.messages.addError({
					body: page.constants.message('register.authquery.error'),
					report: res,
				})
			});
		};

		$scope.$watch('auth.user', function () {
			$scope.updateWizard();

			if (page.auth.isLoggedIn()) {
				updateUserAuth();
			}
		});

		//

		$scope.prepareStep = {
			'register-create': function (step) {
				$scope.registerForm = step.registerForm;
				$scope.registerForm.data = {};
				$scope.registerForm.sent = false;

				$scope.registerForm.ready = function () {
					return $scope.registerForm.$dirty &&
						$scope.registerForm.$valid &&
						$scope.registerForm.data.name &&
						$scope.registerForm.data.email;
				};

				$scope.registerForm.proceed = function () {
					$scope.updateWizard();
				};

				//

				$scope.registerForm.validator.client({
					fieldName: {
						tips: page.constants.message('register.name.help'),
					},
					fieldEmail: {
						errors: page.constants.message('register.email.error'),
						tips: page.constants.message('register.email.help'),
					},
					fieldAffiliation: {
						tips: page.constants.message('register.affiliation.help'),
					},
				}, true);
			},

			'register-agreement': function (step) {
				step.ready = function () {
					return step.agreementCheckbox;
				};

				step.proceed = function () {
					$scope.registerForm.data.agreement = true;

					page.$http
						.post('/register', $scope.registerForm.data)
						.success(function () {
							$scope.registerForm.sent = true;
							$scope.updateWizard();
						})
						.error(function (errors, status) {
							page.messages.addError({
								body: page.constants.message('error.generic'),
								errors: errors,
								status: status
							});
						});
				};
			},

			'register-email': function (step) {
				step.registerForm = $scope.registerForm;
			},

			'register-password': function (step) {
				$scope.userPasswordForm = step.userPasswordForm;
				$scope.userPasswordForm.sent = false;

				$scope.userPasswordForm.saveSuccessFn = function () {
					$scope.userPasswordForm.sent = true;
					$scope.updateWizard();
				};
			},

			'register-agent': function (step) {
				$scope.authSearchForm = step.authSearchForm;
				$scope.authSearchForm.data = {};

				$scope.authSearchForm.selectFn = function (found) {
					$scope.authSearchForm.data.party = found;
					$scope.infoForm.data.query = undefined;
					$scope.updateWizard();
				};

				$scope.authSearchForm.notFoundFn = function (query) {
					$scope.authSearchForm.data.party = true;
					$scope.infoForm.data.query = query;
					$scope.updateWizard();
				};
			},

			'register-request': function (step) {
				$scope.authApplyForm = step.authApplyForm;
				$scope.authApplyForm.sent = false;

				$scope.infoForm = step.infoForm;
				$scope.infoForm.data = {};

				step.ifInfo = function () {
					return angular.isString($scope.infoForm.data.query);
				};

				//

				$scope.infoForm.validator.client({
					info: {
						tips: page.constants.message('auth.request.info.help'),
					},
				}, true);

				//

				$scope.authApplyForm.successFn = function () {
					$scope.authApplyForm.sent = true;
					updateUserAuth();
					$scope.updateWizard();
				};

				$scope.authApplyForm.cancelFn = function () {
					$scope.authSearchForm.data.party = undefined;
					$scope.authApplyForm.other = undefined;
					$scope.updateWizard();
				};

				//

				$scope.infoForm.ready = function () {
					return $scope.infoForm.$dirty &&
						$scope.infoForm.$valid &&
						$scope.infoForm.data.info;
				};

				$scope.infoForm.proceed = function () {
					page.$http
						.get('/api/party/' + page.auth.user.id + '/authorize/search', {
							params: {
								apply: true,
								notfound: true,
								name: $scope.infoForm.data.query,
								info: $scope.infoForm.data.info
							}
						})
						.success(function () {
							$scope.authApplyForm.successFn();
						})
						.error(function (errors, status) {
							$scope.infoForm.messages.addError({
								body: page.constants.message('error.generic'),
								errors: errors,
								status: status
							});
						});
				};

				$scope.infoForm.cancel = function () {
					$scope.infoForm.data.info = '';
					$scope.authSearchForm.data.party = undefined;
					$scope.authApplyForm.other = undefined;
					$scope.infoForm.data.query = undefined;
					$scope.updateWizard();
				};
			},

			'register-pending': function () {
			}
		};

		//

		$scope.updateStep = {
			'register-create': function (step, activate) {
				step.allow = user.anon && !user.password && !$scope.registerForm.sent;

				if (activate && user.anon && !user.password && !$scope.registerForm.ready()) {
					$scope.wizard.activateStep(step);
				}

				step.complete = !user.anon || user.password || !!$scope.registerForm.ready() ? true : undefined;
			},

			'register-agreement': function (step, activate) {
				step.allow = user.anon && !user.password && $scope.registerForm.ready() && !$scope.registerForm.sent;

				if (activate && user.anon && !user.password && $scope.registerForm.ready() && !$scope.registerForm.sent) {
					$scope.wizard.activateStep(step);
				}

				step.complete = !user.anon || user.password || !!$scope.registerForm.sent ? true : undefined;
			},

			'register-email': function (step, activate) {
				step.allow = user.anon && !user.password && $scope.registerForm.sent;

				if (activate && user.anon && !user.password && $scope.registerForm.sent) {
					$scope.wizard.activateStep(step);
				}

				step.complete = !user.anon || user.password ? true : undefined;
			},

			'register-password': function (step, activate) {
				step.allow = user.anon && user.password && !$scope.userPasswordForm.sent;

				if (activate && user.anon && user.password && !$scope.userPasswordForm.sent) {
					$scope.wizard.activateStep(step);
				}

				step.complete = !user.anon ? true : undefined;
			},

			'register-agent': function (step, activate) {
				step.allow = !user.anon && !user.pending && !$scope.authApplyForm.sent;

				if (activate && !user.anon && !user.pending && !$scope.authSearchForm.data.party) {
					$scope.wizard.activateStep(step);
				}

				step.complete = !user.anon && (user.pending || user.auth || !!$scope.authSearchForm.data.party) ? true : undefined;
			},

			'register-request': function (step, activate) {
				step.allow = !user.anon && !user.pending && $scope.authSearchForm.data.party && !$scope.authApplyForm.sent;

				if (activate && !user.anon && !user.pending && $scope.authSearchForm.data.party && !$scope.authApplyForm.sent) {
					$scope.wizard.activateStep(step);
				}

				step.complete = !user.anon && (user.pending || user.auth || !!$scope.authApplyForm.sent) ? true : undefined;

				//

				$scope.authApplyForm.party = page.auth.user;

				step.authSearchForm = $scope.authSearchForm;

				if ($scope.authSearchForm.data.party) {
					$scope.authApplyForm.other = {
						id: $scope.authSearchForm.data.party.id,
						party: $scope.authSearchForm.data.party,
					};
				}
			},

			'register-pending': function (step, activate) {
				step.allow = !user.anon && !user.auth && (user.pending || !!$scope.authApplyForm.sent);

				if (activate && !user.anon && !user.auth && (user.pending || !!$scope.authApplyForm.sent)) {
					$scope.wizard.activateStep(step);
				}

				step.complete = user.auth ? true : undefined;
			}
		};
	}
]);
